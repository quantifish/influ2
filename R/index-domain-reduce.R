.domain_reduce <- function(model, info, years, newdata, weights, classes,
    masks, cell_time, omit, where, covariate_reference, decompose, retain_cells,
    positive, reference_data, settings, uncertainty, ndraws,
    batch_size, draw_batch_size, seed, offset, sampled_only,
    omission_definition = settings$omission_definition %||% "cell_time",
    area = NULL) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  on.exit(if (is.null(old_seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  } else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  ny <- length(years)
  nc <- length(weights)
  if (!is.null(area) && (!is.numeric(area) || !is.null(dim(area)) ||
      length(area) != nc || any(!is.finite(area)) || any(area <= 0))) {
    stop("Native domain cell areas must align with positive-weight reference rows.",
      call. = FALSE)
  }
  if (identical(settings$method, "integrated")) {
    # Like integrate_index(), total_area is area times within-cell averaging
    # weight, not the unweighted sum of repeated seasonal cell-area rows.
    settings$total_area <- sum(weights)
    settings$parent_total_area <- sum(weights)
    settings$area_convention <- paste("sum(cell area * reference averaging weight);",
      "averaging weights are not normalised internally")
    if (!is.null(area)) {
      settings$parent_row_area <- sum(area)
      settings$row_area_interpretation <- paste("Sum of supplied cell areas over",
        "prediction rows; repeated within-cell strata repeat cell areas, so",
        "this is not necessarily unique geographic area")
    }
  }
  if (!is.character(omission_definition) || length(omission_definition) != 1L ||
      is.na(omission_definition) || !omission_definition %in%
      c("cell_time", "cell_ever", "cell_always")) {
    stop("`omission_definition` must select one sampling definition.", call. = FALSE)
  }
  omission_mask <- switch(omission_definition, cell_time = cell_time,
    cell_ever = masks$cell_ever %||%
      matrix(rowSums(cell_time) >= (settings$min_years %||% 1L), nc, ny),
    cell_always = masks$cell_always %||% matrix(rowSums(cell_time) == ny, nc, ny))
  nr <- if (uncertainty) ndraws + 1L else 1L
  class_names <- c("sampled", "previously_sampled", "never_sampled")
  blank <- function() matrix(0, nr, ny, dimnames = list(NULL, years))
  partition <- stats::setNames(lapply(class_names, function(x) blank()), class_names)
  definitions <- settings$sampled_definition %||% names(masks)
  restricted <- if (sampled_only) lapply(masks[definitions], function(x) blank()) else list()
  unsampled <- blank()
  specs <- list()
  for (term in omit) for (location in where) {
    definitions <- if (location == "unsampled") omission_definition else ""
    for (definition in definitions) {
      name <- paste(c(term, location, if (nzchar(definition)) definition), collapse = ":")
      specs[[name]] <- list(term = term, where = location, definition = definition)
    }
  }
  omission <- lapply(specs, function(x) blank())
  field_terms <- c("spatial", "spatiotemporal", "svc", "temporal", "iid",
    omit[grepl("^(svc|iid|temporal):", omit)])
  rules <- list()
  sampled_terms <- character()
  sampled_means <- list()
  sampled_weight <- colSums(cell_time * weights)
  moments <- new.env(parent = emptyenv())
  retained <- list()
  sampler <- NULL
  term_names <- NULL
  components <- NULL
  project <- function(d) {
    if (settings$backend == "gam") {
      .domain_gam_projection(model, d, year = info$year,
        field_terms = settings$gam_field_terms)
    } else .domain_projection(model, settings$backend, d, offset, year = info$year)
  }
  term_value <- function(value, term) {
    if (identical(term, "spatial") && !is.null(value$spatial)) {
      svc <- if (!is.null(value[["svc"]])) value["svc"] else
        value[startsWith(names(value), "svc:")]
      return(Reduce(`+`, c(value["spatial"], svc)))
    }
    if (!is.null(value[[term]])) return(value[[term]])
    pieces <- value[startsWith(names(value), paste0(term, ":"))]
    if (term %in% c("svc", "iid", "temporal") && length(pieces)) return(Reduce(`+`, pieces))
    stop("Unknown omission term: ", term, ".", call. = FALSE)
  }
  check_terms <- function(value, n) {
    if (!is.list(value) || !length(value) || is.null(names(value)) ||
        anyDuplicated(names(value)) || any(!nzchar(names(value)))) {
      stop("Native domain terms must be a named list.", call. = FALSE)
    }
    for (term in names(value)) {
      z <- value[[term]]
      if (!is.matrix(z) || !is.numeric(z) || nrow(z) != n ||
          ncol(z) < 1L || any(!is.finite(z))) {
        stop("Native domain terms or joint draws are non-finite or misaligned.", call. = FALSE)
      }
      if (is.null(colnames(z))) colnames(z) <- paste0("component", seq_len(ncol(z)))
      value[[term]] <- z
    }
    if (length(unique(vapply(value, ncol, integer(1)))) != 1L ||
        !all(vapply(value, function(z) identical(colnames(z), colnames(value[[1L]])), logical(1)))) {
      stop("Native domain term components do not align.", call. = FALSE)
    }
    if (is.null(term_names)) {
      aliases <- c("svc", "iid", "temporal")[vapply(c("svc", "iid", "temporal"),
        function(term) any(startsWith(names(value), paste0(term, ":"))), logical(1))]
      unknown <- setdiff(omit, c(names(value), aliases))
      if (length(unknown)) stop("Unknown omission term: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
      term_names <<- names(value)
      components <<- colnames(value[[1L]])
      covariates <- setdiff(omit, c(field_terms, "year"))
      rules <<- stats::setNames(lapply(covariates,
        function(term) .domain_rule(term, covariate_reference)), covariates)
      sampled_terms <<- names(rules)[vapply(rules,
        function(x) identical(x$rule, "sampled_mean"), logical(1))]
      for (term in sampled_terms) sampled_means[[term]] <<-
        array(0, c(nr, ny, length(components)))
    } else if (!identical(names(value), term_names) ||
        !identical(colnames(value[[1L]]), components)) {
      stop("Native domain terms differ between prediction blocks.", call. = FALSE)
    }
    value
  }
  add <- function(value, y, rows, mask = rep(TRUE, length(rows))) {
    .index_check_prediction(value, length(rows))
    out <- numeric(ny)
    sums <- rowsum(matrix(value * weights[rows] * mask, ncol = 1L), y, reorder = FALSE)
    out[as.integer(rownames(sums))] <- sums[, 1L]
    out
  }
  collect_terms <- function(value, y, rows, pass) {
    for (term in names(value)) for (component in seq_along(components)) {
      z <- value[[term]][, component]
      for (year in unique(y)) for (cl in c("all", class_names)) {
        ids <- which(y == year & (cl == "all" | classes[cbind(rows, y)] == cl))
        if (!length(ids)) next
        key <- paste(year, cl, term, component, sep = "\r")
        w <- weights[rows[ids]]
        v <- z[ids]
        if (pass == 1L) {
          m <- if (exists(key, moments, inherits = FALSE)) moments[[key]] else
            list(year = year, class = cl, term = term, component = components[component],
              weight = 0, mean = 0, m2 = 0, min = Inf, max = -Inf)
          # Merge weighted central moments, avoiding cancellation for large terms.
          sw <- sum(w)
          centre <- sum((w / sw) * v)
          delta <- centre - m$mean
          m$m2 <- m$m2 + sum(w * (v - centre)^2) +
            delta^2 * m$weight * sw / (m$weight + sw)
          m$mean <- m$mean + delta * sw / (m$weight + sw)
          m$weight <- m$weight + sw
          m$min <- min(m$min, v)
          m$max <- max(m$max, v)
        } else {
          m <- moments[[key]]
          width <- (m$max - m$min) / 1024
          bin <- if (width == 0) rep(1L, length(v)) else
            pmin(1024L, pmax(1L, floor((v - m$min) / width) + 1L))
          h <- rowsum(matrix(w, ncol = 1L), bin, reorder = FALSE)
          if (is.null(m$histogram)) m$histogram <- numeric(1024L)
          ids_bin <- as.integer(rownames(h))
          m$histogram[ids_bin] <- m$histogram[ids_bin] + h[, 1L]
        }
        moments[[key]] <- m
      }
      if (pass == 1L && length(retain_cells)) {
        ids <- which(years[y] %in% retain_cells)
        if (length(ids)) {
          d <- data.frame(Year = years[y[ids]], cell = positive[rows[ids]],
            term = term, component = components[component],
            contribution = z[ids], sampled = cell_time[cbind(rows[ids], y[ids])],
            stringsAsFactors = FALSE)
          for (variable in setdiff(names(reference_data), names(d))) {
            d[[variable]] <- reference_data[[variable]][rows[ids]]
          }
          retained[[length(retained) + 1L]] <<- d
        }
      }
    }
  }
  passes <- if (decompose || length(omit)) 2L else 1L
  for (pass in seq_len(passes)) {
    for (first in seq.int(1L, nc * ny, by = batch_size)) {
      index <- seq.int(first, min(nc * ny, first + batch_size - 1L))
      y <- as.integer((index - 1L) %/% nc + 1L)
      rows <- as.integer((index - 1L) %% nc + 1L)
      d <- newdata(y, rows)
      block <- project(d)
      alternatives <- list()
      tryCatch({
        mode_terms <- check_terms(block$terms(info$mode), length(rows))
        if (decompose || length(retain_cells)) {
          collect_terms(mode_terms, y, rows, pass)
        }
        if (pass == 2L) {
          if ("year" %in% omit) {
            alternatives$year <- project(newdata(rep(1L, length(rows)), rows))
          }
          for (term in names(rules)) if (identical(rules[[term]]$rule, "fixed")) {
            alternate <- d
            for (variable in names(rules[[term]]$value)) {
              if (!variable %in% names(alternate)) {
                stop("Fixed covariate reference names an unknown predictor: ", variable, ".", call. = FALSE)
              }
              value <- rules[[term]]$value[[variable]]
              if (is.factor(alternate[[variable]])) {
                if (!as.character(value) %in% levels(alternate[[variable]])) {
                  stop("Fixed covariate reference is not a fitted factor level.", call. = FALSE)
                }
                alternate[[variable]][] <- as.character(value)
              } else alternate[[variable]][] <- value
            }
            alternatives[[term]] <- project(alternate)
          }
        }
        evaluate <- function(par, draw, terms = NULL) {
          if (is.null(terms)) terms <- check_terms(block$terms(par), length(rows))
          if (pass == 1L) {
            value <- block$response(par)
            for (cl in class_names) partition[[cl]][draw, ] <<-
              partition[[cl]][draw, ] + add(value, y, rows, classes[cbind(rows, y)] == cl)
            for (definition in names(restricted)) restricted[[definition]][draw, ] <<-
              restricted[[definition]][draw, ] + add(value, y, rows, masks[[definition]][cbind(rows, y)])
            unsampled[draw, ] <<- unsampled[draw, ] + add(value, y, rows, !cell_time[cbind(rows, y)])
            for (term in sampled_terms) for (component in seq_along(components)) {
              sampled_means[[term]][draw, , component] <<-
                sampled_means[[term]][draw, , component] +
                add(terms[[term]][, component], y, rows, cell_time[cbind(rows, y)])
            }
          } else {
            replacements <- lapply(alternatives, function(a) check_terms(a$terms(par), length(rows)))
            for (name in names(specs)) {
              spec <- specs[[name]]
              term <- spec$term
              subtract <- term_value(terms, term)
              if (term %in% names(replacements)) {
                subtract <- subtract - replacements[[term]][[term]]
              } else if (term %in% sampled_terms) {
                for (component in seq_along(components)) {
                  target <- sampled_means[[term]][draw, y, component] / sampled_weight[y]
                  subtract[, component] <- subtract[, component] - target
                }
              }
              mask <- switch(spec$where, all = rep(TRUE, length(rows)),
                never_sampled = classes[cbind(rows, y)] == "never_sampled",
                unsampled = !omission_mask[cbind(rows, y)])
              if (any(!is.finite(subtract[mask, , drop = FALSE]))) {
                stop("A sampled-mean covariate reference has no sampled weight in a requested year.", call. = FALSE)
              }
              subtract[!mask, ] <- 0
              omission[[name]][draw, ] <<- omission[[name]][draw, ] +
                add(block$response(par, subtract = subtract), y, rows)
            }
          }
        }
        evaluate(info$mode, 1L, mode_terms)
        if (uncertainty && (pass == 1L || length(omit))) {
          if (is.null(sampler)) sampler <- if (settings$backend == "gam") {
            .domain_gam_sampler(info)
          } else .index_spatial_sampler(info, block$obj)
          set.seed(as.integer(seed))
          for (start in seq.int(1L, ndraws, by = draw_batch_size)) {
            ids <- seq.int(start, min(ndraws, start + draw_batch_size - 1L))
            parameters <- sampler(length(ids))
            if (any(!is.finite(parameters))) stop("Joint parameter draws are non-finite.", call. = FALSE)
            for (j in seq_along(ids)) evaluate(parameters[, j], ids[j] + 1L)
          }
        }
      }, finally = {
        for (a in alternatives) if (!is.null(a$obj)) .index_free_tmb(a$obj)
        if (!is.null(block$obj)) .index_free_tmb(block$obj)
      })
    }
  }
  full <- Reduce(`+`, partition)
  if (any(!is.finite(full)) ||
      any(vapply(partition, function(x) any(!is.finite(x)), logical(1))) ||
      any(vapply(omission, function(x) any(!is.finite(x)), logical(1)))) {
    stop("Domain predictions or joint draws are non-finite.", call. = FALSE)
  }
  build <- function(value, metadata = settings) {
    if (any(!is.finite(value) & !is.na(value))) {
      stop("Domain predictions or joint draws are non-finite.", call. = FALSE)
    }
    .domain_index(stats::setNames(value[1L, ], years),
      if (uncertainty) value[-1L, , drop = FALSE] else NULL, metadata)
  }
  restrict_index <- function(value, mask, definition, warn_empty = TRUE) {
    weight <- colSums(mask * weights)
    empty <- weight == 0
    if (any(empty) && warn_empty) warning("Empty ", definition, " domain in year(s): ",
      paste(years[empty], collapse = ", "), if (settings$method == "integrated")
        "; totals are zero." else "; means are undefined (NA).", call. = FALSE)
    if (settings$method != "integrated") {
      value <- sweep(value, 2L, ifelse(empty, NA_real_, weight), "/")
    }
    metadata <- settings
    metadata$domain_definition <- definition
    metadata$definition <- definition
    metadata$sampled_definition <- definition
    metadata$variable_domain <- definition %in% c("cell_time", "unsampled_cell_time")
    metadata$retained_weight <- stats::setNames(weight, years)
    metadata$retained_share <- stats::setNames(weight / sum(weights), years)
    if (identical(settings$method, "integrated")) {
      metadata$retained_area <- stats::setNames(weight, years)
      # Keep the ordinary index-target guard truthful: fixed restrictions have
      # their own scalar area, and genuinely varying areas retain annual labels.
      metadata$total_area <- if (all(weight == weight[1L])) unname(weight[1L]) else {
        stats::setNames(weight, years)
      }
      if (!is.null(area)) {
        metadata$retained_row_area <- stats::setNames(colSums(mask * area), years)
      }
    }
    metadata$reference <- paste(settings$reference, "; restricted to", definition)
    build(value, metadata)
  }
  sampled_indices <- lapply(names(restricted), function(definition) {
    restrict_index(restricted[[definition]], masks[[definition]], definition)
  })
  names(sampled_indices) <- names(restricted)
  unsampled_index <- restrict_index(unsampled, !cell_time, "unsampled_cell_time",
    warn_empty = FALSE)
  omission_indices <- lapply(names(omission), function(name) {
    metadata <- settings
    metadata$omitted_term <- specs[[name]]$term
    metadata$where <- specs[[name]]$where
    metadata$domain_definition <- specs[[name]]$definition
    metadata$omission_definition <- omission_definition
    if (identical(specs[[name]]$term, "spatial")) {
      metadata$omission_bundle <- c("spatial", "svc")
    }
    metadata$counterfactual <- "prediction target only; no refitting"
    build(omission[[name]], metadata)
  })
  names(omission_indices) <- names(omission)
  baseline <- build(full)
  partition_table <- do.call(rbind, lapply(class_names, function(cl) {
    index <- build(partition[[cl]])
    tab <- index$table[, c("Year", "Mean", "SD", "Qlower", "Qupper")]
    tab$class <- cl
    tab$share <- ifelse(full[1L, ] != 0, partition[[cl]][1L, ] / full[1L, ], NA_real_)
    interval <- vapply(seq_len(ny), function(y) {
      if (!uncertainty || any(full[-1L, y] == 0)) return(c(NA_real_, NA_real_))
      .domain_interval(partition[[cl]][-1L, y] / full[-1L, y], settings$probs)
    }, numeric(2))
    tab$share_lower <- interval[1L, ]
    tab$share_upper <- interval[2L, ]
    tab[, c("Year", "class", "Mean", "SD", "Qlower", "Qupper",
      "share", "share_lower", "share_upper")]
  }))
  unsampled_partition <- partition$previously_sampled + partition$never_sampled
  unsampled_intervals <- vapply(seq_len(ny), function(y) {
    if (!uncertainty || any(full[-1L, y] == 0)) return(c(NA_real_, NA_real_))
    .domain_interval(unsampled_partition[-1L, y] / full[-1L, y], settings$probs)
  }, numeric(2))
  unsampled_share <- data.frame(Year = years,
    share = ifelse(full[1L, ] != 0, unsampled_partition[1L, ] / full[1L, ], NA_real_),
    lower = unsampled_intervals[1L, ], upper = unsampled_intervals[2L, ],
    definition = "cell_time", variable_domain = TRUE)
  decomposition <- NULL
  if (decompose) {
    groups <- expand.grid(year = seq_len(ny), class = c("all", class_names),
      term = term_names, component = seq_along(components),
      stringsAsFactors = FALSE)
    decomposition <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
      g <- groups[i, ]
      key <- paste(g$year, g$class, g$term, g$component, sep = "\r")
      if (!exists(key, moments, inherits = FALSE)) {
        return(data.frame(Year = years[g$year], class = g$class, term = g$term,
          component = components[g$component], mean = NA_real_, Mean = NA_real_,
          SD = NA_real_, Q10 = NA_real_, Q50 = NA_real_, Q90 = NA_real_,
          Qlower = NA_real_, Qupper = NA_real_, bin_width = NA_real_))
      }
      m <- moments[[key]]
      width <- (m$max - m$min) / 1024
      quantile <- function(p) {
        if (width == 0) return(m$min)
        cumulative <- cumsum(m$histogram)
        total <- utils::tail(cumulative, 1L)
        # Use the histogram's own total, and resolve floating-point ties at
        # a probability boundary consistently. Otherwise an exactly 10%-mass
        # bin can be skipped after a different blocking order, jumping across
        # an arbitrarily large run of empty bins.
        tolerance <- 128 * .Machine$double.eps * total
        bin <- which(cumulative >= p * total - tolerance)[1L]
        if (is.na(bin)) bin <- 1024L
        m$min + (bin - 0.5) * width
      }
      data.frame(Year = years[m$year], class = m$class, term = m$term,
        component = m$component, mean = m$mean, Mean = m$mean,
        SD = sqrt(m$m2 / m$weight), Q10 = quantile(0.1), Q50 = quantile(0.5),
        Q90 = quantile(0.9), Qlower = quantile(settings$probs[1L]),
        Qupper = quantile(settings$probs[2L]), bin_width = width)
    }))
  }
  comparisons <- c(sampled_indices, list(unsampled_only = unsampled_index), omission_indices)
  omission_summary <- do.call(rbind, lapply(names(comparisons), function(name) {
    x <- comparisons[[name]]
    valid <- is.finite(x$table$Mean) & is.finite(baseline$table$Mean) &
      baseline$table$Mean != 0
    ratio <- log_difference <- rep(NA_real_, ny)
    ratio[valid] <- x$table$Mean[valid] / baseline$table$Mean[valid]
    valid <- valid & x$table$Mean > 0 & baseline$table$Mean > 0
    log_difference[valid] <- log(x$table$Mean[valid]) - log(baseline$table$Mean[valid])
    intervals <- vapply(seq_len(ny), function(y) {
      if (!uncertainty) return(rep(NA_real_, 4L))
      a <- x$draws[, y]
      b <- baseline$draws[, y]
      if (any(!is.finite(c(a, b))) || any(b == 0)) return(rep(NA_real_, 4L))
      if (any(c(a, b) <= 0)) {
        return(c(.domain_interval(a / b, settings$probs), NA_real_, NA_real_))
      }
      c(.domain_interval(a / b, settings$probs),
        .domain_interval(log(a) - log(b), settings$probs))
    }, numeric(4))
    data.frame(series = name, Year = years, ratio = ratio,
      ratio_lower = intervals[1L, ], ratio_upper = intervals[2L, ],
      log_difference = log_difference, log_difference_lower = intervals[3L, ],
      log_difference_upper = intervals[4L, ])
  }))
  retained_cells <- NULL
  if (length(retained)) {
    retained_cells <- do.call(rbind, retained)
    retained_cells <- retained_cells[order(match(retained_cells$Year, years),
      match(retained_cells$term, term_names), match(retained_cells$component, components),
      retained_cells$cell), , drop = FALSE]
    rownames(retained_cells) <- NULL
  }
  list(baseline = baseline, sampled_only = sampled_indices,
    unsampled_only = unsampled_index, omission = omission_indices,
    omission_summary = omission_summary,
    sampled_only_summary = omission_summary[
      omission_summary$series %in% names(sampled_indices), , drop = FALSE],
    comparison_summary = omission_summary,
    partition = partition_table, unsampled_share = unsampled_share,
    partition_draws = lapply(partition, function(x) if (uncertainty)
      x[-1L, , drop = FALSE] else NULL), decomposition = decomposition,
    retained_cells = retained_cells)
}
