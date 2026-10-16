.domain_default_sampled <- function(reference_data, coords = c("X", "Y")) {
  cannot_infer <- function(reason) {
    stop("Cannot infer a default sampling distance: ", reason,
      ". Supply `sampled` explicitly.", call. = FALSE)
  }
  if (!is.data.frame(reference_data) || nrow(reference_data) < 2L) {
    cannot_infer("at least two distinct reference cell centres are required")
  }
  if (!is.character(coords) || length(coords) != 2L || anyNA(coords) ||
      anyDuplicated(coords) || !all(coords %in% names(reference_data))) {
    cannot_infer("`coords` must name two reference coordinate columns")
  }
  for (nm in coords) {
    if (!is.numeric(reference_data[[nm]]) || !is.null(dim(reference_data[[nm]])) ||
        any(!is.finite(reference_data[[nm]]))) {
      cannot_infer("reference coordinates must be finite numeric columns")
    }
  }
  # Work in half-coordinate units to avoid overflowing squared grid distances.
  x <- reference_data[[coords[1L]]] / 2
  y <- reference_data[[coords[2L]]] / 2
  distance <- Inf
  for (i in seq_len(nrow(reference_data) - 1L)) {
    others <- seq.int(i + 1L, nrow(reference_data))
    dx <- abs(x[others] - x[i])
    dy <- abs(y[others] - y[i])
    scale <- pmax(dx, dy)
    distinct <- scale > 0
    if (any(distinct)) {
      lengths <- scale[distinct] * sqrt((dx[distinct] / scale[distinct])^2 +
        (dy[distinct] / scale[distinct])^2)
      distance <- min(distance, lengths)
    }
  }
  if (!is.finite(distance) || distance <= 0) {
    cannot_infer("at least two distinct cell centres with representable spacing are required")
  }
  warning("Default sampling distance is half the nearest distinct grid-centre spacing (",
    format(distance), " coordinate units); this is a heuristic. Supply `sampled` explicitly",
    " when it does not describe the sampling design.", call. = FALSE)
  list(coords = coords, distance = distance)
}

# Sampling is supplied explicitly; prediction covariates never define effort.
.domain_sampling <- function(data, reference_data, year, years, sampled,
    sampled_definition = c("cell_time", "cell_ever", "cell_always"),
    min_obs = 1, min_years = 1, weights = rep(1, nrow(reference_data)),
    core_threshold = 0.05) {
  if (!is.data.frame(data) || !is.data.frame(reference_data) ||
      nrow(reference_data) < 1L) {
    stop("`data` and non-empty `reference_data` must be data frames.", call. = FALSE)
  }
  if (!is.character(year) || length(year) != 1L || is.na(year) ||
      !nzchar(year) || !year %in% names(data)) {
    stop("`year` must name a column in `data`.", call. = FALSE)
  }
  valid_labels <- function(x) {
    (is.character(x) || is.numeric(x) || is.factor(x)) &&
      is.null(dim(x)) && !anyNA(x) &&
      (!is.numeric(x) || all(is.finite(x))) &&
      all(nzchar(as.character(x)))
  }
  if (!valid_labels(years) || !length(years) ||
      anyDuplicated(as.character(years))) {
    stop("`years` must contain unique, non-missing year labels.", call. = FALSE)
  }
  if (!valid_labels(data[[year]])) {
    stop("The `year` column must contain non-missing, finite year labels.", call. = FALSE)
  }
  definitions <- c("cell_time", "cell_ever", "cell_always")
  if (!is.character(sampled_definition) || !length(sampled_definition) ||
      anyNA(sampled_definition) || anyDuplicated(sampled_definition) ||
      !all(sampled_definition %in% definitions)) {
    stop("`sampled_definition` must select cell_time, cell_ever, or cell_always.", call. = FALSE)
  }
  scalar_number <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  }
  if (!scalar_number(min_obs) || min_obs < 1 || min_obs != floor(min_obs)) {
    stop("`min_obs` must be a positive whole number.", call. = FALSE)
  }
  ny <- length(years)
  nr <- nrow(reference_data)
  if (!scalar_number(min_years) || min_years <= 0 ||
      (min_years >= 1 && min_years != floor(min_years)) || min_years > ny) {
    stop("`min_years` must be a positive whole count no larger than the number of years, or a proportion in (0, 1).", call. = FALSE)
  }
  min_years <- as.integer(if (min_years < 1) ceiling(min_years * ny) else min_years)
  if (!is.numeric(weights) || !is.null(dim(weights)) || length(weights) != nr ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights < 0) ||
      !is.finite(sum(weights)) || sum(weights) <= 0) {
    stop("`weights` must be finite, non-negative reference-cell weights with positive total.", call. = FALSE)
  }
  if (!scalar_number(core_threshold) || core_threshold < 0 || core_threshold > 1) {
    stop("`core_threshold` must be a proportion between zero and one.", call. = FALSE)
  }
  if (missing(sampled) || is.null(sampled)) {
    stop("`sampled` is required: supply a shared cell-ID column, coordinates and distance, or a logical matrix.", call. = FALSE)
  }
  year_labels <- as.character(years)
  dims <- list(rownames(reference_data), year_labels)
  counts <- matrix(0L, nr, ny, dimnames = dims)
  year_index <- match(as.character(data[[year]]), year_labels)
  if (is.character(sampled) && length(sampled) == 1L && !is.na(sampled)) {
    if (!sampled %in% names(data) || !sampled %in% names(reference_data)) {
      stop("`sampled` must name a shared cell-ID column in data and reference_data.", call. = FALSE)
    }
    ids <- reference_data[[sampled]]
    if (!valid_labels(ids) || anyDuplicated(as.character(ids)) ||
        !valid_labels(data[[sampled]])) {
      stop("Cell IDs must be non-missing and unique in reference_data.", call. = FALSE)
    }
    cell_index <- match(as.character(data[[sampled]]), as.character(ids))
    for (j in seq_len(ny)) {
      counts[, j] <- tabulate(cell_index[which(year_index == j)], nbins = nr)
    }
  } else if (is.list(sampled) && !is.data.frame(sampled)) {
    if (is.null(names(sampled)) || anyDuplicated(names(sampled)) ||
        !setequal(names(sampled), c("coords", "distance")) ||
        !is.character(sampled$coords) || length(sampled$coords) != 2L ||
        anyNA(sampled$coords) || anyDuplicated(sampled$coords) ||
        !all(sampled$coords %in% names(data)) ||
        !all(sampled$coords %in% names(reference_data))) {
      stop("Coordinate sampling requires list(coords = c('X', 'Y'), distance = d) with shared coordinate columns.", call. = FALSE)
    }
    distance <- sampled$distance
    if (!scalar_number(distance) || distance < 0) {
      stop("Sampling `distance` must be finite and non-negative.", call. = FALSE)
    }
    for (nm in sampled$coords) {
      if (!is.numeric(data[[nm]]) || !is.numeric(reference_data[[nm]]) ||
          !is.null(dim(data[[nm]])) || !is.null(dim(reference_data[[nm]])) ||
          any(!is.finite(data[[nm]])) || any(!is.finite(reference_data[[nm]]))) {
        stop("Sampling coordinates must be finite numeric columns.", call. = FALSE)
      }
    }
    x <- sampled$coords[1L]
    y <- sampled$coords[2L]
    # Only one cell's observation distances are held, never a cell-by-observation array.
    for (j in seq_len(ny)) {
      obs <- which(year_index == j)
      for (i in seq_len(nr)) {
        dx <- abs(data[[x]][obs] - reference_data[[x]][i])
        dy <- abs(data[[y]][obs] - reference_data[[y]][i])
        if (distance == 0) {
          counts[i, j] <- sum(dx == 0 & dy == 0)
        } else {
          counts[i, j] <- sum((dx / distance)^2 + (dy / distance)^2 <= 1)
        }
      }
    }
  } else if (is.matrix(sampled) && is.logical(sampled)) {
    if (!identical(dim(sampled), c(nr, ny)) || anyNA(sampled)) {
      stop("The logical `sampled` matrix must be nrow(reference_data) by length(years), without NA.", call. = FALSE)
    }
    if (min_obs != 1) {
      stop("A logical `sampled` matrix is an indicator and requires min_obs = 1.", call. = FALSE)
    }
    for (axis in 1:2) {
      labels <- dimnames(sampled)[[axis]]
      if (!is.null(labels)) {
        if (anyNA(labels) || anyDuplicated(labels) ||
            !setequal(labels, dims[[axis]])) {
          stop("Logical `sampled` matrix dimnames must align with reference rows and years.", call. = FALSE)
        }
        if (axis == 1L) sampled <- sampled[match(dims[[axis]], labels), , drop = FALSE]
        else sampled <- sampled[, match(dims[[axis]], labels), drop = FALSE]
      }
    }
    counts[] <- as.integer(sampled)
  } else {
    stop("`sampled` must be a shared cell-ID column, a coordinate/distance list, or a logical matrix.", call. = FALSE)
  }
  cell_time <- counts >= min_obs
  ever <- rowSums(cell_time) > 0
  masks <- lapply(sampled_definition, function(definition) {
    switch(definition,
      cell_time = cell_time,
      cell_ever = matrix(rowSums(cell_time) >= min_years, nr, ny, dimnames = dims),
      cell_always = matrix(rowSums(cell_time) == ny, nr, ny, dimnames = dims))
  })
  names(masks) <- sampled_definition
  classification <- matrix(ifelse(ever, "previously_sampled", "never_sampled"),
    nr, ny, dimnames = dims)
  classification[cell_time] <- "sampled"
  classes <- c("sampled", "previously_sampled", "never_sampled")
  coverage <- do.call(rbind, lapply(seq_len(ny), function(j) {
    cw <- vapply(classes, function(cl) sum(weights[classification[, j] == cl]), numeric(1))
    data.frame(Year = rep(year_labels[j], length(classes)), class = classes,
      cells = vapply(classes, function(cl) sum(classification[, j] == cl), integer(1)),
      weight = unname(cw), share = unname(cw) / sum(weights), row.names = NULL)
  }))
  domains <- do.call(rbind, lapply(sampled_definition, function(definition) {
    mask <- masks[[definition]]
    dw <- colSums(mask * weights)
    data.frame(definition = definition, Year = year_labels,
      cells = unname(colSums(mask)), weight = unname(dw),
      share = unname(dw) / sum(weights),
      variable_domain = definition == "cell_time", row.names = NULL)
  }))
  for (definition in setdiff(sampled_definition, "cell_time")) {
    domain <- domains[domains$definition == definition, , drop = FALSE]
    if (domain$weight[1L] == 0) {
      warning("The fixed `", definition, "` sampled domain is empty or has zero weight.", call. = FALSE)
    } else if (domain$share[1L] < core_threshold) {
      warning("The fixed `", definition, "` sampled domain is small: weight share is below core_threshold.", call. = FALSE)
    }
  }
  list(counts = counts, cell_time = cell_time, masks = masks,
    class = classification, coverage = coverage, domains = domains,
    min_years = min_years)
}
