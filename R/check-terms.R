# Model-term inventories are descriptive: screening raw predictors is not a
# test of an individual basis coefficient, random-effect variance, or field.
.check_term_backend <- function(model) {
  classes <- c(brms = "brmsfit", tinyVAST = "tinyVAST", sdmTMB = "sdmTMB",
    glmmTMB = "glmmTMB", lme4 = "merMod", gam = "gam", glm = "glm", lm = "lm")
  found <- names(classes)[vapply(classes, function(x) inherits(model, x), logical(1))]
  if (!length(found)) return("unsupported")
  found[1L]
}

.check_term_formulae <- function(model, backend) {
  formulae <- switch(backend,
    brms = {
      f <- model$formula
      if (inherits(f, "mvbrmsformula")) return(list())
      c(list(mu = f$formula), f$pforms %||% list())
    },
    sdmTMB = {
      f <- model$formula
      if (inherits(f, "formula")) f <- list(f)
      names(f) <- if (isTRUE(model$family$delta) && length(f) == 2L)
        c("encounter", "positive") else paste0("conditional", seq_along(f))
      f
    },
    tinyVAST = {
      main <- model$formula
      delta <- any(vapply(model$internal$family %||% list(),
        function(x) isTRUE(x$delta), logical(1)))
      f <- list(main)
      if (delta) {
        extra <- model$internal$delta_formula %||% model$internal$delta_gam_setup$formula %||%
          model$delta_options$formula %||% model$internal$delta_options$formula
        # The native delta GAM setup may retain pterms rather than formula.
        if (is.null(extra) && !is.null(model$internal$delta_gam_setup$pterms))
          extra <- stats::formula(model$internal$delta_gam_setup$pterms)
        if (is.null(extra)) extra <- main
        f <- list(encounter = main, positive = extra)
      } else names(f) <- "conditional"
      f
    },
    glmmTMB = {
      out <- lapply(c("cond", "zi", "disp"), function(component)
        tryCatch(stats::formula(model, component = component), error = function(e) NULL))
      names(out) <- c("conditional", "zero_inflation", "dispersion")
      out
    },
    list(conditional = tryCatch(stats::formula(model), error = function(e) NULL)))
  formulae[vapply(formulae, inherits, logical(1), "formula")]
}

.check_term_random_calls <- function(expression) {
  if (!is.call(expression)) return(list())
  if (as.character(expression[[1L]])[1L] %in% c("|", "||")) return(list(expression))
  if (as.character(expression[[1L]])[1L] %in% c("ar1", "hetar1", "cs", "diag", "us", "rr", "ou", "exp", "gau", "mat", "homdiag", "propto") &&
      length(unlist(lapply(as.list(expression)[-1L], .check_term_random_calls), recursive = FALSE)))
    return(list(expression))
  unlist(lapply(as.list(expression)[-1L], .check_term_random_calls), recursive = FALSE)
}

.check_term_without_random <- function(expression) {
  if (!is.call(expression)) return(expression)
  if (as.character(expression[[1L]])[1L] %in% c("|", "||")) return(1)
  if (as.character(expression[[1L]])[1L] %in% c("ar1", "hetar1", "cs", "diag", "us", "rr", "ou", "exp", "gau", "mat", "homdiag", "propto") &&
      length(.check_term_random_calls(expression))) return(1)
  expression[-1L] <- lapply(as.list(expression)[-1L], .check_term_without_random)
  expression
}

.check_term_inventory <- function(model, data = NULL) {
  backend <- .check_term_backend(model)
  forms <- .check_term_formulae(model, backend)
  rows <- list()
  add <- function(term, term_type, variables = character(), component = "conditional") {
    rows[[length(rows) + 1L]] <<- data.frame(term = term,
      term_type = term_type, component = component,
      variables = I(list(unique(variables))), stringsAsFactors = FALSE)
  }
  response <- unique(unlist(lapply(forms, function(f)
    if (length(f) == 3L) all.vars(f[[2L]]) else character())))
  for (component in names(forms)) {
    f <- forms[[component]]
    rhs <- f[[length(f)]]
    random <- .check_term_random_calls(rhs)
    fixed <- f
    fixed[[length(fixed)]] <- .check_term_without_random(rhs)
    tt <- tryCatch(stats::terms(fixed, keep.order = TRUE), error = function(e) NULL)
    labels <- attr(tt, "term.labels") %||% character()
    for (label in labels) {
      expr <- tryCatch(str2lang(label), error = function(e) NULL)
      variables <- if (is.null(expr)) character() else all.vars(expr)
      type <- if (grepl("(^|[^[:alnum:]_.])(s|te|ti|t2)\\(", label)) {
        if (grepl("bs\\s*=\\s*[\"']re[\"']", label)) "random" else "smooth"
      } else if (is.call(expr) && identical(expr[[1L]], as.name(":"))) {
        "interaction"
      } else if (length(variables) > 1L) "multivariable" else "fixed"
      add(label, type, variables, component)
    }
    offsets <- attr(tt, "offset") %||% integer()
    for (i in offsets) {
      expr <- attr(tt, "variables")[[i + 1L]]
      add(paste(deparse(expr), collapse = " "), "offset", all.vars(expr), component)
    }
    for (r in random) {
      # For an intercept-only random term, screen its grouping variable. A
      # random slope retains all variables and is not flattened to one column.
      add(paste0("(", paste(deparse(r), collapse = " "), ")"),
        "random", all.vars(r), component)
    }
  }
  if (backend == "sdmTMB") {
    components <- if (isTRUE(model$family$delta)) c("encounter", "positive") else "conditional1"
    present <- function(x) !is.null(x) && any(!is.na(x) & !x %in% c("off", "none", "0", "FALSE"))
    flag_component <- function(x, k) {
      if (length(x) == length(components)) x[k] else x
    }
    for (k in seq_along(components)) {
      component <- components[k]
      if (present(flag_component(model$spatial, k))) add("spatial_field", "spatial", character(), component)
      if (present(flag_component(model$spatiotemporal, k))) add("spatiotemporal_field", "spatiotemporal", model$time %||% character(), component)
      if (!is.null(model$time_varying)) add("time_varying_effects", "process", all.vars(model$time_varying), component)
      if (!is.null(model$spatial_varying)) add("spatially_varying_effects", "spatial", all.vars(model$spatial_varying), component)
      if (!is.null(model$nonlocal_parsed)) add("nonlocal_effects", "process", character(), component)
      if (!is.null(model$threshold_parameter)) add("threshold_effects", "process", character(), component)
    }
  }
  if (backend == "tinyVAST") {
    for (component in names(forms)) {
      info <- model$internal
      prefix <- if (component == "positive") "delta_" else ""
      process_present <- function(name) {
        native <- info[[paste0(prefix, name, "_ram")]]$output$ram
        if (is.matrix(native) || is.data.frame(native)) return(nrow(native) > 0L)
        value <- info[[paste0(prefix, name)]]
        if (is.character(value)) return(any(!is.na(value) & nzchar(value)))
        !is.null(value) && length(value) > 0L
      }
      if (process_present("space_term"))
        add("spatial_field", "spatial", info$space_columns %||% character(), component)
      if (process_present("spacetime_term"))
        add("spatiotemporal_field", "spatiotemporal", c(info$space_columns, info$time_column), component)
      if (process_present("time_term"))
        add("temporal_process", "process", info$time_column %||% character(), component)
      varying <- info[[paste0(prefix, "spatial_varying")]]
      columns <- info[[paste0(prefix, "SVC")]]$W_il
      has_varying <- if (is.matrix(columns)) ncol(columns) > 0L else {
        if (inherits(varying, "formula")) {
          tt <- stats::terms(varying)
          length(attr(tt, "term.labels")) > 0L || attr(tt, "intercept") == 1L
        } else !is.null(varying)
      }
      if (has_varying)
        add("spatially_varying_effects", "spatial", if (inherits(varying, "formula")) all.vars(varying) else character(), component)
    }
  }
  if (!length(rows)) {
    if (length(forms)) add("(Intercept)", "intercept", character(), names(forms)[1L]) else
      add("model_terms", "unavailable", character(), "unknown")
  }
  term_data <- do.call(rbind, rows)
  term_data <- term_data[!duplicated(paste(term_data$component, term_data$term)), , drop = FALSE]
  rownames(term_data) <- NULL
  frame <- .residual_model_frame(model)
  data_error <- NULL
  fitted_data <- NULL
  if (!is.null(frame)) {
    raw_vars <- unique(unlist(term_data$variables))
    if (is.null(data) && all(raw_vars %in% names(frame))) {
      fitted_data <- frame
    } else {
      aligned <- tryCatch(.residual_observations(model, data, seq_len(nrow(frame))),
        error = function(e) e)
      if (inherits(aligned, "error")) {
        if (!is.null(data)) stop(conditionMessage(aligned), call. = FALSE)
        data_error <- conditionMessage(aligned)
        # The retained frame remains authoritative for any raw columns it has.
        fitted_data <- frame
      } else fitted_data <- aligned$data
    }
  } else data_error <- "The fitted observation frame is unavailable; supplied data cannot be aligned safely."
  list(data = fitted_data, terms = term_data, backend = backend,
    response_variables = response, data_error = data_error)
}

.check_term_gam_summaries <- function(model, inventory, alpha) {
  if (inventory$backend != "gam") return(list())
  value <- tryCatch(summary(model), error = function(e) NULL)
  table <- value$s.table
  if (is.null(table) || !nrow(table)) return(list())
  lapply(seq_len(nrow(table)), function(i) {
    label <- rownames(table)[i]
    j <- match(label, vapply(model$smooth, function(s) s$label, character(1)))
    vars <- if (is.na(j)) character() else {
      smooth <- model$smooth[[j]]
      c(smooth$term, if (!is.null(smooth$by) && !identical(smooth$by, "NA")) smooth$by else character())
    }
    recorded <- which(inventory$terms$term_type %in% c("smooth", "random") &
      vapply(inventory$terms$variables, function(x) setequal(x, vars), logical(1)))
    term <- if (length(recorded) == 1L) inventory$terms$term[recorded] else label
    p <- as.numeric(table[i, ncol(table)])
    .cpue_check_row(scope = "term", term = term, check = "smooth_summary",
      status = "not_applicable", statistic = as.numeric(table[i, 3L]),
      df = as.numeric(table[i, 1L]), p_value = p,
      method = paste0("Native mgcv approximate smooth summary for ", label,
        "; statistic is ", colnames(table)[3L], "; df is effective degrees of freedom"),
      message = paste("Native EDF and approximate smooth p-value describe the fitted effect, not residual adequacy.",
        "A small p-value is not a model problem; uncertainty in smoothing parameters and basis choice matters."),
      action = "Inspect the fitted smooth, basis dimension, and residual structure; do not use significance as an automatic selection rule.",
      component = "conditional", conditioning = "native fitted smooth",
      calibration = "native approximate fitted-effect inference; not a residual test", n = stats::nobs(model))
  })
}

.check_terms <- function(model, residuals, data = NULL, terms = NULL, alpha = 0.05) {
  inventory <- .check_term_inventory(model, data)
  selected <- inventory$terms
  if (!is.null(terms)) {
    if (!is.character(terms) || anyNA(terms) || any(!nzchar(terms)) || anyDuplicated(terms))
      stop("`terms` must be distinct non-empty recorded model-term names.", call. = FALSE)
    missing <- setdiff(terms, selected$term)
    if (length(missing)) stop("Unknown model terms: ", paste(missing, collapse = ", "), ".", call. = FALSE)
    selected <- selected[selected$term %in% terms, , drop = FALSE]
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be a finite number between zero and one.", call. = FALSE)
  rows <- list()
  md <- if (inherits(residuals, "influ_residuals")) residuals$metadata else list()
  component <- md$component %||% "not calculated"
  conditioning <- md$conditioning %||% md$scheme %||% "not calculated"
  limitation <- paste("Exploratory fitted-data screen, not a test of the term's coefficients or importance.",
    "Nominal reference p-values do not account for fitting, posterior-data reuse, or residual dependence.")
  aligned <- inventory$data
  obs <- if (inherits(residuals, "influ_residuals")) residuals$observations else NULL
  if (!is.null(obs)) {
    ids <- as.character(obs$row)
    if (!length(ids) || anyNA(ids) || anyDuplicated(ids) || is.null(aligned) ||
        anyDuplicated(rownames(aligned)) || !all(ids %in% rownames(aligned)))
      stop("Residual observation IDs cannot be aligned safely with the fitted model data.", call. = FALSE)
    aligned <- aligned[match(ids, rownames(aligned)), , drop = FALSE]
    # Component diagnostics can legitimately retain a positive-only subset,
    # but response-defined grouping is still prohibited.
    if (!is.numeric(obs$pit) || length(obs$pit) != nrow(aligned) || any(!is.finite(obs$pit)) ||
        any(obs$pit < 0 | obs$pit > 1))
      stop("Stored residual PIT values must be finite, uniform-scale values aligned to the observation IDs.", call. = FALSE)
  }
  term_component <- "unknown"
  add <- function(term, check = "residual_structure", status = "not_assessed",
      statistic = NA_real_, df = NA_real_, p = NA_real_, method = "Not calculated",
      message = "", action = "Inspect the term-specific diagnostic and its support.", n = if (is.null(obs)) NA_integer_ else nrow(obs)) {
    rows[[length(rows) + 1L]] <<- .cpue_check_row(scope = "term", term = term,
      check = check, status = status, statistic = statistic, df = df, p_value = p,
      method = method, message = paste(message, "Residual target:", component, "response."),
      action = action, component = term_component,
      conditioning = conditioning, calibration = "exploratory; fitted-model calibration not established", n = n)
  }
  screened <- character()
  for (i in seq_len(nrow(selected))) {
    term <- selected$term[i]
    term_component <- selected$component[i]
    type <- selected$term_type[i]
    vars <- selected$variables[[i]]
    if (type == "intercept") {
      add(term, status = "not_applicable", message = "An intercept-only model has no predictor-specific residual screen. Assess its response distribution and overall calibration.")
      next
    }
    if (type == "offset") {
      add(term, status = "not_applicable", message = "An offset is fixed exposure, not an estimated term. Verify its definition and units separately.")
      next
    }
    if (is.null(obs)) {
      add(term, message = paste("No stored residuals were supplied; no term-specific residual check was run.",
        inventory$data_error %||% ""))
      next
    }
    if (length(intersect(vars, inventory$response_variables))) {
      add(term, message = "A response variable appears in this predictor/group; no outcome-defined residual screen is valid.")
      next
    }
    if (type %in% c("spatial", "spatiotemporal", "process", "unavailable")) {
      add(term, message = "This latent/multivariate process needs a component-aware spatial or temporal dependence check; no coefficient or arbitrary coordinate proxy test is substituted.")
      next
    }
    if (type %in% c("interaction", "multivariable") || length(vars) != 1L) {
      add(term, message = "This term has multiple predictors or an interaction; a univariate raw-predictor screen would not assess it. Inspect a joint or conditional display instead.")
      next
    }
    if (!selected$component[i] %in% c("conditional", "conditional1", "mu", "encounter", "positive") ||
        (selected$component[i] %in% c("encounter", "positive") &&
          !identical(component, selected$component[i]))) {
      add(term, message = paste("Residuals describe the", component,
        "response, not this", selected$component[i], "component. Calculate its own supported residual diagnostic; no combined-response substitute is made."))
      next
    }
    variable <- vars[1L]
    if (!variable %in% names(aligned)) {
      add(term, message = paste("The underlying raw predictor", variable,
        "is unavailable in safely aligned fitted data; transformed basis columns are not substituted.",
        inventory$data_error %||% ""))
      next
    }
    if (variable %in% screened) {
      add(term, message = paste("The raw predictor", variable,
        "has already been screened for another term. Repeating that proxy would not independently assess this term."))
      next
    }
    x <- aligned[[variable]]
    if (!is.null(dim(x)) || !(is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x))) {
      add(term, message = "This predictor is a matrix or unsupported type; basis columns and arbitrary codes are not flattened.")
      next
    }
    keep <- !is.na(x) & is.finite(obs$pit)
    if (is.numeric(x)) keep <- keep & is.finite(x)
    x <- x[keep]
    pit <- obs$pit[keep]
    if (length(x) < 10L || length(unique(x)) < 2L || length(unique(pit)) < 2L) {
      add(term, message = "Fewer than ten usable observations, a constant predictor, or constant residuals; no significance screen is calculated.", n = length(x))
      next
    }
    categorical <- type == "random" || grepl("^([[:alnum:]_.]+::)?(factor|as.factor|ordered)\\(", term)
    if (is.numeric(x) && !is.factor(x) && !categorical) {
      value <- suppressWarnings(stats::cor.test(x, pit, method = "spearman", exact = FALSE))
      add(term, check = "rank_association", status = if (value$p.value < alpha) "review" else "no_flag",
        statistic = as.numeric(value$estimate), p = value$p.value,
        method = "Spearman raw-predictor/PIT association; statistic is rho, nominal large-sample p-value",
        message = paste(limitation, "This assesses monotone residual association with", variable,
          "rather than the complete fitted basis, nonlinear shape, or interaction."), n = length(x))
    } else {
      group <- droplevels(factor(x))
      counts <- table(group)
      support <- paste(length(counts), "groups; group counts range from", min(counts), "to", max(counts), "records.")
      if (any(counts < 5L) || length(counts) > 40L) {
        add(term, message = paste("Sparse groups (fewer than five records) or more than forty groups; no pooled rank/location screen is calculated.",
          support, "Review group support and a readable grouped display."), n = length(x))
        next
      }
      value <- stats::kruskal.test(pit, group)
      add(term, check = "group_location", status = if (value$p.value < alpha) "review" else "no_flag",
        statistic = as.numeric(value$statistic), df = as.numeric(value$parameter), p = value$p.value,
        method = "Kruskal-Wallis rank/location screen on uniform PIT; not DHARMa testCategorical",
        message = paste(limitation, "Tests rank/location differences across", length(counts),
          "supported groups; not a dispersion test or a test of random-effect variance.", support), n = length(x))
    }
    screened <- c(screened, variable)
  }
  native <- .check_term_gam_summaries(model, inventory, alpha)
  if (!is.null(terms)) native <- Filter(function(x) x$term %in% terms, native)
  rows <- c(rows, native)
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- .cpue_check_row(check = "residual_structure")[FALSE, , drop = FALSE]
  residual_rows <- out$check %in% c("rank_association", "group_location") & is.finite(out$p_value)
  out$p_adjusted[residual_rows] <- stats::p.adjust(out$p_value[residual_rows], "holm")
  out$status[residual_rows] <- ifelse(out$p_adjusted[residual_rows] < alpha, "review", "no_flag")
  out$method[residual_rows] <- paste0(out$method[residual_rows], "; Holm across ", sum(residual_rows), " term screens")
  rownames(out) <- NULL
  attr(out, "term_inventory") <- selected
  attr(out, "p_adjustment") <- "Holm across calculated term residual screens; native GAM fitted-effect p-values excluded"
  out
}
