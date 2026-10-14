# Search specifications retain actual formulas only while fitting. Public search
# results record text and IDs, never these formulas' enclosing environments.
.search_term_key <- function(term) {
  expression <- tryCatch(str2lang(term), error = function(e) NULL)
  if (is.null(expression)) stop("Invalid model-search term: ", term, ".", call. = FALSE)
  while (is.call(expression) && identical(expression[[1L]], as.name("("))) expression <- expression[[2L]]
  paste(deparse(expression, width.cutoff = 500L), collapse = " ")
}

.search_formula_terms <- function(formula) {
  tt <- stats::terms(formula, keep.order = TRUE)
  labels <- attr(tt, "term.labels") %||% character()
  offsets <- .step_offset_terms(formula)
  unique(vapply(c(labels, offsets), .search_term_key, character(1)))
}

.search_term_type <- function(term) {
  expression <- str2lang(term)
  name <- if (is.call(expression)) as.character(expression[[1L]])[1L] else ""
  if (name == "offset") return("offset")
  if (length(.check_term_random_calls(expression))) return("random")
  if (name %in% c("s", "te", "ti", "t2")) {
    if (grepl("bs\\s*=\\s*[\"']re[\"']", term)) return("random")
    variables <- all.vars(expression)
    return(if (length(variables) > 1L) "multivariable_smooth" else "smooth")
  }
  if (name == ":") return("interaction")
  "fixed"
}

.search_scope_formula <- function(base, supplied, original_offsets, label) {
  if (!inherits(supplied, "formula")) stop("`", label, "` must be a formula.", call. = FALSE)
  f <- stats::update.formula(base, supplied)
  if (!identical(f[[2L]], base[[2L]])) stop("Model search must retain the original response.", call. = FALSE)
  if (!identical(attr(stats::terms(f), "intercept"), attr(stats::terms(base), "intercept")))
    stop("Model search must retain the original intercept convention.", call. = FALSE)
  offsets <- .step_offset_terms(f)
  keys <- vapply(offsets, .search_term_key, character(1))
  original_keys <- vapply(original_offsets, .search_term_key, character(1))
  if (length(setdiff(keys, original_keys)))
    stop("Model search must preserve the original offset expressions; changing exposure is not supported.", call. = FALSE)
  missing <- original_offsets[!original_keys %in% keys]
  if (length(missing)) f <- stats::update.formula(f,
    stats::as.formula(paste("~ . +", paste(missing, collapse = " + "))))
  environment(f) <- environment(base)
  f
}

.search_formula_from_terms <- function(base, terms) {
  intercept <- attr(stats::terms(base), "intercept")
  rhs <- if (length(terms)) paste(paste0("(", terms, ")"), collapse = " + ") else as.character(intercept)
  if (length(terms) && intercept == 0L) rhs <- paste("0 +", rhs)
  stats::as.formula(paste(paste(deparse(base[[2L]]), collapse = " "), "~", rhs),
    env = environment(base))
}

.search_native_annual_structure <- function(model, backend, year, component_index) {
  if (backend == "sdmTMB") {
    if (!identical(model$time, year)) return(FALSE)
    value <- model$spatiotemporal
    if (length(value) == 2L && isTRUE(model$family$delta)) value <- value[component_index]
    field <- !is.null(value) && any(!is.na(value) & !as.character(value) %in% c("off", "none", "0", "FALSE", "false"))
    varying <- model$time_varying
    varying <- if (inherits(varying, "formula")) {
      tt <- stats::terms(varying)
      length(attr(tt, "term.labels")) > 0L || attr(tt, "intercept") == 1L
    } else !is.null(varying) && length(varying) > 0L
    return(field || varying)
  }
  if (backend == "tinyVAST") {
    info <- model$internal
    if (!identical(info$time_column, year)) return(FALSE)
    present <- function(name) {
      ram <- info[[paste0(name, "_ram")]]$output$ram
      if (is.matrix(ram) || is.data.frame(ram)) return(nrow(ram) > 0L)
      value <- info[[name]]
      if (inherits(value, "formula")) {
        tt <- stats::terms(value)
        return(length(attr(tt, "term.labels")) > 0L || attr(tt, "intercept") == 1L)
      }
      if (is.character(value)) return(any(!is.na(value) & nzchar(value) & !value %in% c("off", "none", "0", "FALSE", "false")))
      !is.null(value) && length(value) > 0L && !identical(value, FALSE)
    }
    return(present("time_term") || present("spacetime_term"))
  }
  FALSE
}

.search_prepare_spec <- function(model, year = NULL, scope = NULL,
    include = NULL, optional = NULL, component = NULL, blocks = NULL) {
  backend <- .step_backend(model)
  base_formula <- .step_main_formula(model, backend)
  forms <- .step_formulas(base_formula)
  paired <- backend == "sdmTMB" && length(forms) == 2L
  if (paired) {
    if (is.null(component)) stop("Select `component = 'occurrence'` or 'positive' for a joint sdmTMB search; the other component stays fixed.", call. = FALSE)
    if (identical(component, "encounter")) component <- "occurrence"
    if (!is.character(component) || length(component) != 1L || is.na(component) ||
        !component %in% c("occurrence", "positive"))
      stop("Joint sdmTMB search requires one component: 'occurrence' or 'positive'.", call. = FALSE)
    component_index <- match(component, c("occurrence", "positive"))
  } else {
    if (!is.null(component) && (!is.character(component) || length(component) != 1L ||
        is.na(component) || !component %in% c("conditional", "cond", "mu", "mean")))
      stop("This search changes the main mean formula only; auxiliary components stay fixed.", call. = FALSE)
    component <- "conditional"
    component_index <- 1L
  }
  original <- forms[[component_index]]
  original_terms <- .search_formula_terms(original)
  original_offsets <- .step_offset_terms(original)
  if (is.null(year)) {
    native_time <- if (backend == "sdmTMB") model$time else if (backend == "tinyVAST") model$internal$time_column else NULL
    predictors <- all.vars(original[[3L]])
    familiar_time <- predictors[grepl("(^|[_.])f?year($|[_.])|fishing.?year|^fy$|^yr$", predictors, ignore.case = TRUE)]
    # Some spatial backends fabricate a placeholder time column when no time
    # process was fitted. Such a placeholder must not displace a real annual
    # predictor. A native time-only process remains valid when no fixed annual
    # block is available.
    year <- if (length(familiar_time)) familiar_time[1L] else if (
        is.character(native_time) && length(native_time) == 1L && !is.na(native_time) &&
        (native_time %in% predictors || .search_native_annual_structure(model, backend, native_time, component_index))) native_time else predictors[1L]
  }
  if (!is.character(year) || length(year) != 1L || is.na(year) || !nzchar(year))
    stop("`year` must name one original annual predictor.", call. = FALSE)
  variables <- lapply(original_terms, function(term) all.vars(str2lang(term)))
  original_types <- vapply(original_terms, .search_term_type, character(1))
  annual <- vapply(variables, function(vars) year %in% vars, logical(1)) &
    !original_types %in% c("random", "offset")
  native_annual <- !any(annual) && .search_native_annual_structure(model, backend, year, component_index)
  if (!any(annual) && !native_annual) stop("The selected main formula does not contain the requested annual predictor or a matching retained native temporal process.", call. = FALSE)
  simple_annual <- annual & vapply(original_terms, function(term) {
    expr <- str2lang(term)
    is.symbol(expr) || (is.call(expr) && as.character(expr[[1L]])[1L] %in% c("factor", "as.factor") &&
      length(expr) == 2L && is.symbol(expr[[2L]]))
  }, logical(1))

  supplied_lower <- NULL
  supplied_upper <- NULL
  if (inherits(scope, "formula")) supplied_upper <- scope else if (!is.null(scope)) {
    if (!is.list(scope) || is.null(names(scope)) || anyDuplicated(names(scope)) ||
        any(!names(scope) %in% c("lower", "upper")) || !length(scope))
      stop("`scope` must be an upper formula or a named list containing lower and/or upper formulas.", call. = FALSE)
    supplied_lower <- scope$lower
    supplied_upper <- scope$upper
  }
  upper <- if (is.null(supplied_upper)) original else
    .search_scope_formula(original, supplied_upper, original_offsets, "scope$upper")
  lower <- if (is.null(supplied_lower)) NULL else
    .search_scope_formula(original, supplied_lower, original_offsets, "scope$lower")

  include_terms <- character()
  if (!is.null(include)) {
    if (inherits(include, "formula")) {
      include_terms <- .search_formula_terms(.search_scope_formula(original, include,
        original_offsets, "include"))
    } else if (is.character(include) && !anyNA(include) && all(nzchar(include))) {
      include_terms <- unique(vapply(include, .search_term_key, character(1)))
    } else stop("`include` must be a formula or exact term names.", call. = FALSE)
  }
  if (inherits(optional, "formula")) {
    if (length(optional) != 2L) stop("`optional` formulas must be one-sided term selections.", call. = FALSE)
    selected <- stats::update.formula(original, optional)
    optional <- .search_formula_terms(selected)
  }
  if (!is.null(optional) && (!is.character(optional) || !length(optional) || anyNA(optional) ||
      any(!nzchar(optional)) || anyDuplicated(optional)))
    stop("`optional` must be a one-sided formula or distinct, non-empty exact formula term names.", call. = FALSE)
  optional_terms <- if (is.null(optional)) NULL else vapply(optional, .search_term_key, character(1))
  if (!is.null(optional_terms) && anyDuplicated(optional_terms))
    stop("`optional` contains repeated equivalent formula terms.", call. = FALSE)

  explicit_annual <- original_terms[annual & original_terms %in% include_terms]
  annual_terms <- original_terms[simple_annual]
  if (!length(annual_terms)) {
    annual_terms <- if (length(explicit_annual)) explicit_annual else if (any(annual)) {
      sizes <- vapply(variables[annual], length, integer(1))
      original_terms[annual][sizes == min(sizes)]
    } else character()
  }
  mandatory <- unique(c(annual_terms, include_terms,
    if (!is.null(lower)) .search_formula_terms(lower),
    original_terms[original_types == "offset"]))
  structural <- original_terms[original_types %in% c("random", "multivariable_smooth")]
  # Explicitly nominated existing structural blocks may be searched; annual,
  # offset, include, and lower-scope requirements can never be released.
  retained_structure <- setdiff(structural, optional_terms %||% character())
  automatic <- unique(c(annual_terms, retained_structure,
    original_terms[original_types == "offset"]))
  upper_terms <- .search_formula_terms(upper)
  upper_terms <- unique(c(upper_terms, setdiff(automatic, upper_terms)))
  upper <- .search_formula_from_terms(original, upper_terms)
  unknown_include <- setdiff(include_terms, upper_terms)
  if (length(unknown_include)) stop("Unknown included term(s) outside the upper scope: ", paste(unknown_include, collapse = ", "), ".", call. = FALSE)
  unknown_lower <- setdiff(mandatory, upper_terms)
  if (length(unknown_lower)) stop("The lower scope includes terms absent from the upper scope: ", paste(unknown_lower, collapse = ", "), ".", call. = FALSE)
  if (!is.null(optional_terms)) {
    unknown <- setdiff(optional_terms, upper_terms)
    if (length(unknown)) stop("Unknown optional term(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    protected_optional <- intersect(optional_terms, mandatory)
    if (length(protected_optional)) stop("Required annual, offset, included, or lower-scope terms cannot be optional: ", paste(protected_optional, collapse = ", "), ".", call. = FALSE)
  }
  types <- vapply(upper_terms, .search_term_type, character(1))
  protected <- upper_terms %in% unique(c(mandatory, retained_structure)) | types == "offset"
  # Additional multivariable smooths and random terms are essential by default.
  protected <- protected | (types %in% c("random", "multivariable_smooth") &
    !upper_terms %in% (optional_terms %||% character()))
  if (!is.null(optional_terms)) protected <- protected | !upper_terms %in% optional_terms
  reason <- vapply(seq_along(upper_terms), function(i) {
    term <- upper_terms[i]
    if (term %in% annual_terms) "annual predictor" else if (types[i] == "offset") "original exposure offset" else
      if (term %in% include_terms) "explicit include" else if (!is.null(lower) && term %in% .search_formula_terms(lower)) "lower scope" else
        if (protected[i] && types[i] %in% c("random", "multivariable_smooth")) "essential random/spatial structure" else
          if (protected[i]) "not nominated for search" else "optional term block"
  }, character(1))
  terms <- data.frame(id = sprintf("t%03d", seq_along(upper_terms)), term = upper_terms,
    protected = unname(protected), reason = unname(reason), type = unname(types), stringsAsFactors = FALSE)

  # Interaction marginality only requires lower-order blocks already present in
  # the upper model. Intentional year:season coding is not expanded into invented
  # year and season main effects.
  dependencies <- stats::setNames(rep(list(character()), nrow(terms)), terms$id)
  interaction_parts <- lapply(upper_terms, function(term) {
    flatten <- function(expr) {
      if (is.call(expr) && identical(expr[[1L]], as.name(":")))
        return(c(flatten(expr[[2L]]), flatten(expr[[3L]])))
      .search_term_key(paste(deparse(expr), collapse = " "))
    }
    flatten(str2lang(term))
  })
  for (i in which(types == "interaction")) {
    child <- interaction_parts[[i]]
    parents <- which(vapply(interaction_parts, function(parts)
      length(parts) < length(child) && all(parts %in% child), logical(1)))
    dependencies[[terms$id[i]]] <- terms$id[parents]
  }
  # A factor-by smooth requires the factor's existing main-effect block. A
  # tensor interaction ti() likewise retains matching univariate smooths when
  # those form part of the supplied upper model; te() is a complete tensor block.
  for (i in which(types %in% c("smooth", "multivariable_smooth"))) {
    expression <- str2lang(upper_terms[i])
    arguments <- as.list(expression)[-1L]
    named <- names(arguments) %||% rep("", length(arguments))
    if ("by" %in% named) {
      by <- arguments[[which(named == "by")[1L]]]
      by_vars <- all.vars(by)
      parents <- which(vapply(upper_terms, function(term) {
        expr <- str2lang(term)
        (is.symbol(expr) || (is.call(expr) && as.character(expr[[1L]])[1L] %in% c("factor", "as.factor"))) &&
          length(by_vars) > 0L && setequal(all.vars(expr), by_vars)
      }, logical(1)))
      dependencies[[terms$id[i]]] <- unique(c(dependencies[[terms$id[i]]], terms$id[parents]))
    }
    if (identical(expression[[1L]], as.name("ti"))) {
      positional <- arguments[!nzchar(named)]
      variable_keys <- vapply(positional, function(x) paste(deparse(x), collapse = " "), character(1))
      parents <- which(vapply(upper_terms, function(term) {
        expr <- str2lang(term)
        if (!is.call(expr) || !identical(expr[[1L]], as.name("s"))) return(FALSE)
        args <- as.list(expr)[-1L]
        nms <- names(args) %||% rep("", length(args))
        positional <- args[!nzchar(nms)]
        length(positional) == 1L && paste(deparse(positional[[1L]]), collapse = " ") %in% variable_keys
      }, logical(1)))
      dependencies[[terms$id[i]]] <- unique(c(dependencies[[terms$id[i]]], terms$id[parents]))
    }
  }
  id_blocks <- list()
  if (!is.null(blocks)) {
    if (!is.list(blocks) || !length(blocks) || is.null(names(blocks)) || anyNA(names(blocks)) ||
        any(!nzchar(names(blocks))) || anyDuplicated(names(blocks)))
      stop("`blocks` must be a uniquely named list of exact formula term names.", call. = FALSE)
    id_blocks <- lapply(blocks, function(block) {
      if (!is.character(block) || !length(block) || anyNA(block) || any(!nzchar(block)))
        stop("Each search block must contain exact formula term names.", call. = FALSE)
      keys <- unique(vapply(block, .search_term_key, character(1)))
      if (length(setdiff(keys, upper_terms))) stop("Search blocks contain unknown upper-scope terms.", call. = FALSE)
      terms$id[match(keys, upper_terms)]
    })
    if (anyDuplicated(unlist(id_blocks))) stop("A formula term cannot belong to multiple search blocks.", call. = FALSE)
    for (block in id_blocks) if (any(terms$protected[match(block, terms$id)])) {
      protected[match(block, terms$id)] <- TRUE
      terms$protected[match(block, terms$id)] <- TRUE
      terms$reason[match(block, terms$id)] <- "block contains a required term"
    }
  }
  # Close the lower boundary under hierarchy, rather than generate invalid
  # candidates when a retained interaction depends on existing main effects.
  required <- terms$id[terms$protected]
  repeat {
    expanded <- unique(c(required, unlist(dependencies[required], use.names = FALSE)))
    if (length(id_blocks)) for (block in id_blocks) if (any(block %in% expanded)) expanded <- unique(c(expanded, block))
    if (setequal(required, expanded)) break
    added <- setdiff(expanded, required)
    terms$protected[match(added, terms$id)] <- TRUE
    terms$reason[match(added, terms$id)] <- "hierarchy or grouping of a required block"
    required <- expanded
  }
  terms$protected <- terms$id %in% required
  lower <- .search_formula_from_terms(original, terms$term[terms$protected])
  upper_forms <- lower_forms <- forms
  upper_forms[[component_index]] <- upper
  lower_forms[[component_index]] <- lower
  list(backend = backend, year = year, base_formula = base_formula,
    annual_protection = if (native_annual) "Native time column and temporal/spatiotemporal process retained; no fixed annual term invented." else
      "Annual formula block(s) retained; additional year interactions may be optional when an explicit base block is included.",
    component = component, component_index = component_index,
    upper_formula = if (inherits(base_formula, "formula")) upper else upper_forms,
    lower_formula = if (inherits(base_formula, "formula")) lower else lower_forms,
    original_component_formula = original, upper_component_formula = upper,
    lower_component_formula = lower, terms = terms,
    optional_ids = terms$id[!terms$protected], dependencies = dependencies,
    original_ids = terms$id[terms$term %in% original_terms],
    protected_ids = terms$id[terms$protected], blocks = id_blocks)
}

.search_legal <- function(spec, ids) {
  if (!is.character(ids) || anyNA(ids) || anyDuplicated(ids) ||
      any(!ids %in% spec$terms$id) || !all(spec$protected_ids %in% ids)) return(FALSE)
  if (any(vapply(spec$dependencies[ids], function(parent) !all(parent %in% ids), logical(1)))) return(FALSE)
  if (length(spec$blocks) && any(vapply(spec$blocks,
      function(block) any(block %in% ids) && !all(block %in% ids), logical(1)))) return(FALSE)
  TRUE
}

.search_formula <- function(spec, ids) {
  if (!.search_legal(spec, ids)) stop("Illegal model-search candidate: protected terms, hierarchy, or complete blocks are missing.", call. = FALSE)
  selected <- spec$terms$term[spec$terms$id %in% ids]
  f <- .search_formula_from_terms(spec$original_component_formula, selected)
  if (inherits(spec$base_formula, "formula")) return(f)
  result <- spec$base_formula
  result[[spec$component_index]] <- f
  result
}
