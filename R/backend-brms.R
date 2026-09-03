.brms_population_matrix <- function(model, dpar = NULL) {
  standata <- brms::standata(model)
  suffix <- if (is.null(dpar) || dpar == "mu") "" else paste0("_", dpar)
  X <- standata[[paste0("X", suffix)]]
  if (is.null(X)) return(NULL)

  formula <- if (is.null(dpar) || dpar == "mu") {
    model$formula$formula
  } else {
    model$formula$pforms[[dpar]]
  }
  if (is.null(formula)) return(NULL)

  labels <- attr(stats::terms(formula), "term.labels")
  assign <- attr(X, "assign")
  columns <- lapply(seq_along(labels), function(i) which(assign == i))
  names(columns) <- labels
  columns <- columns[lengths(columns) > 0L]

  list(X = X, term_columns = columns)
}

.brms_parameter_names <- function(X, dpar = NULL) {
  prefix <- if (is.null(dpar) || dpar == "mu") "b_" else paste0("b_", dpar, "_")
  paste0(prefix, colnames(X))
}

.brms_population_draws <- function(model, X, dpar = NULL, ndraws = NULL,
                                   keep_draws = TRUE) {
  variables <- .brms_parameter_names(X, dpar)
  available <- posterior::variables(model)
  missing <- setdiff(variables, available)
  if (length(missing)) {
    stop(
      "The brms population-level parameters could not be aligned with its ",
      "model matrix: ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  draws <- posterior::as_draws_matrix(model, variable = variables)
  if (!is.null(ndraws) && nrow(draws) > ndraws) {
    keep <- unique(round(seq(1, nrow(draws), length.out = ndraws)))
    draws <- draws[keep, , drop = FALSE]
  }
  colnames(draws) <- colnames(X)
  beta <- colMeans(draws)
  if (!keep_draws) draws <- NULL
  list(beta = beta, draws = draws)
}

.brms_component_diag <- function(model, data, focus, dpar, component,
                                 family_spec, weights, uncertainty, retain,
                                 probs, ndraws, draws_path) {
  matrix_info <- .brms_population_matrix(model, dpar)
  if (is.null(matrix_info) || !length(matrix_info$term_columns)) return(NULL)

  use_draws <- uncertainty != "none"
  posterior <- .brms_population_draws(
    model, matrix_info$X, dpar, ndraws = ndraws, keep_draws = use_draws
  )

  .influ_linear_engine(
    backend = "brms",
    model = model,
    data = data,
    response = if (is.null(dpar) || dpar == "mu") names(data)[1] else NULL,
    focus = focus,
    family_spec = family_spec,
    X = matrix_info$X,
    beta = posterior$beta,
    vcov = NULL,
    term_columns = matrix_info$term_columns,
    uncertainty = if (uncertainty == "none") "none" else "posterior",
    retain = retain,
    probs = probs,
    weights = weights,
    component = component,
    beta_draws = posterior$draws,
    draws_path = draws_path,
    keep_model = FALSE
  )
}

#' Influence diagnostics for brms models
#'
#' Population-level posterior coefficients are projected directly into the
#' compact focus-by-term contrasts. The method therefore never constructs an
#' observation-by-draw-by-term array. Joint posterior dependence is preserved
#' while calculating the requested diagnostics.
#'
#' @inheritParams influ.glm
#' @param model A fitted object from [brms::brm()].
#' @param uncertainty Either `"auto"`, `"posterior"`, or `"none"`. The
#'   `"none"` mode reduces the posterior to coefficient means before influence
#'   calculations.
#' @param ndraws Optional maximum number of posterior draws. `NULL` uses every
#'   available draw.
#'
#' @return An [influ_diag] object.
#' @export
influ.brmsfit <- function(model, focus, data = NULL, weights = NULL,
                          uncertainty = "auto", retain = "summary",
                          probs = c(0.025, 0.975), ndraws = NULL,
                          seed = NULL, draws_path = NULL,
                          keep_model = FALSE, ...) {
  if (!requireNamespace("brms", quietly = TRUE) ||
      !requireNamespace("posterior", quietly = TRUE)) {
    stop("Packages 'brms' and 'posterior' are required for this model.", call. = FALSE)
  }
  uncertainty <- match.arg(uncertainty, c("auto", "posterior", "none"))
  if (uncertainty == "auto") uncertainty <- "posterior"
  data <- if (is.null(data)) as.data.frame(model$data) else as.data.frame(data)

  response_structure <- .normalise_family_name(model$family$family)$structure
  overall_spec <- .new_family_spec(
    model$family$family,
    model$family$link,
    backend = "brms",
    response_structure = response_structure
  )
  component_retain <- if (retain == "disk") "derived_draws" else retain
  main_component <- if (response_structure == "hurdle") {
    "positive"
  } else if (response_structure == "zero_inflated") {
    "count"
  } else {
    "conditional"
  }

  components <- list(
    conditional = .brms_component_diag(
      model, data, focus, NULL, main_component, overall_spec, weights,
      uncertainty, component_retain, probs, ndraws, NULL
    )
  )

  probability_dpar <- intersect(c("hu", "zi"), model$family$dpars)
  if (length(probability_dpar)) {
    dpar <- probability_dpar[1]
    probability_spec <- .new_family_spec(
      "binomial", "logit", backend = "brms",
      response_structure = response_structure,
      complement = dpar == "hu"
    )
    components$probability <- .brms_component_diag(
      model, data, focus, dpar,
      if (dpar == "hu") "occurrence" else "zero_probability",
      probability_spec, weights, uncertainty, component_retain, probs,
      ndraws, NULL
    )
  }

  omitted <- character()
  if (nrow(model$ranef) > 0L) {
    omitted <- c(omitted, "group-level effects")
  }
  standata_names <- names(brms::standata(model))
  if (any(grepl("^(Xs|Zs)", standata_names))) {
    omitted <- c(omitted, "smooth terms")
  }

  out <- .combine_influ_diags(
    components,
    backend = "brms",
    family_spec = overall_spec,
    focus = focus,
    model = model,
    keep_model = keep_model,
    notes = c(
      "Population-level terms use compact posterior projections; no observation-by-draw arrays are retained.",
      if (length(omitted)) paste("Pending brms adapters:", paste(omitted, collapse = ", ")),
      if (response_structure != "single") "Component influence is available; combined unconditional-mean influence is pending."
    )
  )

  if (retain == "disk") {
    if (is.null(draws_path)) stop("`draws_path` is required when `retain = \"disk\"`.", call. = FALSE)
    saveRDS(out$draws, draws_path, compress = FALSE)
    out$draws <- NULL
    out$retained$mode <- "disk"
    out$retained$path <- normalizePath(draws_path, mustWork = TRUE)
  }
  out
}
