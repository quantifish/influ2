# A shared, deliberately small schema for fit, residual, and term evidence.
.cpue_check_row <- function(scope = "overall", term = "", check,
    status = "no_flag", statistic = NA_real_, df = NA_real_,
    p_value = NA_real_, p_adjusted = NA_real_, method = "", message = "",
    action = "", component = "", conditioning = "",
    calibration = "not applicable", n = NA_integer_) {
  allowed <- c("no_flag", "review", "issue", "not_assessed", "not_applicable")
  if (length(status) != 1L || !status %in% allowed)
    stop("Invalid model-check status.", call. = FALSE)
  scalar <- function(x, default) if (is.null(x) || !length(x)) default else x[[1L]]
  data.frame(scope = as.character(scalar(scope, "overall")),
    term = as.character(scalar(term, "")), check = as.character(scalar(check, "")),
    status = status, statistic = as.numeric(scalar(statistic, NA_real_)),
    df = as.numeric(scalar(df, NA_real_)),
    p_value = as.numeric(scalar(p_value, NA_real_)),
    p_adjusted = as.numeric(scalar(p_adjusted, NA_real_)),
    method = as.character(scalar(method, "")),
    message = as.character(scalar(message, "")),
    action = as.character(scalar(action, "")),
    component = as.character(scalar(component, "")),
    conditioning = as.character(scalar(conditioning, "")),
    calibration = as.character(scalar(calibration, "not applicable")),
    n = as.integer(scalar(n, NA_integer_)), stringsAsFactors = FALSE)
}

.cpue_model_annotation <- function(stats) {
  if (!is.data.frame(stats) || !nrow(stats)) return("Model statistics not stored.")
  labels <- c(nobs = "n", df = "Likelihood df", logLik = "logLik", AIC = "AIC",
    BIC = "BIC", deviance = "Deviance", deviance_explained = "Explained deviance",
    adjusted_R2 = "Adjusted R2", bayes_R2 = "Bayesian R2")
  values <- vapply(names(labels), function(name) {
    value <- stats[[name]]
    if (!is.numeric(value) || !length(value) || !is.finite(value[1L])) return("")
    formatted <- if (name == "deviance_explained")
      paste0(format(signif(100 * value[1L], 3), trim = TRUE), "%") else
      format(signif(value[1L], 4), trim = TRUE)
    paste0(labels[[name]], " = ", formatted)
  }, character(1))
  values <- values[nzchar(values)]
  if (!length(values)) return("Native model statistics unavailable; not a successful check.")
  lines <- split(values, ceiling(seq_along(values) / 4L))
  paste(vapply(lines, paste, collapse = "; ", FUN.VALUE = character(1)), collapse = "\n")
}

#' Check a CPUE model, overall and term by term
#'
#' Combine native fitting evidence, available model statistics, optional
#' simulation-based residual tests, and exploratory checks for every model
#' term. The report does not refit or select the original model.
#'
#' @param model A fitted GLM, GAM, glmmTMB, lme4, brms, sdmTMB, or single-response
#'   tinyVAST model.
#' @param data Original fitting data, with original row names. Supply it when
#'   raw predictors are unavailable in the saved model frame (for example,
#'   `poly(depth, 3)`). Changed data fail alignment checks.
#' @param year Original-data time column. `NULL` uses [influ_residuals()] time
#'   discovery. An explicit name must exist in the aligned data.
#' @param residuals Optional precomputed native [influ_residuals()] result from this
#'   **same fit**. Reuses all stored values and tests, without simulation or
#'   recalculating missing tests. Backend, response, and observation IDs are
#'   checked, but old objects cannot prove identical fitted parameters;
#'   the caller remains responsible for same-fit provenance.
#' @param simulate Calculate residuals when none are supplied? Default `TRUE`;
#'   use `FALSE` for a quick native-fit/term-inventory report. Missing residual
#'   checks are recorded as `"not_assessed"`, not passes. A hard numerical
#'   issue also prevents automatic residual simulation.
#' @param terms Optional subset of exact recorded term names. `NULL` includes
#'   every term. Interaction, auxiliary-component, and latent-process terms
#'   remain visible even when no valid univariate residual screen is available.
#' @param nsim,batch_size,seed,component,conditioning,dharma_max_mb,trial_counts,response_transform,dharma_storage
#'   Passed to [influ_residuals()] for new calculations. No MCMC or original
#'   model refit is run. Compact DHARMa calculations default to bounded response
#'   blocks and a temporary disk cache; `dharma_max_mb` is a response-workspace
#'   budget, not a process-wide memory bound. Component selection is explicit for joint models.
#' @param residual_method Residual engine for a new report: `"dharma"` (the
#'   checker default, requiring optional DHARMa >= 0.4.7) or `"simulation_pit"`.
#'   This does not change the default of [influ_residuals()]. There is no silent
#'   fallback if an engine, family, or conditioning target is unavailable.
#' @param residual_tests DHARMa-reference test set for new calculations: `"none"`,
#'   `"standard"`, or `"full"`. `NULL` selects `"standard"` for DHARMa and
#'   `"none"` for simulation PIT. Full adds auxiliary quantile regressions and
#'   outlier tests, with additional computation, but no original-model refits.
#' @param alpha Exploratory flag threshold, default 0.05. Term-screen p-values
#'   are Holm-adjusted across the calculated raw-predictor screens. Native
#'   DHARMa within-year p-values use their separately recorded Holm family;
#'   there is no universal correction across all different diagnostic questions.
#'
#' @details `model_statistics` contains available native sample size, likelihood
#'   degrees of freedom, log-likelihood, AIC, BIC, and deviance. Native GLM/GAM
#'   explained deviance and GAM adjusted R-squared are labelled separately;
#'   Bayesian R-squared is reused only when already stored. Missing quantities
#'   are `NA`, not fabricated or calculated by fitting a null model. Likelihood
#'   criteria are descriptive here: unlike [table_criterion()], this function
#'   does not establish comparability between different fits.
#'
#'   Overall native checks distinguish numerical fitting issues from unavailable
#'   evidence. Term screens use uniform-scale PIT ranks: Spearman rho for a raw
#'   numeric predictor, or Kruskal-Wallis rank/location comparisons for adequately
#'   sampled categories. These are **not DHARMa tests**, coefficient-significance
#'   tests, tests of random-effect variance, or automatic term-removal rules.
#'   A raw-predictor check for `poly(x, 3)` is not a test of each polynomial
#'   coefficient. Native GAM smooth EDF/statistics are reported separately, not
#'   classified as residual failures. Joint/latent structures requiring an
#'   explicitly defined dependence check are reported as unassessed.
#'
#'   Optional uniformity/year/quantile tests use genuine DHARMa vector interfaces.
#'   Disk-based dispersion and outlier tests use labelled bounded-memory
#'   equivalents; `dharma_storage = "memory"` calls native object-based tests.
#'   The compact report retains statistics, implementation sources, and
#'   metadata, not that object's response matrix, the fitted model, or formula
#'   environments. Plotting and printing never repeat tests or consume RNG.
#'   Supplied residuals are compacted even if their original object retains
#'   native DHARMa simulations. Caller RNG is preserved during calculation.
#'
#'   `"issue"` denotes a numerical fitting problem; `"review"` denotes evidence
#'   worth inspecting; `"no_flag"` means only that the particular screen did
#'   not flag; `"not_assessed"` is unavailable/inapplicable-to-current-data
#'   evidence; and `"not_applicable"` denotes a genuinely irrelevant check or
#'   descriptive fitted-effect statistic. There is no overall model-pass flag.
#'   Correlation, fitting, posterior-data reuse, and simulation conditioning can
#'   invalidate nominal test calibration. A combined-response check does not
#'   validate encounter or positive components separately. No rotation, OSA,
#'   model-specific false-positive calibration, or hidden refitting is applied.
#'
#' @return A compact S3 `influ_check` object, with `model_statistics`,
#'   `overall`, `term_checks`, `term_inventory`, `residuals`, `predictors`, and
#'   `metadata`. [base::summary()] returns report tables; [base::as.data.frame()] combines
#'   all checks. `plot(report)` reuses the residual overview, and
#'   `plot(report, type = "term", term = "depth")` displays a retained raw
#'   predictor alongside its exploratory screen. Reports without residuals
#'   can still display the status table using `type = "status"`.
#' @seealso [influ_residuals()], [table_criterion()], [plot.influ_check()]
#' @examples
#' d <- data.frame(year = factor(rep(2010:2013, each = 20)),
#'                 x = seq(-1, 1, length.out = 80))
#' d$y <- rep(0:3, length.out = 80)
#' fit <- glm(y ~ year + x, family = poisson(), data = d)
#' report <- check_cpue(fit, simulate = FALSE)
#' report
#' summary(report)$model_statistics
#' \donttest{
#' if (requireNamespace("DHARMa", quietly = TRUE)) {
#'   report <- check_cpue(fit, nsim = 50, seed = 42)
#'   plot(report)
#'   plot(report, type = "term", term = "x")
#' }
#' }
#' @md
#' @export
check_cpue <- function(model, data = NULL, year = NULL, residuals = NULL,
    simulate = TRUE, terms = NULL, nsim = 250L, batch_size = 25L, seed = 1L,
    component = c("auto", "combined", "encounter", "positive"),
    conditioning = "backend_default",
    residual_method = c("dharma", "simulation_pit"), residual_tests = NULL,
    alpha = 0.05, dharma_max_mb = 256, trial_counts = NULL,
    response_transform = "identity", dharma_storage = c("auto", "disk", "memory")) {
  engine_requested <- !missing(residual_method)
  source_supplied <- !is.null(residuals)
  if (!is.logical(simulate) || length(simulate) != 1L || is.na(simulate))
    stop("`simulate` must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be between zero and one.", call. = FALSE)
  component <- match.arg(component)
  residual_method <- match.arg(residual_method)
  .resid_dharma_storage(dharma_storage, residual_method, FALSE)
  if (is.null(residual_tests)) residual_tests <- if (residual_method == "dharma") "standard" else "none"
  residual_tests <- .resid_test_options(residual_tests, residual_method)
  .resid_integer(nsim, "nsim", 20L)
  .resid_integer(batch_size, "batch_size", 1L)
  .resid_integer(seed, "seed", 0L)
  if (!is.numeric(dharma_max_mb) || length(dharma_max_mb) != 1L ||
      !is.finite(dharma_max_mb) || dharma_max_mb <= 0)
    stop("`dharma_max_mb` must be a positive finite number.", call. = FALSE)
  if (!is.null(year) && (!is.character(year) || length(year) != 1L || is.na(year) || !nzchar(year)))
    stop("`year` must name one original-data column.", call. = FALSE)
  if (!is.character(conditioning) || length(conditioning) != 1L || is.na(conditioning) ||
      !conditioning %in% c("backend_default", "fitted", "conditional_draw", "new_effects", "posterior_predictive"))
    stop("Unknown `conditioning` target.", call. = FALSE)
  if (!is.character(response_transform) || length(response_transform) != 1L ||
      is.na(response_transform) || !response_transform %in% c("identity", "log"))
    stop("`response_transform` must be 'identity' or 'log'.", call. = FALSE)
  fit <- .check_fit(model)
  inventory <- .check_term_inventory(model, data)
  # Validate requested terms and source rows before doing any simulation.
  empty_terms <- .check_terms(model, NULL, data, terms, alpha)
  if (!is.null(year) && !year %in% names(inventory$data))
    stop("`year` was not found in safely aligned fitted data.", call. = FALSE)
  if (!is.null(trial_counts)) .resid_known_trials(trial_counts, inventory$data)
  if (!is.null(residuals)) {
    .check_cpue_residual_source(model, residuals, inventory)
    if ((component != "auto" && !identical(component, residuals$metadata$component)) ||
        (conditioning != "backend_default" && !identical(conditioning, residuals$metadata$conditioning)) ||
        (!is.null(year) && !identical(year, residuals$metadata$year)) ||
        (engine_requested && !identical(residual_method, residuals$metadata$residual_method)))
      stop("Explicit residual component, conditioning, year, or engine disagrees with the supplied object; reuse cannot change these targets.", call. = FALSE)
  }
  residual_error <- NULL
  warnings <- character()
  if (is.null(residuals) && simulate) {
    if (any(fit$checks$check == "native_objective_availability" & fit$checks$status == "not_assessed")) {
      residual_error <- "Automatic simulation was skipped because the saved native objective is unavailable. Reusing a precomputed residual object remains possible; the original native objective is not reinitialised."
    } else if (any(fit$checks$status == "issue")) {
      residual_error <- "Automatic simulation was skipped because a native numerical check reported an issue. Review the fit first."
    } else {
      result <- tryCatch(withCallingHandlers(influ_residuals(model,
        data = data, year = year, nsim = nsim, batch_size = batch_size, seed = seed,
        component = component, conditioning = conditioning,
        residual_method = residual_method, residual_tests = residual_tests,
        dharma_max_mb = dharma_max_mb, trial_counts = trial_counts,
        response_transform = response_transform, dharma_storage = dharma_storage), warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }), error = function(e) e)
      if (inherits(result, "error")) residual_error <- conditionMessage(result) else residuals <- result
    }
  }
  overall <- fit$checks
  tests <- if (!is.null(residuals)) residuals$tests else NULL
  if (is.data.frame(tests) && nrow(tests)) {
    # Re-flag nominal/native-adjusted values at the requested threshold, but
    # do not change unavailable tests or descriptive native effect summaries.
    flaggable <- tests$status %in% c("review", "no_flag") & is.finite(tests$p_value)
    decision <- ifelse(is.finite(tests$p_adjusted), tests$p_adjusted, tests$p_value)
    tests$status[flaggable] <- ifelse(decision[flaggable] < alpha, "review", "no_flag")
    tm <- attr(tests, "test_metadata")
    if (!is.null(tm)) {
      tm$alpha <- alpha
      attr(tests, "test_metadata") <- tm
    }
    residuals$tests <- tests
    if (!is.null(residuals$metadata$residual_tests)) residuals$metadata$residual_tests$alpha <- alpha
    overall <- rbind(overall, tests[tests$scope == "overall", , drop = FALSE])
  } else {
    reason <- residual_error %||% if (is.null(residuals))
      "Residual simulation was not requested; no overall residual test was run." else
      "No native residual tests were stored; reusing residuals never regenerates missing tests."
    overall <- rbind(overall, .cpue_check_row(check = "residual_tests",
      status = "not_assessed", method = "No residual-test calculation",
      message = reason, action = "Inspect the numerical fit and availability; calculate supported residuals explicitly when appropriate.",
      component = if (is.null(residuals)) component else residuals$metadata$component,
      conditioning = if (is.null(residuals)) conditioning else residuals$metadata$conditioning,
      calibration = "not assessed"))
  }
  term_checks <- if (is.null(residuals)) empty_terms else
    .check_terms(model, residuals, data, terms, alpha)
  recorded_terms <- attr(term_checks, "term_inventory") %||% inventory$terms
  if (is.data.frame(tests) && nrow(tests)) term_checks <- rbind(term_checks, tests[tests$scope == "term", , drop = FALSE])
  retained_inventory <- inventory
  retained_inventory$terms <- recorded_terms
  predictors <- .check_cpue_predictors(retained_inventory, residuals)
  if (!is.null(residuals)) {
    residuals <- .check_cpue_compact_residuals(residuals)
    residuals$metadata$retention <- "Compact summaries and test statistics only; no model or observation-by-simulation matrix retained"
    residuals$model_statistics <- fit$model_statistics
  }
  # Never retain a formula (and its enclosing environment), a fitted model,
  # backend objective, or the full original source data.
  metadata <- list(schema_version = 1L, backend = fit$backend,
    created_with = as.character(utils::packageVersion("influ2")),
    term_adjustment = "Holm across calculated raw-predictor screens",
    alpha = alpha, residuals_calculated = !is.null(residuals),
    residual_error = residual_error, warnings = unique(warnings),
    residual_provenance = if (source_supplied)
      "Native residual source checked for backend, response encoding, and all selected original rows; caller remains responsible for identical fitted parameters." else if (!is.null(residuals))
      "Residuals calculated directly from the supplied fit in this report." else "No residuals",
    calibration = "Exploratory fitted-data screening; no model-specific false-positive calibration or overall pass/fail rule",
    retention = "Compact native statistics, checks, residual summaries, and selected raw predictors; no original model or simulation matrix")
  structure(list(model_statistics = fit$model_statistics, overall = overall,
    term_checks = term_checks, term_inventory = recorded_terms,
    residuals = residuals, predictors = predictors, metadata = metadata), class = "influ_check")
}

.check_cpue_residual_source <- function(model, residuals, inventory) {
  if (!inherits(residuals, "influ_residuals"))
    stop("`residuals` must be an influ_residuals object from the same fit.", call. = FALSE)
  if (!identical(residuals$metadata$backend, inventory$backend))
    stop("Residual backend does not match the fitted model.", call. = FALSE)
  .check_residual_model(model)
  obs <- residuals$observations
  if (!is.data.frame(obs) || !all(c("row", "observed", "pit", "residual", "predicted", "year") %in% names(obs)))
    stop("Residual observation summaries are incomplete.", call. = FALSE)
  ids <- as.character(obs$row)
  if (!length(ids) || is.null(inventory$data) || anyNA(ids) || anyDuplicated(ids) ||
      !all(ids %in% rownames(inventory$data)))
    stop("Residual observation IDs do not match the fitted data.", call. = FALSE)
  frame <- .residual_model_frame(model)
  f <- .resid_formula(model)
  lhs <- f[[2L]]
  if (inherits(model, "brmsfit") && is.call(lhs) && identical(lhs[[1L]], as.name("|"))) lhs <- lhs[[2L]]
  native_label <- paste(deparse(f[[2L]]), collapse = " ")
  label <- residuals$metadata$native_response %||% residuals$metadata$response
  display_only <- residuals$metadata$response %in% c("Successes", "Encounter (positive catch)")
  if (!display_only && !identical(label, native_label))
    stop("Residual response does not match the fitted model formula.", call. = FALSE)
  value <- if (inherits(model, c("glm", "gam", "glmmTMB", "merMod"))) stats::model.response(frame) else
    if (is.symbol(lhs) && as.character(lhs) %in% names(frame)) frame[[as.character(lhs)]] else NULL
  if (is.null(value)) stop("Native observed response cannot be reconstructed safely for residual reuse.", call. = FALSE)
  fitted_ids <- rownames(frame)
  binomial <- residuals$metadata$response_kind %in% c("bernoulli", "grouped_binomial")
  trials <- NULL
  if (is.matrix(value)) {
    if (!binomial || ncol(value) != 2L) stop("A supported two-column binomial response is required.", call. = FALSE)
    trials <- rowSums(value)
    value <- value[, 1L]
  } else if (is.factor(value)) {
    if (!binomial || nlevels(value) != 2L) stop("A supported two-level binomial response is required.", call. = FALSE)
    value <- as.integer(value) - 1L
    trials <- rep(1, length(value))
  } else if (binomial && inherits(model, c("glm", "gam", "glmmTMB", "merMod"))) {
    trials <- if (inherits(model, c("glm", "gam"))) model$prior.weights else stats::model.weights(frame)
    trials <- trials %||% rep(1, length(value))
    value <- value * trials
  }
  if (identical(residuals$metadata$component, "encounter") && !identical(residuals$metadata$response, "Successes")) {
    value <- as.numeric(value > 0)
    trials <- rep(1, length(value))
  }
  positive_subset <- identical(residuals$metadata$component, "positive") &&
    inherits(model, "sdmTMB") && isTRUE(model$family$delta)
  expected_ids <- if (positive_subset) fitted_ids[value > 0] else fitted_ids
  if (!setequal(ids, expected_ids))
    stop("Residual observation IDs must contain every fitted row for the selected component, not an arbitrary subset.", call. = FALSE)
  if (identical(residuals$metadata$response_transform, "log")) value <- exp(value)
  if (!is.numeric(value) || !is.null(dim(value))) stop("Native observed response encoding is unsupported for safe residual reuse.", call. = FALSE)
  value <- value[match(ids, fitted_ids)]
  if (!isTRUE(all.equal(as.numeric(value), as.numeric(obs$observed), tolerance = 1e-10)))
    stop("Residual observed responses do not match the fitted observation rows.", call. = FALSE)
  if (!is.null(trials) && (!"trials" %in% names(obs) ||
      !isTRUE(all.equal(as.numeric(trials[match(ids, fitted_ids)]), as.numeric(obs$trials)))))
    stop("Stored residual trials do not match the native fitted response encoding.", call. = FALSE)
  invisible(NULL)
}

.check_cpue_compact_residuals <- function(x) {
  keep <- c("observations", "qq", "ecdf", "observed_ecdf", "calibration",
    "groups", "metadata", "tests")
  x <- x[intersect(keep, names(x))]
  unsafe <- function(z) {
    if (is.environment(z) || is.function(z) || is.language(z) || typeof(z) == "externalptr") return(TRUE)
    if (is.list(z) && any(vapply(z, unsafe, logical(1)))) return(TRUE)
    a <- attributes(z)
    if (!is.null(a) && any(vapply(a, unsafe, logical(1)))) return(TRUE)
    FALSE
  }
  if (unsafe(x)) stop("Residual summaries contain a hidden model, function, or mutable environment; supply the unmodified compact native result.", call. = FALSE)
  structure(x, class = "influ_residuals")
}

.check_cpue_predictors <- function(inventory, residuals) {
  if (is.null(residuals) || is.null(inventory$data)) return(NULL)
  vars <- setdiff(unique(unlist(inventory$terms$variables)), inventory$response_variables)
  vars <- intersect(vars, names(inventory$data))
  vars <- vars[vapply(inventory$data[vars], function(x)
    is.atomic(x) && is.null(dim(x)) && (is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x)), logical(1))]
  d <- inventory$data[match(residuals$observations$row, rownames(inventory$data)), vars, drop = FALSE]
  rownames(d) <- as.character(residuals$observations$row)
  d
}

#' @rdname check_cpue
#' @param x,object An `influ_check` report.
#' @param ... Further arguments to printing or residual plotting methods.
#' @export
print.influ_check <- function(x, ...) {
  cat("CPUE model check (", x$metadata$backend, ")\n", sep = "")
  cat("Overall model statistics:\n")
  print(x$model_statistics, row.names = FALSE)
  cat("\nOverall checks:\n")
  print(x$overall[c("check", "status", "statistic", "p_value", "p_adjusted")], row.names = FALSE)
  cat("\nModel-term checks:\n")
  print(x$term_checks[c("term", "component", "check", "status", "statistic", "df", "p_value", "p_adjusted")], row.names = FALSE)
  cat("\nNo flag is not proof of adequacy; unavailable checks are not passes.\n")
  if (!is.null(x$residuals$metadata$residual_tests)) cat("Simulation-test p-values are finite Monte Carlo estimates (",
    x$residuals$metadata$nsim, " response simulations); a reported zero is not an exact probability.\n", sep = "")
  cat("Use summary() for methods, limitations, and suggested follow-up.\n")
  invisible(x)
}

#' @rdname check_cpue
#' @export
summary.influ_check <- function(object, ...) {
  list(model_statistics = object$model_statistics, overall = object$overall,
    terms = object$term_checks, term_inventory = object$term_inventory,
    metadata = object$metadata)
}

#' @rdname check_cpue
#' @param row.names Optional output row names.
#' @param optional Passed to the data-frame method; retained for the standard
#'   [base::as.data.frame()] signature.
#' @export
as.data.frame.influ_check <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- rbind(x$overall, x$term_checks)
  rownames(out) <- row.names
  out
}

#' Plot stored CPUE model-check results
#'
#' Plots do not recompute tests, simulation, or model statistics.
#' @param x,object An `influ_check` report from [check_cpue()].
#' @param type `"overview"` (default) reuses its four-panel residual plot;
#'   `"term"` shows residuals against a retained raw predictor; `"status"`
#'   displays the recorded check-status table without residual data.
#' @param term Exact model term to display for `type = "term"`. Only terms
#'   with one available raw predictor have this univariate view; no basis,
#'   interaction, or latent-process plot is invented.
#' @param component Optional model component to disambiguate identical term
#'   names in a joint model. It does not recalculate component residuals.
#' @param annotations Saved annotations for the overview: `"all"` (default),
#'   `"none"`, `"tests"`, or `"model"`. Term plots show their stored
#'   exploratory screens. See [plot.influ_residuals()].
#' @param ... Further arguments passed to [plot.influ_residuals()] for an overview.
#' @return A ggplot or patchwork object.
#' @md
#' @export
plot.influ_check <- function(x, type = c("overview", "term", "status"),
    term = NULL, component = NULL, annotations = c("all", "none", "tests", "model"), ...) {
  type <- match.arg(type)
  annotations <- match.arg(annotations)
  if (type == "status") {
    d <- as.data.frame(x)
    d$label <- paste(ifelse(nzchar(d$term), paste0(d$term, " [", d$component, "]"), "Overall"), d$check, sep = ": ")
    d$label <- factor(make.unique(d$label), levels = rev(make.unique(d$label)))
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data$status, y = .data$label, colour = .data$status)) +
      ggplot2::geom_point(size = 2) + ggplot2::labs(x = "Recorded status", y = NULL,
        title = "CPUE check evidence", subtitle = "No flag is not a model pass; not assessed is not a successful check") +
      ggplot2::theme(legend.position = "none"))
  }
  if (is.null(x$residuals)) stop("This report has no residuals to plot. Use type = 'status' or calculate supported residuals.", call. = FALSE)
  if (type == "overview") return(plot(x$residuals, annotations = annotations, ...))
  if (!is.character(term) || length(term) != 1L || is.na(term)) stop("Specify one recorded `term`.", call. = FALSE)
  selected <- x$term_inventory[x$term_inventory$term == term, , drop = FALSE]
  if (!is.null(component)) selected <- selected[selected$component == component, , drop = FALSE]
  if (nrow(selected) != 1L) stop("The term is missing or ambiguous; specify its recorded `component`.", call. = FALSE)
  vars <- selected$variables[[1L]]
  if (length(vars) != 1L || !vars %in% names(x$predictors) || selected$term_type %in% c("spatial", "spatiotemporal", "process", "offset", "unavailable"))
    stop("No valid univariate raw-predictor view is available for this term.", call. = FALSE)
  selected_checks <- x$term_checks[x$term_checks$term == term & x$term_checks$component == selected$component, , drop = FALSE]
  if (any(grepl("not this .* component", selected_checks$message)))
    stop("The stored residual target does not assess this model component; calculate its own supported residual report.", call. = FALSE)
  d <- x$residuals$observations
  d$predictor <- x$predictors[[vars]]
  if (selected$term_type == "random" || grepl("^(factor|as.factor)\\(", term))
    d$predictor <- factor(d$predictor)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$predictor, y = .data$residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey40")
  if (is.numeric(d$predictor)) {
    p <- p + ggplot2::geom_point(colour = "purple4", alpha = 0.3, size = 1)
    if (nrow(d) >= 20L && length(unique(d$predictor)) >= 5L)
      p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "steelblue4")
  } else p <- p + ggplot2::geom_boxplot(colour = "purple4", varwidth = TRUE, outlier.alpha = 0.3)
  tests <- x$term_checks[x$term_checks$term == term & x$term_checks$component == selected$component &
    x$term_checks$check %in% c("rank_association", "group_location"), , drop = FALSE]
  subtitle <- if (!nrow(tests)) "No univariate residual screen calculated; inspect the recorded limitation" else
    paste0(if (tests$check[1L] == "rank_association") "Spearman rho = " else "Kruskal-Wallis H = ",
      .resid_test_number(tests$statistic[1L]), "; Holm p ", .resid_test_p(tests$p_adjusted[1L]))
  p + ggplot2::labs(title = paste("Residual structure:", term), subtitle = subtitle,
    x = vars, y = if (identical(x$residuals$metadata$residual_method, "dharma")) "Normal-score DHARMa residual" else "Normal-score PIT residual",
    caption = paste("Exploratory raw-predictor screen, not term significance. Loess is descriptive; nominal p-values do not correct fitted-model dependence.",
      paste0("Residual target: ", x$residuals$metadata$component, "; conditioning: ", x$residuals$metadata$conditioning, "."),
      .resid_endpoint_caption(x$residuals), sep = "\n"))
}

#' @rdname plot.influ_check
#' @export
autoplot.influ_check <- function(object, ...) plot.influ_check(object, ...)
