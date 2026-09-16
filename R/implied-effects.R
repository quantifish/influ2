#' Calculate residual-implied effects by year and group
#'
#' Estimate a local effect-scale adjustment while holding the supplied model
#' fixed, then add it to a centred year (and optionally group) contribution.
#' These exploratory trajectories are not refitted interactions or regional
#' abundance indices. Calculate once and plot the compact result repeatedly.
#'
#' @param model A retained `lm`, GLM, `mgcv` GAM, or ML `glmmTMB` fit. These
#'   adapters support Gaussian identity-link models (including
#'   an explicitly logged response), Poisson, NB2, and Gamma log-link models.
#'   ML `sdmTMB` Bernoulli(logit), lognormal(log), and standard delta-lognormal
#'   fits are also supported with explicit joint-component selection.
#'   ML `tinyVAST` and native `brms` fits support Gaussian, Poisson, NB2, Gamma,
#'   Bernoulli, and lognormal responses with the links described below, plus
#'   standard tinyVAST delta-lognormal and brms hurdle-lognormal components.
#'   Other families, backends, joint components, non-unit case weights, and year
#'   interactions fail explicitly rather than substitute another calculation.
#' @param component `NULL` for a single-response fit. A joint standard
#'   delta-lognormal `sdmTMB`/`tinyVAST` or hurdle-lognormal `brms` fit requires `"positive"`, `"encounter"`, or
#'   `"combined"`. Only positive observations inform the positive adjustment
#'   and its `min_n`; encounter uses all rows. The combined display uses both
#'   adjustments and reports expected response, not a coefficient.
#'   `"conditional"` may explicitly select a single-response
#'   fit. No joint component is selected automatically.
#' @param year_term For sdmTMB, tinyVAST, or brms effects, the original fixed annual
#'   predictor column, defaulting to `year`. For example, use `year = "year_factor"`
#'   and `year_term = "year_scaled"` if the encounter formula uses a continuous
#'   annual trend. The predictor must be constant within each requested year.
#'   Not used for combined implied responses, which do not have a term baseline.
#' @param draw_id For brms only, one positive integer identifying a joint
#'   posterior draw. `NULL` uses native posterior-mean parameters before
#'   predicting. Both options condition on a single parameter state; neither
#'   propagates posterior uncertainty or produces Bayesian credible intervals.
#' @param data Original model data with original row names, if needed to
#'   recover the year or grouping column. Values are checked against the fit.
#' @param year Time column; `NULL` uses the usual automatic detection.
#' @param groups One original-data grouping column, not defined by the response.
#' @param method `"likelihood"` (default) estimates a one-parameter conditional
#'   likelihood shift per stratum. `"traditional"` adds mean log-scale residuals
#'   for a Gaussian model of `log(response)`; see `traditional_scale`.
#' @param baseline `"year_group"` adds the centred, fixed main contributions of
#'   year and group, where the group main effect is present. `"year"` adds only
#'   the year contribution. Terms are centred over all fitted observations,
#'   not separately within panels. An additive fixed year term is required.
#'   Random effects and smooths remain in fitted predictions, not the baseline.
#'   For `component = "combined"`, this term-baseline choice does not apply:
#'   the baseline is the fitted arithmetic response mean over each stratum's
#'   original observation rows, without centring or reference-grid standardisation.
#' @param min_n Minimum records in a year-by-group stratum. Sparse and empty
#'   strata remain in the table with an explicit status, but are not plotted.
#' @param level Conditional profile-likelihood interval coverage; default 0.95.
#' @param interval `"auto"` uses profile-likelihood intervals for the new
#'   method, or mean +/- one descriptive SE for traditional calculations.
#'   `"descriptive"` uses SD/sqrt(n) of log-scale residuals for either method
#'   in constant-variance Gaussian log-response models. `"none"` omits bars.
#' @param traditional_scale `"log_response"` uses ordinary log-response
#'   residuals. `"standardised"` (or `"standardized"`) reproduces the historical
#'   analyser GLM convention: native `rstandard()` residuals, globally centred,
#'   added to term contributions. This latter display mixes scales, is retained
#'   only for comparison, and is restricted to plain Gaussian log-response GLMs.
#'
#' @details For each stratum, the likelihood method replaces its original
#'   linear predictor eta by eta + delta, holding all other parameters,
#'   fitted random effects, smooths, offsets, and dispersion values fixed.
#'   Gaussian shifts are precision-weighted mean response residuals. With
#'   constant variance and an explicitly logged response this equals the
#'   traditional mean log-response residual. NB2 uses the fitted size parameter
#'   and its log likelihood, not a mean of Pearson or PIT residuals.
#'   Gamma(log) uses the native fitted scale phi (variance = phi * mean^2),
#'   with shape = 1/phi. Its shift is log(sum(shape * response / fitted_mean)
#'   / sum(shape)), or log(mean(response / fitted_mean)) for constant scale.
#'   GAMs retain `sig2`, GLMs use `summary(model)$dispersion`, and glmmTMB
#'   uses squared native dispersion predictions. No shape is re-estimated.
#'   Gamma responses must be strictly positive; other Gamma links and joint
#'   delta models are not automatically reinterpreted as Gamma(log) fits.
#'
#'   Automatic intervals condition on the whole original fit. They omit
#'   uncertainty in its parameters, latent effects, and baseline, and do not
#'   account for residual dependence. They are neither full interaction
#'   confidence intervals nor Bayesian credible intervals. Descriptive bars
#'   are one SE, not intervals with `level` coverage. No MCMC, full-model refit,
#'   posterior averaging, response simulation, or area integration is done.
#'
#'   For all-zero count strata the optimum is delta = -Inf. These boundary
#'   results are retained and flagged, not replaced with a pseudocount or a
#'   finite correction. Their points/bars are omitted from the plot. Empty and
#'   sparse cells, and missing numeric years, break trajectories. Existing `influ_residuals` objects do not
#'   contain the native likelihood needed here: use [plot_grouped_residuals()]
#'   for their zero-centred grouped PIT summaries.
#'
#'   For sdmTMB and tinyVAST lognormal(log), eta is log(arithmetic mean), and the native
#'   dispersion sigma is the log-scale SD. The local likelihood is
#'   `dlnorm(response, eta + delta - sigma^2/2, sigma)`. Offsets, vessel effects,
#'   and spatial and spatiotemporal fields remain at their fitted values.
#'   Baselines are centred over all original fitted rows, including zeros in a
#'   joint fit; only positive rows contribute to its local adjustments. A group
#'   without a fixed main effect uses the year-only baseline, not a fabricated
#'   group coefficient. Zero-only strata remain in the table as empty positive
#'   strata. Native likelihood observations and supplied data are checked before
#'   selecting positive rows. Poisson-link delta and mixture families fail explicitly.
#'   Bernoulli(logit) encounter shifts use all observations; all-zero or all-one
#'   strata have infinite shifts and are retained as flagged boundary results.
#'
#'   tinyVAST uses native fitted predictors, including spatial and yearly fields,
#'   and native log-SD, Gaussian SD, NB2 size, or squared Gamma CV. Only one
#'   response and one family are supported. Bernoulli models require unit trials.
#'
#'   brms uses [brms::prepare_predictions()] with all fitted group-level effects,
#'   smooths, offsets, and distributional parameters retained. The default
#'   collapses parameters before predicting, not response predictions after
#'   nonlinear transformations. `draw_id` instead selects one coherent joint
#'   posterior state for sensitivity comparisons. Neither creates an
#'   observations-by-all-draws prediction array; native preparation may still
#'   materialise the parameter draws. Original posterior convergence must be
#'   checked separately. The output records its reference and conditioning.
#'   brms lognormal/hurdle-lognormal requires its native identity link for
#'   log-location mu. Internally, eta = mu + sigma^2/2 aligns the common
#'   log-arithmetic-mean likelihood; sigma can vary between observations but is
#'   held fixed during each adjustment. The fixed-term baseline contains the
#'   selected mu terms, not sigma terms. For encounter, both the predictor and
#'   baseline coefficients of hu are negated, since hu is the zero probability.
#'   Gaussian requires identity, Bernoulli requires logit, and Poisson, NB2,
#'   and Gamma require log links. Multivariate/nonlinear models, response
#'   additions (weights, censoring, truncation, or trials), autocorrelation,
#'   Gaussian-process terms, and special predictors need separate adapters.
#'
#'   Combined delta-lognormal displays estimate both shifts separately, then
#'   average `plogis(eta_encounter + delta_encounter) * exp(eta_positive +
#'   delta_positive)` over the stratum's original rows, including zero responses.
#'   Both components must satisfy `min_n`, with at least one zero and one positive
#'   observation. Unsupported strata remain flagged. The `estimate`, `baseline`,
#'   and interval bounds are on the response scale; `adjustment` is their log
#'   mean ratio. Separate component shifts and positive counts are also retained.
#'   Profile intervals maximise the joint conditional likelihood over how both
#'   shifts contribute to the derived mean; `std_error` is its conditional
#'   delta-method SE. No equal-shift constraint is imposed. These observed-mix
#'   means retain exposure and sampling composition: they are not CPUE indices,
#'   regional standardisations, or area-integrated abundance. In a model of
#'   response totals with an effort offset, the display remains in those totals'
#'   units, not per-unit-effort units. Traditional/descriptive options do not
#'   apply to combined responses.
#'
#'   glmmTMB direct lognormal is not yet supported: it parameterises mean and SD on
#'   the response scale; holding that SD fixed is not the same as a constant
#'   log-SD shift. Use a Gaussian model of log(response) for the demonstrated
#'   equivalence, not an automatic reinterpretation of another fitted family.
#'
#' @return A compact `influ_implied` object with a stratum `table` and explicit
#'   `metadata`. No fitted model or observation-level arrays are retained.
#' @references The historical analyser `Diagnoser` implementation adds selected
#'   fitted terms to globally centred `rstandard()` residuals for GLMs:
#'   <https://github.com/trophia/analyser/blob/master/R/diagnoser.r>.
#'   Ordinary log-response residuals are a separate, explicitly named convention.
#' @seealso [plot_implied_residuals()], [plot_grouped_residuals()]
#' @examples
#' data(lobsters_per_pot)
#' fit <- glm(lobsters ~ year + month + depth, family = poisson(),
#'   data = lobsters_per_pot)
#' result <- implied_effects(fit, groups = "month")
#' head(as.data.frame(result))
#' plot(result)
#' @md
#' @export
implied_effects <- function(model, data = NULL, year = NULL, groups = "area",
    method = c("likelihood", "traditional"), baseline = c("year_group", "year"),
    min_n = 10L, level = .95, interval = c("auto", "descriptive", "none"),
    traditional_scale = c("log_response", "standardised", "standardized"),
    component = NULL, year_term = NULL, draw_id = NULL) {
  method <- match.arg(method)
  baseline <- match.arg(baseline)
  interval <- match.arg(interval)
  if (method != "traditional" && !missing(traditional_scale)) {
    stop("`traditional_scale` applies only to method = 'traditional'.", call. = FALSE)
  }
  traditional_scale <- match.arg(traditional_scale)
  if (traditional_scale == "standardized") traditional_scale <- "standardised"
  .resid_integer(min_n, "min_n", 1L)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    stop("`level` must be between zero and one.", call. = FALSE)
  }
  if (!is.character(groups) || length(groups) != 1L || is.na(groups) || !nzchar(groups)) {
    stop("`groups` must name one original-data grouping column.", call. = FALSE)
  }
  if (!is.null(component)) component <- match.arg(component, c("conditional", "positive", "encounter", "combined"))
  if (!is.null(draw_id) && !inherits(model, "brmsfit")) stop("`draw_id` applies only to brms implied effects.", call. = FALSE)
  a <- .implied_adapter(model, data, year, groups, baseline, component, year_term, draw_id)
  if (identical(a$component, "combined")) {
    if (method != "likelihood" || interval == "descriptive") {
      stop("Combined implied responses require method = 'likelihood' and interval = 'auto' or 'none'.", call. = FALSE)
    }
    return(.implied_combined(a, groups, min_n, level, interval))
  }
  if (method == "traditional" && !a$log_response) {
    stop("Traditional comparison requires a Gaussian identity-link model of log(response).", call. = FALSE)
  }
  if ((method == "traditional" || interval == "descriptive") &&
      (!a$log_response || diff(range(a$dispersion)) > 1e-10 * max(a$dispersion))) {
    stop("Traditional/descriptive comparison requires constant log-response variance.", call. = FALSE)
  }
  legacy <- a$observed - a$eta
  if (method == "traditional" && traditional_scale == "standardised") {
    if (a$backend != "glm") {
      stop("Historical standardised comparison requires a plain Gaussian GLM, not a GAM or mixed model.", call. = FALSE)
    }
    raw <- stats::rstandard(model)
    raw <- .align_observation_values(raw, a$frame, "Standardised residuals", model)
    if (any(!is.finite(raw))) stop("Historical standardised residuals are not finite.", call. = FALSE)
    legacy <- raw - mean(raw)
  }
  actual_interval <- if (interval == "auto") {
    if (method == "traditional") "descriptive" else "conditional_profile"
  } else interval
  time <- .focus_info(a$data, a$year)
  group <- .focus_info(a$data, groups)
  grid <- expand.grid(level = time$levels, group = group$levels, stringsAsFactors = FALSE)
  table <- do.call(rbind, lapply(seq_len(nrow(grid)), function(j) {
    cell <- grid[j, ]
    i <- which(time$value == cell$level & group$value == cell$group)
    if (!is.null(a$included)) i <- i[a$included[i]]
    n <- length(i)
    b <- if (n) mean(a$baseline[i]) else NA_real_
    shift <- se <- lo <- hi <- NA_real_
    status <- if (!n) "empty" else if (n < min_n) "sparse" else "ok"
    if (status == "ok") {
      if (method == "traditional") {
        shift <- mean(legacy[i])
        se <- if (n > 1L) stats::sd(legacy[i]) / sqrt(n) else NA_real_
      } else {
        fitted <- .implied_shift(a$observed[i], a$eta[i], a$dispersion[i], a$family)
        shift <- fitted$shift
        se <- fitted$std_error
        if (!is.finite(shift)) status <- if (shift > 0) "boundary_one" else "boundary_zero"
      }
      if (status == "ok" && actual_interval == "conditional_profile") {
        ci <- .implied_profile(shift, a$observed[i], a$eta[i], a$dispersion[i], a$family, level)
        lo <- ci[1L]
        hi <- ci[2L]
      } else if (status == "ok" && actual_interval == "descriptive") {
        se <- if (n > 1L) stats::sd(legacy[i]) / sqrt(n) else NA_real_
        lo <- shift - se
        hi <- shift + se
      }
    }
    data.frame(level = cell$level, group = cell$group, n = n, baseline = b,
      adjustment = shift, estimate = b + shift, std_error = se,
      lower = b + lo, upper = b + hi, status = status)
  }))
  metadata <- list(backend = a$backend, family = a$family, link = a$link,
    response = a$response, log_response = a$log_response, year = a$year,
    year_levels = time$levels, groups = groups, group_levels = group$levels,
    method = method, baseline = baseline,
    baseline_terms = a$baseline_terms, centring = "Mean term contributions over fitted observations",
    conditioning = "Original parameters, smooths, offsets, and fitted random effects held fixed",
    traditional_scale = if (method == "traditional") traditional_scale else NULL,
    interval = actual_interval, level = if (actual_interval == "conditional_profile") level else NA_real_,
    min_n = min_n, n = if (is.null(a$included)) length(a$observed) else sum(a$included),
    component = a$component %||% "single fitted response",
    format_version = 1L)
  if (a$family == "Gamma") {
    metadata$dispersion <- "Native fitted Gamma scale phi = variance / mean^2; shape = 1/phi"
  }
  if (a$backend %in% c("sdmTMB", "tinyVAST", "brms")) {
    if (a$family == "lognormal") metadata$dispersion <- "Native fitted log-scale SD; meanlog = eta - sigma^2/2"
    metadata$n_total <- length(a$observed)
    metadata$n_excluded <- sum(!a$included)
    metadata$component_index <- a$component_index
    metadata$baseline_population <- "All original fitted rows, including zeros in joint models"
    metadata$baseline_group_present <- a$baseline_group_present
    metadata$year_term <- a$year_term
  }
  if (a$backend == "brms") {
    metadata$conditioning <- a$conditioning
    metadata$reference <- a$reference
    metadata$draw_id <- a$draw_id
  }
  structure(list(table = table, metadata = metadata), class = "influ_implied")
}

.implied_adapter <- function(model, data, year, groups, baseline, component = NULL, year_term = NULL, draw_id = NULL) {
  if (inherits(model, "sdmTMB")) {
    return(.implied_sdmtmb_adapter(model, data, year, groups, baseline, component, year_term))
  }
  if (inherits(model, "tinyVAST")) return(.implied_tinyvast_adapter(model, data, year, groups, baseline, component, year_term))
  if (inherits(model, "brmsfit")) return(.implied_brms_adapter(model, data, year, groups, baseline, component, year_term, draw_id))
  if (!is.null(year_term)) stop("`year_term` applies only to sdmTMB, tinyVAST, and brms implied effects.", call. = FALSE)
  if (!is.null(component) && component != "conditional") {
    stop("This backend does not yet support component-specific implied effects.", call. = FALSE)
  }
  if (inherits(model, "influ_residuals")) {
    stop("Stored PIT residuals do not contain the native likelihood. Supply the fitted model, or use plot_grouped_residuals() for grouped PIT departures.", call. = FALSE)
  }
  backend <- if (inherits(model, "glmmTMB")) "glmmTMB" else if (inherits(model, "gam")) "gam" else
    if (inherits(model, "glm")) "glm" else if (inherits(model, "lm")) "lm" else NA_character_
  if (is.na(backend)) stop("Implied effects require a supported lm, GLM, GAM, glmmTMB, sdmTMB, tinyVAST, or brms fit.", call. = FALSE)
  .require_model_backend(model)
  if (backend == "lm" && is.null(model$model)) stop("A retained model frame is required.", call. = FALSE)
  frame <- .residual_model_frame(model)
  if (is.null(frame) || !nrow(frame)) stop("A retained model frame is required.", call. = FALSE)
  f <- stats::formula(model)
  if (is.null(data) && groups %in% names(frame) &&
      (is.null(year) || year %in% names(frame))) data <- frame
  aligned <- .residual_observations(model, data, seq_len(nrow(frame)))$data
  time <- .resid_year(model, aligned, year)
  if (!groups %in% names(aligned) || anyNA(aligned[[groups]]) ||
      !is.atomic(aligned[[groups]]) || !is.null(dim(aligned[[groups]])) ||
      (is.numeric(aligned[[groups]]) && any(!is.finite(aligned[[groups]])))) {
    stop("`groups` must name a finite categorical column in the original model data.", call. = FALSE)
  }
  if (groups %in% all.vars(f[[2L]]) || time$name %in% all.vars(f[[2L]])) {
    stop("Year and groups must not be defined from the response.", call. = FALSE)
  }
  if (identical(groups, time$name)) stop("Choose a grouping column different from year.", call. = FALSE)
  if (backend == "glmmTMB") {
    if (.glmmTMB_has_component(model, "zi")) {
      stop("Joint hurdle/zero-inflated implied effects need an explicit component definition; combined responses are not replaced with positive components.", call. = FALSE)
    }
    if (isTRUE(model$modelInfo$REML) || !isTRUE(model$sdr$pdHess) || model$fit$convergence != 0L) {
      stop("Implied effects require a converged ML glmmTMB fit with a positive-definite Hessian.", call. = FALSE)
    }
  } else if (isFALSE(model$converged)) stop("The supplied model did not converge.", call. = FALSE)
  weights <- stats::model.weights(frame)
  if (!is.null(weights) && any(!is.finite(weights) | weights != 1)) {
    stop("Non-unit case weights are not yet supported for implied effects.", call. = FALSE)
  }
  fam <- if (backend == "lm") stats::gaussian() else stats::family(model)
  raw_family <- fam$family
  family <- if (grepl("^Negative Binomial|^nbinom2$", raw_family, ignore.case = TRUE)) "nbinom2" else raw_family
  if (!((family == "gaussian" && fam$link == "identity") ||
      (family %in% c("poisson", "nbinom2", "Gamma") && fam$link == "log"))) {
    stop("Supported implied-effect families are Gaussian(identity), Poisson(log), NB2(log), and Gamma(log). Other parameterisations require a validated adapter.", call. = FALSE)
  }
  log_response <- family == "gaussian" && is.call(f[[2L]]) && length(f[[2L]]) == 2L &&
    identical(f[[2L]][[1L]], as.name("log")) && is.symbol(f[[2L]][[2L]])
  if (!is.symbol(f[[2L]]) && !log_response) {
    stop("Use a named response or a Gaussian model of log(response); other transformations are not supported.", call. = FALSE)
  }
  observed <- stats::model.response(frame)
  if (!is.numeric(observed) || !is.null(dim(observed)) || any(!is.finite(observed))) {
    stop("A finite numeric response vector is required.", call. = FALSE)
  }
  n <- length(observed)
  if (family %in% c("poisson", "nbinom2") && any(observed < 0 | abs(observed - round(observed)) > 1e-7)) {
    stop("Count-model responses must be non-negative integers.", call. = FALSE)
  }
  if (family == "Gamma" && any(observed <= 0)) {
    stop("Gamma implied effects require strictly positive responses.", call. = FALSE)
  }
  X <- stats::model.matrix(model)
  beta <- if (backend == "glmmTMB") glmmTMB::fixef(model)$cond else stats::coef(model)
  if (any(!is.finite(beta)) || !all(colnames(X) %in% names(beta))) {
    stop("Aliased or unaligned coefficients are not supported for implied effects.", call. = FALSE)
  }
  beta <- beta[colnames(X)]
  if (nrow(X) != n || any(!is.finite(X))) stop("The fitted model matrix is not aligned.", call. = FALSE)
  terms <- if (backend == "gam") model$pterms else stats::terms(model)
  labels <- attr(terms, "term.labels")
  assignments <- if (backend == "gam") model$assign else attr(X, "assign")
  variables <- lapply(labels, function(label) all.vars(stats::as.formula(paste("~", label))))
  year_terms <- which(vapply(variables, function(v) time$name %in% v, logical(1)))
  if (!length(year_terms) || any(lengths(variables[year_terms]) != 1L) ||
      (backend == "gam" && any(vapply(model$smooth, function(s) time$name %in% c(s$term, s$by), logical(1))))) {
    stop("An additive fixed year term is required; year interactions and year smooths need a separate baseline definition.", call. = FALSE)
  }
  chosen <- year_terms
  if (baseline == "year_group") chosen <- union(chosen,
    which(vapply(variables, function(v) identical(v, groups), logical(1))))
  columns <- which(assignments %in% chosen)
  if (!length(columns)) stop("No fixed-effect baseline columns were found.", call. = FALSE)
  term <- drop(X[, columns, drop = FALSE] %*% beta[columns])
  base <- term - mean(term)
  if (backend == "glmmTMB") {
    alignment <- model
    alignment$na.action <- attr(frame, "na.action")
    eta <- .align_observation_values(stats::predict(model, type = "link", re.form = NULL), frame,
      "Fitted linear predictors", alignment)
    dispersion <- .align_observation_values(stats::predict(model, type = "disp", re.form = NULL), frame,
      "Fitted dispersion", alignment)
    # glmmTMB reports Gamma sigma = 1/sqrt(shape), not phi = 1/shape.
    if (family == "Gamma") dispersion <- dispersion^2
  } else {
    eta <- if (backend == "lm") model$fitted.values else model$linear.predictors
    dispersion <- if (family == "nbinom2") {
      if (is.function(fam$getTheta)) fam$getTheta(TRUE) else model$theta
    } else if (family == "gaussian") {
      if (backend == "gam") sqrt(model$sig2) else stats::sigma(model)
    } else if (family == "Gamma") {
      if (backend == "gam") model$sig2 else summary(model)$dispersion
    } else 1
  }
  if (!is.numeric(dispersion) || !length(dispersion) || !length(dispersion) %in% c(1L, n)) {
    stop("A native dispersion value per row, or one common value, is required.", call. = FALSE)
  }
  dispersion <- rep_len(dispersion, n)
  if (length(eta) != n || any(!is.finite(eta)) || length(dispersion) != n ||
      any(!is.finite(dispersion) | dispersion <= 0)) {
    stop("Finite aligned predictors and positive dispersion values are required.", call. = FALSE)
  }
  list(backend = backend, family = family, link = fam$link, frame = frame, data = aligned,
    year = time$name, observed = as.numeric(observed), eta = as.numeric(eta),
    dispersion = as.numeric(dispersion), baseline = base, baseline_terms = labels[chosen],
    response = paste(deparse(f[[2L]]), collapse = " "), log_response = log_response)
}

.implied_loglik <- function(delta, y, eta, dispersion, family) {
  z <- eta + delta
  if (family == "binomial") {
    # Bernoulli/logit likelihood, stable in both tails.
    softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
    return(sum(-y * softplus(-z) - (1 - y) * softplus(z)))
  }
  if (family == "lognormal") return(sum(stats::dlnorm(y, z - dispersion^2 / 2, dispersion, log = TRUE)))
  if (family == "gaussian") return(sum(stats::dnorm(y, z, dispersion, log = TRUE)))
  if (family == "poisson") return(sum(y * z - exp(z) - lgamma(y + 1)))
  if (family == "Gamma") {
    log_shape <- -log(dispersion)
    shape <- exp(log_shape)
    log_ratio <- log(y) - z
    return(sum(shape * log_shape - lgamma(shape) - log(y) +
      shape * log_ratio - exp(log_shape + log_ratio)))
  }
  if (family != "nbinom2") stop("Unsupported implied-effect likelihood.", call. = FALSE)
  # Stable NB2 log likelihood, including for extreme finite shifts.
  softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
  v <- z - log(dispersion)
  sum(lgamma(y + dispersion) - lgamma(dispersion) - lgamma(y + 1) -
    dispersion * softplus(v) - y * softplus(-v))
}

.implied_shift <- function(y, eta, dispersion, family) {
  if (family == "binomial") {
    if (all(y == 0)) return(list(shift = -Inf, std_error = NA_real_))
    if (all(y == 1)) return(list(shift = Inf, std_error = NA_real_))
    score <- function(delta) sum(y - stats::plogis(eta + delta))
    bounds <- c(-max(eta) - 40, -min(eta) + 40)
    delta <- stats::uniroot(score, bounds, tol = 1e-10)$root
    p <- stats::plogis(eta + delta)
    return(list(shift = delta, std_error = 1 / sqrt(sum(p * (1 - p)))))
  }
  if (family == "lognormal") {
    w <- 1 / dispersion^2
    return(list(shift = sum(w * (log(y) - eta + dispersion^2 / 2)) / sum(w),
      std_error = sqrt(1 / sum(w))))
  }
  if (family == "gaussian") {
    w <- 1 / dispersion^2
    return(list(shift = sum(w * (y - eta)) / sum(w), std_error = sqrt(1 / sum(w))))
  }
  if (family == "Gamma") {
    # Log-sum-exp avoids constructing y/exp(eta), which can overflow.
    lse <- function(x) { largest <- max(x); largest + log(sum(exp(x - largest))) }
    log_shape <- -log(dispersion)
    total <- lse(rep_len(log_shape, length(y)))
    delta <- lse(log_shape + log(y) - eta) - total
    return(list(shift = delta, std_error = exp(-total / 2)))
  }
  if (!family %in% c("poisson", "nbinom2")) stop("Unsupported implied-effect likelihood.", call. = FALSE)
  if (all(y == 0)) return(list(shift = -Inf, std_error = NA_real_))
  if (family == "poisson") {
    largest <- max(eta)
    delta <- log(sum(y)) - largest - log(sum(exp(eta - largest)))
    return(list(shift = delta, std_error = 1 / sqrt(sum(y))))
  }
  score <- function(delta) {
    p <- stats::plogis(eta + delta - log(dispersion))
    sum(y * (1 - p) - dispersion * p)
  }
  bounds <- c(-1, 1)
  for (j in seq_len(12L)) {
    if (score(bounds[1]) >= 0 && score(bounds[2]) <= 0) break
    bounds <- bounds * 2
  }
  if (score(bounds[1]) < 0 || score(bounds[2]) > 0) stop("Could not bracket the NB2 local effect.", call. = FALSE)
  delta <- stats::uniroot(score, bounds, tol = 1e-10)$root
  p <- stats::plogis(eta + delta - log(dispersion))
  list(shift = delta, std_error = 1 / sqrt(sum((y + dispersion) * p * (1 - p))))
}

.implied_profile <- function(delta, y, eta, dispersion, family, level) {
  if (family == "lognormal") {
    # Exact quadratic profile in delta; avoids subtracting large log likelihoods.
    se <- sqrt(1 / sum(1 / rep_len(dispersion, length(y))^2))
    return(delta + c(-1, 1) * stats::qnorm((1 + level) / 2) * se)
  }
  if (family == "Gamma") {
    # Profile relative to the exact optimum: no subtraction of large log
    # likelihoods and no dependence on an extreme original linear predictor.
    total_shape <- sum(1 / rep_len(dispersion, length(y)))
    cutoff <- stats::qchisq(level, df = 1) / 2
    loss <- function(h) {
      if (abs(h) < 1e-4) h^2 * (.5 - h / 6 + h^2 / 24 - h^3 / 120) else h + expm1(-h)
    }
    gamma_profile <- function(h) total_shape * loss(h) - cutoff
    return(delta + vapply(c(-1, 1), function(direction) {
      width <- .25
      for (j in seq_len(14L)) {
        end <- direction * width
        if (gamma_profile(end) >= 0) return(stats::uniroot(gamma_profile, sort(c(0, end)), tol = 1e-10)$root)
        width <- width * 2
      }
      direction * Inf
    }, numeric(1)))
  }
  maximum <- .implied_loglik(delta, y, eta, dispersion, family)
  cutoff <- stats::qchisq(level, df = 1) / 2
  f <- function(x) maximum - .implied_loglik(x, y, eta, dispersion, family) - cutoff
  vapply(c(-1, 1), function(direction) {
    width <- .25
    for (j in seq_len(14L)) {
      end <- delta + direction * width
      if (f(end) >= 0) return(stats::uniroot(f, sort(c(delta, end)), tol = 1e-9)$root)
      width <- width * 2
    }
    direction * Inf
  }, numeric(1))
}
