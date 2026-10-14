# Optional tests use a genuine native object or genuine DHARMa PIT vectors plus
# bounded-memory simulation reducers. Never fabricate a partial DHARMa object.
# Saved plots format these compact results; they never invoke a test or RNG.
.resid_test_options <- function(residual_tests, residual_method) {
  mode <- match.arg(residual_tests, c("none", "standard", "full"))
  if (mode != "none" && residual_method != "dharma") {
    stop("`residual_tests` requires `residual_method = 'dharma'`; influ2's default PIT ranks are not DHARMa residuals.", call. = FALSE)
  }
  mode
}

.resid_test_run <- function(fun) {
  notes <- character()
  value <- tryCatch(withCallingHandlers(fun(), warning = function(w) {
    notes <<- c(notes, conditionMessage(w))
    invokeRestart("muffleWarning")
  }, message = function(m) {
    notes <<- c(notes, conditionMessage(m))
    invokeRestart("muffleMessage")
  }), error = function(e) e)
  list(value = if (inherits(value, "error")) NULL else value,
    error = if (inherits(value, "error")) conditionMessage(value) else NULL,
    notes = unique(notes))
}

.resid_test_categorical <- function(native, year) {
  # DHARMa 0.5.0 calls title() even with plot = FALSE. Isolate that native
  # graphics behaviour in a file-free device, restoring the caller's device.
  old <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  device <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(which = device)
    if (old > 1L && old %in% grDevices::dev.list()) grDevices::dev.set(old)
  }, add = TRUE)
  graphics::plot.new()
  DHARMa::testCategorical(native, catPred = year, plot = FALSE)
}

.resid_dharma_test_table <- function(native, result, mode = c("standard", "full"), cache = NULL) {
  mode <- match.arg(mode)
  disk <- !is.null(cache)
  if ((!disk && !inherits(native, "DHARMa")) ||
      !identical(result$metadata$residual_method, "dharma")) {
    stop("Native DHARMa tests require a genuine DHARMa object and matching DHARMa residual metadata.", call. = FALSE)
  }
  if (!disk && !identical(as.numeric(native$scaledResiduals), as.numeric(result$observations$pit))) {
    stop("The native DHARMa object does not match the stored PIT values.", call. = FALSE)
  }
  if (!disk && (length(native$scaledResiduals) != nrow(result$observations) ||
      !identical(as.numeric(native$observedResponse), as.numeric(result$observations$observed)) ||
      !isTRUE(all.equal(as.numeric(native$fittedPredictedResponse),
        as.numeric(result$observations$predicted), tolerance = 1e-12)))) {
    stop("Native response data or predictive means do not match the compact residual result.", call. = FALSE)
  }
  if (!disk && isTRUE(native$refit)) {
    stop("These compact tests require non-refitted response simulations; no Pearson/refitted dispersion substitution is made.", call. = FALSE)
  }
  if (disk) {
    .resid_dharma_stream_check(cache, result$observations$observed)
    .resid_dharma_validate_pit(result$observations$pit, cache$n)
  }
  input <- if (disk) result$observations$pit else native
  nsim <- if (disk) cache$nsim else native$nSim
  integer <- if (disk) isTRUE(result$metadata$integer_response) else isTRUE(native$integerResponse)
  saved_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    get(".Random.seed", .GlobalEnv) else NULL
  on.exit({
    if (is.null(saved_seed)) {
      if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", saved_seed, envir = .GlobalEnv)
  }, add = TRUE)
  seed <- as.integer((as.double(result$metadata$dharma_seed %||% result$metadata$seed %||% 1L) + 130363) %% .Machine$integer.max)
  set.seed(seed)
  n <- nrow(result$observations)
  conditioning <- result$metadata$conditioning %||% result$metadata$scheme
  limitation <- paste("Exploratory DHARMa reference; not calibrated for this fitted model.",
    "Parameter estimation, posterior-data reuse, latent conditioning, and dependence affect interpretation.",
    "No flag is not proof of model adequacy.")
  if (result$metadata$response_kind %in% c("bernoulli", "grouped_binomial")) {
    limitation <- paste(limitation, "Pooled PIT checks do not replace probability calibration with known trials.")
  }
  if (identical(result$metadata$component, "combined")) {
    limitation <- paste(limitation, "The combined-response check does not validate either component separately.")
  }
  rows <- list()
  row <- function(check, value = NULL, term = "response", p_adjusted = NA_real_,
      message = "", method = NULL, status = NULL, size = n, notes = character()) {
    p <- if (!is.null(value$p.value)) as.numeric(value$p.value)[1L] else NA_real_
    statistic <- if (!is.null(value$statistic)) as.numeric(value$statistic)[1L] else NA_real_
    df <- if (check == "year_homogeneity" && !is.null(value$parameter)) as.numeric(value$parameter)[1L] else NA_real_
    decision_p <- if (is.finite(p_adjusted)) p_adjusted else p
    if (is.null(status)) status <- if (!is.finite(p)) "not_assessed" else if (decision_p < 0.05) "review" else "no_flag"
    rows[[length(rows) + 1L]] <<- .cpue_check_row(scope = if (check %in% c("uniformity", "dispersion", "outliers")) "overall" else "term", term = term,
      check = check, status = status, statistic = statistic, df = df,
      p_value = p, p_adjusted = p_adjusted,
      method = method %||% value$method %||% "Native DHARMa test",
      message = paste(c(message, notes, limitation), collapse = " "),
      action = if (status == "not_assessed") "Review availability and data support; this is not a successful check." else
        "Inspect the corresponding plot and effect size; do not use this p-value as an automatic model-selection rule.",
      component = result$metadata$component, conditioning = conditioning,
      calibration = "exploratory; fitted-model calibration not established", n = size)
  }
  native_test <- function(check, fun, term = "response", message = "") {
    run <- .resid_test_run(fun)
    failed_equivalent <- disk && is.null(run$value) && check %in% c("dispersion", "outliers")
    row(check, run$value, term = term, message = paste(message, run$error %||% ""), notes = run$notes,
      method = if (failed_equivalent) "Bounded-memory DHARMa equivalent unavailable" else NULL)
    run$value
  }
  native_test("uniformity", function() DHARMa::testUniformity(input, plot = FALSE),
    message = "KS statistic D tests the uniform-scale DHARMa values, not normal-score display limits.")
  native_test("dispersion", function() if (disk)
    .resid_dharma_stream_dispersion(cache, result$observations$observed, result$observations$predicted) else
    DHARMa::testDispersion(native, type = "DHARMa", plot = FALSE),
    message = "Observed/simulated raw-response residual variance ratio; no Pearson test or model refitting.")
  years <- droplevels(factor(result$observations$year))
  counts <- table(years)
  eligible <- names(counts)[counts >= 5L]
  sparse <- names(counts)[counts < 5L]
  for (level in sparse) {
    row("year_uniformity", term = level, size = as.integer(counts[level]),
      status = "not_assessed", message = "Fewer than five observations in this year; no within-year significance test.")
  }
  if (length(sparse)) {
    row("year_homogeneity", term = result$metadata$year, status = "not_assessed",
      message = "At least one year has fewer than five observations; the all-year variance comparison is not calculated.")
  } else if (length(counts) < 2L) {
    row("year_homogeneity", term = result$metadata$year, status = "not_assessed",
      message = "Only one sampled year; no between-year comparison is available.")
  }
  if (length(eligible)) {
    # Native per-year KS values are unaltered. Restrict sparse groups before
    # testing and record the Holm family explicitly in the saved rows.
    selected <- years %in% eligible
    categorical_native <- if (disk) input[selected] else native
    if (!disk) {
      categorical_native$scaledResiduals <- native$scaledResiduals[selected]
      categorical_native$nObs <- sum(selected)
    }
    categorical_years <- droplevels(years[selected])
    run <- .resid_test_run(function() .resid_test_categorical(categorical_native, categorical_years))
    value <- run$value
    if (is.null(value)) {
      row("year_uniformity", term = result$metadata$year,
        status = "not_assessed", message = run$error %||% "Native categorical test unavailable.", notes = run$notes)
      if (!length(sparse) && length(counts) > 1L) row("year_homogeneity", term = result$metadata$year,
        status = "not_assessed", message = run$error %||% "Native categorical test unavailable.", notes = run$notes)
    } else {
      details <- value$uniformity$details
      adjusted <- stats::p.adjust(value$uniformity$p.value, method = "holm")
      for (i in seq_along(details)) {
        level <- levels(categorical_years)[i]
        row("year_uniformity", details[[i]], term = level, p_adjusted = adjusted[i],
          size = as.integer(counts[level]), method = "DHARMa within-year KS; Holm correction across tested years",
          message = paste("Uniform-scale test; Holm family contains", length(details), "supported years."), notes = run$notes)
      }
      if (!length(sparse) && length(counts) > 1L) {
        homogeneity <- value$homogeneity
        row("year_homogeneity", list(statistic = homogeneity$`F value`[1L],
          parameter = homogeneity$Df[1L], p.value = homogeneity$`Pr(>F)`[1L]),
          term = result$metadata$year, method = "DHARMa median-centred Levene test",
          message = "Compares between-year variance on the uniform PIT scale; not Kruskal-Wallis.", notes = run$notes)
      }
    }
  }
  if (mode == "full") {
    if (n < 20L || length(unique(result$observations$predicted)) < 5L) {
      row("quantiles", term = "predictive_mean", status = "not_assessed",
        message = "Quantile regressions require at least 20 observations and five distinct predictive means.")
    } else {
      value <- native_test("quantiles", function() DHARMa::testQuantiles(input,
        predictor = result$observations$predicted, plot = FALSE), term = "predictive_mean",
        message = "Auxiliary qgam regressions of uniform PIT quartiles; BH-adjusted combined p-value, not significance of the displayed loess or a CPUE refit.")
      if (!is.null(value) && any(!is.finite(value$pvals))) {
        rows[[length(rows)]]$status <- "not_assessed"
        rows[[length(rows)]]$p_value <- NA_real_
        rows[[length(rows)]]$message <- paste(rows[[length(rows)]]$message,
          "At least one requested quantile failed; no complete combined test is available.")
      }
    }
    native_test("outliers", function() if (disk)
      .resid_dharma_stream_outliers(cache, result$observations$observed,
        result$observations$pit, integer, nboot = min(100L, nsim)) else
      DHARMa::testOutliers(native,
        type = if (integer) "bootstrap" else "binomial", nBoot = min(100L, nsim), plot = FALSE),
      message = if (integer) paste("Bootstrap outlier reference resamples existing response simulations;",
        "up to 100 bootstrap replicates, extra calculation cost, finite Monte Carlo precision, no model refit.") else
        "Continuous-response binomial outlier reference; outside the finite simulation range is not a measure of response extremeness.")
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "test_metadata") <- list(mode = mode,
    dharma_version = as.character(utils::packageVersion("DHARMa")),
    qgam_version = if (mode == "full") tryCatch(as.character(utils::packageVersion("qgam")), error = function(e) NULL) else NULL,
    seed = seed, nsim = nsim, refit = FALSE,
    storage = if (disk) "disk" else "memory",
    implementation = if (disk) c(uniformity = "native DHARMa vector test",
      dispersion = "influ2 bounded-memory DHARMa equivalent",
      year = "native DHARMa vector test", quantiles = "native DHARMa vector test",
      outliers = "influ2 bounded-memory DHARMa equivalent") else "native DHARMa object tests",
    equivalent_reference = if (disk) .resid_dharma_stream_source() else NULL,
    alpha = 0.05, year_adjustment = "Holm within eligible years",
    quantile_adjustment = "Native DHARMa BH within quantile tests; no across-check correction",
    outlier_type = if (mode == "full") if (integer) "bootstrap" else "binomial" else NULL,
    outlier_nboot = if (mode == "full" && integer) min(100L, nsim) else NULL,
    calibration = limitation, retention = "Compact test statistics only; no response matrix or auxiliary diagnostic fits")
  out
}

.resid_test_number <- function(x) if (!is.finite(x)) "NA" else format(signif(x, 3), trim = TRUE)
.resid_test_p <- function(x) if (!is.finite(x)) "not available" else if (x < 0.001) "< 0.001" else paste0("= ", format(signif(x, 3), trim = TRUE))

.resid_test_annotation <- function(x, type) {
  tests <- x$tests
  if (!is.data.frame(tests) || !nrow(tests)) return("Tests not stored (calculate residual_tests = 'standard' with DHARMa).")
  select <- function(name) tests[tests$check == name, , drop = FALSE]
  one <- function(name, label, statistic = NULL) {
    d <- select(name)
    if (!nrow(d)) return(paste(label, "not calculated"))
    d <- d[1L, , drop = FALSE]
    if (!is.finite(d$p_value) || d$status == "not_assessed") return(paste(label, "not available"))
    replicates <- if (name == "outliers" && identical(x$metadata$residual_tests$outlier_type, "bootstrap"))
      x$metadata$residual_tests$outlier_nboot else x$metadata$nsim
    p <- if (name %in% c("dispersion", "outliers") && d$p_value == 0)
      paste0("Monte Carlo p = 0 (", replicates, " simulations)") else paste("p", .resid_test_p(d$p_value))
    paste0(label, if (!is.null(statistic)) paste0(" ", statistic, " = ", .resid_test_number(d$statistic), ";") else "",
      if (grepl("^Monte Carlo", p)) "\n" else " ", p)
  }
  if (type %in% c("qq", "pit_ecdf", "pit_ecdf_diff")) return(one("uniformity", "DHARMa uniformity", "D"))
  if (type == "fitted") return(one("quantiles", "DHARMa quantile test (uniform PIT)"))
  if (type == "year") {
    d <- select("year_uniformity")
    supported <- is.finite(d$p_adjusted)
    alpha <- x$metadata$residual_tests$alpha %||% 0.05
    grouped <- if (any(supported)) paste0(sum(d$p_adjusted[supported] < alpha), "/", sum(supported),
      " years with Holm-adjusted KS p < ", .resid_test_number(alpha)) else "Within-year KS tests unavailable"
    return(paste(one("year_homogeneity", "Uniform-PIT Levene", "F"), grouped, sep = "\n"))
  }
  if (type == "distribution") {
    d <- select("dispersion")
    equivalent <- any(grepl("bounded-memory equivalent", d$method, fixed = TRUE))
    return(one("dispersion", if (equivalent) "DHARMa-equivalent response dispersion" else
      "DHARMa response dispersion", "ratio"))
  }
  "Probability calibration is exploratory; no automatic calibration p-value."
}

.resid_plot_annotations <- function(p, x, type, annotations) {
  if (annotations %in% c("tests", "all")) {
    p <- p + ggplot2::labs(subtitle = paste(c(p$labels$subtitle,
      .resid_test_annotation(x, type)), collapse = "\n"))
  }
  if (annotations %in% c("model", "all")) {
    model <- if (is.null(x$model_statistics)) "Model statistics not stored; calculate check_cpue() with a fitted model." else .cpue_model_annotation(x$model_statistics)
    p <- p + ggplot2::labs(caption = paste(c(p$labels$caption, model), collapse = "\n"))
  }
  p
}
