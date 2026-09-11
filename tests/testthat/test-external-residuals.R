external_residual_args <- function(kind = "distribution") {
  case <- residual_baseline_case(kind)
  d <- case$data
  d$operation <- rownames(d)
  args <- list(simulations = case$simulations, data = d, response = "response",
    year = "year", response_kind = case$adapter$response_kind,
    conditioning = "Joint responses from a fixed test distribution",
    component = case$adapter$component, observation_id = "operation",
    seed = 601L, batch_size = 7L, grid_size = 25L, groups = "fleet",
    calibration_bins = 3L, calibration_min_n = 4L)
  if (kind %in% c("bernoulli", "grouped")) {
    args$data$p <- case$adapter$probability
    args$data$trials <- case$adapter$trials
    args$probability <- "p"
    args$probability_conditioning <- "Fixed fitted probabilities"
    args$trial_counts <- "trials"
    args$calibration_groups <- c("year", "fleet")
  }
  args
}

test_that("external joint simulations reproduce ranks, means, and ECDF quantiles", {
  local_mocked_bindings(.resid_adapter = function(...) stop("must not simulate a model"))
  for (kind in c("distribution", "continuous", "combined", "positive", "bernoulli", "grouped")) {
    args <- external_residual_args(kind)
    before <- args
    set.seed(601L)
    u <- runif(nrow(args$data))
    set.seed(37L)
    rng <- .Random.seed
    x <- do.call(as_influ_residuals, args)
    expect_identical(.Random.seed, rng)
    expect_identical(args, before)
    y <- args$data$response
    s <- args$simulations
    pit <- (rowSums(s < y) + u * (rowSums(s == y) + 1)) / (ncol(s) + 1)
    expect_equal(x$observations$pit, pit, ignore_attr = TRUE)
    expect_equal(x$observations$residual, qnorm(pit), ignore_attr = TRUE)
    expect_equal(x$observations$predicted, rowMeans(s), ignore_attr = TRUE)
    curves <- apply(s, 2, function(column) ecdf(column)(x$ecdf$response))
    expected <- t(apply(curves, 1, quantile, c(0.025, 0.5, 0.975), names = FALSE))
    expect_equal(as.matrix(x$ecdf[c("lower", "median", "upper")]), expected, ignore_attr = TRUE)
    expect_equal(x$observed_ecdf$probability, ecdf(y)(x$observed_ecdf$response))
    expect_identical(x$observations$row, args$data$operation)
    expect_identical(x$metadata$component, args$component)
    expect_identical(x$metadata$scheme, args$conditioning)
    expect_identical(x$metadata$backend, "external")
    expect_identical(x$metadata$input, "external_response_matrix")
    expect_identical(x$metadata$observation_id, "operation")
    expect_equal(x$metadata$nsim, ncol(s))
    expect_s3_class(plot(x), "patchwork")
    expect_s3_class(plot_predicted_residuals(x), "ggplot")
    grouped <- plot_grouped_residuals(x, groups = "fleet", min_n = 2)
    expect_s3_class(grouped, "ggplot")
    expect_identical(attr(grouped, "residual_metadata")$component, args$component)
    expect_identical(x, do.call(as_influ_residuals, args))
    if (!is.null(args$probability)) {
      ids <- x$observations$calibration_bin
      for (j in seq_len(nrow(x$calibration$bins))) {
        rows <- which(ids == j)
        simulated_proportions <- colSums(s[rows, , drop = FALSE]) / sum(args$data$trials[rows])
        expect_equal(unname(unlist(x$calibration$bins[j, c("lower", "median", "upper")])),
          quantile(simulated_proportions, c(0.025, 0.5, 0.975), names = FALSE))
      }
      expect_s3_class(plot(x, type = "calibration_groups"), "ggplot")
      expect_match(x$metadata$prediction_type, args$probability_conditioning, fixed = TRUE)
    }
  }
})

test_that("external batching preserves numerical order and discards input storage", {
  args <- external_residual_args("continuous")
  a <- do.call(as_influ_residuals, args)
  args$batch_size <- 1L
  b <- do.call(as_influ_residuals, args)
  args$batch_size <- ncol(args$simulations)
  c <- do.call(as_influ_residuals, args)
  expect_identical(a$observations, b$observations)
  expect_identical(a$observations, c$observations)
  expect_identical(a$qq, c$qq)
  expect_false(identical(b$ecdf$response, c$ecdf$response)) # documented first-batch grid
  forbidden <- function(x) {
    if (is.function(x) || is.environment(x) || is.matrix(x)) return(TRUE)
    is.list(x) && any(vapply(x, forbidden, logical(1)))
  }
  expect_false(forbidden(a))
  size <- as.numeric(object.size(a))
  args$simulations <- args$simulations[, rep(seq_len(ncol(args$simulations)), 100), drop = FALSE]
  colnames(args$simulations) <- NULL
  args$batch_size <- 7L
  large <- do.call(as_influ_residuals, args)
  expect_lte(as.numeric(object.size(large)), size + 2048)
  expect_false(forbidden(large))
  expect_lt(as.numeric(object.size(large)), as.numeric(object.size(args$simulations)) / 5)
})

test_that("explicit operation IDs preserve alignment when data row names differ", {
  args <- external_residual_args()
  args$data$operation <- seq(1001L, 1030L)
  rownames(args$simulations) <- as.character(args$data$operation)
  before <- args
  x <- do.call(as_influ_residuals, args)
  expect_identical(x$observations$row, as.character(args$data$operation))
  expect_identical(rownames(x$groups), x$observations$row)
  expect_identical(x$groups$fleet, args$data$fleet)
  expect_identical(args, before)
  expect_s3_class(plot_grouped_residuals(x, groups = "fleet", min_n = 2), "ggplot")
})

test_that("a native glmmTMB simulation replay agrees with the external route", {
  skip_if_not_installed("glmmTMB")
  set.seed(91)
  d <- expand.grid(year = factor(2010:2013), repeat_id = 1:25)
  d$x <- rnorm(nrow(d))
  effects <- rnorm(25, sd = 0.6)
  d$response <- rnbinom(nrow(d), size = 2,
    mu = exp(0.5 + 0.1 * as.numeric(d$year) + 0.3 * d$x + effects[d$repeat_id]))
  fit <- glmmTMB::glmmTMB(response ~ year + x + (1 | repeat_id),
    family = glmmTMB::nbinom2(), data = d)
  original_adapter <- .resid_adapter
  chunks <- list()
  local_mocked_bindings(.resid_adapter = function(...) {
    adapter <- original_adapter(...)
    native <- adapter$simulate
    adapter$simulate <- function(ids) {
      x <- native(ids)
      chunks[[length(chunks) + 1L]] <<- x
      x
    }
    adapter
  })
  native <- influ_residuals(fit, nsim = 24L, batch_size = 7L, seed = 61L, groups = "repeat_id")
  sims <- do.call(cbind, chunks)
  rownames(sims) <- native$observations$row
  colnames(sims) <- paste0("draw-", seq_len(ncol(sims)))
  external <- as_influ_residuals(sims, d, response = "response", year = "year",
    response_kind = "distribution", conditioning = native$metadata$scheme,
    batch_size = 7L, seed = 61L, groups = "repeat_id")
  for (field in c("observations", "qq", "ecdf", "observed_ecdf", "groups")) {
    expect_identical(external[[field]], native[[field]])
  }
})

test_that("external inputs require explicit provenance, IDs, and scalar columns", {
  args <- external_residual_args()
  for (value in list(NULL, as.data.frame(args$simulations), rep(1, 30),
      array(1, c(30, 24, 2)), matrix("x", 30, 24))) {
    bad <- args; bad$simulations <- value
    expect_error(do.call(as_influ_residuals, bad), "matrix|missing")
  }
  bad <- args; bad$simulations <- bad$simulations[, 1:19]
  expect_error(do.call(as_influ_residuals, bad), "at least 20")
  for (name in c("response_kind", "conditioning", "response", "year")) {
    bad <- args; bad[[name]] <- NULL
    expect_error(do.call(as_influ_residuals, bad))
  }
  bad <- args; bad["response_kind"] <- list(NULL)
  expect_error(do.call(as_influ_residuals, bad), "response_kind")
  for (name in c("conditioning", "probability_conditioning")) {
    for (value in list("", "  ", NA_character_, c("a", "b"), 1)) {
      bad <- external_residual_args("bernoulli"); bad[[name]] <- value
      expect_error(do.call(as_influ_residuals, bad), "one non-empty")
    }
  }
  for (ids in list(NULL, rev(rownames(args$simulations)), rep("same", 30),
      c(NA_character_, rownames(args$simulations)[-1]), c("", rownames(args$simulations)[-1]))) {
    bad <- args; rownames(bad$simulations) <- ids
    expect_error(do.call(as_influ_residuals, bad), "row names")
  }
  bad <- args; colnames(bad$simulations) <- rep("same", 24)
  expect_error(do.call(as_influ_residuals, bad), "column names")
  bad <- args; bad$data$operation[1] <- bad$data$operation[2]
  expect_error(do.call(as_influ_residuals, bad), "Observation IDs")
  bad <- args; bad$data <- bad$data[30:1, ]
  expect_error(do.call(as_influ_residuals, bad), "data order")
  bad <- args; bad$observation_id <- NULL
  expect_identical(do.call(as_influ_residuals, bad)$metadata$observation_id, "row.names")
  bad <- args; bad$data <- bad$data[-1, ]
  expect_error(do.call(as_influ_residuals, bad), "exactly match")
  bad <- args; names(bad$data)[1] <- names(bad$data)[2]
  expect_error(do.call(as_influ_residuals, bad), "column names")
  for (name in c("year", "response", "observation_id", "groups", "calibration_groups")) {
    bad <- external_residual_args("bernoulli"); bad[[name]] <- "missing"
    expect_error(do.call(as_influ_residuals, bad), "column")
  }
  for (column in c("year", "response", "operation", "fleet")) {
    for (value in list(rep(NA_real_, 30), rep(Inf, 30), matrix(1, 30, 1), as.list(1:30))) {
      bad <- args; bad$data[[column]] <- value
      expect_error(do.call(as_influ_residuals, bad), "finite|scalar|atomic")
    }
  }
  bad <- args; bad$data$response <- as.character(bad$data$response)
  expect_error(do.call(as_influ_residuals, bad), "numeric observed")
  for (name in c("groups", "calibration_groups")) {
    for (value in list(character(), NA_character_, rep("fleet", 2), "response", 1)) {
      bad <- args; bad[[name]] <- value
      expect_error(do.call(as_influ_residuals, bad), "distinct columns|observed response")
    }
  }
  for (name in c("batch_size", "seed", "grid_size", "calibration_bins", "calibration_min_n")) {
    bad <- args; bad[[name]] <- -1
    expect_error(do.call(as_influ_residuals, bad), "integer")
  }
  for (value in list(0, 1, NA_real_, c(0.5, 0.95), "0.95")) {
    bad <- args; bad$level <- value
    expect_error(do.call(as_influ_residuals, bad), "number between")
  }
})

test_that("components and binomial contracts cannot be silently substituted", {
  args <- external_residual_args("combined")
  args$component <- NULL
  expect_identical(do.call(as_influ_residuals, args)$metadata$component, "combined")
  for (component in c("positive", "encounter", "single")) {
    bad <- args; bad$component <- component
    expect_error(do.call(as_influ_residuals, bad), "must agree")
  }
  bad <- args; bad$simulations[1, 24] <- -1
  expect_error(do.call(as_influ_residuals, bad), "non-negative")
  for (where in c("observed", "simulated")) {
    bad <- external_residual_args("positive")
    if (where == "observed") bad$data$response[1] <- 0 else bad$simulations[1, 24] <- 0
    expect_error(do.call(as_influ_residuals, bad), "strictly positive")
  }
  for (kind in c("bernoulli", "grouped")) {
    args <- external_residual_args(kind)
    for (field in c("probability", "probability_conditioning")) {
      bad <- args; bad[[field]] <- NULL
      expect_error(do.call(as_influ_residuals, bad), "probability")
    }
    for (value in c(-0.1, 1.1)) {
      bad <- args; bad$data$p[1] <- value
      expect_error(do.call(as_influ_residuals, bad), "probabilities")
    }
    for (value in c(0, -1, 1.5)) {
      bad <- args; bad$data$trials[1] <- value
      expect_error(do.call(as_influ_residuals, bad), "Trial counts")
    }
    bad <- args; bad$data$response[1] <- 100
    expect_error(do.call(as_influ_residuals, bad), "integer successes")
    bad <- args; bad$simulations[1, 24] <- 0.5
    expect_error(do.call(as_influ_residuals, bad), "success counts")
    bad <- args; bad$component <- "positive"
    expect_error(do.call(as_influ_residuals, bad), "must agree")
  }
  args <- external_residual_args("bernoulli"); args$trial_counts <- NULL
  args$component <- "encounter"
  x <- do.call(as_influ_residuals, args)
  expect_true(all(x$observations$trials == 1))
  expect_identical(x$metadata$component, "encounter")
  args$component <- NULL
  expect_identical(do.call(as_influ_residuals, args)$metadata$component, "single")
  args <- external_residual_args("grouped"); args$data$trials[] <- 1
  expect_error(do.call(as_influ_residuals, args), "Trial counts")
  for (field in c("probability", "trial_counts", "probability_conditioning")) {
    args <- external_residual_args(); args[[field]] <- "not applicable"
    expect_error(do.call(as_influ_residuals, args), "only to Bernoulli")
  }
  args <- external_residual_args(); args$calibration_groups <- "fleet"
  expect_error(do.call(as_influ_residuals, args), "requires an encounter")
})

test_that("external errors restore RNG and never mutate the supplied matrix", {
  args <- external_residual_args()
  for (value in c(NA_real_, NaN, Inf)) {
    args$simulations[2, 24] <- value
    before <- args
    set.seed(402)
    rng <- .Random.seed
    expect_error(do.call(as_influ_residuals, args), "finite responses")
    expect_identical(.Random.seed, rng)
    expect_identical(args, before)
  }
  rng <- .Random.seed
  on.exit(assign(".Random.seed", rng, .GlobalEnv), add = TRUE)
  rm(".Random.seed", envir = .GlobalEnv)
  expect_error(do.call(as_influ_residuals, args), "finite responses")
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
  x <- do.call(as_influ_residuals, external_residual_args())
  expect_s3_class(x, "influ_residuals")
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("extreme ranks and all-tie rows follow the finite-simulation formula", {
  d <- data.frame(year = c(2010, 2012, 2013), response = c(-100, 100, 2))
  sims <- matrix(rep(c(0, 0, 2), 24), 3, dimnames = list(rownames(d), NULL))
  set.seed(17L)
  u <- runif(3)
  x <- as_influ_residuals(sims, d, "response", "year", "distribution",
    "Fixed joint response vectors", seed = 17L, batch_size = 1L)
  expect_equal(x$observations$pit, c(u[1] / 25, (24 + u[2]) / 25, u[3]))
  expect_true(all(is.finite(x$observations$residual)))
  if (requireNamespace("bayesplot", quietly = TRUE) && packageVersion("bayesplot") >= "1.16.0") {
    expect_s3_class(suppressMessages(plot(x, type = "pit_ecdf", pit_grid_size = 2)), "ggplot")
    expect_s3_class(suppressMessages(plot(x,
      panels = c("qq", "fitted", "year", "pit_ecdf_diff"), pit_grid_size = 2)), "patchwork")
  }
})
