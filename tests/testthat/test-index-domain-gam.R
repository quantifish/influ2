.domain_gam_fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(family = "poisson") {
    if (exists(family, cache, inherits = FALSE)) return(cache[[family]])
    set.seed(291)
    data <- expand.grid(cell = letters[1:6], replicate = 1:12,
      year = factor(2001:2003))
    data$x <- runif(nrow(data), -1, 1)
    data$z <- runif(nrow(data), -0.5, 0.5)
    data$log_effort <- log(runif(nrow(data), 0.7, 1.4))
    eta <- 0.8 + 0.15 * (as.numeric(data$year) - 1) +
      0.4 * sin(data$x * 2) + 0.2 * data$z + data$log_effort
    mu <- exp(eta)
    response <- "response"
    if (family == "gamma") {
      data$response <- rgamma(nrow(data), shape = 4, scale = mu / 4)
      distribution <- Gamma(link = "log")
    } else if (family == "negative_binomial") {
      data$response <- rnbinom(nrow(data), mu = mu, size = 3)
      distribution <- mgcv::nb(link = "log")
    } else if (family %in% c("binomial", "binomial_matrix")) {
      probability <- plogis(eta - 1)
      trials <- if (family == "binomial_matrix") 5 else 1
      data$response <- rbinom(nrow(data), trials, probability)
      distribution <- binomial()
      if (family == "binomial_matrix") {
        data$failure <- trials - data$response
        response <- "cbind(response, failure)"
      }
    } else if (family == "tweedie") {
      data$response <- mgcv::rTweedie(mu = mu, p = 1.5, phi = 0.5)
      distribution <- mgcv::tw(link = "log")
    } else if (family == "gaussian") {
      data$response <- mu + rnorm(nrow(data), sd = 0.4)
      distribution <- gaussian()
    } else {
      data$response <- rpois(nrow(data), mu)
      distribution <- poisson()
    }
    rownames(data) <- paste0("observation", seq_len(nrow(data)))
    formula <- stats::as.formula(paste(response,
      "~ year + s(x, k = 5) + z + offset(log_effort)"))
    # mgcv::tw() closes over .GlobalEnv and resolves its density helpers on
    # the search path while fitting; attach only for this native fit.
    model <- withr::with_package("mgcv",
      mgcv::gam(formula, data = data, family = distribution, method = "REML"))
    reference <- data.frame(cell = letters[1:6], x = seq(-0.8, 0.8, length.out = 6),
      z = seq(-0.3, 0.3, length.out = 6), log_effort = log(seq(0.8, 1.3, length.out = 6)))
    cache[[family]] <- list(model = model, data = data, reference = reference,
      weights = c(1, 2, 1, 3, 2, 1), years = levels(data$year))
    cache[[family]]
  }
})

.domain_gam_native_means <- function(fixture, reference = fixture$reference,
    weights = fixture$weights, parameters = NULL) {
  model <- fixture$model
  out <- lapply(fixture$years, function(year) {
    newdata <- reference
    newdata$year <- factor(year, levels = fixture$years)
    if (is.null(parameters)) {
      return(sum(weights * predict(model, newdata, type = "response")) / sum(weights))
    }
    X <- predict(model, newdata, type = "lpmatrix")
    offset <- as.numeric(predict(model, newdata, type = "link")) -
      as.numeric(X %*% coef(model))
    eta <- X %*% parameters + offset
    colSums(model$family$linkinv(eta) * weights) / sum(weights)
  })
  do.call(cbind, out)
}

for (family in c("poisson", "gamma", "negative_binomial", "binomial",
    "binomial_matrix", "tweedie", "gaussian")) local({
  selected <- family
  test_that(paste("GAM domain", selected, "matches native predictions, CPUE, totals, and terms"), {
    skip_if_not_installed("mgcv")
    f <- .domain_gam_fixture(selected)
    result <- index_domain(f$model, f$reference, year = "year", sampled = "cell",
      data = f$data, reference_weights = f$weights, uncertainty = "none",
      retain_cells = f$years, batch_size = 5)
    native <- as.numeric(.domain_gam_native_means(f))
    standardised <- cpue_index(f$model, year = "year", reference_data = f$reference,
      reference_weights = f$weights, uncertainty = "none")
    expect_equal(result$baseline$table$Mean, native, tolerance = 1e-9)
    expect_equal(result$baseline$table$Mean, standardised$table$Mean, tolerance = 1e-9)
    expect_true(all(is.na(result$baseline$table$SD)))
    expect_match(result$metadata$gam_covariance_target, "coefficient covariance")
    for (index in result$sampled_only) expect_equal(index$table$Mean, native)
    expect_equal(Reduce(`+`, lapply(split(result$partition$Mean,
      result$partition$class), as.numeric)), native)
    expect_true("offset" %in% result$decomposition$term)
    for (year in f$years) {
      d <- f$reference
      d$year <- factor(year, levels = f$years)
      block <- .domain_gam_projection(f$model, d, year = "year")
      terms <- block$terms(coef(f$model))
      expect_equal(as.numeric(Reduce(`+`, terms)),
        as.numeric(predict(f$model, d, type = "link")), tolerance = 1e-10)
      expect_equal(as.numeric(terms$offset), d$log_effort, tolerance = 1e-10)
      expect_true(all(vapply(terms, function(z)
        identical(colnames(z), "conditional"), logical(1))))
      expect_equal(block$response(coef(f$model)),
        as.numeric(predict(f$model, d, type = "response")), tolerance = 1e-10)
    }
    total <- index_domain(f$model, f$reference, year = "year", sampled = "cell",
      data = f$data, area = f$weights, area_units = "km^2",
      response_units = "response/km^2", uncertainty = "none")
    ordinary_total <- integrate_index(f$model, f$reference, area = f$weights,
      year = "year", area_units = "km^2", response_units = "response/km^2",
      uncertainty = "none")
    expect_equal(total$baseline$table$Mean, native * sum(f$weights), tolerance = 1e-9)
    expect_equal(total$baseline$table$Mean, ordinary_total$table$Mean, tolerance = 1e-9)
  })
})

test_that("GAM domain uncertainty replays paired coefficient draws across all blocks", {
  skip_if_not_installed("mgcv")
  f <- .domain_gam_fixture()
  sampled <- matrix(TRUE, 6, 3)
  sampled[6, ] <- FALSE
  sampled[2, 2] <- FALSE
  sampled[3, 1] <- FALSE
  before <- list(coef = coef(f$model), covariance = f$model$Vp, frame = f$model$model)
  set.seed(292)
  rng <- .Random.seed
  a <- index_domain(f$model, f$reference, year = "year", sampled = sampled,
    reference_weights = f$weights, omit = "s(x)", where = "all",
    covariate_reference = list("s(x)" = list(rule = "fixed", value = list(x = 0))),
    ndraws = 31, seed = 293, batch_size = 4, draw_batch_size = 7)
  expect_identical(.Random.seed, rng)
  b <- index_domain(f$model, f$reference, year = "year", sampled = sampled,
    reference_weights = f$weights, omit = "s(x)", where = "all",
    covariate_reference = list("s(x)" = list(rule = "fixed", value = list(x = 0))),
    ndraws = 31, seed = 293, batch_size = 100, draw_batch_size = 31)
  expect_identical(.Random.seed, rng)
  expect_equal(a$baseline$draws, b$baseline$draws, tolerance = 1e-10)
  expect_equal(a$sampled_only, b$sampled_only, tolerance = 1e-10)
  expect_equal(a$omission, b$omission, tolerance = 1e-10)
  expect_equal(a$partition, b$partition, tolerance = 1e-10)
  expect_equal(a$decomposition, b$decomposition, tolerance = 1e-10)
  expect_equal(a$trend, b$trend, tolerance = 1e-10)
  expect_identical(before,
    list(coef = coef(f$model), covariance = f$model$Vp, frame = f$model$model))
  expect_equal(Reduce(`+`, a$partition_draws), a$baseline$draws, tolerance = 1e-10)
  info <- .domain_gam_info(f$model, "year")
  set.seed(293)
  parameters <- .domain_gam_sampler(info)(31)
  manual <- .domain_gam_native_means(f, parameters = parameters)
  expect_equal(unname(a$baseline$draws), unname(manual), tolerance = 1e-10)
  expect_equal(a$baseline$table$SD, apply(manual, 2, sd), ignore_attr = TRUE)
  expect_equal(a$baseline$covariance$response, stats::cov(manual),
    ignore_attr = TRUE, tolerance = 1e-10)
  changed <- f$reference
  changed$x <- 0
  manual_changed <- .domain_gam_native_means(f, changed, parameters = parameters)
  expect_equal(unname(a$omission[["s(x):all"]]$draws),
    unname(manual_changed), tolerance = 1e-10)
  summary <- a$omission_summary[a$omission_summary$series == "s(x):all", ]
  expected <- apply(manual_changed / manual, 2, quantile, probs = c(0.025, 0.975))
  expect_equal(summary$ratio_lower, expected[1, ], ignore_attr = TRUE)
  expect_equal(summary$ratio_upper, expected[2, ], ignore_attr = TRUE)
})

test_that("GAM covariate and year omissions use explicit native prediction references", {
  skip_if_not_installed("mgcv")
  f <- .domain_gam_fixture()
  result <- index_domain(f$model, f$reference, year = "year", sampled = "cell",
    data = f$data, reference_weights = f$weights, omit = c("s(x)", "z", "year"),
    where = "all", uncertainty = "none", covariate_reference = list(
      "s(x)" = list(rule = "fixed", value = list(x = 0)),
      z = list(rule = "fixed", value = list(z = 0.1))))
  for (term in c("s(x)", "z")) {
    changed <- f$reference
    if (term == "s(x)") changed$x <- 0 else changed$z <- 0.1
    expect_equal(result$omission[[paste0(term, ":all")]]$table$Mean,
      as.numeric(.domain_gam_native_means(f, changed)), tolerance = 1e-9)
  }
  expect_equal(result$omission[["year:all"]]$table$Mean,
    rep(result$baseline$table$Mean[1], length(f$years)), tolerance = 1e-9)
  wrapped <- cpue_index(f$model, year = "year", reference_data = f$reference,
    reference_weights = f$weights, data = f$data, sampled = "cell",
    omit_terms = "s(x)", omit_where = "all", uncertainty = "none",
    covariate_reference = list("s(x)" = list(rule = "fixed", value = list(x = 0))))
  expect_equal(wrapped$table, result$omission[["s(x):all"]]$table)
  expect_identical(wrapped$metadata$backend, "gam")
})

test_that("GAM spatial aliases support tensor, by-variable, and random-effect smooths", {
  skip_if_not_installed("mgcv")
  set.seed(294)
  data <- data.frame(year = rep(1:3, each = 90),
    X = runif(270), Y = runif(270), z = runif(270),
    group = factor(rep(c("a", "b"), 135)), vessel = factor(rep(1:5, 54)))
  data$response <- rpois(270, exp(1 + data$X - data$Y + 0.2 * data$year))
  model <- mgcv::gam(response ~ factor(year) + te(X, Y, k = c(3, 3)) +
    ti(X, year, k = c(3, 3)) + group + s(z, by = group, k = 4) +
    s(vessel, bs = "re"), data = data, family = poisson(), method = "REML")
  reference <- data.frame(X = c(0.2, 0.4, 0.7, 0.8), Y = c(0.3, 0.5, 0.8, 0.6),
    z = c(0.2, 0.3, 0.7, 0.8), group = factor(c("a", "a", "b", "b")),
    vessel = factor(1:4, levels = levels(data$vessel)))
  fields <- list(spatial = "te(X,Y)", spatiotemporal = "ti(X,year)")
  newdata <- reference
  newdata$year <- 1
  native <- .domain_gam_projection(model, newdata, "year", fields)
  terms <- native$terms(coef(model))
  expect_true(all(c("spatial", "spatiotemporal", "s(z):groupa", "s(z):groupb", "s(vessel)") %in% names(terms)))
  expect_false(any(c("te(X,Y)", "ti(X,year)") %in% names(terms)))
  expect_equal(as.numeric(Reduce(`+`, terms)),
    as.numeric(predict(model, newdata, type = "link")), tolerance = 1e-10)
  result <- index_domain(model, reference, year = "year", sampled = matrix(TRUE, 4, 3),
    spatial_terms = fields$spatial, spatiotemporal_terms = fields$spatiotemporal,
    omit = c("spatial", "spatiotemporal"), where = "all", uncertainty = "none")
  expect_identical(result$metadata$gam_field_terms, fields)
  for (field in names(fields)) {
    expected <- vapply(sort(unique(data$year)), function(year) {
      nd <- reference
      nd$year <- year
      eta <- predict(model, nd, type = "link")
      contribution <- predict(model, nd, type = "terms")[, fields[[field]]]
      mean(exp(eta - contribution))
    }, numeric(1))
    expect_equal(result$omission[[paste0(field, ":all")]]$table$Mean,
      unname(expected), tolerance = 1e-9)
  }
  expect_error(index_domain(model, reference, year = "year", sampled = matrix(TRUE, 4, 3),
    spatial_terms = "missing"), "native GAM smooth labels")
  expect_error(index_domain(model, reference, year = "year", sampled = matrix(TRUE, 4, 3),
    spatial_terms = fields$spatial, spatiotemporal_terms = fields$spatial), "both spatial")
})

test_that("GAM sampling rejects changed data and accepts explicit aligned observation columns", {
  skip_if_not_installed("mgcv")
  f <- .domain_gam_fixture()
  expect_error(index_domain(f$model, f$reference, year = "year", sampled = "cell",
    uncertainty = "none"), "shared cell-ID")
  expect_error(index_domain(f$model, f$reference, year = "year", uncertainty = "none"),
    "Supply `sampled` explicitly for GAM")
  changed <- f$data
  changed$response[1] <- changed$response[1] + 1
  expect_error(index_domain(f$model, f$reference, year = "year", sampled = "cell",
    data = changed, uncertainty = "none"), "does not match|does not reproduce")
  shuffled <- f$data[nrow(f$data):1, ]
  x <- index_domain(f$model, f$reference, year = "year", sampled = "cell",
    data = shuffled, uncertainty = "none")
  expect_true(all(x$counts == 12L))
  expect_identical(x$metadata$sampling_support$supported_rows, nrow(f$data))
})

test_that("GAM reference predictors never fall back to same-length mutable globals", {
  skip_if_not_installed("mgcv")
  f <- .domain_gam_fixture()
  globals <- list(x = rep(0.9, nrow(f$reference)),
    log_effort = rep(log(2), nrow(f$reference)),
    group = factor(rep(c("a", "b"), length.out = nrow(f$reference))))
  existed <- vapply(names(globals), exists, logical(1),
    envir = .GlobalEnv, inherits = FALSE)
  original <- mget(names(globals)[existed], envir = .GlobalEnv)
  withr::defer({
    rm(list = names(globals)[!existed], envir = .GlobalEnv)
    list2env(original, envir = .GlobalEnv)
  })
  list2env(globals, envir = .GlobalEnv)
  for (column in c("x", "log_effort")) {
    reference <- f$reference
    reference[[column]] <- NULL
    expect_error(index_domain(f$model, reference, year = "year",
      sampled = matrix(TRUE, 6, 3), uncertainty = "none"),
      paste0("missing: ", column))
    reference$year <- factor(rep(f$years[1], nrow(reference)), levels = f$years)
    expect_error(.domain_gam_projection(f$model, reference, "year"),
      paste0("missing: ", column))
  }
  data <- f$data
  data$group <- factor(rep(c("a", "b"), length.out = nrow(data)))
  model <- mgcv::gam(response ~ year + group + s(x, by = group, k = 4),
    data = data, family = poisson(), method = "REML")
  expect_error(index_domain(model, f$reference, year = "year",
    sampled = matrix(TRUE, 6, 3), uncertainty = "none"), "missing: group")
  reference <- f$reference
  reference$year <- factor(rep(f$years[1], nrow(reference)), levels = f$years)
  expect_error(.domain_gam_projection(model, reference, "year"), "missing: group")
})

test_that("unsupported GAM response structures fail explicitly without fitting again", {
  skip_if_not_installed("mgcv")
  f <- .domain_gam_fixture()
  transformed <- mgcv::gam(log(response + 1) ~ year + s(x, k = 4),
    family = gaussian(), data = f$data)
  expect_error(index_domain(transformed, f$reference, year = "year",
    sampled = matrix(TRUE, 6, 3)), "natural response.*transformed")
  distributional <- mgcv::gam(list(response ~ year + s(x, k = 4), ~ 1),
    family = mgcv::gaulss(), data = f$data)
  expect_error(index_domain(distributional, f$reference, year = "year",
    sampled = matrix(TRUE, 6, 3)), "single-predictor")
  incomplete <- f$model
  incomplete$model <- NULL
  expect_error(index_domain(incomplete, f$reference, year = "year",
    sampled = matrix(TRUE, 6, 3)), "Retain the fitted GAM model frame")
  expect_error(cpue_index(f$model, year = "year", reference_data = f$reference,
    spatial_terms = "s(x)"), "require `omit_terms`")
})
