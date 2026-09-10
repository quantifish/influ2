reference_backend_data <- function() {
  set.seed(953)
  d <- expand.grid(year = factor(1:3), vessel = factor(1:8), replicate = 1:8)
  d$x <- rnorm(nrow(d)) + as.numeric(d$year) / 3
  d$y <- rpois(nrow(d), exp(0.4 + 0.1 * as.numeric(d$year) +
    0.3 * d$x + rep(seq(-0.8, 0.8, length.out = 8), each = 3, times = 8)))
  d$w <- rep(c(1, 2, 4), length.out = nrow(d))
  d$time <- as.integer(d$year)
  d$var <- "catch"
  d$dist <- "poisson"
  d
}

expect_reference_contribution <- function(diagnostic, data, reference,
                                          observed, projected, term, component) {
  effect <- subset(diagnostic$influence, scale == "link")
  effect <- effect[effect$term == term & effect$component == component, ]
  expect_equal(nrow(effect), nlevels(data$year))
  expected <- vapply(effect$level, function(year) {
    rows <- data$year == year
    weighted.mean(observed[rows], data$w[rows]) -
      weighted.mean(projected, reference$w)
  }, numeric(1))
  expect_equal(effect$estimate, unname(expected), tolerance = 1e-8)
}

expect_disk_diagnostic <- function(model, ...) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  args <- list(model = model, focus = "year", ndraws = 12L, seed = 95L, ...)
  retained <- do.call(influ, c(args, list(retain = "derived_draws")))
  disk <- do.call(influ, c(args, list(retain = "disk", draws_path = path)))
  expect_identical(disk$influence, retained$influence)
  expect_identical(readRDS(path), retained$draws)
  expect_null(disk$draws)
  expect_identical(disk$retained$mode, "disk")
  expect_identical(disk$retained$path, normalizePath(path))
  expect_error(do.call(influ, c(args, list(retain = "disk"))), "draws_path.*required")
}

test_that("glmmTMB reference random effects align with named vessel modes", {
  skip_if_not_installed("glmmTMB")
  d <- reference_backend_data()
  model <- glmmTMB::glmmTMB(y ~ year + x + (1 | vessel), poisson(), data = d)
  reference <- d[c(31, 5, 46, 19, 12, 41), ]
  reference$w <- c(1, 7, 3, 2, 9, 4)
  modes <- glmmTMB::ranef(model)$cond$vessel[, "(Intercept)"]
  observed <- modes[as.integer(d$vessel)]
  projected <- modes[as.integer(reference$vessel)]
  Z <- .glmmTMB_reference_random_matrix(model, reference)
  expect_equal(as.numeric(Z %*% glmmTMB::getME(model, "b")), projected)
  for (uncertainty in c("none", "auto")) {
    diagnostic <- influ(model, focus = "year", data = d, weights = "w",
      reference_data = reference, reference_weights = "w", uncertainty = uncertainty)
    expect_reference_contribution(diagnostic, d, reference, observed, projected,
      "random_effects", "random_effects")
    slope <- glmmTMB::fixef(model)$cond[["x"]]
    expect_reference_contribution(diagnostic, d, reference, slope * d$x,
      slope * reference$x, "x", "conditional")
    se <- subset(diagnostic$influence, component == "random_effects")$std_error
    if (uncertainty == "none") expect_true(all(is.na(se))) else expect_true(all(is.finite(se)))
  }
  expect_disk_diagnostic(model, reference_data = reference, reference_weights = "w")
})

test_that("GAM reference predictions retain the fitted smooth basis", {
  skip_if_not_installed("mgcv")
  d <- reference_backend_data()
  model <- mgcv::gam(y ~ year + s(x, k = 4), poisson(), data = d, method = "REML")
  reference <- d[c(2, 7, 15, 21, 33), ]
  diagnostic <- influ(model, focus = "year", data = d, weights = "w",
    reference_data = reference, reference_weights = "w")
  observed <- predict(model, type = "terms")[, "s(x)"]
  projected <- predict(model, newdata = reference, type = "terms")[, "s(x)"]
  expect_reference_contribution(diagnostic, d, reference, observed, projected,
    "s(x)", "conditional")
})

test_that("glmmTMB fixed-reference matrices match native predictions, not fitted rows", {
  skip_if_not_installed("glmmTMB")
  d <- reference_backend_data()
  d$y[rbinom(nrow(d), 1, plogis(-0.5 + d$x / 4)) == 1] <- 0
  model <- glmmTMB::glmmTMB(y ~ year + poly(x, 2), ziformula = ~year + x,
    data = d, family = poisson())
  expect_true(model$sdr$pdHess)
  # Equal row counts used to conceal the ignored reference grid entirely.
  for (rows in list(rev(seq_len(nrow(d))), c(31, 5, 17, 22))) {
    reference <- d[rows, ]
    reference$x <- reference$x / 2 + 0.7
    for (component in c("cond", "zi")) {
      design <- .glmmTMB_reference_matrix(model, reference, component)
      beta <- glmmTMB::fixef(model)[[component]]
      expected <- predict(model, newdata = reference,
        type = if (component == "cond") "link" else "zlink")
      expect_equal(as.numeric(design %*% beta), as.numeric(expected), tolerance = 1e-10)
    }
    diagnostic <- influ(model, "year", reference_data = reference,
      reference_weights = "w", uncertainty = "none")
    cond <- .glmmTMB_reference_matrix(model, reference, "cond")
    zi <- .glmmTMB_reference_matrix(model, reference, "zi")
    cond_beta <- glmmTMB::fixef(model)$cond
    zi_beta <- glmmTMB::fixef(model)$zi
    cond_ref <- colSums(cond * reference$w) / sum(reference$w)
    zi_ref <- colSums(zi * reference$w) / sum(reference$w)
    base_cond <- sum(cond_ref * cond_beta)
    base_zero <- sum(zi_ref * zi_beta)
    effect <- subset(diagnostic$influence,
      component == "unconditional_mean" & term == "year" & scale == "ratio")
    expected <- vapply(effect$level, function(year) {
      keep <- d$year == year
      year_cols <- grep("^year", names(cond_beta))
      zero_cols <- grep("^year", names(zi_beta))
      delta_cond <- sum((colMeans(model.matrix(model)[keep, year_cols, drop = FALSE]) -
        cond_ref[year_cols]) * cond_beta[year_cols])
      delta_zero <- sum((colMeans(model.matrix(model, component = "zi")[keep, zero_cols, drop = FALSE]) -
        zi_ref[zero_cols]) * zi_beta[zero_cols])
      exp(delta_cond) * plogis(-base_zero - delta_zero) / plogis(-base_zero)
    }, numeric(1))
    expect_equal(effect$estimate, unname(expected))
  }
  testthat::local_mocked_bindings(predict = function(...) list(), .package = "stats")
  expect_error(.glmmTMB_reference_matrix(model, d, "cond"), "fixed-effect reference matrix")
})

test_that("tinyVAST mixed-response reference weights remain response-specific", {
  skip_if_not_installed("tinyVAST")
  d <- reference_backend_data()
  d$var <- rep(c("catch", "count"), length.out = nrow(d))
  d$dist <- ifelse(d$var == "catch", "normal", "poisson")
  model <- tinyVAST::tinyVAST(y ~ year + x, data = d, spatial_domain = NULL,
    family = list(normal = gaussian(), poisson = poisson()))
  reference <- d[c(1:12, 61:72), ]
  reference$w <- seq_len(nrow(reference))
  args <- list(model = model, focus = "year", reference_data = reference,
    uncertainty = "none", keep_model = TRUE)
  numeric <- do.call(influ, c(args, list(weights = d$w, reference_weights = reference$w)))
  named <- do.call(influ, c(args, list(weights = "w", reference_weights = "w")))
  expect_equal(numeric$influence, named$influence)
  expect_identical(numeric$model, model)
  slope <- model$internal$parlist$alpha_j[which(colnames(model$tmb_inputs$tmb_data$X_ij) == "x")]
  expect_length(slope, 1L)
  for (response in unique(d$var)) {
    rows <- d$var == response
    ref_rows <- reference$var == response
    expect_reference_contribution(numeric, d[rows, ], reference[ref_rows, ],
      slope * d$x[rows], slope * reference$x[ref_rows], "x", paste0(response, ":conditional"))
  }
  expect_disk_diagnostic(model, reference_data = reference, reference_weights = "w")
  expect_error(influ(model, "year", reference_data = reference[, names(reference) != "var"]),
    "must contain.*variable column")
  expect_error(influ(model, "year", reference_data = reference[reference$var == "catch", ]),
    "no rows.*count")
  expect_error(.tinyVAST_family_specs(model), "distribution.*required")
  expect_error(.tinyVAST_family_specs(model, "missing"), "No tinyVAST family")
})

test_that("delta fixed-reference effects agree between native spatial backends", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("sdmTMB")
  d <- reference_backend_data()
  set.seed(195)
  d$y <- ifelse(rbinom(nrow(d), 1, plogis(-0.3 + d$x / 4)),
    rgamma(nrow(d), shape = 3, scale = exp(0.2 + d$x / 3) / 3), 0)
  d$dist <- "dgamma"
  reference <- d[c(2, 11, 20, 43, 76, 97), ]
  a <- sdmTMB::sdmTMB(y ~ year + x, data = d, spatial = "off",
    family = sdmTMB::delta_gamma(), silent = TRUE)
  b <- tinyVAST::tinyVAST(y ~ year + x, data = d, spatial_domain = NULL,
    delta_options = list(formula = ~year + x),
    family = list(dgamma = tinyVAST::delta_gamma(link1 = "logit")))
  results <- lapply(list(a, b), influ, focus = "year", uncertainty = "none",
    weights = "w", reference_data = reference, reference_weights = "w")
  select <- function(x) {
    z <- x$influence
    z <- z[order(z$component, z$term, z$scale, z$level), ]
    rownames(z) <- NULL
    z[, c("component", "term", "scale", "level", "estimate")]
  }
  expect_equal(select(results[[1]]), select(results[[2]]), tolerance = 1e-3)
  for (model in list(a, b)) {
    expect_disk_diagnostic(model, reference_data = reference, reference_weights = "w")
  }
})

test_that("brms distributional reference projections support preview and disk retention", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  model <- readRDS(system.file("extdata", "brms-fixtures", "m1.rds", package = "influ2"))
  reference <- model$data[seq(1, nrow(model$data), length.out = 30), ]
  weights <- seq_len(nrow(reference))
  preview <- influ(model, "year", reference_data = reference,
    reference_weights = weights, uncertainty = "none")
  expect_true(all(is.finite(preview$influence$estimate)))
  expect_true(all(is.na(preview$influence$std_error)))
  expect_disk_diagnostic(model, reference_data = reference, reference_weights = weights)
})
