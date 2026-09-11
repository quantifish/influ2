implied_fixture <- function() {
  set.seed(630)
  d <- expand.grid(year = factor(2011:2016), area = factor(c("A", "B", "C")), record = 1:18)
  d$x <- rnorm(nrow(d))
  d$vessel <- factor(rep(1:18, each = 18))
  d$exposure <- exp(runif(nrow(d), -.4, .4))
  eta <- 1 + .2 * as.integer(d$year) + .3 * as.integer(d$area) + .4 * d$x +
    .08 * (as.integer(d$year) - 3) * (as.integer(d$area) - 2)
  d$cpue <- exp(eta + rnorm(nrow(d), sd = .65))
  d$count <- rnbinom(nrow(d), mu = exp(eta) * d$exposure, size = 4)
  rownames(d) <- paste0("record", seq_len(nrow(d)))
  d
}

test_that("ordinary log-residual and likelihood implied effects agree exactly", {
  d <- implied_fixture()
  m <- glm(log(cpue) ~ year + area + x, data = d)
  current <- implied_effects(m, groups = "area", interval = "descriptive")
  traditional <- implied_effects(m, groups = "area", method = "traditional")
  expect_equal(current$table, traditional$table, tolerance = 1e-12)
  # Independent reconstruction: ordinary model residuals + selected term effects.
  terms <- predict(m, type = "terms")
  reference <- rowSums(terms[, c("year", "area"), drop = FALSE])
  for (j in seq_len(nrow(current$table))) {
    cell <- current$table[j, ]
    i <- d$year == cell$level & d$area == cell$group
    expect_equal(cell$baseline, mean(reference[i]), tolerance = 1e-12)
    expect_equal(cell$estimate, mean(reference[i] + residuals(m, type = "response")[i]), tolerance = 1e-12)
    expect_equal(cell$std_error, sd(residuals(m, type = "response")[i]) / sqrt(sum(i)))
  }
  conditional <- implied_effects(m, groups = "area")
  expect_equal(conditional$table$estimate, current$table$estimate)
  expect_equal(conditional$table$upper - conditional$table$estimate,
    qnorm(.975) * sigma(m) / sqrt(conditional$table$n), tolerance = 1e-7)
  expect_false(isTRUE(all.equal(conditional$table$lower, current$table$lower)))
  yr <- implied_effects(m, groups = "area", baseline = "year")
  expect_equal(yr$table$adjustment, current$table$adjustment)
  expect_false(isTRUE(all.equal(yr$table$baseline, current$table$baseline)))
  d$area <- factor(d$area, levels = c("C", "A", "B"))
  reordered <- implied_effects(glm(log(cpue) ~ year + area + x, data = d), groups = "area")
  expect_identical(levels(plot(reordered)$data$group), c("C", "A", "B"))
})

test_that("historical standardised GLM convention is reproduced independently", {
  d <- implied_fixture()
  m <- glm(log(cpue) ~ year + area + x, data = d)
  x <- implied_effects(m, groups = "area", method = "traditional", traditional_scale = "standardised")
  raw <- rstandard(m)
  centred <- raw - mean(raw)
  baseline <- rowSums(predict(m, type = "terms")[, c("year", "area")])
  for (j in seq_len(nrow(x$table))) {
    cell <- x$table[j, ]
    i <- d$year == cell$level & d$area == cell$group
    expect_equal(cell$estimate, mean(baseline[i] + centred[i]), tolerance = 1e-12)
    expect_equal(cell$std_error, sd(centred[i]) / sqrt(sum(i)))
  }
  expect_equal(x$table, implied_effects(m, groups = "area", method = "traditional",
    traditional_scale = "standardized")$table)
  expect_false(isTRUE(all.equal(x$table$estimate, implied_effects(m, groups = "area")$table$estimate)))
})

test_that("count shifts and profile intervals match independent likelihoods", {
  skip_if_not_installed("MASS")
  d <- implied_fixture()
  nb <- MASS::glm.nb(count ~ year + area + x + offset(log(exposure)), data = d)
  x <- implied_effects(nb, groups = "area")
  for (j in seq_len(nrow(x$table))) {
    cell <- x$table[j, ]
    i <- d$year == cell$level & d$area == cell$group
    likelihood <- function(delta) sum(dnbinom(d$count[i], mu = exp(nb$linear.predictors[i] + delta),
      size = nb$theta, log = TRUE))
    independent <- optimize(function(delta) -likelihood(delta), c(-5, 5), tol = 1e-10)$minimum
    expect_equal(cell$adjustment, independent, tolerance = 1e-6)
    expect_equal(2 * (likelihood(cell$adjustment) - likelihood(cell$lower - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
    expect_equal(2 * (likelihood(cell$adjustment) - likelihood(cell$upper - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
  }
  pois <- glm(count ~ year + area + x + offset(log(exposure)), family = poisson(), data = d)
  p <- implied_effects(pois, groups = "area")
  i <- d$year == p$table$level[1] & d$area == p$table$group[1]
  expect_equal(p$table$adjustment[1], log(sum(d$count[i]) / sum(fitted(pois)[i])))
  expect_error(implied_effects(nb, groups = "area", method = "traditional"), "Gaussian")
  expect_error(implied_effects(nb, groups = "area", interval = "descriptive"), "log-response variance")
})

test_that("mixed effects and varying dispersion are held at fitted values", {
  skip_if_not_installed("glmmTMB")
  d <- implied_fixture()
  m <- glmmTMB::glmmTMB(log(cpue) ~ year + area + x + (1 | vessel), data = d)
  expect_true(m$sdr$pdHess)
  original <- m$obj$env$last.par.best
  x <- implied_effects(m, groups = "area", interval = "descriptive")
  t <- implied_effects(m, groups = "area", method = "traditional")
  expect_equal(x$table, t$table)
  expect_identical(m$obj$env$last.par.best, original)
  eta <- predict(m, type = "link", re.form = NULL)
  i <- d$year == x$table$level[1] & d$area == x$table$group[1]
  expect_equal(x$table$adjustment[1], mean(log(d$cpue[i]) - eta[i]))
  nb <- glmmTMB::glmmTMB(count ~ year + area + x + (1 | vessel),
    family = glmmTMB::nbinom2(), data = d, dispformula = ~ area)
  z <- implied_effects(nb, groups = "area")
  j <- d$year == z$table$level[1] & d$area == z$table$group[1]
  e <- predict(nb, type = "link", re.form = NULL)
  phi <- predict(nb, type = "disp")
  opt <- optimize(function(delta) -sum(dnbinom(d$count[j], mu = exp(e[j] + delta), size = phi[j], log = TRUE)), c(-5, 5))
  expect_equal(z$table$adjustment[1], opt$minimum, tolerance = 1e-4)
  expect_error(implied_effects(m, method = "traditional", traditional_scale = "standardised"), "plain Gaussian GLM")
})

test_that("GAMs and lm use fitted contributions without refitting", {
  skip_if_not_installed("mgcv")
  d <- implied_fixture()
  m <- mgcv::gam(log(cpue) ~ year + area + s(x, k = 5), data = d, method = "REML")
  a <- implied_effects(m, groups = "area", interval = "descriptive")
  expect_equal(a$table, implied_effects(m, groups = "area", method = "traditional")$table)
  nb <- mgcv::gam(count ~ year + area + s(x, k = 5), data = d, family = mgcv::nb(), method = "REML")
  expect_s3_class(implied_effects(nb, groups = "area"), "influ_implied")
  ordinary <- lm(log(cpue) ~ year + area + x, data = d)
  expect_equal(implied_effects(ordinary, groups = "area")$table,
    implied_effects(glm(log(cpue) ~ year + area + x, data = d), groups = "area")$table, tolerance = 1e-6)
  d$time <- as.integer(d$year)
  curved <- mgcv::gam(log(cpue) ~ s(time, k = 4) + area + x, data = d)
  expect_error(implied_effects(curved, year = "time", groups = "area"), "additive fixed year")
})

test_that("alignment, support, and explicit failure boundaries are enforced", {
  d <- implied_fixture()
  d$x[2] <- NA
  m <- glm(log(cpue) ~ year + area + x, data = d, na.action = na.exclude)
  a <- implied_effects(m, groups = "area", min_n = 1)
  expect_equal(a$table, implied_effects(m, data = d[nrow(d):1, ], groups = "area", min_n = 1)$table)
  changed <- d
  changed$cpue[3] <- changed$cpue[3] * 2
  expect_error(implied_effects(m, data = changed, groups = "area"), "does not reproduce")
  expect_error(implied_effects(m, data = d[1:10, ], groups = "area"), "row names")
  expect_error(implied_effects(m, groups = "cpue"), "defined from the response")
  expect_error(implied_effects(m, groups = "year"), "different from year")
  expect_error(implied_effects(m, groups = c("area", "x")), "one original-data")
  expect_error(implied_effects(m, groups = "area", min_n = 0), "integer of at least")
  expect_error(implied_effects(m, groups = "area", level = 1), "between zero and one")
  expect_error(implied_effects(m, groups = "area", traditional_scale = "log_response"), "only to method")
  expect_error(implied_effects(structure(list(), class = "sdmTMB")), "currently support")
  expect_error(implied_effects(structure(list(), class = "influ_residuals")), "plot_grouped_residuals")
  interaction <- glm(log(cpue) ~ year * area + x, data = d)
  expect_error(implied_effects(interaction, groups = "area"), "year interactions")
  weighted <- glm(log(cpue) ~ year + area + x, data = d, weights = rep(2, nrow(d)))
  expect_error(implied_effects(weighted, groups = "area"), "Non-unit")
  gamma <- glm(cpue ~ year + area + x, data = d, family = Gamma(link = "log"))
  expect_error(implied_effects(gamma, groups = "area"), "Supported implied-effect families")
  transformed <- glm(sqrt(cpue) ~ year + area + x, data = d)
  expect_error(implied_effects(transformed, groups = "area"), "other transformations")
  noframe <- glm(log(cpue) ~ year + area + x, data = d, model = FALSE)
  expect_error(implied_effects(noframe, groups = "area"), "retained model frame")
})

test_that("sparse, absent, and all-zero strata are not fabricated or joined", {
  d <- implied_fixture()
  d <- subset(d, !(year == "2013" & area == "A"))
  d$count[d$year == "2014" & d$area == "B"] <- 0
  m <- glm(count ~ year + area + x, data = d, family = poisson())
  x <- implied_effects(m, groups = "area")
  expect_equal(nrow(x$table), 18L)
  expect_equal(subset(x$table, level == "2013" & group == "A")$status, "empty")
  boundary <- subset(x$table, level == "2014" & group == "B")
  expect_equal(boundary$status, "boundary_zero")
  expect_equal(boundary$adjustment, -Inf)
  p <- plot(x)
  shown <- p$layers[[1]]$data
  expect_false(any(shown$status != "ok"))
  expect_false(subset(shown, group == "A" & level == "2012")$segment ==
    subset(shown, group == "A" & level == "2014")$segment)
  sparse <- implied_effects(m, groups = "area", min_n = 50)
  expect_error(plot(sparse), "No finite supported")
})

test_that("compact implied results redraw and round-trip without live models", {
  d <- implied_fixture()
  m <- glm(log(cpue) ~ year + area + x, data = d)
  set.seed(59)
  seed <- .Random.seed
  x <- implied_effects(m, groups = "area")
  expect_identical(.Random.seed, seed)
  expect_null(x$model)
  expect_null(x$draws)
  p <- plot_implied_residuals(x)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data, ggplot2::autoplot(x)$data)
  expect_equal(p$data, plot_implied_residuals(m, groups = "area")$data)
  expect_error(plot_implied_residuals(x, method = "traditional"), "recalculate")
  expect_output(print(x), "Original fitted model held fixed")
  expect_identical(as.data.frame(x), x$table)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(x, file)
  expect_identical(readRDS(file), x)
  expect_equal(plot(readRDS(file))$data, p$data)
  expect_identical(.Random.seed, seed)
  expect_lt(as.numeric(object.size(x)), 25000)
  expect_true(all(is.na(implied_effects(m, groups = "area", interval = "none")$table$lower)))
  expect_error(plot(x, ncol = 0), "integer of at least")
})

test_that("new implied-effect figures remain visually stable", {
  skip_if_not_installed("vdiffr")
  d <- implied_fixture()
  m <- glm(log(cpue) ~ year + area + x, data = d)
  vdiffr::expect_doppelganger("likelihood residual-implied effects",
    plot(implied_effects(m, groups = "area")))
  vdiffr::expect_doppelganger("historical standardised implied effects",
    plot(implied_effects(m, groups = "area", method = "traditional", traditional_scale = "standardised")))
})
