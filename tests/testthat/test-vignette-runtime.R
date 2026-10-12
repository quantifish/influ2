test_that("the faster lme4 article fit retains the complete worked example", {
  sources <- c(test_path("..", "..", "vignettes", "influ2.Rmd"),
    system.file("doc", "influ2.Rmd", package = "influ2"))
  sources <- sources[nzchar(sources) & file.exists(sources)]
  if (!length(sources)) skip("The article source is unavailable in this installation")
  lines <- readLines(sources[1L], warn = FALSE)
  start <- which(startsWith(lines, "```{r lobster-lme4,"))
  expect_length(start, 1L)
  end <- which(seq_along(lines) > start & trimws(lines) == "```")[1L]
  expressions <- parse(text = lines[seq.int(start + 1L, end - 1L)])
  fit <- expressions[[1L]][[3L]]
  expect_identical(fit[[1L]], quote(lme4::glmer.nb))
  expect_identical(fit[[2L]], quote(lobsters ~ year + poly(depth, 2) +
    poly(soak, 2) + (1 | month)))
  expect_identical(fit$data, quote(lobsters_per_pot))
  expect_identical(fit$control,
    quote(lme4::glmerControl(optimizer = "nloptwrap",
      optCtrl = list(ftol_abs = 1e-10))))
  # Do not accelerate checking by changing data, quadrature, derivative
  # settings, or NB-size estimation, or by turning the example off.
  expect_identical(names(as.list(fit))[-1L], c("", "data", "control"))
  expect_false(grepl("eval[[:space:]]*=[[:space:]]*FALSE", lines[start]))
  expect_identical(expressions[[2L]][[3L]],
    quote(influ(lobster_lme4, focus = "year")))
})

test_that("native NB optimiser choices preserve diagnostics and assessment indices", {
  skip_if_not_installed("lme4", "1.1.36")
  set.seed(194)
  d <- expand.grid(year = factor(1:4), month = factor(1:6), replicate = 1:15)
  d$depth <- runif(nrow(d), -1, 1) + .15 * as.integer(d$year)
  d$soak <- runif(nrow(d), .5, 2)
  eta <- 1 + .16 * as.integer(d$year) + .4 * d$depth + .2 * d$soak +
    c(-.8, -.4, -.1, .2, .5, .9)[d$month]
  d$y <- rnbinom(nrow(d), mu = exp(eta), size = 3)
  original <- d
  rng <- .Random.seed
  default <- lme4::glmer.nb(y ~ year + depth + soak + (1 | month), data = d)
  fast <- lme4::glmer.nb(y ~ year + depth + soak + (1 | month), data = d,
    control = lme4::glmerControl(optimizer = "nloptwrap",
      optCtrl = list(ftol_abs = 1e-10)))
  expect_identical(d, original)
  expect_identical(.Random.seed, rng)
  for (fit in list(default, fast)) {
    expect_equal(fit@optinfo$conv$opt, 0)
    expect_null(fit@optinfo$conv$lme4$messages)
    expect_equal(lme4::getME(fit, "devcomp")$dims[["nAGQ"]], 1L)
  }
  expect_lte(abs(as.numeric(logLik(fast) - logLik(default))), 1e-6)
  expect_equal(lme4::fixef(fast), lme4::fixef(default), tolerance = 1e-4)
  expect_equal(lme4::getME(fast, "theta"), lme4::getME(default, "theta"),
    tolerance = 1e-4)
  expect_equal(lme4::getME(fast, "glmer.nb.theta"),
    lme4::getME(default, "glmer.nb.theta"), tolerance = 1e-4)
  expect_equal(as.matrix(vcov(fast, full = TRUE)),
    as.matrix(vcov(default, full = TRUE)), tolerance = 1e-4)
  a <- influ(default, focus = "year")
  b <- influ(fast, focus = "year")
  expect_identical(a$influence[c("term", "component", "level", "scale")],
    b$influence[c("term", "component", "level", "scale")])
  for (column in c("estimate", "std_error", "lower", "upper")) {
    expect_equal(a$influence[[column]], b$influence[[column]], tolerance = 1e-4)
  }
  reference <- data.frame(depth = 0, soak = 1)
  a <- cpue_index(default, year = "year", reference_data = reference)
  b <- cpue_index(fast, year = "year", reference_data = reference)
  expect_identical(a$table$Year, b$table$Year)
  for (column in c("Mean", "SD", "CV", "Qlower", "Qupper")) {
    expect_equal(a$table[[column]], b$table[[column]], tolerance = 1e-4)
  }
  expect_equal(index_vcov(a, scale = "response"),
    index_vcov(b, scale = "response"), tolerance = 1e-4)
  expect_equal(index_vcov(a, scale = "log"),
    index_vcov(b, scale = "log"), tolerance = 1e-4)
})
