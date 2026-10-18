example_cache_fixture <- function() {
  withr::local_seed(184)
  d <- expand.grid(year = factor(1:3), month = factor(1:7), replicate = 1:8)
  d$x <- rnorm(nrow(d))
  effects <- seq(-.9, .9, length.out = 7)
  d$y <- rnbinom(nrow(d), size = 5,
    mu = exp(1.4 + .4 * d$x + effects[as.integer(d$month)]))
  d
}

example_cache_helper <- function() {
  e <- new.env(parent = environment())
  source_article_models(e)
  e$example_model
}

test_that("identical native GLM calls reuse one key without retaining article state", {
  example_model <- example_cache_helper()
  d <- example_cache_fixture()
  alias <- d
  cache <- withr::local_tempdir()
  withr::local_seed(310)
  rng <- .Random.seed
  fresh <- example_model(d, glm(y ~ year + month + poly(x, 2),
    family = poisson(link = "log"), data = d), cache_dir = cache)
  hit <- example_model(alias, stats::glm(y ~ year + month + poly(x, 2),
    data = alias, family = poisson(link = "log")), cache_dir = cache)
  expect_identical(.Random.seed, rng)
  expect_length(list.files(cache), 1L)
  expect_equal(coef(hit), coef(fresh), tolerance = 0)
  expect_equal(vcov(hit), vcov(fresh), tolerance = 0)
  expect_equal(predict(hit), predict(fresh), tolerance = 0)
  expect_identical(deparse(getCall(hit)), deparse(getCall(fresh)))
  private <- environment(formula(hit))
  expect_identical(ls(private, all.names = TRUE), ".example_data")
  expect_identical(parent.env(private), asNamespace("stats"))
  expect_identical(eval(getCall(hit)$data), d)
  expect_equal(coef(update(hit, . ~ year + month)),
    coef(glm(y ~ year + month, family = poisson(link = "log"), data = d)))
  diagnostic <- influ(hit, focus = "year")
  expect_equal(diagnostic$influence, influ(fresh, focus = "year")$influence)

  # Change only the documentation helper, never an already loaded namespace.
  internal <- environment(example_model)
  native_fit <- internal$fit_native
  withr::defer(assign("fit_native", native_fit, envir = internal))
  assign("fit_native", function(...) stop("The cached model must not be refitted"),
    envir = internal)
  expect_equal(coef(example_model(d, glm(y ~ year + month + poly(x, 2),
    family = poisson(link = "log"), data = d), cache_dir = cache)), coef(fresh))
})

test_that("example keys include exact data, formula, family, options, and builds", {
  example_model <- example_cache_helper()
  d <- example_cache_fixture()
  cache <- withr::local_tempdir()
  run <- function(data = d) example_model(data,
    glm(y ~ year + month + x, family = poisson(), data = data), cache_dir = cache)
  run()
  variants <- list(
    transform(d, x = x + .001), d[rev(seq_len(nrow(d))), ],
    transform(d, month = factor(month, levels = rev(levels(month)))))
  attributed <- d
  attr(attributed, "example_note") <- "An explicit data attribute"
  variants[[4L]] <- attributed
  for (i in seq_along(variants)) {
    run(variants[[i]])
    expect_length(list.files(cache), i + 1L)
  }
  example_model(d, glm(y ~ year + month, family = poisson(), data = d), cache_dir = cache)
  example_model(d, glm(y ~ year + month + x, family = gaussian(), data = d), cache_dir = cache)
  expect_length(list.files(cache), 7L)
  example_model(d, glm(y ~ year + month + x, family = poisson(), data = d,
    control = glm.control(maxit = 30)), cache_dir = cache)
  expect_length(list.files(cache), 8L)
  withr::local_options(contrasts = c("contr.sum", "contr.poly"))
  run()
  expect_length(list.files(cache), 9L)
  internal <- environment(example_model)
  native_versions <- internal$versions
  withr::defer(assign("versions", native_versions, envir = internal))
  assign("versions", function(backend) {
    x <- native_versions(backend)
    x[[1L]]$Built <- paste(x[[1L]]$Built, "different compiler build")
    x
  }, envir = internal)
  run()
  expect_length(list.files(cache), 10L)
})

test_that("native-only thread options do not invalidate GLM example keys", {
  example_model <- example_cache_helper()
  d <- example_cache_fixture()
  cache <- withr::local_tempdir()
  a <- example_model(d, glm(y ~ year + month + x, family = poisson(), data = d), cache_dir = cache)
  withr::local_options(glmmTMB.cores = 2L, glmmTMB.autopar = FALSE)
  b <- example_model(d, glm(y ~ year + month + x, family = poisson(), data = d), cache_dir = cache)
  expect_length(list.files(cache), 1L)
  expect_equal(coef(a), coef(b))
})

test_that("MASS fits and uncached references retain their native self-contained calls", {
  skip_if_not_installed("MASS")
  example_model <- example_cache_helper()
  d <- example_cache_fixture()
  cache <- withr::local_tempdir()
  a <- example_model(d, MASS::glm.nb(y ~ year + month + x, link = log, data = d), cache_dir = cache)
  b <- example_model(d, MASS::glm.nb(y ~ year + month + x, data = d, link = log), cache_dir = cache)
  uncached <- example_model(d, MASS::glm.nb(y ~ year + month + x, link = log, data = d), cache_dir = NULL)
  native <- MASS::glm.nb(y ~ year + month + x, link = log, data = d)
  expect_s3_class(b, "negbin")
  expect_length(list.files(cache), 1L)
  expect_equal(coef(a), coef(b), tolerance = 0)
  expect_equal(coef(a), coef(uncached), tolerance = 0)
  expect_equal(a$theta, b$theta, tolerance = 0)
  expect_equal(a$theta, uncached$theta, tolerance = 0)
  native_arguments <- setdiff(names(as.list(getCall(native))), c("", "formula", "data"))
  for (name in native_arguments) {
    expect_identical(getCall(a)[[name]], getCall(native)[[name]], info = name)
    expect_identical(getCall(b)[[name]], getCall(native)[[name]], info = name)
    expect_identical(getCall(uncached)[[name]], getCall(native)[[name]], info = name)
  }
  expect_identical(getCall(b)$init.theta, signif(native$theta, 10))
  cached_update <- update(b, . ~ year + month)
  direct_update <- update(native, . ~ year + month, data = d)
  expect_equal(coef(cached_update), coef(direct_update), tolerance = 0)
  expect_equal(cached_update$theta, direct_update$theta, tolerance = 0)
  expect_equal(as.numeric(logLik(cached_update)), as.numeric(logLik(direct_update)), tolerance = 0)
})

test_that("malformed example caches and unsupported workspace references fail explicitly", {
  example_model <- example_cache_helper()
  d <- example_cache_fixture()
  cache <- withr::local_tempdir()
  run <- function() example_model(d,
    glm(y ~ year + month + x, family = poisson(), data = d), cache_dir = cache)
  run()
  path <- list.files(cache, full.names = TRUE)
  original <- readRDS(path)
  stale <- original
  stale$identity$schema <- -1L
  saveRDS(stale, path)
  expect_error(run(), "Malformed or stale")
  bad <- original
  bad$model[1L] <- as.raw(bitwXor(as.integer(bad$model[1L]), 255L))
  saveRDS(bad, path)
  expect_error(run(), "Malformed or stale")
  bad <- original
  bad$model <- serialize(list(), NULL, version = 3L)
  bad$checksum <- environment(example_model)$hash(bad$model, serialise = FALSE)
  saveRDS(bad, path)
  expect_error(run(), "wrong native model class")
  saveRDS("not a model record", path)
  expect_error(run(), "Malformed or stale")
  saveRDS(original, path)
  expect_s3_class(run(), "glm")
  expect_error(example_model(d, lm(y ~ x, data = d), cache_dir = cache), "supports only")
  workspace_predictor <- seq_len(nrow(d))
  expect_error(example_model(d, glm(y ~ year + workspace_predictor,
    family = poisson(), data = d), cache_dir = NULL), "isolated example model")
  workspace_formula <- y ~ year + x
  expect_error(example_model(d, glm(workspace_formula,
    family = poisson(), data = d), cache_dir = NULL), "isolated example formula")
  altered <- transform(d, x = x + 1)
  expect_error(example_model(d, glm(y ~ x, data = altered), cache_dir = cache), "fitting data differ")
  expect_error(example_model(d, glm(y ~ x, data = d), cache_dir = ""), "cache_dir")
})

test_that("example caching preserves both existing and absent RNG state", {
  example_model <- example_cache_helper()
  d <- example_cache_fixture()
  cache <- withr::local_tempdir()
  withr::local_seed(143)
  rng <- .Random.seed
  example_model(d, glm(y ~ year + month + x, family = poisson(), data = d), cache_dir = cache)
  expect_identical(.Random.seed, rng)
  rm(".Random.seed", envir = globalenv())
  example_model(d, glm(y ~ year + month + x, family = poisson(), data = d), cache_dir = cache)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  example_model(d, glm(y ~ year + month + x, family = poisson(), data = d), cache_dir = NULL)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("reloaded glmmTMB examples preserve native diagnostics without refitting", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("TMB")
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  example_model <- example_cache_helper()
  d <- example_cache_fixture()
  cache <- withr::local_tempdir()
  withr::local_seed(51)
  rng <- .Random.seed
  thread <- TMB::openmp(DLL = "glmmTMB")
  fresh <- example_model(d, glmmTMB::glmmTMB(y ~ year + x + (1 | month),
    family = glmmTMB::nbinom2(), data = d), cache_dir = cache)
  expect_identical(.Random.seed, rng)
  expect_identical(TMB::openmp(DLL = "glmmTMB"), thread)
  native <- glmmTMB::glmmTMB(y ~ year + x + (1 | month),
    family = glmmTMB::nbinom2(), data = d)
  expect_equal(coef(fresh), coef(native), tolerance = 0)
  expect_equal(fresh$fit$parfull, native$fit$parfull, tolerance = 0)
  native_arguments <- setdiff(names(as.list(getCall(native))), c("", "formula", "data"))
  for (name in native_arguments) {
    actual <- getCall(fresh)[[name]]
    expected_argument <- getCall(native)[[name]]
    # The isolated fit intentionally has a different formula environment.
    if (inherits(actual, "formula") && inherits(expected_argument, "formula")) {
      attr(actual, ".Environment") <- NULL
      attr(expected_argument, ".Environment") <- NULL
    }
    expect_identical(actual, expected_argument, info = name)
  }
  signatures <- function(model) {
    report <- check_cpue(model, simulate = FALSE)
    diagnostic <- influ(model, focus = "year")
    index <- cpue_index(model, reference_data = data.frame(x = 0))
    list(predicted = predict(model), vcov = vcov(model),
      influence = diagnostic$influence, statistics = report$model_statistics,
      overall = report$overall, terms = report$term_checks,
      index = index$table, covariance = index_vcov(index),
      simulated = withr::with_seed(971, as.numeric(simulate(model)[[1L]])),
      call = deparse(getCall(model)))
  }
  expected <- signatures(fresh)
  # Do not send the parent's fitted models or test workspace to the child.
  environment(signatures) <- globalenv()
  helper <- source_article_models(new.env(parent = environment()))
  package_path <- normalizePath(getNamespaceInfo(asNamespace("influ2"), "path"), winslash = "/")
  loaded <- callr::r(function(data, cache, helper, package_path, development, signatures) {
    if (development) pkgload::load_all(package_path, quiet = TRUE, helpers = FALSE,
      attach_testthat = FALSE)
    else library("influ2", lib.loc = dirname(package_path), character.only = TRUE)
    requireNamespace("glmmTMB", quietly = TRUE)
    source(helper, local = TRUE)
    # A cache hit must never enter the helper's native fitting evaluator.
    # The backend and base-package namespaces remain untouched.
    assign("fit_native", function(...) stop("The native model was refitted"),
      envir = environment(example_model))
    set.seed(51)
    seed <- .Random.seed
    previous <- TMB::openmp(DLL = "glmmTMB")
    fit <- example_model(data, glmmTMB::glmmTMB(y ~ year + x + (1 | month),
      data = data, family = glmmTMB::nbinom2()), cache_dir = cache)
    environment(signatures) <- environment()
    list(values = signatures(fit), unchanged_seed = identical(.Random.seed, seed),
      thread = TMB::openmp(DLL = "glmmTMB"), previous_thread = previous,
      native_mode = fit$obj$env$last.par.best, fitted_mode = fit$fit$parfull,
      private_names = ls(environment(formula(fit)), all.names = TRUE))
  }, args = list(data = d, cache = cache, helper = helper,
    package_path = package_path, development = pkgload::is_dev_package("influ2"),
    signatures = signatures), system_profile = FALSE, user_profile = FALSE,
    package = FALSE, timeout = 60)
  expect_equal(loaded$values, expected, tolerance = 1e-10)
  expect_true(loaded$unchanged_seed)
  expect_identical(loaded$thread, loaded$previous_thread)
  expect_equal(loaded$native_mode, loaded$fitted_mode, tolerance = 1e-10)
  expect_identical(loaded$private_names, ".example_data")
  expect_length(list.files(cache), 1L)
  cached <- example_model(d, glmmTMB::glmmTMB(y ~ year + x + (1 | month),
    family = glmmTMB::nbinom2(), data = d), cache_dir = cache)
  expect_equal(coef(update(cached, . ~ . - x)),
    coef(update(native, . ~ . - x, data = d)))
  # Native execution settings are part of glmmTMB cache identity.
  withr::local_options(glmmTMB.cores = 1L)
  example_model(d, glmmTMB::glmmTMB(y ~ year + x + (1 | month),
    family = glmmTMB::nbinom2(), data = d), cache_dir = cache)
  expect_length(list.files(cache), 2L)
})
