test_that("family specifications supply correct defaults and structure guards", {
  links <- c(gaussian = "identity", binomial = "logit", poisson = "log",
             negative_binomial = "log", lognormal = "identity", gamma = "log",
             tweedie = "log")
  for (family in names(links)) {
    spec <- .new_family_spec(family, link = NULL)
    expect_s3_class(spec, "influ_family_spec")
    expect_identical(spec$family, family)
    expect_identical(spec$link, links[[family]])
    expect_identical(spec$structure, "single")
    expected_scale <- if (links[[family]] == "log" || family == "lognormal") "ratio" else "difference"
    expect_identical(spec$natural_scale, expected_scale)
  }
  for (value in list(NULL, NA_character_, "", c("gamma", "poisson"))) {
    expect_error(.normalise_family_name(value), "single, non-missing family name")
  }
  expect_error(.new_family_spec("gamma", "inverse"), "Unsupported link")
  expect_error(.new_family_spec("hurdle_gaussian", "identity"), "Hurdle/delta support")
  expect_error(.new_family_spec("zero_inflated_gamma", "log"), "Zero-inflated support")
  expect_identical(.normalise_family_name("delta_lognormal")$structure, "hurdle")
  expect_identical(.normalise_family_name("zi_nbinom2")$structure, "zero_inflated")
  expect_identical(.normalise_family_name("truncated_poisson")$family, "poisson")
  expect_identical(.normalise_family_name("Negative Binomial(2)")$family, "negative_binomial")
  expect_identical(.new_family_spec("binomial", "PROBIT")$link, "probit")
})

test_that("effect scales agree with independent inverse-link differences", {
  eta <- c(-2, 0, 1)
  delta <- c(-0.5, 0, 0.7)
  inverses <- list(identity = identity, logit = plogis, probit = pnorm,
                   cloglog = function(x) -expm1(-exp(x)))
  for (link in names(inverses)) {
    inverse <- inverses[[link]]
    family <- if (link == "identity") "gaussian" else "binomial"
    spec <- .new_family_spec(family, link)
    expected <- inverse(eta + delta) - inverse(eta)
    expect_equal(.link_inverse(eta, link), inverse(eta), tolerance = 1e-14)
    expect_equal(.effect_transform(delta, spec, eta), expected, tolerance = 1e-14)
    complement <- .new_family_spec(family, link, complement = TRUE)
    expect_equal(.effect_transform(delta, complement, eta), -expected, tolerance = 1e-14)
  }
  expect_equal(.link_inverse(eta, "log"), exp(eta))
  expect_equal(.effect_transform(delta, .new_family_spec("poisson", "log"), eta),
               exp(delta))
  expect_equal(.effect_transform(delta, .new_family_spec("lognormal", "identity"), eta),
               exp(delta))
  expect_error(.link_inverse(eta, "inverse"), "No inverse-link implementation")
})
