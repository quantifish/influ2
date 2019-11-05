context("CDI plot")

test_that("summary gives the same thing as get_coefs for population-level effects", {
  
  library(brms)
  
  data(iris)
  iris <- iris %>% 
    mutate(PetalLength = Petal.Length, 
           SepalWidth = factor(round(Sepal.Width)))
  
  fit <- brm(PetalLength ~ SepalWidth, data = iris, family = lognormal())
  
  c1 <- get_coefs(fit = fit, var = "SepalWidth", normalise = FALSE) %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), Est.Error = sd(value), Q5 = quantile(value, probs = 0.05), Q95 = quantile(value, probs = 0.95))
  
  c2 <- fixef(fit, probs = c(0.05, 0.95)) %>%
    data.frame() %>%
    mutate(variable = rownames(.)) %>%
    filter(grepl("SepalWidth", variable))
  
  expect_is(fit, "brmsfit")
  expect_error(ranef(fit))
  expect_equal(c1$Estimate, c2$Estimate)
  expect_equal(c1$Est.Error, c2$Est.Error)
  expect_equal(c1$Q5, c2$Q5)
  expect_equal(c1$Q95, c2$Q95)

})

test_that("summary gives the same thing as get_coefs for group-level effects", {

  library(brms)

  data(iris)
  iris <- iris %>%
    mutate(PetalLength = Petal.Length,
           SepalWidth = factor(round(Sepal.Width)))

  fit <- brm(PetalLength ~ (1|SepalWidth), data = iris, family = lognormal())

  c1 <- get_coefs(fit = fit, var = "SepalWidth", normalise = FALSE) %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), Est.Error = sd(value), Q5 = quantile(value, probs = 0.05), Q95 = quantile(value, probs = 0.95))

  c2 <- ranef(fit, groups = "SepalWidth", probs = c(0.05, 0.95))[[1]][,,1] %>%
    data.frame() %>%
    mutate(variable = rownames(.))
  
  expect_is(fit, "brmsfit")
  expect_equal(c1$Estimate, c2$Estimate)
  expect_equal(c1$Est.Error, c2$Est.Error)
  expect_equal(c1$Q5, c2$Q5)
  expect_equal(c1$Q95, c2$Q95)

})
