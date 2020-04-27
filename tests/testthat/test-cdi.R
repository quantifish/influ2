context("CDI plot")

test_that("summary gives the same thing as get_coefs for population-level effects", {
  
  library(brms)
  
  data(iris)
  iris <- iris %>% 
    mutate(PetalLength = Petal.Length, SepalWidth = factor(round(Sepal.Width)))
  
  fit <- brm(PetalLength ~ SepalWidth, data = iris, family = lognormal())
  
  # This is get_coefs from influ2
  c1 <- get_coefs(fit = fit, var = "SepalWidth", normalise = FALSE) %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), Est.Error = sd(value), Q5 = quantile(value, probs = 0.05), Q95 = quantile(value, probs = 0.95))
  
  # Here I use the fixef fuction from the nlme package
  c2 <- fixef(fit, probs = c(0.05, 0.95)) %>%
    data.frame() %>%
    mutate(variable = c1$variable)
  c2[1,1:4] <- 0

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

  # This is get_coefs from influ2
  c1 <- get_coefs(fit = fit, var = "SepalWidth", normalise = FALSE) %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), Est.Error = sd(value), Q5 = quantile(value, probs = 0.05), Q95 = quantile(value, probs = 0.95))

  # Here I use the ranef fuction from the nlme package
  c2 <- ranef(fit, groups = "SepalWidth", probs = c(0.05, 0.95))[[1]][,,1] %>%
    data.frame() %>%
    mutate(variable = rownames(.))
  
  expect_is(fit, "brmsfit")
  expect_equal(c1$Estimate, c2$Estimate)
  expect_equal(c1$Est.Error, c2$Est.Error)
  expect_equal(c1$Q5, c2$Q5)
  expect_equal(c1$Q95, c2$Q95)
})


test_that("this matches Nokome Bentley's influ package", {
  
  library(brms)
  library(proto)
  
  data(iris)
  iris <- iris %>% mutate(Sepal.Width = factor(Sepal.Width))

  fit1 <- glm(Petal.Length ~ Sepal.Width + Species, data = iris)
  
  source("influ.R")
  # source("tests/testthat/influ.R")
  myInfl <- Influence$new(fit1)
  c1a <- myInfl$coeffs()
  c1a <- c1a[2:length(c1a)]
  c1b <- coef(fit1)
  c1b <- c1b[grepl("Sepal.Width", names(c1b))]
  # myInfl$summary
  # myInfl$stanPlot()
  # myInfl$stepPlot()
  # myInfl$influPlot()
  # myInfl$cdiPlot('Species', 'Sepal.Length')
  # myInfl$cdiPlotAll()
  
  # In brms
  fit2 <- brm(Petal.Length ~ Sepal.Width + Species, data = iris)
  
  c2a <- get_coefs(fit = fit2, var = "Sepal.Width", normalise = FALSE) %>%
    filter(variable != "Sepal.Width2") %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), Est.Error = sd(value), Q5 = quantile(value, probs = 0.05), Q95 = quantile(value, probs = 0.95))
  c2b <- fixef(fit2, probs = c(0.05, 0.95)) %>%
    data.frame() %>%
    mutate(variable = rownames(.)) %>%
    filter(grepl("Sepal.Width", variable))
  
  # Check the coefficients are the same
  plot(c1a, type = "b")
  lines(c2a$Estimate, col = 2)
  
  plot(c1b, type = "b")
  lines(c2b$Estimate, col = 2)
  
  expect_equal(c1a, c1b, tolerance = 0.01)
  expect_equal(c2a$Estimate, c2b$Estimate)
  expect_equal(as.numeric(c1a), c2a$Estimate, tolerance = 0.06)
  expect_equal(as.numeric(c1b), c2b$Estimate, tolerance = 0.06)
  
  # Check the influences are the same - I couldn't get this to work as Noko's code is broken
  # myInfl$calc()
  # i1 <- myInfl$influences
  # i2 <- get_influ(fit = fit2, group = c("Sepal.Width", "Species")) %>%
  #   group_by(Sepal.Width) %>%
  #   summarise(delta = mean(delta))
  # 
  # plot(i1$Species, type = "b")
  # lines(i2$delta, col = 2)
  # 
  # expect_equal(i1$Species, i2$delta, tolerance = 0.002)
})
