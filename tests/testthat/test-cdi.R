library(influ2)
library(brms)

data(lobsters_per_pot)

brm1 <- brm(lobsters ~ year + month, data = lobsters_per_pot, family = poisson, chains = 1, file = "brm1", file_refit = "never")
glm1 <- glm(lobsters ~ year + month, data = lobsters_per_pot, family = poisson)
brm2 <- brm(lobsters ~ year + (1 | month), data = lobsters_per_pot, family = negbinomial(), chains = 1, file = "brm2", file_refit = "never")


context("CDI plot")

test_that("summary gives the same thing as get_coefs for population-level effects", {
  
  # This is get_coefs from influ2
  c1 <- get_coefs(fit = brm1, var = "year", normalise = FALSE) %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), Est.Error = sd(value), 
              Q5 = quantile(value, probs = 0.05), Q95 = quantile(value, probs = 0.95))
  
  # Here I use the fixef fuction from the nlme package
  c2 <- fixef(brm1, probs = c(0.05, 0.95)) %>%
    data.frame() %>%
    filter(!grepl("month", rownames(.))) %>%
    mutate(variable = c1$variable)
  c2[1,1:4] <- 0

  expect_is(brm1, "brmsfit")
  expect_error(ranef(brm1))
  expect_equal(c1$Estimate, c2$Estimate)
  expect_equal(c1$Est.Error, c2$Est.Error)
  expect_equal(as.numeric(c1$Q5), c2$Q5)
  expect_equal(as.numeric(c1$Q95), c2$Q95)
})


test_that("summary gives the same thing as get_coefs for group-level effects", {

  # This is get_coefs from influ2
  c1 <- get_coefs(fit = brm2, var = "month", normalise = FALSE) %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), 
              Est.Error = sd(value), 
              Q5 = quantile(value, probs = 0.05), 
              Q95 = quantile(value, probs = 0.95))

  # Here I use the ranef fuction from the nlme package
  c2 <- ranef(brm2, groups = "month", probs = c(0.05, 0.95))[[1]][,,1] %>%
    data.frame() %>%
    mutate(variable = rownames(.))
  
  expect_is(brm2, "brmsfit")
  expect_equal(c1$Estimate, c2$Estimate)
  expect_equal(c1$Est.Error, c2$Est.Error)
  expect_equal(as.numeric(c1$Q5), c2$Q5)
  expect_equal(as.numeric(c1$Q95), c2$Q95)
})


test_that("this matches Nokome Bentley's influ package", {
  
  # Test glm
  myInfl <- Influence$new(glm1)
  c1a <- myInfl$coeffs()
  c1a <- c1a[2:length(c1a)]
  c1b <- coef(glm1)
  c1b <- c1b[grepl("year", names(c1b))]
  # myInfl$summary
  # myInfl$stanPlot()
  # myInfl$stepPlot()
  # myInfl$influPlot()
  # myInfl$cdiPlot('Species', 'Sepal.Length')
  # myInfl$cdiPlotAll()
  
  # Test brms
  c2a <- get_coefs(fit = brm1, var = "year", normalise = FALSE, transform = FALSE) %>%
    filter(variable != "year2000") %>%
    group_by(variable) %>%
    summarise(Estimate = mean(value), 
              Est.Error = sd(value), 
              Q5 = quantile(value, probs = 0.05), 
              Q50 = quantile(value, probs = 0.5), 
              Q95 = quantile(value, probs = 0.95))
  
  c2b <- fixef(object = brm1, probs = c(0.05, 0.95)) %>%
    data.frame() %>%
    mutate(variable = rownames(.)) %>%
    filter(grepl("year", variable))
  
  # Test Nokomes coeffs are the same as stats::coef function
  # plot(c1a, type = "b")
  # lines(c1b, col = 2)
  expect_equal(c1a, c1b)
  
  # Check that my get_coefs function is the same as the nlme::fixef function
  # plot(c2a$Estimate, type = "b")
  # lines(c2b$Estimate, col = 2)
  expect_equal(c2a$Estimate, c2b$Estimate)

  # Check that Nokomes coeffs are the same as mine. This is maximum likelihood vs Bayesian so tolerance needs to be a little higher.
  # plot(c1a, type = "b")
  # lines(c2a$Estimate, col = 2)
  expect_equal(as.numeric(c1a), c2a$Estimate, tolerance = 0.07)

  # Check that stats::coef is the same as nlme::fixef. This is maximum likelihood vs Bayesian so tolerance needs to be a little higher.
  # plot(c1b, type = "b")
  # lines(c2b$Estimate, col = 2)
  expect_equal(as.numeric(c1b), c2b$Estimate, tolerance = 0.07)
  
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
