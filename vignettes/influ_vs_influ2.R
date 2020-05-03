## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
library(reshape2)
library(readr)
library(brms)
library(influ2)
library(bayesplot)
source("../tests/testthat/influ.R")

# options(mc.cores = parallel::detectCores())

# Get some data to use
data(iris)
glimpse(iris)

iris2 <- iris %>% 
  mutate(CPUE = Petal.Length, 
         Year = factor(Sepal.Width * 10 + 1970), 
         Area = factor(round(Petal.Width)), 
         Duration = Sepal.Length,
         Duration2 = cut(Sepal.Length, 20)) %>%
  select(CPUE, Year, Species, Area, Duration, Duration2)
glimpse(iris2)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Fit a brms model
fit0 <- brm(CPUE ~ Year, data = iris2, family = lognormal(), refresh = 0)
fit1 <- brm(CPUE ~ Year + Species, data = iris2, family = lognormal(), refresh = 0)
fit2 <- brm(CPUE ~ Year + Species + Area, data = iris2, family = lognormal(), refresh = 0)

# Also fit a model using glm and generate influence statistics using the original influ package
fit_glm <- glm(log(CPUE) ~ Year + Species + Area, data = iris2)
myInfl <- Influence$new(fit_glm)
myInfl$calc()
myInfl$cdiPlot(term = "Species")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a CDI plot for a factor
plot_bayesian_cdi2(fit = fit2, group = c("Year", "Species"), xlab = "Species")
plot_bayesian_cdi(fit = fit2, group = c("Year", "Species"), xlab = "Species")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a step plot
fits <- list(fit0, fit1, fit2)
plot_step(fits = fits, year = "Year", probs = c(0.25, 0.75), show_probs = TRUE)
myInfl$stepPlot()

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
plot_index(fit2, year = "Year")
myInfl$stanPlot()

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
plot_influ(fit2, year = "Year")
myInfl$influPlot()

i1 <- myInfl$influences

i2 <- get_influ(fit = fit2, group = c("Year", "Species")) %>%
  group_by(Year) %>%
  summarise(delta = mean(delta))

plot(i1$Species, type = "b")
lines(i2$delta, col = 2)

i2 <- get_influ(fit = fit2, group = c("Year", "Area")) %>%
  group_by(Year) %>%
  summarise(delta = mean(delta))

plot(i1$Area, type = "b")
lines(i2$delta, col = 2)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
fit <- fit2
yrs <- sort(unique(fit$data$Year))
ce <- conditional_effects(x = fit, effects = "Year", method = "pp_expect")
newdata <- data.frame(id = 1:length(yrs), Year = yrs, Species = NA, Area = NA)
pp <- pp_expect(fit, newdata = newdata) %>%
  melt(varnames = c("iteration", "id")) %>%
  left_join(newdata, by = "id")

ggplot(data = pp, aes(x = Year, y = value)) +
  geom_violin() +
  geom_pointrange(data = ce[[1]], aes(x = Year, y = estimate__, ymin = lower__, ymax = upper__), colour = "red", alpha = 0.5)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Deriving coeffs in three different ways
ce <- conditional_effects(x = fit2, effects = "Area", method = "pp_expect")

newdata <- data.frame(id = 1:3, Year = NA, Species = NA, Area = unique(fit2$data$Area))
pp <- pp_expect(fit2, newdata = newdata) %>%
  melt(varnames = c("iteration", "id")) %>%
  left_join(newdata, by = "id")
# pp$value <- pp$value - mean(pp$value)
# cc <- get_coefs(fit2, var = "Area") %>%
#   mutate(Area = extract_numeric(variable))
# cc$value <- cc$value - mean(cc$value)

psi <- posterior_samples(fit2, pars = "Intercept") %>%
  mutate(iteration = 1:n()) %>%
  melt(id.vars = "iteration") %>%
  mutate(variable = gsub("b_", "", .data$variable)) 
psa <- posterior_samples(fit2, pars = "Area") %>%
  mutate(iteration = 1:n()) %>%
  melt(id.vars = "iteration") %>%
  mutate(variable = gsub("b_", "", .data$variable)) %>%
  mutate(Area = parse_number(variable)) %>%
  left_join(psi, by = "iteration") %>%
  mutate(value = exp(value.x + value.y)) %>%
  select(iteration, Area, value)
psi2 <- psi %>%
  mutate(Area = 0, value = exp(value)) %>%
  select(iteration, Area, value)
ps <- rbind(psi2, psa)

p <- ggplot(data = pp) +
  geom_violin(aes(x = Area, y = value), draw_quantiles = c(0.025, 0.975)) +
  geom_pointrange(data = ce[[1]], aes(x = Area, y = estimate__, ymin = lower__, ymax = upper__), colour = "red", alpha = 0.5) +
  # geom_violin(data = cc, aes(x = factor(Area), y = value), fill = "green", alpha = 0.5)
  geom_violin(data = ps, aes(x = factor(Area), y = value), fill = "green", alpha = 0.5)
p

