## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
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
         Area2 = factor(Petal.Width)) %>%  # Area2 is a random-effects version
  select(CPUE, Year, Species, Area, Area2)
glimpse(iris2)
as.character(sort(unique(iris2$Year)))

plot_bubble(df = iris2, group = c("Year", "Species"), fill = "green")
plot_bubble(df = iris2, group = c("Year", "Area"), fill = "green")
plot_bubble(df = iris2, group = c("Year", "Area2"), fill = "green")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Fit a series of models using brms
fit0 <- brm(CPUE ~ Year, data = iris2, family = lognormal(), refresh = 0)
fit1 <- brm(CPUE ~ Year + Species, data = iris2, family = lognormal(), refresh = 0)
fit2 <- brm(CPUE ~ Year + Species + Area, data = iris2, family = lognormal(), refresh = 0)

get_coefs(fit2, var = "Species") %>% head()
get_influ(fit2, group = c("Year", "Species")) %>% head()

# Also fit a model using glm and generate influence statistics using the original influ package
fit_glm <- glm(log(CPUE) ~ Year + Species + Area, data = iris2)
myInfl <- Influence$new(fit_glm)
myInfl$calc()

# Generate a CDI plot
plot_bayesian_cdi(fit = fit2, group = c("Year", "Species"), xlab = "Species")
myInfl$cdiPlot(term = "Species")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Fit a model with random-effects using brms
fit_re <- brm(CPUE ~ Year + (1|Area2), data = iris2, family = lognormal(), verbose = FALSE, refresh = 0)

unique(get_coefs(fit = fit_re, var = "Area2")$variable)

# Generate a CDI plot
plot_bayesian_cdi(fit = fit_re, group = c("Year", "Area2"), xlab = "Area")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a step plot
fits <- list(fit0, fit1, fit2)
plot_step(fits = fits, year = "Year", probs = c(0.25, 0.75), show_probs = TRUE)
myInfl$stepPlot()

plot_index(fit2, year = "Year")
dev.new()
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
# Here I evaluate model fit using loo and waic. 
# Other options include kfold, loo_subsample, bayes_R2, loo_R2 and marglik
fit0 <- add_criterion(fit0, criterion = c("loo", "waic"))
fit1 <- add_criterion(fit1, criterion = c("loo", "waic"))
fit2 <- add_criterion(fit2, criterion = c("loo", "waic"))

fit0$criteria$loo
fit0$criteria$waic
loo_compare(fit0, fit1, fit2, criterion = "loo")
loo_compare(fit0, fit1, fit2, criterion = "waic")

yrep <- posterior_predict(fit2, draws = 500)
ppc_dens_overlay(y = iris2$CPUE, yrep = yrep[1:100,]) + 
  theme_bw() +
  labs(x = "CPUE", y = "Density")

plot_predicted_residuals(fit2)

fit_g <- brm(CPUE ~ Year + Species, data = iris2, family = Gamma(link = "log"), refresh = 0)
plot_qq(fit_g)

plot(fit_glm)

