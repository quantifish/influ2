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

# options(mc.cores = parallel::detectCores())

# Get some data to use
flats <- read_csv("flats.csv") %>%
  rename(vessel = vess, duration = towDur, catch = yfswt, latitude = lat, longitude = long, area = adfg) %>%
  filter(!is.na(latitude), !is.na(longitude), catch > (fhswt + nrswt)) %>%
  mutate(vessel = as.numeric(as.factor(vessel)), cpue = catch / duration) %>%
  select(cpue, catch, duration, vessel, year, week, latitude, longitude, area)
core_area <- flats %>%
  group_by(area) %>%
  summarise(catch = sum(catch)) %>%
  arrange(-catch)
write_csv(flats %>% filter(area %in% core_area$area[1:30]), "flats2.csv")
ct <- cols(col_double(), col_double(), col_double(), col_character(), col_character(), col_character(), col_double(), col_double(), col_character())
flats <- read_csv("flats2.csv", col_types = ct)
glimpse(flats)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Fit a series of models using glm
fit_glm0 <- glm(log(cpue) ~ year, data = flats)
fit_glm1 <- glm(log(cpue) ~ year + week, data = flats)
fit_glm2 <- glm(log(cpue) ~ year + week + vessel, data = flats)

# Fit a series of models using brms
fit0 <- brm(cpue ~ year, data = flats, family = lognormal(), refresh = 0)
fit1 <- brm(cpue ~ year + area, data = flats, family = lognormal(), refresh = 0)
fit2 <- brm(cpue ~ year + area + vessel, data = flats, family = lognormal(), refresh = 0)

# Generate a CDI plot using the influ2 package
plot_bayesian_cdi(fit = fit2, group = c("year", "vessel"), xlab = "Vessel")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a step plot
fits <- list(fit0, fit1, fit2)
plot_step(fits, year = "year", probs = c(0.25, 0.75), show_probs = TRUE)

plot_index(fit2, year = "year")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
plot_influ(fit2, year = "year")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Here I evaluate model fit using loo and waic. 
# Other options include kfold, loo_subsample, bayes_R2, loo_R2 and marglik
fit0 <- add_criterion(fit0, criterion = c("loo", "waic"))
fit1 <- add_criterion(fit1, criterion = c("loo", "waic"))
fit2 <- add_criterion(fit2, criterion = c("loo", "waic"))

loo_compare(fit0, fit1, fit2, criterion = "loo")
loo_compare(fit0, fit1, fit2, criterion = "waic")
fit0$criteria$loo
fit1$criteria$waic

yrep <- posterior_predict(fit2, draws = 500)
ppc_dens_overlay(y = flats$cpue, yrep = yrep[1:100,]) + 
  theme_bw() +
  coord_cartesian(xlim = c(0, 20)) +
  labs(x = "CPUE", y = "Density")

plot_predicted_residuals(fit2)

