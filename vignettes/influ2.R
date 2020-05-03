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
plot_bubble(df = iris2, group = c("Year", "Duration2"), fill = "green")
plot_bubble(df = iris2, group = c("Year", "Species"), fill = "Area")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Fit a series of models using brms
do_mcmc <- FALSE
if (do_mcmc) {
  fit0 <- brm(CPUE ~ Year, data = iris2, family = lognormal(), refresh = 0)
  fit1 <- brm(CPUE ~ Year + Species, data = iris2, family = lognormal(), refresh = 0)
  fit2 <- brm(CPUE ~ Year + Species + Area, data = iris2, family = lognormal(), refresh = 0)
  
  fit3 <- brm(CPUE ~ Year + Duration, data = iris2, refresh = 0)
  fit4 <- brm(CPUE ~ Year + poly(Duration, 3), data = iris2, refresh = 0)
  fit5 <- brm(CPUE ~ Year + s(Duration), data = iris2, refresh = 0)
  fit6 <- brm(CPUE ~ Year + (1|Duration2), data = iris2, refresh = 0)
  
  save(fit0, fit1, fit2, fit3, fit4, fit5, fit6, file = "mcmc.rda")
} else{
  load("mcmc.rda")
}

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a step plot
fits <- list(fit0, fit1, fit2)
plot_step(fits = fits, year = "Year", probs = c(0.25, 0.75), show_probs = TRUE)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Here I evaluate model fit using loo and waic. 
# Other options include kfold, loo_subsample, bayes_R2, loo_R2 and marglik
fit0 <- add_criterion(fit0, criterion = c("loo", "waic"))
fit1 <- add_criterion(fit1, criterion = c("loo", "waic"))
fit2 <- add_criterion(fit2, criterion = c("loo", "waic"))

fit0$criteria$loo
fit0$criteria$waic

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
loo_compare(fit0, fit1, fit2, criterion = "loo") %>%
  knitr::kable(digits = 1)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
loo_compare(fit0, fit1, fit2, criterion = "waic") %>%
  knitr::kable(digits = 1)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
plot_influ(fit = fit2, year = "Year")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a CDI plot for a factor
plot_bayesian_cdi(fit = fit2, group = c("Year", "Species"), xlab = "Species")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a CDI plot for a continuous variable
plot_bayesian_cdi(fit = fit3, group = c("Year", "Duration"), xlab = "Duration")

# Generate a CDI plot for a polynomial variable
plot_bayesian_cdi(fit = fit4, group = c("Year", "Duration"), xlab = "Duration")

# Generate a CDI plot for a spline
plot_bayesian_cdi(fit = fit5, group = c("Year", "Duration"), xlab = "Duration")

# Generate a CDI plot for a random-effect
plot_bayesian_cdi(fit = fit6, group = c("Year", "Duration2"), xlab = "Area")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
fits <- list(fit3, fit4, fit5, fit6)
plot_compare(fits = fits, labels = c("linear", "poly", "spline", "random-effect"), year = "Year")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
plot_index(fit = fit4, year = "Year")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
get_index(fit = fit2, year = "Year") %>%
  select(Year, Estimate, Est.Error, Qlower, Q50, Qupper) %>%
  knitr::kable(digits = 3)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
cpue1 <- get_index(fit = fit2, year = "Year") %>% mutate(Type = "GLM")
cpue2 <- cpue1 %>% mutate(Type = "Simulated")
for (ii in 1:nrow(cpue1)) {
  # sdd <- cpue1$Est.Error[ii] # wrong
  sdd <- log(1 + cpue1$Est.Error[ii] / cpue1$Estimate[ii])
  r1 <- rlnorm(n = 5000, log(cpue1$Q50[ii]), sdd)
  cpue2$Q50[ii] <- median(r1)
  cpue2$Qlower[ii] <- quantile(r1, probs = 0.025)
  cpue2$Qupper[ii] <- quantile(r1, probs = 0.975)
}
ggplot(data = rbind(cpue1, cpue2), aes(x = Year)) +
  geom_pointrange(aes(y = Q50, ymin = Qlower, ymax = Qupper, colour = Type), position = position_dodge(width = 0.5)) +
  theme_bw()

