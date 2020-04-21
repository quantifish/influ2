## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
library(influ2)
library(brms)
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

plot_bubble(df = iris2, group = c("Year", "Species"), fill = "green")
plot_bubble(df = iris2, group = c("Year", "Area"), fill = "green")
plot_bubble(df = iris2, group = c("Year", "Area2"), fill = "green")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Fit a series of models using brms
fit0 <- brm(CPUE ~ Year, data = iris2, family = lognormal())
fit1 <- brm(CPUE ~ Year + Species, data = iris2, family = lognormal())
fit2 <- brm(CPUE ~ Year + Species + Area, data = iris2, family = lognormal())

get_coefs(fit2, var = "Species") %>% head()
get_influ(fit2, group = c("Year", "Species")) %>% head()

# Also fit a model using glm
fit_glm <- glm(log(CPUE) ~ Year + Species + Area, data = iris2)
myInfl <- Influence$new(fit_glm)
myInfl$calc()

# Generate a CDI plot
plot_bayesian_cdi(fit = fit2, group = c("Year", "Species"), xlab = "Species")
myInfl$cdiPlot(term = "Species")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Fit a model with random-effects using brms
fit_re <- brm(CPUE ~ Year + (1|Area2), data = iris2, family = lognormal(), verbose = FALSE, refresh = 500)

unique(get_coefs(fit = fit_re, var = "Area2")$variable)

# Generate a CDI plot
plot_bayesian_cdi(fit = fit_re, group = c("Year", "Area2"), xlab = "Area")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
# Generate a step plot
fits <- list(fit0, fit1, fit2)
step_plot(fits, year = "Year", probs = c(0.25, 0.75), show_probs = TRUE)

myInfl$stepPlot()

index_plot(fit2, year = "Year")

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

