## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE-----------------
library(influ2)
library(brms)

# Get some data to use
data(iris)
glimpse(iris)
iris <- iris %>% 
  mutate(PetalLength = Petal.Length, 
         SepalLength = factor(round(Sepal.Length)), 
         SepalWidth = factor(round(Sepal.Width)))

plot_bubble(df = iris, group = c("SepalLength", "SepalWidth"), fill = "pink")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE-----------------
# Fit a model using brms
fit0 <- brm(PetalLength ~ SepalLength + SepalWidth, data = iris, family = lognormal())

# Generate a CDI plot - note this only works for integer factor levels at this stage
get_coefs(fit0, var = "SepalWidth") %>% head()
get_influ(fit0, group = c("SepalLength", "SepalWidth")) %>% head()
plot_bayesian_cdi(fit = fit0, group = c("SepalLength", "SepalWidth"))

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE-----------------
# Fit a model with random-effects using brms
fit1 <- brm(PetalLength ~ SepalLength + (1|SepalWidth), data = iris, family = lognormal())

# Generate a CDI plot - note this only works for integer factor levels at this stage
plot_bayesian_cdi(fit = fit1, group = c("SepalLength", "SepalWidth"))

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE-----------------
# data(iris)
# glimpse(iris)
# iris <- iris %>% mutate(Sepal.Width = factor(round(Sepal.Width)))
# library(reshape2)
# library(readr)
# 
# # Get it working for non-integer factors too and with full stops in it
# fit0 <- brm(Petal.Length ~ Species + Sepal.Width + Sepal.Length, data = iris, family = lognormal())
# fit=fit0
# group = c("Species", "Sepal.Width")
# plot_bayesian_cdi(fit0, group = c("Species", "Sepal.Width"))


