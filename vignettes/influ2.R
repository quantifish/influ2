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
iris <- iris %>% mutate(PetalLength = Petal.Length, SepalLength = factor(round(Sepal.Length)), SepalWidth = factor(round(Sepal.Width)))

plot_bubble(df = iris, group = c("SepalLength", "SepalWidth"), xlab = "SepalWidth", ylab = "SepalLength", zlab = "N", fill = "grey")

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE-----------------
# Fit a model using brms
fit1 <- brm(PetalLength ~ SepalLength + (1|SepalWidth), data = iris, family = lognormal())

# fit = fit1
# group = c("SepalLength", "SepalWidth")
# xlab = "Month"
# ylab = "Fishing year"
# colour = "purple"

# Generate a CDI plot - note this only works for integer factor levels at this stage
plot_bayesian_cdi(fit1, group = c("SepalLength", "SepalWidth"))

