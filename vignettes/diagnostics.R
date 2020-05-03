## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
library(brms)
library(influ2)
library(bayesplot)

# Get some data to use
data(iris)

iris2 <- iris %>% 
  mutate(CPUE = Petal.Length, 
         Year = factor(Sepal.Width * 10 + 1970), 
         Area = factor(round(Petal.Width)), 
         Duration = Sepal.Length,
         Duration2 = cut(Sepal.Length, 15)) %>%
  select(CPUE, Year, Species, Area, Duration, Duration2)
glimpse(iris2)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
fit <- brm(CPUE ~ Year + Species, data = iris2, family = Gamma(link = "log"), refresh = 0)

## ----echo=TRUE, fig.height=6, fig.width=6, message=FALSE----------------------
yrep <- posterior_predict(fit, draws = 500)
ppc_dens_overlay(y = iris2$CPUE, yrep = yrep[1:100,]) + 
  theme_bw() +
  labs(x = "CPUE", y = "Density")

plot_predicted_residuals(fit)

plot_qq(fit)

