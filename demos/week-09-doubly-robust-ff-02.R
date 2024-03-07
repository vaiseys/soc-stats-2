library(tidyverse)
library(broom)
library(WeightIt)
library(MatchIt)

set.seed(1234)
nobs <- 1e4


simdata <- tibble(
  U = rbinom(nobs, 1, .5),
  x1 = rnorm(nobs, -.5 + U, 1),
  x2 = rpois(nobs, 3 + 5*U),
  treat_log_odds = -3 + x1 + log(x2+1) + U,
  treat_prob = exp(treat_log_odds) / (1 + exp(treat_log_odds)),
  treat = rbinom(nobs, 1, treat_prob),
  y = x1 + log(x2+1) + treat + rnorm(nobs, 0, 2)
)