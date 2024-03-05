library(tidyverse)
library(broom)
library(WeightIt)
library(MatchIt)

set.seed(1234)
nobs <- 1e4

simdata <- tibble(
  x = rbinom(nobs, 1, .5),
  z = rbinom(nobs, 1, .5),
  treat_prob = if_else(x*z == 1, .75, .25),  # note ff here
  treat = rbinom(nobs, 1, treat_prob),
  y = 2*x*z + treat + rnorm(nobs)            # note ff here
)

# naive (wrong answer)
naive_fit <- lm(y ~ treat,
                data = simdata)
tidy(naive_fit)

# wrong spec (wrong answer)
wrong_fit <- lm(y ~ treat + x + z,
                data = simdata)
tidy(wrong_fit)

# glm pscore (wrong answer)
w1 <- weightit(treat ~ x + z,
               data = simdata,
               method = "glm",
               estimand = "ate")

est_w1 <- lm(y ~ treat,
             data = simdata,
             weights = w1$weights)

tidy(est_w1)

# cbps (wrong answer)
w2 <- weightit(treat ~ x + z,
               data = simdata,
               method = "cbps",
               estimand = "ate")

est_w2 <- lm(y ~ treat,
             data = simdata,
             weights = w2$weights)

tidy(est_w2)

# ebal (wrong answer)
w3 <- weightit(treat ~ x + z,
               data = simdata,
               method = "ebal",
               estimand = "ate")

est_w3 <- lm(y ~ treat,
             data = simdata,
             weights = w3$weights)

tidy(est_w3)

# one version of DR (correct!)
w3 <- weightit(treat ~ x + z,
               data = simdata,
               method = "ebal",
               estimand = "ate")

dr_est_w3 <- lm(y ~ treat + x * z,         # this makes it work
                data = simdata,
                weights = w3$weights)

tidy(dr_est_w3)

# exact (correct!)
m1 <- matchit(treat ~ x + z,         
              data = simdata,
              method = "exact",            # matches perfectly so it works
              estimand = "ate")

est_m1 <- lm(y ~ treat,
             data = simdata,
             weights = m1$weights)

tidy(est_m1)

# DR exact (correct!)
dr_est_m1 <- lm(y ~ treat + x + z,         # wrong ff but doesn't matter
                data = simdata,
                weights = m1$weights)

tidy(dr_est_m1)                            # right answer with smaller SEs


# DR exact v2 (correct!)
dr2_est_m1 <- lm(y ~ treat + x * z,        # now ff is correct on both sides  
                 data = simdata,
                 weights = m1$weights)

tidy(dr2_est_m1)                           # smallest SEs yet!  
