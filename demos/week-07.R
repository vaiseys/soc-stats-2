### prep ####
library(tidyverse)
library(broom)
library(cobalt)
library(MatchIt)
library(WeightIt)
library(here)

# bring in smoking data
load(here("data", "cattaneo2.Rdata"))

# we need to remove labels
d <- d |> 
  haven::zap_labels()

# get a look at it
psych::describe(d, fast = TRUE)

# add a new variable
d <- d |> 
  mutate(mcollege = if_else(medu >= 16, 1L, 0L))

# custom love plot
love_plot <- function(x) {
  love.plot(x,
            binary = "std",
            stats = c("m", "ks"),
            thresholds = c(.1, .05))
}

### Naive ####
naive_te <- lm(bweight ~ mbsmoke,
               data = d)
tidy(naive_te, conf.int = TRUE)

### Exact Matching ####
ematch_out <- matchit(mbsmoke ~ mmarried + mhisp + foreign + alcohol +
                        deadkids + prenatal1 + mcollege,
                      data = d,
                      method = "exact",
                      estimand = "ATT")
summary(ematch_out)
love_plot(ematch_out)

ematch_att_mod <- lm(bweight ~ mbsmoke,
                     data = d,
                     weights = ematch_out$weights)
tidy(ematch_att_mod, conf.int = TRUE)


### Propensity score ####
trt_form_ff <- "mbsmoke ~ mmarried + mhisp + foreign + alcohol +
                        deadkids + prenatal1 + mcollege + mrace +
                        mage + I(mage^2) + medu + I(medu^2) +
                        nprenatal + I(nprenatal^2)"

W1 <- weightit(as.formula(trt_form_ff),
               method = "ps",
               estimand = "ATT",
               data = d)
summary(W1)
love_plot(W1)

W1_att_mod <- lm(bweight ~ mbsmoke,
                 data = d,
                 weights = W1$weights)

tidy(W1_att_mod, conf.int = TRUE)

# pscore dist
bal.plot(W1, which = "both")

# for a single variable
bal.plot(W1,
         var.name = "medu",
         which = "both",
         type = "histogram")

### Bad propensity score ####
bad_form_ff <- "mbsmoke ~ mmarried + alcohol + mrace +
                        mage + medu + nprenatal"

W2 <- weightit(as.formula(bad_form_ff),
               method = "ps",
               estimand = "ATT",
               data = d)
summary(W2)
love_plot(W2)

W2_att_mod <- lm(bweight ~ mbsmoke,
                 data = d,
                 weights = W2$weights)

tidy(W2_att_mod, conf.int = TRUE)

# pscore dist
bal.plot(W2, which = "both")

# for a single variable
bal.plot(W2,
         var.name = "medu",
         which = "both",
         type = "histogram")
