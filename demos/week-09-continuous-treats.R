### prep ####
library(tidyverse)
library(broom)
library(cobalt)
library(MatchIt)
library(WeightIt)
library(here)

### data prep ####
load(here("data", "cattaneo2.RData"))

# get rid of labels
d <- d |> haven::zap_labels()

# add a new variable
d <- d |> 
  mutate(mcollege = if_else(medu >= 16, 1L, 0L))

### naive ####
naive_est <- lm(bweight ~ msmoke,
                data = d)
tidy(naive_est,
     conf.int = TRUE)

### formulas ####

# NOTE: here we use MSMOKE not MBSMOKE

trt_form_ls <- as.formula("msmoke ~ 
                            mmarried + mhisp + foreign + alcohol +
                            deadkids + prenatal1 + mcollege + mrace +
                            mage + medu + nprenatal")

trt_form_ff <- as.formula("msmoke ~ 
                            mmarried + mhisp + foreign + alcohol +
                            deadkids + prenatal1 + mcollege + mrace +
                            mage + medu + nprenatal +
                            I(mage^2) + I(medu^2) + I(nprenatal^2)")

out_form <- as.formula("bweight ~ 
                         msmoke + mmarried + mhisp + foreign + 
                         alcohol + deadkids + prenatal1 + 
                         mcollege + mrace + mage + I(mage^2) + 
                         medu + I(medu^2) + 
                         nprenatal + I(nprenatal^2)")

### CBPS ####
cbps_weights <- weightit(trt_form_ff,
                         data = d,
                         method = "cbps",
                         estimand = "ate",
                         over = FALSE)

summary(cbps_weights)

love.plot(cbps_weights,
          thresholds = .1)

cbps_att_est <- lm(out_form,
                   data = d,
                   weights = cbps_weights$weights)
tidy(cbps_att_est,
     conf.int = TRUE) |> 
  filter(term == "msmoke")

### alternate outcome formula ####
out_form_2 <- as.formula("bweight ~ 
                           msmoke * (mmarried + mhisp + foreign + 
                           alcohol + deadkids + prenatal1 + 
                           mcollege + mrace + mage + I(mage^2) + 
                           medu + I(medu^2) + 
                           nprenatal + I(nprenatal^2))")

cbps_att_est_2 <- lm(out_form_2,
                     data = d,
                     weights = cbps_weights$weights)

library(marginaleffects)
avg_slopes(cbps_att_est_2,
           variables = "msmoke",
           wts = cbps_weights$weights,
           by = "msmoke")

