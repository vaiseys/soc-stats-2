### prep ####
library(tidyverse)
library(broom)
library(cobalt)
library(MatchIt)
library(WeightIt)
library(here)

# custom love plot
love_plot <- function(x) {
  love.plot(x,
            binary = "std",
            stats = c("m", "ks"),
            thresholds = c(.1, .05))
}

### data prep ####
load(here("data", "cattaneo2.RData"))

# get rid of labels
d <- d |> haven::zap_labels()

# add a new variable
d <- d |> 
  mutate(mcollege = if_else(medu >= 16, 1L, 0L))

### prep for function ####
# just the list because I'm using ebal
treat_form <- "mbsmoke ~ mmarried + mhisp + foreign + alcohol +
                         deadkids + prenatal1 + mcollege + mrace +
                         mage + medu + nprenatal"

out_form <- "bweight ~ mbsmoke + mmarried + mhisp + foreign + alcohol +
                       deadkids + prenatal1 + mcollege + mrace +
                       mage + I(mage^2) + medu + I(medu^2) +
                       nprenatal + I(nprenatal^2)"

### full process ####
# weights
est_weights <- weightit(as.formula(treat_form),
                        data = d,
                        estimand = "ATT",
                        over = TRUE)

# regression
est_att <- lm(as.formula(out_form),
              data = d,
              weights = est_weights$weights)

# get ATT
tidy(est_att, conf.int = TRUE) |> 
  filter(term == "mbsmoke")

### make the function ####
get_att <- function() {
  
  # get bootstrap sample
  db <- slice_sample(d,
                     prop = 1,
                     replace = TRUE)
  
  # using ebal because it's doable with this dataset
  my_weights <- weightit(as.formula(treat_form),
                         data = db,
                         method = "ebal",
                         moments = 3,
                         estimand = "ATT")
  
  # regression with weights
  my_model <- lm(as.formula(out_form),
                 data = db,
                 weights = my_weights$weights)
  
  # keep the only number you need
  return(my_model$coefficients["mbsmoke"])
  
}

### do the bootstraps ####
set.seed(12345)

results <- tibble(
  rep = 1:250) |>  # should be 500-1000 for real
  rowwise() |> 
  mutate(att = get_att()) |> 
  ungroup()

### summarize ####
results |> 
  drop_na() |>                           # in case of non-convergence
  summarise(mean_att = mean(att),
            lower = quantile(att, .025),
            upper = quantile(att, .975),
            se = sd(att),
            runs = n())                  # count runs to identify non-converg. 
