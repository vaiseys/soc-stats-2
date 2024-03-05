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

### bring in LaLonde ####
load(here("data", "exercise_data.RData"))

### prep for function ####
treat_form <- "treat ~ age + I(age^2) + 
                       educ + nodegr +
                       black + hisp + married + u74 + u75 +
                       re74 + re75"

out_form <- "re78 ~ treat + age + I(age^2) + 
                    educ + nodegr +
                    black + hisp + married + u74 + u75 +
                    re74 + re75"

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
  filter(term == "treat")

### make the function ####
get_att <- function() {
  
  # get bootstrap sample
  db <- slice_sample(d,
                     prop = 1,
                     replace = TRUE)
  
  # using simple glm-estimated weights for speed
  my_weights <- weightit(as.formula(treat_form),
                         data = db,
                         method = "glm",
                         estimand = "ATT")
  
  # regression with weights
  my_model <- lm(as.formula(out_form),
                 data = db,
                 weights = my_weights$weights)
  
  # keep the only number you need
  return(my_model$coefficients["treat"])
  
}

### do the bootstraps ####
results <- tibble(
  rep = 1:1000) |> 
  rowwise() |> 
  mutate(att = get_att()) |> 
  ungroup()

### summarize ####
results |> 
  summarise(mean_att = mean(att),
            lower = quantile(att, .025),
            upper = quantile(att, .975),
            se = sd(att))
