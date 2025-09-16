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

### CBPS ####

# overidentified version (see Imai and Ratkovic 2014)
CBPS1 <-  weightit(as.formula(trt_form_ff),
                   method = "cbps",
                   estimand = "ATT",
                   data = d)

summary(CBPS1)
love_plot(CBPS1)

# not overidentified version
CBPS2 <-  weightit(as.formula(trt_form_ff),
                   method = "cbps",
                   estimand = "ATT",
                   over = FALSE,
                   data = d)
summary(CBPS2)
love_plot(CBPS2)

### change example to LaLonde ####
load(here("data", "exercise_data.RData"))

# experimental estimate
exp_ate <- lm(re78 ~ treat,
             data = d_exper)

tidy(exp_ate, conf.int = TRUE)

# define formula
library(splines)
ll_treat_form_ff <- "treat ~ ns(age, df = 3) + 
                     ns(educ, knots = c(8, 12, 14)) +
                     black + hisp + married + u74 + u75 +
                     ns(re74, df = 3) +
                     ns(re75, df = 3)"

# GLM with splines
IPW1 <- weightit(as.formula(ll_treat_form_ff),
                 data = d,
                 estimand = "ATT",
                 method = "glm")
summary(IPW1)
bal.plot(IPW1,
         which = "both")

love_plot(IPW1)

# GLM with no splines
ll_treat_form_ff2 <- "treat ~ age + I(age^2) + 
                      educ + nodegr +
                      black + hisp + married + u74 + u75 +
                      re74 + re75"

IPW2 <- weightit(as.formula(ll_treat_form_ff2),
                 data = d,
                 estimand = "ATT",
                 method = "glm")
summary(IPW2)
bal.plot(IPW2,
         which = "both")

love_plot(IPW2)

### CBPS ####
ll_treat_form_ff2 <- "treat ~ age + I(age^2) + 
                      educ + nodegr +
                      black + hisp + married + u74 + u75 +
                      re74 + re75"

CBPS1 <- weightit(as.formula(ll_treat_form_ff2),
                  data = d,
                  estimand = "ATT",
                  method = "cbps")
summary(CBPS1)
bal.plot(CBPS1,
         which = "both")

love_plot(CBPS1)
bal.plot(CBPS1,
         var.name = "age",
         which = "both")

cbps_att <- lm(re78 ~ treat,
               data = d,
               weights = CBPS1$weights)
tidy(cbps_att, conf.int = TRUE)


### comparison with regression ####

### THEEEEEE effect of the job training program
ll_out_form_ff <- "re78 ~ treat + age + I(age^2) + 
                   educ + nodegr +
                   black + hisp + married + u74 + u75 +
                   re74 + re75"
reg_ate1 <- lm(as.formula(ll_out_form_ff),
               data = d)
tidy(reg_ate1, conf.int = TRUE)

### can the effect be different?
ll_out_form_ff2 <- "re78 ~ treat * (age + I(age^2) + 
                   educ + nodegr +
                   black + hisp + married + u74 + u75 +
                   re74 + re75)"
reg_ate2 <- lm(as.formula(ll_out_form_ff2),
               data = d)
tidy(reg_ate2, conf.int = TRUE)

### compare the models
performance::compare_performance(list(reg_ate1,
                                      reg_ate2))
anova(reg_ate1, reg_ate2, test = "Chisq")

### marginal effects
library(marginaleffects)
avg_slopes(reg_ate2,
           variables = "treat",
           by = "treat")

### Entropy Balancing ####
ll_treat_form_list <- "treat ~ age + 
                       educ + nodegr +
                       black + hisp + married + u74 + u75 +
                       re74 + re75"

ebal1 <- weightit(as.formula(ll_treat_form_list),
                  data = d,
                  estimand = "ATT",
                  method = "ebal",
                  moments = 3,
                  maxit = 1e6)
summary(ebal1)
love_plot(ebal1)

bal.plot(ebal1,
         var.name = "age",
         which = "both")

ebal_att <- lm(re78 ~ treat,
               data = d,
               weight = ebal1$weights)
tidy(ebal_att, conf.int = TRUE)