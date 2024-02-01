library(tidyverse)
library(broom)
library(dagitty)

set.seed(123)
sampsize <- 100000  # big enough to avoid much samp. variation

# functions to save typing
## bernoulli
rbern <- function(n, p = .5) {
  rbinom(n = n,
         size = 1,
         prob = p)
}

## mytidy
mytidy <- function(mod) {
  tidy(mod) |> 
    select(term, estimate, std.error)
}

### EXAMPLE 1 ####

# dag
dagitty("dag{
        U -> S ;
        U -> X ;
        S -> T ;
        X -> Y ;
        T -> Y}") |> plot()

# simulated data
d <- tibble(
  U = rbern(sampsize, .5),
  S = rbern(sampsize, U*.25 + (1-U)*.75),
  X = rbern(sampsize, U*.25 + (1-U)*.75),
  T = rbern(sampsize, S*.75 + (1-S)*.25),
  Y = 1*T + 3*X + rnorm(sampsize, 0, 1)
)

# viz
ggplot(d,
       aes(x = Y)) +
  geom_histogram(color = "white") +
  theme_light()

# models
## no adjustment
m0 <- lm(Y ~ T,
         data = d)
mytidy(m0)

## proper adjustment
m1 <- lm(Y ~ T + X,
         data = d)
mytidy(m1)

## suboptimal adjustment (note larger standard error!)
m2 <- lm(Y ~ T + S,
         data = d)
mytidy(m2)

## overkill (for identification)
m3 <- lm(Y ~ T + S + X,
         data = d)
mytidy(m3)

### EXAMPLE 2 ####
# dag
dagitty("dag{
        U -> S ;
        U -> X ;
        S -> T ;
        X -> Y ;
        T -> Z ;
        Z -> Y ;
        T -> Y}") |> plot()

# simulated data
d <- tibble(
  U = rbern(sampsize, .5),
  S = rbern(sampsize, U*.25 + (1-U)*.75),
  X = rbern(sampsize, U*.25 + (1-U)*.75),
  T = rbern(sampsize, S*.75 + (1-S)*.25),
  Z = .5*T + rnorm(sampsize, 0, 1),
  Y = 1*T + 3*X + .5*Z + rnorm(sampsize, 0, 1)
)

# models
m0 <- lm(Y ~ T, data = d)
mytidy(m0)

m1 <- lm(Y ~ T + X, data = d)
mytidy(m1)

m2 <- lm(Y ~ T + X + Z, data = d)
mytidy(m2)

### MORE STUFF ####

# conditioning on a collider
dcc <- tibble(
  x = rnorm(10000, 0, 1),
  y = rnorm(10000, 0, 1),
  log_odds_c = x + y,
  prob_c = exp(log_odds_c) / (1 + exp(log_odds_c)),
  c = rbern(10000, prob_c)
)

dcc_yes <- dcc |> 
  filter(c == 1)

ccmod <- lm(y ~ x,
            data = dcc_yes)
mytidy(ccmod)

### andres version
ccmod2 <- lm(y ~ x + c,
             data = dcc)
mytidy(ccmod2)


# marginal effects
library(gssr)
library(marginaleffects)

# data
gss2022 <- gss_get_yr(2022)
d <- gss2022 |> 
  select(tvhours, degree, madeg, padeg) |> 
  mutate(pardeg = pmax(madeg, padeg, na.rm = TRUE),
         college = if_else(degree >= 3, 1L, 0L),
         parcol = if_else(pardeg >= 3, 1L, 0L)) |>
  select(tvhours, college, parcol) |> 
  drop_na()

# model with heterogeneity
mod1 <- lm(tvhours ~ college * parcol, 
           data = d)
mytidy(mod1) # spend time interpreting this!!!

# ATE estimate
avg_slopes(mod1,
           variables = "college") |> 
  tidy()

# ATT/ATU estimate
avg_slopes(mod1,
           variables = "college",
           by = "college") |> # separately by treatment group
  tidy()

