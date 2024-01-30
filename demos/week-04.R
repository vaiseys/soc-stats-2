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
  S = rbern(sampsize, .25 + .50*U),
  X = rbern(sampsize, .25 + .50*U),
  T = rbern(sampsize, .25 + .50*S),
  Y = 1*T + 1*X + rnorm(sampsize, 0, 1)
)

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


### EXAMPLE 2 ####

