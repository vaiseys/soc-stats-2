library(tidyverse)

### Exercise Review ####

d <- tibble(
  id = LETTERS[1:8],
  T = c(0, 0, 1, 0, 0, 1, 1, 1),
  Y0 = c(5, 8, 5, 12, 4, 8, 4, 9),
  Y1 = c(5, 10, 3, 13, 2, 9, 1, 13) 
)

d

d <- d |> 
  mutate(Y = T*Y1 + (1-T)*Y0)

d

mean(d$Y[d$T==1]) - mean(d$Y[d$T==0])

d |> 
  group_by(T) |> 
  summarize(mean_Y = mean(Y)) |> 
  pivot_wider(values_from = mean_Y,
              names_from = T,
              names_prefix = "Y") |> 
  mutate(delta = Y1-Y0)

### next part

d2 <- d |> 
  mutate(T = sample(d$T))
mean(d2$Y[d2$T==1]) - mean(d2$Y[d2$T==0])

myexperiment <- function(x) {
  d2 <- d |> 
    mutate(T = sample(d$T))
  mean(d2$Y[d2$T==1]) - mean(d2$Y[d2$T==0])
}

experiments <- tibble(
  simnumber = 1:500) |> 
  rowwise() |> 
  mutate(estimate = myexperiment())

ggplot(experiments,
       aes(x = estimate)) +
  geom_histogram(binwidth = .5)

mean(experiments$estimate)

## next part

pop <- tibble(
  y0 = rpois(100000,     2),
  y1 = rpois(100000, 2.125)) # .125 is minimum interesting effect

mean(pop$y1 - pop$y0) # true ATE

do_experiment <- function(n = 100) {

  experiment1 <- pop |> 
    slice_sample(n = n) |>
    rowwise() |> 
    mutate(treat = rbinom(1, 1, .5),
           y = treat*y1 + (1-treat)*y0)
  
  mean_treat <- mean(experiment1$y[experiment1$treat==1])
  mean_control <- mean(experiment1$y[experiment1$treat==0])
  est_delta <- mean_treat - mean_control
  
  return(est_delta)

}

xsims <- tibble(
  sim = 1:1000) |> 
  rowwise() |> 
  mutate(est_ate = do_experiment(n = 100))


### get p-value ####

get_pvalue <- function(n = 100) {
  
  experiment1 <- pop |> 
    slice_sample(n = n) |>
    rowwise() |> 
    mutate(treat = rbinom(1, 1, .5),
           y = treat*y1 + (1-treat)*y0)
  
  tt <- t.test(y ~ factor(treat), data = experiment1)
  return(tt$p.value)
  
}

### kludgy power analysis

psims <- tibble(
  sim = 1:500) |> 
  rowwise() |> 
  mutate(p = get_pvalue(n = 16000),
         reject = if_else(p < .05, 1L, 0L))

mean(psims$reject)


### formula based
# assume SD of exercise is 1.43ish so .125 is .087 SDs

power.t.test(n = NULL, 
             delta = .05, 
             sig.level = .01, 
             type = "two.sample",
             power = .8,
             alternative = "two.sided")