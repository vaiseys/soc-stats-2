library(tidyverse)
library(gssr)
library(marginaleffects)
library(broom)

## mytidy
mytidy <- function(mod) {
  tidy(mod) |> 
    select(term, estimate, std.error)
}

## expanding to more adjustment variables and nonlinear models ##

# data
gss2022 <- gss_get_yr(2022)

### Poisson (with response) ####
d <- gss2022 |>
  filter(wrkstat == 1) |> # full time workers
  select(realrinc, degree, madeg, padeg, sex, age) |> 
  mutate(pardeg = pmax(madeg, padeg, na.rm = TRUE),
         college = if_else(degree >= 3, 1L, 0L),
         parcol = if_else(pardeg >= 3, 1L, 0L),
         female = if_else(sex == 2, 1L, 0L),
         realrinc = floor(realrinc)) |>             # integer
  select(realrinc, college, parcol, female, age) |> 
  drop_na()

# model with heterogeneity
qp1 <- glm(realrinc ~ college * (parcol + female + age + I(age^2)), 
           data = d,
           family = "quasipoisson")
mytidy(qp1) # spend time interpreting this!!!

# alternative (negative binomial vs. Poisson)
nb1 <- MASS::glm.nb(realrinc ~ college * (parcol + female + age + I(age^2)),
                    data = d)
mytidy(nb1)

# ATE estimate
avg_slopes(qp1,
           variables = "college",
           type = "response") |> 
  tidy()

# ATT/ATU estimate
avg_slopes(qp1,
           variables = "college",
           type = "response",
           by = "college") |> # separately by treatment group
  tidy()

# are ATT/ATU different?
avg_slopes(qp1,
           variables = "college",
           type = "response",
           by = "college",
           hypothesis = "pairwise") |>
  tidy()

### Poisson (with link as "real" outcome) ####
# ATE estimate
avg_slopes(qp1,
           variables = "college",
           type = "link") |> 
  tidy()

# ATT/ATU estimate
avg_slopes(qp1,
           variables = "college",
           type = "link",
           by = "college") |> # separately by treatment group
  tidy()

# are ATT/ATU different?
avg_slopes(qp1,
           variables = "college",
           type = "link",
           by = "college",
           hypothesis = "pairwise") |>
  tidy()

### Logistic ####
d <- gss2022 |>
  select(abany, degree, madeg, padeg, sex, age) |> 
  mutate(pardeg = pmax(madeg, padeg, na.rm = TRUE),
         college = if_else(degree >= 3, 1L, 0L),
         parcol = if_else(pardeg >= 3, 1L, 0L),
         female = if_else(sex == 2, 1L, 0L),
         abany = if_else(abany == 1, 1L, 0L)) |>
  select(abany, college, parcol, female, age) |> 
  drop_na()

# model
m1 <- glm(abany ~ college * (parcol + female + age + I(age^2)),
          data = d,
          family = binomial)

# ATE estimate
avg_slopes(m1,
           variables = "college",
           type = "response") |> 
  tidy()

# ATT/ATU estimate
avg_slopes(m1,
           variables = "college",
           by = "college",
           type = "response") |> 
  tidy()

# male/female TEs
avg_slopes(m1,
           variables = "college",
           by = "female",
           type = "response") |> 
  tidy()

# contrast?
avg_slopes(m1,
           variables = "college",
           by = "female",
           type = "response",
           hypothesis = "pairwise") |> 
  tidy()