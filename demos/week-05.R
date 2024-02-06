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

# # alternative (negative binomial vs. Poisson) [discussed but not used]
# nb1 <- MASS::glm.nb(realrinc ~ college * (parcol + female + age + I(age^2)),
#                     data = d)
# mytidy(nb1)

# ATE estimate
avg_slopes(qp1,
           variables = "college",
           type = "response") |> 
  tidy()

# ATE estimate rowwise
mfx <- slopes(qp1,
              variables = "college",
              type = "response")
View(select(mfx, estimate, female, parcol, age))

# DIGRESSION: why are these numbers different for different people?
## linear model with no interactions 
## effects are the same because ADDITIVE LINK and NO INTERACTIONS
lm1 <- lm(realrinc ~ college + parcol + female + age + I(age^2), 
          data = d)
mytidy(lm1)

mfx_lm1 <- slopes(lm1,
                  variables = "college")
View(select(mfx_lm1, estimate, female, parcol, age))

## linear model with interactions
## effects differ because of INTERACTIONS
lm2 <- lm(realrinc ~ college * (parcol + female + age + I(age^2)), 
          data = d)
mytidy(lm2)

mfx_lm2 <- slopes(lm2,
                  variables = "college")
View(select(mfx_lm2, estimate, female, parcol, age))

## log-link model with no interactions
## effect differ because the log link is MULTIPLICATIVE
p1 <- glm(realrinc ~ college + parcol + female + age + I(age^2), 
          data = d,
          family = "quasipoisson")
mytidy(p1) # spend time interpreting this!!!

mfx_p1 <- slopes(p1,
                 variables = "college")
View(select(mfx_p1, estimate, female, parcol, age))

## the qp1 model has different estimates because of BOTH MULT. and INTERACTIONS

## now back to the main story...
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