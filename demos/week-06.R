library(tidyverse)
library(gssr)
library(marginaleffects)

## mytidy
mytidy <- function(mod) {
  tidy(mod) |> 
    select(term, estimate, std.error)
}

# data
gss2022 <- gss_get_yr(2022)
d <- gss2022 |> 
  select(happy, degree, madeg, padeg, 
         age, wrkstat, marital, sex, realinc) |> 
  mutate(pardeg = pmax(madeg, padeg, na.rm = TRUE),
         parcol = if_else(pardeg >= 3, 1L, 0L),
         log2inc = log2(realinc),
         female = if_else(sex == 2, 1L, 0L),
         happy = case_when(
           happy == 3 ~  0,
           happy == 2 ~ .5,
           happy == 1 ~  1 ),
         vhappy = if_else(happy == 1, 1L, 0L)) |>
  select(happy, vhappy, female, degree, parcol, age, wrkstat, marital, realinc,
         log2inc) |>
  drop_na()

# linear models
lm1 <- lm(happy ~ log2inc + factor(degree) + parcol + age + I(age^2) +
             factor(wrkstat) + factor(marital),
           data = d)

lm2 <- lm(happy ~ log2inc * (factor(degree) + parcol + age + I(age^2) +
            factor(wrkstat) + factor(marital)),
          data = d)

anova(lm1, lm2) # lm2 isn't better but let's use it...

# mfx
avg_slopes(lm2,
           variables = "log2inc")

slopes(lm2,
       variables = "log2inc") |> 
  as_tibble() |> 
  ggplot(aes(x = estimate)) +
  geom_histogram(color = "white") +
  theme_light()

# logit models
glm1 <- glm(vhappy ~ log2inc + factor(degree) + parcol + age + I(age^2) +
            factor(wrkstat) + factor(marital),
           family = binomial,
           data = d)

glm2 <- lm(vhappy ~ log2inc * (factor(degree) + parcol + age + I(age^2) +
                                factor(wrkstat) + factor(marital)),
          family = binomial,
          data = d)

AIC(glm1, glm2) # isn't better but...

# mfx
avg_slopes(glm2,
           variables = "log2inc")

slopes(glm2,
       variables = "log2inc") |> 
  as_tibble() |> 
  ggplot(aes(x = estimate)) +
  geom_histogram(color = "white") +
  theme_light()
