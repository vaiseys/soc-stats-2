library(tidyverse)
library(WeightIt)
library(cobalt)
library(broom)

d <- read_rds(here::here("data", "cattaneo2.rds")) |> 
  haven::zap_labels()

w1 <- weightit(mbsmoke ~ mage + medu + nprenatal + fbaby +
                 alcohol + mrace + mmarried +
                 I(mage^2) + I(medu^2) + I(nprenatal^2),
               estimand = "ATT",
               method = "glm",
               data = d)
summary(w1)

mod1 <- lm(bweight ~ mbsmoke,
           data = d,
           weight = w1$weights)
tidy(mod1)

psmod1 <- glm(mbsmoke ~ mage + medu + nprenatal + fbaby +
                alcohol + mrace + mmarried +
                I(mage^2) + I(medu^2) + I(nprenatal^2),
              data = d,
              family = binomial())

d$phat <- predict(psmod1, type = "response")
d$weights <- w1$weights

love.plot(w1,
          stats = "m",
          abs = TRUE,
          binary = "std",
          thresholds = .1)

### Covariate balancing propensity scores
cbpswt <- weightit(mbsmoke ~ mage + medu + nprenatal + fbaby +
                     alcohol + mrace + mmarried,
                   data = d,
                   estimand = "ATT",
                   method = "cbps")

love.plot(cbpswt,
          stats = "ks",
          abs = TRUE,
          binary = "std",
          thresholds = .1)

bal.plot(cbpswt,
         var.name = "mage",
         type = "histogram")


### Entropy balancing

entwt <- weightit(mbsmoke ~ mage + medu + nprenatal + fbaby +
                    alcohol + mrace + mmarried,
                  data = d,
                  estimand = "ATT",
                  method = "ebal",
                  moments = 3)

love.plot(entwt,
          stats = "ks",
          abs = TRUE,
          binary = "std",
          thresholds = .05)

