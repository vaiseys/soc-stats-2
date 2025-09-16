library(tidyverse)

load(here::here("data", "cattaneo2.Rdata"))

d <- d |> 
  haven::zap_labels()

ggplot(d,
       aes(x = mage,
           y = mbsmoke)) +
  geom_smooth(method = "loess") +
  theme_light()

psmod <- glm(mbsmoke ~ mmarried + mrace + fbaby + alcohol +
               mage + nprenatal + medu,
             family = "binomial",
             data = d)

summary(psmod)

dreal <- d |> 
  select(mmarried, mrace, fbaby, alcohol, mage, nprenatal, medu)

dreal$logodds <- predict(psmod, 
                         newdata = dreal, 
                         type = "link")

d0 <- dreal
d0$mmarried <- 0
d0$logodds0 <- predict(psmod,
                       newdata = d0,
                       type = "link")

d0$mmarried <- 1
d0$logodds1 <- predict(psmod,
                       newdata = d0,
                       type = "link")

d0$p0 <- exp(d0$logodds0) / (1 + exp(d0$logodds0))
d0$p1 <- exp(d0$logodds1) / (1 + exp(d0$logodds1))

d0$me <- d0$p1 - d0$p0

ggplot(d0,
       aes(x = me)) +
  geom_histogram(binwidth = .01,
                 boundary = .1,
                 color = "white") +
  theme_light()

d0$mmarried <- dreal$mmarried

mean(d0$me)

marginaleffects::avg_slopes(psmod, variables = "mmarried")

library(performance)

check_model(psmod, verbose = TRUE)

performance_hosmer(psmod)

dreal$mbsmoke <- d$mbsmoke

ggplot(dreal,
       aes(x = logodds,
           group = factor(mbsmoke),
           fill = factor(mbsmoke))) +
  geom_density(alpha = .4,
               color = "white") +
  theme_light()

myinsane <- dreal |>
  mutate(prob = exp(logodds) / (1 + exp(logodds))) |> 
  arrange(-logodds) |>
  slice_head(prop = .1) |> 
  summarize(estprob = mean(prob),
            actprop = mean(mbsmoke))
myinsane




