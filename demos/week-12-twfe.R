library("tidyverse")
library("panelr")
library("glmmTMB")
library("fixest")
library("broom")

data("teen_poverty")

d <- teen_poverty |> 
  long_panel(id = "id",
             wave = "t",
             begin = 1,
             end = 5)

line_plot(d,
          hours,
          id = "id",
          wave = "t",
          subset.ids = TRUE,
          n.random.subset = 5)

femod1 <- fixest::feols(pov ~ mother | id + t,
                        data = d)
tidy(femod1)

femod2 <- feols(pov ~ mother | id + t,
                data = d)


data(WageData)


wagemod1 <- feols(lwage ~ union + occ + ind + south + smsa + ms | id + t,
                  data = WageData)
tidy(wagemod1)

dummymod <- lm(lwage ~ union + occ + ind + south + smsa + ms +
                 factor(id) + factor(t),
               data = WageData)

tidy(dummymod) |> 
  filter(term == "union")

library(ggeffects)
ggpredict(dummymod,
          terms = "t") |> plot()

line_plot(WageData,
          lwage,
          id = "id",
          wave = "t",
          subset.ids = TRUE,
          n.random.subset = 10)

mm1 <- glmmTMB(lwage ~ 1 + t + (1 | id),
               data = WageData)
ggpredict(mm1,
          terms = c("t", "id [sample = 9]"),
          type = "random",
          ci_level = NA) |> plot()


mm2 <- glmmTMB(lwage ~ 1 + t + (1 + t | id),
               data = WageData)

ggpredict(mm2,
          terms = c("t", "id [sample = 9]"),
          type = "random",
          ci_level = NA) |> plot()

BIC(mm1, mm2)



