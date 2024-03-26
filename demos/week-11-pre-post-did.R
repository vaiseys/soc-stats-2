library("tidyverse")
library("glmmTMB")
library("marginaleffects")
library("broom")
library("broom.mixed")
library("modelsummary")
theme_set(theme_light())

d <- haven::read_dta("data/cardkrueger1994.dta") |> 
  haven::zap_labels()


### pre-post ####
dnj <- d |> 
  filter(treated == 1)

# keep only treated
dnj |> 
  group_by(t) |> 
  summarize(mean_fte = mean(fte))

# make wide
dnj_wide <- dnj |> 
  pivot_wider(id_cols = id,
              names_from = t,
              names_prefix = "fte",
              values_from = fte)

# paired t-test (why tho)
t.test(Pair(fte0, fte1) ~ 1,
       data = dnj_wide)

# long form (wrong)
pre_post_lm <- lm(fte ~ t,
                  data = dnj)
tidy(pre_post_lm,
     conf.int = TRUE) |> 
  select(term, estimate, conf.low, conf.high)

# long form (right)
pre_post_mlm <- glmmTMB(fte ~ t + (1 | id),
                        data = dnj)
tidy(pre_post_mlm,
     conf.int = TRUE) |> 
  select(term, estimate, conf.low, conf.high)

# compare
msummary(list(pre_post_lm,
              pre_post_mlm),
         estimate = "{estimate} [{conf.low}, {conf.high}]")