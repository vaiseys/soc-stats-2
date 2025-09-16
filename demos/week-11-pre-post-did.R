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

# visualization
ggplot(dnj,
       aes(x = t,
           y = fte,
           group = id)) +
  geom_line(alpha = .3)

### diff-in-diff ####
# table
d |> 
  group_by(treated, t) |> 
  summarize(fte = mean(fte))

# graph
d |> 
  group_by(treated, t) |> 
  summarize(fte = mean(fte)) |> 
  ggplot(aes(x = t,
             y = fte,
             group = treated,
             color = factor(treated))) +
  geom_line() +
  ylim(c(15, 25))

# DiD
did_mlm <- glmmTMB(fte ~ treated + t + treated:t + (1 | id),
                   data = d)
tidy(did_mlm,
     conf.int = TRUE) |> 
  select(term, estimate, conf.low, conf.high)

# wide (why not)
d_wide <- d |> 
  pivot_wider(id_cols = c(id, treated),
              names_from = t,
              names_prefix = "fte",
              values_from = fte)

d_wide <- d_wide |> 
  mutate(diff = fte1 - fte0)

wide_did <- lm(diff ~ treated,
               data = d_wide)
tidy(wide_did,
     conf.int = TRUE)

# with "controls"
d <- d |> 
  mutate(chain = case_when(bk == 1 ~ "bk",
                           roys == 1 ~ "Roys",
                           wendys == 1 ~ "Wendys",
                           .default = "KFC" ))

did_mlm2 <- lme4::lmer(fte ~ treated + t + treated:t +
                         chain + (1 | id),
                       data = d,
                       REML = FALSE)
tidy(did_mlm2,
     conf.int = TRUE) |> 
  select(term, estimate, conf.low, conf.high)

