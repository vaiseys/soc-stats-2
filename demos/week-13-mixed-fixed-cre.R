pacman::p_load(tidyverse,
               panelr,
               fixest,
               glmmTMB,
               broom,
               broom.mixed,
               gssr,
               marginaleffects)

# data prep
data("gss_panel10_long")

d <- gss_panel10_long |>
  haven::zap_labels() |> 
  select(firstid, wave, abany, attend, sex, age) |>
  drop_na() |>
  mutate(nobs = n(), .by = firstid) |> 
  filter(nobs == 3) |>
  mutate(abany = if_else(abany == 1, 1L, 0L),
         attend01 = scales::rescale(attend),
         age01 = scales::rescale(age),
         female = if_else(sex == 2, 1L, 0L),
         wave = factor(wave)) |> 
  mutate(m_attend01 = mean(attend01), .by = firstid)


# mixed
re_fit <- glmmTMB(abany ~ attend01 + age01 + I(age01^2) +
                    female + wave + (1 | firstid),
                  data = d)
tidy(re_fit) |>
  filter(term == "attend01") |> 
  select(term:p.value)

# fixed
fe_fit <- feols(abany ~ attend01 + age01 + I(age01^2) | 
                  firstid + wave,
                data = d)
tidy(fe_fit) |> 
  filter(term == "attend01") |> 
  select(term:p.value)

# how much variance in attend is within?
vcm <- glmmTMB(attend01 ~ (1 | firstid),
               data = d)
performance::icc(vcm)

line_plot(d,
          id = "firstid",
          wave = "wave",
          attend01,
          subset.ids = TRUE,
          n.random.subset = 5)

# Correlated Random Effect Model
cre_fit <- glmmTMB(abany ~ attend01 + m_attend01 +
                     age01 + I(age01^2) +
                     female + wave + (1 | firstid),
                   data = d)
tidy(cre_fit) |>
  filter(term == "attend01" | term == "m_attend01") |> 
  select(term:p.value)

# adding random coefficients
cre_fit2 <- glmmTMB(abany ~ attend01 + m_attend01 +
                      age01 + I(age01^2) +
                      female + wave + (1 + attend01 | firstid),
                    data = d)
summary(cre_fit2)

BIC(cre_fit,
    cre_fit2)




