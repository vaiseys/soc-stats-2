library(tidyverse)
library(glmmTMB)
library(here)
library(broom.mixed)
library(patchwork)
library(panelr)

data("WageData")
data("teen_poverty")
d <- long_panel(teen_poverty,
                id = "id",
                wave = "t",
                begin = 1,
                end = 5)

vcm <- glmmTMB(wks ~ 1 + (1 | id),
               data = WageData)
tidy(vcm)

beta <- vcm$sdr$par.fixed["beta"]
tau <- exp(vcm$sdr$par.fixed["theta"])
sigma <- sqrt(exp(vcm$sdr$par.fixed["betad"]))

L2_lo <- beta - 3*tau
L2_hi <- beta + 3*tau
L1_lo <- beta - 3*sigma
L1_hi <- beta + 3*sigma

xmax <- max(L2_hi, L1_hi)
xmin <- min(L2_lo, L1_lo)

p2 <- ggplot() +
  xlim(c(xmin, xmax)) +
  stat_function(fun = dnorm,
                args = list(mean = beta,
                            sd = tau),
                geom = "density",
                fill = "lightblue",
                color = NA) +
  theme_light() +
  theme(
    axis.title.y = element_blank(),  # Removes the y-axis title
    axis.text.y = element_blank()    # Removes the y-axis labels
  )

p1 <- ggplot() +
  xlim(c(xmin, xmax)) +
  stat_function(fun = dnorm,
                args = list(mean = beta,
                            sd = sigma),
                geom = "density",
                fill = "lightgreen",
                color = NA) +
  theme_light() +
  theme(
    axis.title.y = element_blank(),  # Removes the y-axis title
    axis.text.y = element_blank()    # Removes the y-axis labels
  )

p2 / p1
