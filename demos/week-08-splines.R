library(tidyverse)
library(splines)
library(gapminder)

data(gapminder)

gap2 <- gapminder |> 
  filter(year == 2007)

gapscatter <- ggplot(gap2,
                     aes(x = gdpPercap,
                         y = lifeExp)) +
  geom_point() +
  theme_light()

gapscatter

# linear
gapscatter +
  geom_smooth(method = "lm",
              formula = "y ~ x")

# quadratic
gapscatter +
  geom_smooth(method = "lm",
              formula = "y ~ x + I(x^2)")

# spline (experiment with df = from 2 to 5)
gapscatter +
  geom_smooth(method = "lm",
              formula = "y ~ ns(x, df = 5)")

# regression with spline
spline_lm <- lm(lifeExp ~ ns(gdpPercap, df = 4),
                data = gap2)

summary(spline_lm)

# model selection
s2 <- lm(lifeExp ~ ns(gdpPercap, df = 2),
         data = gap2)
s3 <- lm(lifeExp ~ ns(gdpPercap, df = 3),
         data = gap2)
s4 <- lm(lifeExp ~ ns(gdpPercap, df = 4),
         data = gap2)
s5 <- lm(lifeExp ~ ns(gdpPercap, df = 5),
         data = gap2)
s6 <- lm(lifeExp ~ ns(gdpPercap, df = 6),
         data = gap2)

AIC(s2, s3, s4, s5, s6)
