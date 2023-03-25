#CODE FOR CLASS 6
#Coded by Sally Choi
#Date: 25-Mar-2023

#load libraries
library(tidyverse)
library(socviz)
library(ggplot2)
library(gapminder)
library(ggrepel)

#build colour palette
model_colours <- RColorBrewer::brewer.pal(3,"Set1")
model_colours

#plot our different models
p0 <- ggplot(data = gapminder, mapping = aes(x = log(gdpPercap), y = lifeExp))
p1 <- p0 + geom_point(alpha=0.2) +
  geom_smooth(method = "lm", aes(color = "OLS", fill = "OLS")) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3),
              aes(color = "Cubic Spline", fill = "Cubic Spline")) +
  geom_smooth(method = "loess", aes(color = "LOESS", fill = "LOESS"))

#make legend pretty
p1 + scale_color_manual(name="Models", values=model_colours) +
  scale_fill_manual(name = "Models", values=model_colours) +
  theme(legend.position = "top")
p1

#view dataset and its structure
gapminder
str(gapminder)

#make our model object
out <- lm(formula = lifeExp ~ gdpPercap + pop + continent, data = gapminder)

str(out)
summary(out)

#make dataframe for predictions
min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)
med_pop <- median(gapminder$pop)
pred_df <- expand.grid(gdpPercap = seq(from = min_gdp, to = max_gdp, length.out = 100),
                       pop = med_pop,
                       continent = c("Africa", "Americas", "Asia", "Europe", "Oceania"))

#view dimensions
dim(pred_df)

#make predictions
pred_out <- predict(object = out, newdata = pred_df, interval = "predict")
head(pred_out)

#bind data and predictions
pred_df <- cbind(pred_df, pred_out)
head(pred_df)

#how does percap gdp affect life expectancy in Europe and in Africa?
p <- ggplot(data=subset(pred_df, continent %in% c("Europe", "Africa")),
            aes(x = gdpPercap, y = fit, ymin = lwr, ymax = upr, color = continent, fill = continent, group = continent))
p + geom_point(data=subset(gapminder, continent %in% c("Europe", "Africa")),
               aes(x = gdpPercap, y = lifeExp),
               alpha = 0.5,
               inherit.aes = FALSE) +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_x_log10(labels = scales::dollar)

#activity: how does the population affect life expectancy in the Americas and Asia?
min_pop <- min(gapminder$pop)
max_pop <- max(gapminder$pop)
med_gdpPercap <- median(gapminder$gdpPercap)
pred_df2 <- expand.grid(pop = seq(from = min_pop, to = max_pop, length.out = 10000),
                       gdpPercap = med_gdpPercap,
                       continent = c("Africa", "Americas", "Asia", "Europe", "Oceania"))

pred_out2 <- predict(object = out, newdata = pred_df2, interval = "predict")

pred_df2 <- cbind(pred_df2, pred_out2)

p <- ggplot(data = subset(pred_df2, continent %in% c("Americas", "Asia")),
            aes(x = pop, y = fit, ymin = lwr, ymax = upr, color = continent, fill = continent, group = continent))
p + geom_point(data=subset(gapminder, continent %in% c("Americas", "Asia")),
               aes(x = pop, y = lifeExp),
               alpha = 0.5,
               inherit.aes = FALSE) +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE)

#install broom
install.packages("broom")
library(broom)

#generate component level info on our model
out_conf <- tidy(out, conf.int = TRUE)
out_conf |>
  round_df()

#plot our results
p <- ggplot(out_conf, mapping = aes(x = term, y = estimate))
p + geom_point() +
  coord_flip()

#refine our data
out_conf <- subset(out_conf, term %nin% "(Intercept)")
out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")

#plot our new data
p <- ggplot(out_conf,
            mapping = aes(x = reorder(nicelabs, estimate),
                          y = estimate,
                          ymin = conf.low,
                          ymax = conf.high))
p + geom_pointrange() +
  coord_flip() +
  labs(x = "",
       y = "OLS Estimate")

#explore individual observations
out_aug <- augment(out)
head(out_aug) |>
  round_df()

#plot augmented data
p <- ggplot(data = out_aug,
            mapping = aes(x = .fitted, y = .resid))
p + geom_point()

#model level stats
glance(out) |>
  round_df()

#explore subset of data
eu77 <- gapminder |>
  filter(continent == "Europe", year == 1977)
fit <- lm(lifeExp ~log(gdpPercap), data = eu77)
summary(fit)

#nest!!
out_le <- gapminder |>
  group_by(continent, year) |>
  nest()
out_le

#unnest
out_le |>
  filter(continent == "Europe" & year == 1977) |>
  unnest()

#make our function and iterate
fit_ols <- function(df){
  lm(lifeExp ~ log(gdpPercap), data = df)
}
out_le <- gapminder |>
  group_by(continent, year) |>
  nest() |>
  mutate(model = map(data, fit_ols))
out_le

#tidy our models
fit_ols <- function(df){
  lm(lifeExp ~ log(gdpPercap), data = df)
}
out_tidy <- gapminder |>
  group_by(continent, year) |>
  nest() |>
  mutate(model = map(data, fit_ols), tidied = map(model, tidy)) |>
  unnest(tidied) |>
  filter(term %nin% "(Intercept)" & continent %nin% "Oceania")
out_tidy

#plot our model results
p <- ggplot(data = out_tidy,
            mapping = aes(x = year,
                          y = estimate,
                          ymin = estimate - 2*std.error,
                          ymax = estimate + 2*std.error,
                          group = continent,
                          color = continent))
p + geom_pointrange(position = position_dodge(width=1)) +
  scale_x_continuous(breaks = unique(gapminder$year)) +
  theme(legend.position = "top") +
  labs(x = "Year",
       y = "Estimate",
       color = "Continent")
