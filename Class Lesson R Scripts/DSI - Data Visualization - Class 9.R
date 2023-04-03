#CODE FOR CLASS 9
#Coded by Sally Choi
#Date: 01-Apr-2023

#install and load packages
install.packages("rAmCharts")
library(rAmCharts)

#view our dataset
head(iris)

#make a dynamic data viz
amPlot(iris, col=c("Sepal.Length", "Sepal.Width"),
                   type = c("line", "step"),
                   zoom = TRUE,
                   legend = TRUE)

#load packages for gganimate
library(ggplot2)
library(gapminder)
install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)

#make standard ggplot that we'll animate
p <- ggplot(gapminder, aes(x = gdpPercap, 
                           y = lifeExp, 
                           size = pop,
                           color = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2,12)) +
  scale_x_log10() +
  labs(x = "GDP per capita",
       y = "Life Expectancy")
p

#animate our plot
p2 <- p + transition_time(year) +
  labs(title = "Year: {frame_time}")
animate(p2,
        duration = 10,
        renderer = gifski_renderer())

#install.packages("shiny")
