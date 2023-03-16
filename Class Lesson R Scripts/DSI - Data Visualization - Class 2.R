##CODE FOR CLASS 2
##Coded by Sally Choi
##Date: 03-Mar-2023

#load libraries
library(tidyverse)
library(ggplot2)
library(socviz)
library(gapminder)

#define data source and x and y axes for ggplot and assign to an object
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))

#specify graph to be a scatterplot
p + geom_point()

#specify graph to be a line plot with error spread
#note to self: by default uses a generalized additive model
p + geom_smooth() 

#add a method 'lm' for linear model of line plot
p + geom_smooth(method = "lm")

#overlay scatterplot with line plot
p + geom_point() + geom_smooth(method = "lm") 

#change scale of x axis by log of 10
#note to self: need to put + at end of line, not beginning
p + geom_point() + geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar)

#add colour to the mapping to show plots by continent
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp,
                          colour = continent))
p + geom_smooth(method = "lm") + geom_point() +
  scale_x_log10(labels = scales::dollar)

#change colour to purple in scatterplot
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_smooth(method = "lm") + geom_point(colour = "purple") +
  scale_x_log10(labels = scales::dollar)

#add opacity to scatterplot
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_smooth(method = "lm") + geom_point(alpha = 0.3) +
  scale_x_log10(labels = scales::dollar)

#explore a heatmap in ggplot - conclusion: maybe not the most useful plot for data
p <- ggplot(data = gapminder, 
       mapping = aes(x = continent, y = country, fill = lifeExp))
p +  geom_tile()
