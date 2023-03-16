library(tidyverse)
library(socviz)
library(ggplot2)

#Preparing our data for visualizing
#Data should be in long format rather than wide format. Every observation 
#should be a row. Every variable should be a column.

#Make a basic figure
install.packages("gapminder")
library(gapminder)

gapminder

p <-  ggplot(data = gapminder,
             mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

#the good - mostly legible, has axis, shows some kind of relationship between variables
#the not-so-good - font size is tiny, not visually interesting or pleasing, what are we trying to show?