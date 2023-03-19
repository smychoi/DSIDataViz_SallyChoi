#CODE FOR CLASS 3
#Coded by Sally Choi
#Date: 18-Mar-2023

#load libraries
library(tidyverse)
library(ggplot2)
library(socviz)
library(gapminder)

#attempt to plot the trajectory of GDP over time for each country in the dataset
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap, color = country))
p + geom_point()

#try a graph, add facet according to variable continent
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))
p + geom_line(aes(group = country)) + facet_wrap(~continent)

#new and improved graph
ggplot(data=gapminder,mapping=aes(x = year, y = gdpPercap))
p + geom_line(color = "gray70", aes(group = country)) +
  geom_smooth(size=1.1,method= "loess", se=FALSE) +
  scale_y_log10(labels=scales::dollar) +
  facet_wrap(~continent,ncol=5) +
  labs(x = "Year",
       y = "GDP per capita",
       Title = "GDP per capita on Five Continents")



#look at new dataset
gss_sm

#plot with facet_grid
p <- ggplot(data = gss_sm, mapping = aes(x=age, y=childs))
p + geom_point(alpha = 0.2) + geom_smooth() + facet_grid(sex ~ race)

#exploring stat functions
p <- ggplot(data = gss_sm, mapping = aes(x=bigregion))
p + geom_bar()

#using non-default stat
p <- ggplot(data = gss_sm, mapping = aes(x=bigregion))
p + geom_bar(mapping = aes(y = ..prop.., group = 1)) #group - uses whole dataset 
                                                    #to establish denominator for ..prop..

#compare proportions across religious groups
p <- ggplot(data = gss_sm, mapping = aes(x=bigregion, fill=religion))
p + geom_bar(position = "fill")

#add faceting!
p <- ggplot(data = gss_sm, mapping = aes(x=religion))
p + geom_bar(position = "dodge", mapping = aes(y = ..prop.., group = bigregion)) +
  facet_wrap(~bigregion, ncol = 1)

#histogram
p <- ggplot(data = midwest, mapping = aes(x=area))
p + geom_histogram(bins = 10) 
#bins - how many chunks putting data into for a continuous variable
#e.g., if variable is years, bin = 10 group into every 10 years

#comparing histograms
oh_wi <- c("OH", "WI")
p <- ggplot(data = subset(midwest, subset = state %in% oh_wi), 
            mapping = aes(x = percollege, fill = state))
p + geom_histogram(alpha = 0.4, bins = 20)



#look at dataset
midwest

#density plot
p <- ggplot(data = midwest, mapping = aes(x=area))
p + geom_density()

#add subsetting to density plot
oh_wi <- c("OH", "WI")
p <- ggplot(data = subset(midwest, subset = state %in% oh_wi),
            mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.3)



#look at organdata dataset
organdata

#piping 
organdata |>
  select(1:6) |>
  sample_n(size=5)

#make line graph to explore each country’s time series with grouping and faceting
p <- ggplot(data = organdata, mapping = aes(x = year, y = donors))
p + geom_line(aes(group = country)) + facet_wrap(~country)

#make a boxplot
p <- ggplot(data = organdata, mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                                            y = donors))
#can use reorder to make the plot easier to read - can list countries from high to low mean donation
p + geom_boxplot() + coord_flip() 
#coord_flip switches axes to make it easier to read labels if too many


#make a scatterplot to show individual observations and map 'world' variable to colour
p <- ggplot(data = organdata, mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                                            y = donors, colour = world))
p + geom_point() + coord_flip()

#use geom_jitter to see points better when you have overlapping points
#good for exploring - sacrifices accuracy for completeness
p <- ggplot(data = organdata, mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                                            y = donors, colour = world))
p + geom_jitter() + coord_flip()


#process data with a pipeline
by_country <- organdata |> 
  group_by(consent_law, country) |>
  summarize_if(is.numeric, funs(mean, sd), na.rm=TRUE) |>
  ungroup()

#make a cleveland dotplot
p <- ggplot(by_country, mapping = aes(x=donors_mean,
                                      y=reorder(country, donors_mean),
                                      color = consent_law))
p + geom_point(size=3) +
  labs(x = "Donor Procurement Rate",
       y = "",
       color = "Consent Law") +
  theme(legend.position="top")

#add text labels to data points and adjust position of text using hjust
p <- ggplot(data = by_country, mapping = aes(x=roads_mean, y=donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country), hjust = 0)



#install ggrepel package
install.packages("ggrepel")

#load library
library(ggrepel)

#look at new dataset
elections_historic

#make our ggplot using ggrepel
p <- ggplot(elections_historic, aes(x = popular_pct,
                                    y = ec_pct,
                                    label = winner_label))
p + geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Winner’s share of Popular Vote",
       y = "Winner’s share of Electoral College Votes",
       title = "Presidential Elections: Popular & Electoral College Margins", 
       subtitle = "1824-2016")