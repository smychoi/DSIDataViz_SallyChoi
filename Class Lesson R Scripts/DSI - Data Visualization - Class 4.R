#CODE FOR CLASS 4
#Coded by Sally Choi
#Date: 20-Mar-2023

#load libraries
library(tidyverse)
library(socviz)
library(ggplot2)
library(gapminder)
library(ggrepel)

#summarize our data
by_country <- organdata |> 
  group_by(consent_law, country) |>
  summarize_if(is.numeric, funs(mean, sd), na.rm=TRUE) |>
  ungroup()

#try using subset
p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))
p + geom_point() +
  geom_text_repel(data = subset(by_country, gdp_mean > 25000),
                  mapping = aes(label = country))

#annotate with text and rectangular shape
p <- ggplot(data = organdata, mapping = aes(x = roads, y = donors))
p + geom_point() +
  annotate(geom = "rect", xmin = 85, xmax = 135,
           ymin = 30, ymax = 35,
           fill = "red", alpha = 0.2) +
  annotate(geom = "text", x = 91, y = 33, label = "A surprisingly high \n recovery rate",
           hjust = 0)

#view new dataset
head(asasec)

#create a scatterplot and smoothed graph comparing membership and revenues for the year 2014
p <- ggplot(data = subset(asasec, Year == 2014), 
            mapping = aes(x = Members, y = Revenues))
p + geom_point(mapping = aes(colour = Journal)) + 
  geom_smooth(method = "lm")

#intermediate objects
p0 <- ggplot(data = subset(asasec, Year == 2014),
             mapping = aes(x = Members, y = Revenues, label = Sname))
p1 <- p0 + geom_smooth(method = "lm", se = FALSE, color = "gray80") +
  geom_point(mapping = aes(color = Journal))
p2 <- p1 + geom_text_repel(data = subset(asasec, Year == 2014 & Revenues > 7000),
                          size = 2)
p2

p3 <- p2 + labs(x = "Membership",
                y = "Revenues",
                color = "Section has its own Journal",
                title = "ASA Sections",
                subtitle = "2014 Calendar year",
                caption = "Source: ASA annual report")
p4 <- p3 + scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top")
p4

#install RColorBrewer package
install.packages("RColorBrewer")

#load library
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

#try playing with colours
p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, colour = world))
p + geom_point(size = 2) +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")

#manual colour vector
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p4 + scale_color_manual(values = cb_palette)

#view new dataset
head(county_data)

#define election palette
party_colors <- c("#2E74C0", "#CB454A")

#start our graph
p0 <- ggplot(data = subset(county_data, flipped == "No"),
             mapping = aes(x = pop, y = black/100))
p1 <- p0 + geom_point(alpha = 0.15, color = "gray50") +
  scale_x_log10(labels = scales::comma)
p1

p2 <- p1 + geom_point(data = subset(county_data, flipped == "Yes"),
                      mapping = aes(x = pop, y = black/100, color = partywinner16)) +
  scale_color_manual(values = party_colors)
p2

p3 <- p2 + scale_y_continuous(labels=scales::percent) +
                                labs(color = "County flipped to ... ",
                                     x = "County Population (log scale)",
                                     y = "Percent Black Population",
                                     title = "Flipped counties, 2016",
                                     caption = "Counties in gray did not flip.")
p3

p4 <- p3 + geom_text_repel(data = subset(county_data, flipped == "Yes" & black > 25),
                           mapping = aes(x = pop, y = black/100, label = state),
                           size = 2)
p4 + theme_minimal() + theme(legend.position = "top")


theme_set(theme_dark())
p4

#install ggthemes
install.packages("ggthemes")
library(ggthemes)

theme_set(theme_economist()) #theme_set sets default theme
p4

#reset theme
theme_set(theme_minimal())
p4
p4 + theme_economist() #applies theme for just this plot

#activity - case study 1 - code for suggested plot by Healy
#Clearer Y axis, well labelled, easier to interpret single graph, but the extra year variable
#and the winding shape of the line still requires some effort to interpret. The legend might
#require a little explanation as well.
p <- ggplot(data = yahoo,
            mapping = aes(x = Employees, y = Revenue))
p + geom_path(color = "gray80") +
  geom_text(aes(color = Mayer, label = Year),
            size = 3, fontface = "bold") +
  theme(legend.position = "bottom") +
  labs(color = "Mayer is CEO",
       x = "Employees", y = "Revenue (Millions)",
       title = "Yahoo Employees vs Revenues, 2004-2014") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(labels = scales::comma)

#alternative plot suggestion
#Clearly labelled x and y axes and units, straightforward graph showing a trend that's
#easy to interpret, along with a clear line showing the time before Mayer becomes CEO and after
p <- ggplot(data = yahoo,
            mapping = aes(x = Year, y = Revenue/Employees))
p + geom_vline(xintercept = 2012) +
  geom_line(color = "gray60", size = 2) +
  annotate("text", x = 2013, y = 0.44,
           label = " Mayer becomes CEO", size = 2.5) +
  labs(x = "Year\n",
       y = "Revenue/Employees",
       title = "Yahoo Revenue to Employee Ratio, 2004-2014")
