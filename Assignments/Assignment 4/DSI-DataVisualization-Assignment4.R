#load libraries
library(ggplot2)
library(tidyverse)

#import greenhouse gas emissions .csv file
ghg.data <- read.csv("C:\\Users\\smych\\Documents\\Continuing Studies\\DSI Data Science Certificate Program\\Data Visualization\\Assignments\\GHG_Data_2010_2020_data_Dec162021.csv", header = TRUE)
str(ghg.data)

#look at dataset
str(ghg.data)
head(ghg.data)

#convert Year variable to date-time type
library(lubridate)
Year <- as.Date(as.character(ghg.data$Year), format = "%Y")

#filter data by five facility NAICS code associated with highest methane levels
ghg.top5naics <- 
  ghg.data |> 
  filter(Facility.Primary.NAICS.Code %in% c(221210, 562210, 486210, 322121, 
                                            331110, 322112))

#change labels for NAICS codes: 
#Source: https://ggplot2.tidyverse.org/reference/labeller.html
naics_names <- c(
  '221210' = "Natural gas distribution",
  '562210' = "Waste treatment and disposal",
  '486210' = "Pipeline transportation of natural gas",
  '322121' = "Paper (except newsprint) mills",
  '331110' = "Iron and steel mills manufacturing",
  '322112' = "Chemical pulp mills"
)

#plot methane levels by years
#sources: https://stackoverflow.com/questions/38722202/how-do-i-change-the-number-of-decimal-places-on-axis-labels-in-ggplot2
#https://www.tutorialspoint.com/increase-the-space-between-facets-in-a-facetted-plot-created-using-ggplot2-in-r
#https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
#https://stackoverflow.com/questions/14942681/change-size-of-axes-title-and-labels-in-ggplot2

p <- ggplot(data = ghg.top5naics,
            mapping = aes(x = Year,
                          y = Methane..CH4..in.CO2e..t.,
                          na.rm = TRUE))
p + geom_line(alpha=0.3, size = 0.5) +
  geom_smooth(size = 0.75, method="loess", se=FALSE) +
  facet_wrap(~ Facility.Primary.NAICS.Code, ncol = 3, labeller = labeller(Facility.Primary.NAICS.Code = naics_names)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.spacing=unit(1,"lines")) + #add space between facet grid panels
  theme_grey(base_size = 15) + 
  labs(x = "Year",
       y = "Methane in CO2 equivalent (tonnes)",
       title = "Methane in CO2 equivalent tonnes reported by Ontario companies, 2010-2020",
       caption = "Source: Ministry of the Environment, Conservation and Parks. (2020). 2010-2020 Specified GHG Activities.")