#load packages
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(tidyverse)
library(socviz)

#review data
election |> 
  select(state, total_vote, r_points, pct_trump, party, census) |>
  sample_n(5)

#set party colours
party_colors <- c("#2E74C0", "#CB454A")

#create a plot using the election data where the state is not DC
#with r-points on x axis and state ordered by r_points on y-axis
p0 <- ggplot(data = subset(election, st %nin% "DC"),
             mapping = aes(x = r_points, y = reorder(state, r_points), color = party))

#create a scatterplot with a vertical line where the x-intercept is 0
p1 <-  p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)

#add party colours to the scatterplot
p2 <- p1 + scale_color_manual(values = party_colors)

#add units on x-axis
p3 <- p2 + scale_x_continuous(breaks = c( -30, -20, -10, 0, 10, 20, 30,
                                      40), labels = c("30\n (Clinton)", "20", "10", "0", "10", "20", "30",
                                                      "40\n(Trump)"))

#add facets for each value of the census variable in a single column 
#and free scales on the y-axis
p3 + facet_wrap(~ census, ncol=1, scales="free_y") +
  guides(color=FALSE) + labs(x = "Point Margin", y = "") +
  theme(axis.text=element_text(size=8))

#install and preview maps
install.packages("maps")
library(maps)
us_states <- map_data("state")
head(us_states)

#make a map!
p <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group, fill = region))
p + geom_polygon(color = "gray90", linewidth = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

#merge election and map datasets
election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election, by = 'region')
head(us_states_elec)

#plot election data on a map
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long,
                           y = lat,
                           group = group,
                           fill = party))
p1 <- p0 + geom_polygon(color = "gray90", linewidth = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2016", fill = NULL)
p2 + theme_map()

#view county datasets
county_map |>
  sample_n(5)
county_data |>
  select(id, name, state, pop_dens) |>
  sample_n(5)

#merge datasets
county_full <- left_join(county_map, county_data, by = "id")
head(county_full)

#plot county population data (pop_dens ranges taken from dataset)
p <- ggplot(data = county_full,
            mapping = aes(x = long,
                          y = lat,
                          fill = pop_dens,
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) +
  coord_equal()
p2 <- p1 + scale_fill_brewer(palette = "Blues",
                             labels = c("0-10", "10-50", "50-100", "100-500","500-1000",
                                        "1000-5000", ">5000"))
p2 + labs(fill = "Population per \n square mile") +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom")

#install package
install.packages("DiagrammeR")
library(DiagrammeR)

#make basic flowchart (reproducible)
grViz(diagram = "digraph flowchart {
  node [fontname = calibri, shape = rectangle] #'NODE' defines our nodes' appearance
  tab1 [label = '@@1'] #define how many nodes
  tab2 [label = '@@2']
  tab3 [label = '@@3']
      
  tab1 -> tab2 -> tab3; #define connections between nodes
}

  [1]: 'Artefact collection in field' #define text of nodes
  [2]: 'Preliminary dating of artefacts (visual)'
  [3]: 'Artefacts sent to lab for dating'
  ")

#can change font and shape
#fonts like calibri, courier, helvetica
#shapes like triangle, circle, square, rectangle, diamond
#attributes we can modify: https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html

#play around with creating flowchart
grViz(diagram = "digraph flowchart {
      tab1 [label = '@@1', fontname = calibri, shape = diamond, fontcolor = blue]
      tab2 [label = '@@2', fontname = helvetica, shape = rectangle, fontcolor = cyan4]
      tab3 [label = '@@3', fontname = futura, shape = rectangle, fontcolor = red]
      tab4 [label = '@@4', fontname = futura, shape = oval, fontcolor = red]
      
      tab1 -> tab2;
      tab1 -> tab3;
      tab3 -> tab4;
}
      [1]: 'Diamond'
      [2]: 'Rectangle 1'
      [3]: 'Rectangle 2'
      [4]: 'Oval'
      ")
      