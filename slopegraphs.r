require(dplyr)
require(ggplot2)
require(ggrepel)
require(kableExtra)
moredata <- structure(list(Date = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                            .Label = c("11-May-18", "18-May-18", "25-May-18"), 
                                            class = "factor"), 
                           Party = structure(c(5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L), 
                                             .Label = c("Green", "Liberal", "NDP", "Others", "PC"), 
                                             class = "factor"), 
                           Pct = c(42.3, 28.4, 22.1, 5.4, 1.8, 41.9, 29.3, 22.3, 5, 1.4, 41.9, 26.8, 26.8, 5, 1.4)), 
                      class = "data.frame", 
                      row.names = c(NA, -15L))
tail(moredata)

ggplot(data = moredata, aes(x = Date, y = Pct, group = Party)) +
  geom_line(aes(color = Party, alpha = 1), size = 2) +
  geom_point(aes(color = Party, alpha = 1), size = 4) +
  geom_text_repel(data = moredata %>% filter(Date == "11-May-18"), 
                  aes(label = paste0(Party, " - ", Pct, "%")) , 
                  hjust = "left", 
                  fontface = "bold", 
                  size = 4, 
                  nudge_x = -.45, 
                  direction = "y") +
  geom_text_repel(data = moredata %>% filter(Date == "25-May-18"), 
                  aes(label = paste0(Party, " - ", Pct, "%")) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 4, 
                  nudge_x = .5, 
                  direction = "y") +
  # move the x axis labels up top
  scale_x_discrete(position = "top") +
  theme_bw() +
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none") +
  # Remove the panel border
  theme(panel.border     = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5)) +
  #  Labelling as desired
  labs(
    title = "Bogus Data",
    subtitle = "(Chuck Powell)",
    caption = "https://www.mainstreetresearch.ca/gap-between-ndp-and-pcs-narrows-while-liberals-hold-steady/"
  )


library(hrbrthemes)
library(tidyverse)

tibble(
  val1 = c(3, 2, 4),
  val2 = c(1, 4, 5),
  val3 = c(5, 8, 6),
  cat = factor(month.name[1:3], levels = rev(month.name[1:3]))
) -> xdf

ggplot() +
  # reshape the data frame & get min value so you can draw an eye-tracking line (this is one geom)
  geom_segment(
    data = gather(xdf, measure, val, -cat) %>% 
      group_by(cat) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 0, xend = val, y = cat, yend = cat),
    linetype = "dotted", size = 0.5, color = "gray80"
  ) +
  # reshape the data frame & get min/max category values so you can draw the segment (this is another geom)
  geom_segment(
    data = gather(xdf, measure, val, -cat) %>% 
      group_by(cat) %>% 
      summarise(start = range(val)[1], end = range(val)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = cat, yend = cat),
    color = "gray80", size = 2
  ) +
  # reshape the data frame & plot the points
  geom_point(
    data = gather(xdf, measure, value, -cat),
    aes(value, cat, color = measure), 
    size = 4
  ) +
  # i just extended the scale a bit + put axis on top; choose aesthetics that work 
  # for you
  scale_x_comma(position = "top", limits = c(0, 10)) +
  scale_color_ipsum(name = "A real legend title") +
  labs(
    x = "Description of the value", y = NULL,
    title = "A good plot title"
  ) +
  theme_ipsum_rc(grid = "X") +
  theme(legend.position = "top")

compare_to_day %>% ggplot(aes(x = visits_3_da, y= reorder(journey_name, visits_3_da))) +
  geom_line(aes(group = 1),
            color="grey",
            arrow = arrow(type = "closed",
                          length=unit(0.075, "inches"))) +
    geom_point(aes(color=visits_3_da), size =4) +
  geom_point(aes(color=visits_yest), size =4) +
#  scale_x_log10()+
  labs(y="Journey Name")


library(tidyverse)
library(gapminder)
theme_set(theme_bw(16))
df = gapminder %>%
  filter(year %in% c(1952,2007)) %>%
  filter(continent %in% c("Europe")) %>%
  select(country,year,lifeExp, gdpPercap)%>%
  mutate(paired = rep(1:(n()/2),each=2),
         year=factor(year))
df %>%
  ggplot(aes(gdpPercap,lifeExp)) +
  geom_point(aes(color=year)) +
  scale_x_log10()

df %>%
  ggplot(aes(gdpPercap,lifeExp)) +
  geom_point(aes(color=year)) +
  geom_line(aes(group = paired))

df %>%
  ggplot(aes(gdpPercap,lifeExp, color=year)) +
  geom_point(aes(fill=year),size=3) +
  scale_x_log10()+
  geom_line(aes(group = paired),color="grey")

df %>%
  ggplot(aes(gdpPercap,lifeExp, color=year)) +
  geom_point(aes(fill=year),size=3) +
  scale_x_log10()+
  geom_line(aes(group = paired),
            color="grey",
            arrow = arrow(type = "closed",
                          length=unit(0.075, "inches")))

library(GGally)
library(dplyr)

# Data set is provided by R natively
data <- iris

# Plot
compare_to_day %>%
  filter(journey_name %in% c("ALL Visits", "ALL SEO", "ALL Desktop", "ALL Main Site")) %>% 
  arrange(desc(journey_name)) %>%
  ggparcoord(
    columns = 2:ncol(compare_to_day), groupColumn = 1,
    scale="globalminmax",
    showPoints = TRUE, 
    title = "Journey Changes over Time",
    alphaLines = .9
  ) +
  theme(
    plot.title = element_text(size=10)
  ) +
  xlab("")

compare_to_day %>%
  arrange(desc(journey_name)) %>%
ggplot(aes(x=visits_3_da, y=baseline_pre, group=journey_name)) +
  geom_line() +
  geom_text_repel(aes(label = paste0(journey_name)) , 
            hjust = 1.35, 
            fontface = "bold", 
            size = 4) +
  geom_point()


data <- structure(list( Date = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), 
                                         .Label = c("11-May-18", "18-May-18"), 
                                         class = "factor"), 
                        Party = structure(c(5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L), 
                                          .Label = c("Green", "Liberal", "NDP", "Others", "PC"), 
                                          class = "factor"), 
                        Pct = c(42.3, 28.4, 22.1, 5.4, 1.8, 41.9, 29.3, 22.3, 5, 1.4)), 
                  class = "data.frame", 
                  row.names = c(NA, -10L))


ggplot(data = data, aes(x = Date, y = Pct, group = Party)) +
  geom_line(aes(color = Party, alpha = 1), size = 2) +
  geom_point(aes(color = Party, alpha = 1), size = 4) +
  
  #  Labelling as desired
  labs(
    title = "Voter's stated preferences for June 7 elections in Ontario",
    subtitle = "(Mainstreet Research)",
    caption = "https://www.mainstreetresearch.ca/gap-between-ndp-and-pcs-narrows-while-liberals-hold-steady/"
  )


moredata <- structure(list(Date = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                            .Label = c("11-May-18", "18-May-18", "25-May-18"), 
                                            class = "factor"), 
                           Party = structure(c(5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L), 
                                             .Label = c("Green", "Liberal", "NDP", "Others", "PC"), 
                                             class = "factor"), 
                           Pct = c(42.3, 28.4, 22.1, 5.4, 1.8, 41.9, 29.3, 22.3, 5, 1.4, 41.9, 26.8, 26.8, 5, 1.4)), 
                      class = "data.frame", 
                      row.names = c(NA, -15L))

ggplot(data = data, aes(x = Date, y = baseline_pre, group = journey_name)) +
  geom_line(aes(color = Party, alpha = 1), size = 2) +
  geom_point(aes(color = Party, alpha = 1), size = 4) +
  geom_text(data = data %>% filter(Date == "11-May-18"), 
            aes(label = paste0(Party, " - ", Pct, "%")) , 
            hjust = 1.35, 
            fontface = "bold", 
            size = 4) +
  geom_text(data = data %>% filter(Date == "18-May-18"), 
            aes(label = paste0(Party, " - ", Pct, "%")) , 
            hjust = -.35, 
            fontface = "bold", 
            size = 4) +
  # move the x axis labels up top
  scale_x_discrete(position = "top") +
  theme_bw() +
  #  Labelling as desired
  labs(
    title = "Voter's stated preferences for June 7 elections in Ontario",
    subtitle = "(Mainstreet Research)",
    caption = "https://www.mainstreetresearch.ca/gap-between-ndp-and-pcs-narrows-while-liberals-hold-steady/"
  )
