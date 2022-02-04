##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Dumbbell Plot for Journey Summary                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

options(scipen = 999) #disable scientific notation in ggplot
# TODO Create a difference column %

db_plot_data <- baseline %>% select(journey_name, journey_type, Visits_mean) %>% 
  pivot_wider(
    names_from = journey_type,
    values_from = 'Visits_mean') %>% 
  arrange(desc(post)) %>%
  mutate(journey_name = fct_reorder(journey_name, post)) %>% 
  mutate(diff_vs_baseline = ((post - pre)/pre)) %>%
  mutate(rank = order(order(diff_vs_baseline))) %>% 
  mutate(change = ifelse(post > pre, "Higher", "Lower"))

db1 <- db_plot_data %>% filter(pre <= 10000) %>% 
  ggplot(aes(x = pre, xend = post, y = journey_name)) +
  geom_dumbbell(colour="#95D1CC",
                colour_x = "#406882",
                colour_xend="#F05454", 
                size=4.0, dot_guide=TRUE, 
                dot_guide_size=0.1, 
                dot_guide_colour = "grey60",
                show.legend = TRUE)+
  labs(title = "Baseline Journey Comparison from Before & After CMS Launch", x="Pre vs. Post Baseline (Visits)", y = "Journey Name") +
  theme_tq() +
  theme(
    panel.grid.major.y=element_blank(),
    panel.border=element_blank()
  ) +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##              Percentage Change Relative to the Baseline Summary          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p1 <- relative_change %>%
  filter(journey_name %in% c("ALL Mobile", "ALL Visits", "ALL Tablet", "ALL SEO")) %>%
  ggplot() +
  aes(x = Day, y = diff_to_mean, colour = journey_name) +
  geom_line(size = 0.5) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "red", linetype=4, lwd = .8, alpha=0.5) +
  geom_hline(yintercept=0, linetype=3, col = 'blue', lwd = .8, alpha=0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "90 Days Pre & 90 Days Post",
    y = "% Change Relative to Baseline",
    title = "Percentage Change Relative to Baseline",
    subtitle = "Baseline Pre & Post CMS Launch",
    caption = "% Change Relative to Baseline"
  ) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  facet_wrap(vars(journey_name), scales = "free", ncol = 2L) +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Box Plot - Journey Pre/Post Comparison               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c1 <- relative_change %>%
  filter(!(journey_name %in% c("ALL Tablet", "ALL Social", "ALL Shop", "ALL PPC", "ALL Paid Social", "ALL Holidays", "ALL Email", "ALL Affiliates"))) %>%
  ggplot() +
  aes(x = journey_name, y = Visits, fill = journey_type) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(x = "Journey Name", y = "Visits", title = "Journey Pre & Post Comparison", 
       subtitle = "Box Plot") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))

r1 <- relative_change %>%
  filter(!(sub_category %in% c("social", "shop", "holidays"))) %>%
  ggplot() +
  aes(x = sub_category, y = Visits, fill = journey_type) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(x = "Journey Category", y = "Visits", title = "Journey Categorisations", 
       subtitle = "Grouped Journeys") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Top/Bottom 10 Journeys By Improvement               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Top 10 Most improved Journeys from Baseline in New CMS
top_10_journeys <- db_plot_data %>% 
  mutate(journey_name = fct_reorder(journey_name, diff_vs_baseline)) %>% 
  arrange(desc(diff_vs_baseline)) %>% 
  slice(1:10)

top10 <- ggplot(top_10_journeys) +
  aes(x = journey_name, weight = diff_vs_baseline*100) +
  geom_bar(fill = "#228B22") +
  labs(x = "Journey Name", y = "% Percentage Increase over Baseline", title = "Top 10 Journeys by Improvement") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))

# Bottom 10 Journeys with Highest Differences to their Baseline
bot_10_journeys <- db_plot_data %>% 
  mutate(journey_name = fct_reorder(journey_name, diff_vs_baseline, .desc = TRUE)) %>% 
  arrange(diff_vs_baseline) %>% slice(1:10)

bot10 <- ggplot(bot_10_journeys) +
  aes(x = journey_name, weight = diff_vs_baseline*100) +
  geom_bar(fill = "#B22222") +
  labs(x = "Journey Name", y = "% Percentage Decrease over Baseline", title = "Bottom 10 Journeys by Decrease") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            Total Number of Journeys Higher or Lower than Baseline        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Journey Performance Overview
highlow_count<- ggplot(db_plot_data) +
  aes(x = change, group = journey_name) +
  geom_bar(fill = "#4682B4") +
  labs(x = "Higher or Lower than Baseline", 
       y = "Journey Count", title = "Number of Journeys Higher or Lower than Baseline", subtitle = "Journey Performance") +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##           Parallel Plot Graph Comparing Journey Changes Over Time        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Facet wrap all journey comparisons
sl1 <- compare_to_day %>%
  arrange(desc(journey_name)) %>% rename('Baseline' = baseline_pre, 
                                         '14 Day Avg' = visits_14_da, 
                                         '7 Day Avg' = visits_7_da, 
                                         '3 Day Avg' = visits_3_da, 
                                         'Yesterday' = visits_yest) %>% 
  ggparcoord(
    columns = 3:ncol(compare_to_day), groupColumn = 1,
    alphaLines = 0.9,    
    splineFactor = TRUE,
    scale = "globalminmax",
    #scale="std",
    showPoints = TRUE, 
    title = "Journey Changes over Time",
    mapping = ggplot2::aes_string(x = "`All Journeys`", y = "Visits")
  ) +
  theme(
    plot.title = element_text(size=10),
    legend.position = "none"
  ) +
  facet_wrap(vars(journey_name), scales = "free", ncol = 5L)


# Specific Journeys
sl2 <- compare_to_day %>%
  filter(journey_name %in% c("ALL Apple iOS", "ALL Visits", "ALL SEO", "Days Out Entry")) %>% 
  arrange(desc(journey_name)) %>% rename('Baseline' = baseline_pre, 
                                         '14 Day Avg' = visits_14_da, 
                                         '7 Day Avg' = visits_7_da, 
                                         '3 Day Avg' = visits_3_da, 
                                         'Yesterday' = visits_yest) %>% 
  ggparcoord(
    columns = 3:ncol(compare_to_day), groupColumn = 1,
    alphaLines = 0.9,    
    splineFactor = TRUE,
    scale = "globalminmax",
    #scale="std",
    showPoints = TRUE, 
    title = "Journey Changes over Time",
    mapping = ggplot2::aes_string(x = "Journeys", y = "Visits")
  ) +
  theme(
    plot.title = element_text(size=10),
    legend.position = "none"
  ) +
  facet_wrap(vars(journey_name), scales = "free", ncol = 5L)


# Sub Category Group of Pages
sl3 <- compare_to_day %>%
  filter(sub_category %in% c("landing_pages")) %>% 
  arrange(desc(journey_name)) %>% rename('Baseline' = baseline_pre, 
                                         '14 Day Avg' = visits_14_da, 
                                         '7 Day Avg' = visits_7_da, 
                                         '3 Day Avg' = visits_3_da, 
                                         'Yesterday' = visits_yest) %>% 
  ggparcoord(
    columns = 3:ncol(compare_to_day), groupColumn = 1,
    alphaLines = 0.9,    
    splineFactor = TRUE,
    scale = "globalminmax",
    #scale="std",
    showPoints = TRUE, 
    title = "Journey Changes over Time",
    mapping = ggplot2::aes_string(x = "`Landing Page Journey`", y = "Visits")
  ) +
  theme(
    plot.title = element_text(size=10),
    legend.position = "none"
  ) +
  facet_wrap(vars(journey_name), scales = "free")
