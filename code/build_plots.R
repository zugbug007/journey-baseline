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

db1 <- baseline %>% select(journey_name, journey_type, Visits_mean) %>% 
  pivot_wider(
    names_from = journey_type,
    values_from = 'Visits_mean') %>% 
  arrange(desc(post)) %>%
  mutate(journey_name = fct_reorder(journey_name, post)) %>% slice(1:15) %>% 
plot_ly(
  marker = list(size = 20)) %>%
  add_segments(
    x = ~pre, y = ~journey_name,
    xend = ~post, yend = ~journey_name, 
    color = I("gray"), line = list(
      color = 'rgb(192,192,192)',
      width = 10
    ), showlegend = FALSE
  ) %>%
  add_markers(
    x = ~pre, y = ~journey_name, 
    marker = list(
      color = 'rgb(64, 104, 130)',
      size = 15,
      line = list(
        color = 'rgb(192,192,192)',
        width = 1
      )
    ),
    name = "Pre"
  ) %>%
  add_markers(
    x = ~post, y = ~journey_name,
    marker = list(
      color = 'rgb(240, 84, 84)',
      size = 15,
      line = list(
        color = 'rgb(128,128,128)',
        width = 1
      )
    ),
    name  = "Post"
  ) %>%
  layout(xaxis = list(title = "Pre vs. Post Baseline (Visits)")) %>% 
          layout(yaxis = list(title = "Journey Name"))
db1
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
 # filter(sub_category %in% c("landing_pages")) %>% 
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


# Commercial Plots for Revenue, AOV, Sales Performance
# Shop Pre Funnel Plot
shop_step_labels <- c("Order Step 1 - Basket", 
                      "Order Step 2 - Delivery Details", 
                      "Order Step 3 - Payment Details", 
                      "Order Step 4 - Order Confirmation")

shop_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, contains("Shop - Order Step")) %>% 
  filter(Day >= start_14_sun_start & Day <= end_14_sun_end) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

shop_fun_pre_plot <- plot_ly() 
shop_fun_pre_plot <- shop_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = shop_step_labels,
    x = shop_funnel_pre, 
    textinfo = "value+percent initial",
    opacity = 0.8)
shop_fun_pre_plot <- shop_fun_pre_plot %>%
  layout(yaxis = list(categoryarray = shop_step_labels))

# Shop Post Funnel Plot
shop_funnel_post <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, contains("Shop - Order Step")) %>% 
  filter(Day >= start_7_sun_start & Day <= end_7_sun_end) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

shop_fun_post_plot <- plot_ly() 
shop_fun_post_plot <- shop_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = shop_step_labels,
    x = shop_funnel_post, 
    textinfo = "value+percent initial",
    opacity = 0.8)
shop_fun_post_plot <- shop_fun_post_plot %>%
  layout(yaxis = list(categoryarray = shop_step_labels))

# Anomaly Plots for each journey with Ribbon and Pre/Post separations
shop_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Shop Checkout Steps 1-4")
shop_revenue_plot_title <- "Shop Revenue Trended"
plot_journey_name <- shop_funnel$journey_name    # Get the journey name
anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'revenue')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
shop_revenue <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#69b3a2", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="red", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "red", linetype='dotted', lwd = .8, alpha=0.5) +
  labs(title = plot_journey_name,
       caption = paste0('There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.')) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  ylab("Revenue") +
  xlab("Day") +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  expand_limits(y=0)
shop_revenue <- ggplotly(shop_revenue) %>%
  layout(title = list(text = paste0(shop_revenue_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))
# Shop Orders Trended
shop_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Shop Checkout Steps 1-4")
shop_orders_plot_title <- "Shop Orders Trended"
plot_journey_name <- shop_funnel$journey_name    # Get the journey name
anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'orders')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
shop_orders <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#69b3a2", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="red", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "red", linetype='dotted', lwd = .8, alpha=0.5) +
  labs(title = plot_journey_name,
       caption = paste0('There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.')) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  ylab("Orders") +
  xlab("Day") +
  #scale_y_continuous(labels = dollar_format(suffix = "", prefix = "£")) +
  expand_limits(y=0)
shop_orders <- ggplotly(shop_orders) %>%
  layout(title = list(text = paste0(shop_orders_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))

