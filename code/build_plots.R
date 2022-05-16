##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             Build the Daily Baseline Journey Changes Table          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pre_baseline <- journey_data %>% filter(journey_type=="pre") %>% 
  group_by(journey_name, .drop=FALSE) %>% add_count(journey_name, name = "day_count") %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max, median = median), .names = "{.col}_{.fn}")) %>% 
  add_column(journey_type = "pre", .after = "journey_name") %>% 
  mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>% 
  mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0))

post_baseline <- journey_data %>% filter(journey_type=="post") %>% 
  group_by(journey_name, .drop=FALSE) %>% add_count(journey_name, name = "day_count") %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max, median = median), .names = "{.col}_{.fn}")) %>% 
  add_column(journey_type = "post", .after = "journey_name") %>% 
  mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>% 
  mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0))

baseline <- rbind(pre_baseline, post_baseline)

# baseline_flex_table <- baseline %>% 
#   arrange(journey_name) %>% 
#   kbl() %>%
#   kable_styling(bootstrap_options = c("striped", "condensed"))
  

baseline_summary <- pre_baseline %>% full_join(post_baseline, by = c("journey_name" = "journey_name"), suffix =c(".a.pre", ".b.post")) %>% select(-journey_type.a.pre, -journey_type.b.post) %>% select(journey_name, order(colnames(.))) 
baseline_visits <- baseline_summary %>% select(journey_name, contains("Visits"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             Build the Daily Comparison of Journey Changes Table          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

compare_day <- journey_data %>% 
  select(Day, journey_type, journey_name, Visits, category, sub_category) %>% 
  group_by(journey_name) %>%
  right_join(baseline, by.x = C("journey_name" = "journey_name","journey_type" = "journey_type")) %>% 
  select(Day, journey_type, journey_name, Visits, Visits_mean, category, sub_category) %>% 
  rename(baseline_pre = Visits_mean) 

compare_baseline <- compare_day %>% filter(journey_type == "pre") %>% 
  select(journey_name, baseline_pre, sub_category) %>% group_by(journey_name) %>% distinct()

compare_yesterday <- compare_day %>% filter(Day == yesterday & journey_type == "post") %>%
  mutate(visits_yest = mean(Visits)) %>% select(journey_name, visits_yest) %>% distinct()

compare_3_day <- compare_day %>% filter(Day >= three_days_ago & Day < last_valid_date & journey_type == "post") %>%
  mutate(visits_3_da = mean(Visits)) %>% select(journey_name, visits_3_da) %>% distinct()

compare_7_day <- compare_day %>% filter(Day >= seven_days_ago & Day < last_valid_date & journey_type == "post") %>%
  mutate(visits_7_da = mean(Visits)) %>% select(journey_name, visits_7_da) %>% distinct()

compare_14_day <- compare_day %>% filter(Day >= fourteen_days_ago & Day < last_valid_date & journey_type == "post") %>%
  mutate(visits_14_da = mean(Visits)) %>% select(journey_name, visits_14_da) %>% distinct()

compare_to_day <- compare_baseline %>% 
  right_join(compare_14_day, by = "journey_name") %>% 
  right_join(compare_7_day, by = "journey_name") %>% 
  right_join(compare_3_day, by = "journey_name") %>% 
  right_join(compare_yesterday, by = "journey_name") %>% 
  relocate(sub_category, .after = journey_name)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             Build the Anomaly Counts Table                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculating incorrectly - fix needed

# Pre column - good
pre_anomaly_good <- anomaly_data %>% 
  select(day, metric, journey_name, journey_type, dataAnomalyDetected_0_1, anomalies_good) %>% 
  filter(metric =='visits' & journey_type == 'pre') %>% 
  group_by(journey_name) %>% 
  summarise(journey_name, pre_good = sum(anomalies_good)) %>% 
  distinct_all()

pre_anomaly_bad <- anomaly_data %>% 
  select(day, metric, journey_name, journey_type, dataAnomalyDetected_0_1, anomalies_bad) %>% 
  filter(metric =='visits' & journey_type == 'pre') %>% 
  group_by(journey_name) %>% 
  summarise(journey_name, pre_bad = sum(anomalies_bad)) %>% 
  distinct_all()

pre_anomaly <- merge(x = pre_anomaly_good, y = pre_anomaly_bad, by = "journey_name", all = TRUE)
pre_anomaly <- pre_anomaly %>% mutate(pre_net = pre_good - pre_bad)
  

post_anomaly_good <- anomaly_data %>% 
  select(day, metric, journey_name, journey_type, dataAnomalyDetected_0_1, anomalies_good) %>% 
  filter(metric =='visits' & journey_type == 'post') %>% 
  group_by(journey_name) %>% 
  summarise(journey_name, post_good = sum(anomalies_good)) %>% 
  distinct_all()

post_anomaly_bad <- anomaly_data %>% 
  select(day, metric, journey_name, journey_type, dataAnomalyDetected_0_1, anomalies_bad) %>% 
  filter(metric =='visits' & journey_type == 'post') %>% 
  group_by(journey_name) %>% 
  summarise(journey_name, post_bad = sum(anomalies_bad)) %>% 
  distinct_all()

post_anomaly <- merge(x = post_anomaly_good, y = post_anomaly_bad, by = "journey_name", all = TRUE)
post_anomaly <- post_anomaly %>% mutate(post_net = post_good - post_bad)

anomaly_count <- merge(x = pre_anomaly, y = post_anomaly, by = "journey_name", all = TRUE)

anomaly_count <- anomaly_count %>% arrange(post_net) %>% 
  kable(col.names = c("Journey Name", "Positive", "Negative", "Net", "Positive", "Negative", "Net")) %>% 
  add_header_above(c(" " = 1, "Pre Launch Anomalies" = 3, "Post Launch Anomalies" = 3)) %>% 
  column_spec(7, color = "white", bold = T,
              background = spec_color(1:43, end = 1, option = "viridis", direction = 1),
              popover = paste("am:", anomaly_count$post_net[1:43])) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             Build the Relative Change Table                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
relative_change <- journey_data %>% 
  select(Day, journey_type, journey_name, Visits, category, sub_category) %>% 
  group_by(journey_name) %>% 
  arrange(desc(journey_name), Day, journey_type) %>% 
  right_join(baseline, by.x = C("journey_name" = "journey_name","journey_type" = "journey_type")) %>% 
  select(Day, journey_type, journey_name, Visits, Visits_mean, category, sub_category) %>% 
  mutate(diff_to_mean = ((Visits - Visits_mean)/Visits_mean))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      SCATTER PLOT CHANNEL CONVERSION                     ----                                                                           ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Membership 
# Segment Name: Membership Completion
membership_conv_scatter_plot <- get_scatter_plot(previous_week, "event26", "s1957_6076f28b40d50e441ab3f0bd", "Membership")   

# Membership Renewals
# Segment Name: Membership Renewal Completion
membership_renewals_conv_scatter_plot <- get_scatter_plot(previous_week, "event73", "s1957_6076f79f1786ec6aa730cdbc", "Membership Renewals")

# Holidays
# Segment Name: Holiday Booking Completion
holidays_conv_scatter_plot <- get_scatter_plot(previous_week, "event134", "s1957_6076f8100e41ff781cd16ed3", "Holidays")

# Shop
# Segment Name: Shop Order Completion
shop_conv_scatter_plot <- get_scatter_plot(previous_week, "event182", "s1957_6076f7deb2029b591cf20e4a", "National Trust Shop")

# Donate
# Segment Name: Donation Completion
donate_conv_scatter_plot <- get_scatter_plot(previous_week, "event116", "s1957_6076f83b0e41ff781cd16ed5", "Donations")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          CONVERSION CHANNEL FLOW                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Segment Name: Membership Completion
membership_conv_flow_plot <- get_conversion_flow(previous_week, "event26", "s1957_6076f28b40d50e441ab3f0bd", "Membership")

# Membership Renewals
# Segment Name: Membership Renewal Completion
membership_renewals_conv_flow_plot <- get_conversion_flow(previous_week, "event73", "s1957_6076f79f1786ec6aa730cdbc", "Membership Renewals")

# Holidays
# Segment Name: Holiday Booking Completion
holidays_conv_flow_plot <- get_conversion_flow(previous_week, "event134", "s1957_6076f8100e41ff781cd16ed3", "Holidays")

# Shop
# Segment Name: Shop Order Completion
shop_conv_flow_plot <- get_conversion_flow(previous_week, "event182", "s1957_6076f7deb2029b591cf20e4a", "National Trust Shop")

# Donate
# Segment Name: Donation Completion
donate_conv_flow_plot <- get_conversion_flow(previous_week, "event116", "s1957_6076f83b0e41ff781cd16ed5", "Donations")



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            SHOP EVENT DATA                               ----                                                                     
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shop_events_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, contains("Shop - Order Step")) %>% 
  filter(Day >= start_14_sun_start & Day <= end_14_sun_end) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(start_14_sun_start," to ", end_14_sun_end)) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

shop_events_post <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, contains("Shop - Order Step")) %>% 
  filter(Day >= start_7_sun_start & Day <= end_7_sun_end) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(start_7_sun_start," to ", end_7_sun_end)) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

shop_events <- rbind(shop_events_pre, shop_events_post)
shop_events <- shop_events %>% kable(col.names = c("Date Range", "Journey Name", "Step 1", "Step 2", "Step 3", "Step 4 - Confirmation")) %>% kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  Dumbbell Plot for All Journey Summary Tabs              ----
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
  mutate(change = ifelse(post > pre, "Higher", "Lower")) %>% 
  mutate(change_0_1 = ifelse(post > pre, 1, 0))  # Added column to simplify downstream sums

db1 <- baseline %>% select(journey_name, journey_type, Visits_mean) %>% 
  pivot_wider(
    names_from = journey_type,
    values_from = 'Visits_mean') %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()


all_journeys_before_after_plot <- left_join(db1, journey_data %>% 
        dplyr::select(journey_name, journey_desc, category, sub_category), by = "journey_name") %>% 
        distinct() %>% 
        arrange(post) %>% 
        mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
        droplevels()

all_landing_page_journeys <- all_journeys_before_after_plot %>% filter(category == "landing") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()


all_device_journeys <- all_journeys_before_after_plot %>% filter(sub_category == "device") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

all_commercial_journeys <- all_journeys_before_after_plot %>% 
  filter(sub_category == "renew"| sub_category == "donate"| sub_category == "holidays"| sub_category == "membership") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

all_top_level_journeys <- all_journeys_before_after_plot %>% filter(category == "primary_menu") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

all_top_level_journeys <- all_journeys_before_after_plot %>% filter(category == "primary_menu") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

all_property_page_journeys <- all_journeys_before_after_plot %>% filter(sub_category == "property_page") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

all_discovery_journeys <- all_journeys_before_after_plot %>% filter(category == "discovery") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels() 



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
  filter(str_detect(journey_name,"Commercial")) %>%
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


top_bot_journeys <- rbind(top_10_journeys,bot_10_journeys)
top_bot <- ggplot(top_bot_journeys) +
  aes(
    x = journey_name,
    fill = change,
    weight = diff_vs_baseline*100
  ) +
  geom_bar() +
  scale_fill_hue(direction = -1) +
  labs(
    x = "Journey Name",
    y = "% Percentage inc./dec. over the baseline",
    title = "Top 10 Highest & Lowest Performing Journeys",
    fill = "Performance"
  ) +
  coord_flip() +
  ggthemes::theme_pander()

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


high_low_table <- db_plot_data %>% select(change_0_1) %>% 
  mutate(Higher = sum(change_0_1 == 1)) %>% 
           mutate(Lower = sum(change_0_1 == 0)) %>% 
  distinct() %>% select(-change_0_1) %>% 
  kbl(caption = "Number of Journeys Higher or Lower than the Baseline") %>%
  column_spec(1:2, width = "30em") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

high_low_table
# df <- db_plot_data %>% 
#   mutate(journey_name = fct_reorder(journey_name, diff_vs_baseline, .desc = TRUE)) %>% 
#   arrange(diff_vs_baseline) 
# 
# ggplot(df) +
#   aes(x = journey_name, fill = change, weight = diff_vs_baseline) +
#   geom_bar() +
#   scale_fill_hue(direction = -1) +
#   labs(x = "d", y = "e", title = "a", subtitle = "b", caption = "c", 
#        fill = "f") +
#   coord_flip() +
#   ggthemes::theme_pander()

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


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  HEATMAPS                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

heatmap_visits_by_day <- journey_data %>% 
  filter(Day >= last_valid_date-180 & Day < last_valid_date) %>% 
  select(journey_name, Day, Visits) %>% 
  group_by(journey_name) %>% 
  arrange(desc(journey_name), Day) %>% 
  tidyr::pivot_wider(
    names_from = 'Day',
    values_from = 'Visits')


heatmap_anomalies_by_day <- anomaly_data %>%  
  filter(day >= last_valid_date-180 & day < last_valid_date) %>% 
  filter(metric == 'visits') %>% 
  select(journey_name, day, dataAnomalyDetected_0_1) %>% 
  group_by(journey_name) %>% 
  arrange(desc(journey_name), day) %>% 
  tidyr::pivot_wider(
    names_from = 'day',
    values_from = 'dataAnomalyDetected_0_1')


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





