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
##             Build the Anomaly Counts Table                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
              background = spec_color(1:44, end = 1, option = "viridis", direction = 1),
              popover = paste("am:", anomaly_count$post_net[1:44])) %>% 
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
  mutate(Lower = sum(change_0_1 == 0)) %>% 
           mutate(Higher = sum(change_0_1 == 1)) %>% 
   select(-change_0_1) %>% distinct() %>%
  pivot_longer(
    everything()
    ) %>%     
  kable(col.names = c("Number of Journeys Higher or Lower than Baseline", "No.")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
high_low_table

low_detail <- db_plot_data %>% select(journey_name, diff_vs_baseline, change) %>% 
  mutate(diff_vs_baseline = round((diff_vs_baseline*100), digits = 0)) %>% 
  filter(change == "Lower") %>% 
  arrange(diff_vs_baseline) %>% 
  kable(col.names = c("Journey Name", "% Diff. to Baseline", "Change")) %>% 
  column_spec(2, color = "white", bold = T, background = spec_color(1:12)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) 
  
#low_detail

high_detail <- db_plot_data %>% select(journey_name, diff_vs_baseline, change) %>% 
  mutate(diff_vs_baseline = round((diff_vs_baseline*100), digits = 0)) %>% 
  filter(change == "Higher") %>% 
  arrange(desc(diff_vs_baseline)) %>% 
  kable(col.names = c("Journey Name", "% Diff. to Baseline", "Change")) %>% 
  #column_spec(2, color = "white", bold = T, background = spec_color(1:12)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) 

high_detail


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  HEATMAP                                 ----
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


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Commercial Funnel Plots                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Membership Funnel Plot
# Pre Plot
membership_step_labels <- c("Step 1", 
                            "Step 2", 
                            "Step 3", 
                            "Step 4 - Confirmation")


# Add the Join Us Page to the top of the funnel
add_start_page_value_pre <- journey_data %>% 
  filter(journey_name == "Membership: Join Us") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= start_14_sun_start & Day <= end_14_sun_end) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)


membership_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Membership Checkout Steps 1-4") %>% 
  select(Day, journey_name, 
         `Membership Step 1.0 (Serialized) (ev30)`,
         `Membership Step 2.0 (Serialized) (ev24)`,
         `Membership Step 3.0 (Serialized) (ev25)`,
         `Membership Step 4.0 - Confirmation (Serialized) (ev26)`) %>% 
  filter(Day >= start_14_sun_start & Day <= end_14_sun_end) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)
 
membership_fun_pre_plot <- plot_ly() 
membership_fun_pre_plot <- membership_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = membership_step_labels,
    x = membership_funnel_pre, 
    textinfo = "value+percent initial",
    opacity = 0.8)
membership_fun_pre_plot <- membership_fun_pre_plot %>%
  layout(yaxis = list(categoryarray = membership_step_labels))

# Build Pre with a start Page
membership_funnel_pre_start_values <- c()
membership_funnel_pre_start_values <- c(add_start_page_value_pre, membership_funnel_pre)
membership_step_labels_start <- c("Join Us Page", membership_step_labels)

membership_fun_pre_plot_start <- plot_ly() 
membership_fun_pre_plot_start <- membership_fun_pre_plot_start %>%
  add_trace(
    type = "funnel",
    y = membership_step_labels_start,
    x = membership_funnel_pre_start_values, 
    textinfo = "value+percent initial",
    opacity = 0.8)
membership_fun_pre_plot_start <- membership_fun_pre_plot_start %>%
  layout(yaxis = list(categoryarray = membership_step_labels_start))
#--------------------------------------------------------------------------------------------

# Post Plot
# Membership Post Funnel Plot
membership_funnel_post <- journey_data %>% 
  filter(journey_name == "Commercial: Membership Checkout Steps 1-4") %>% 
  select(Day, journey_name, 
         `Membership Step 1.0 (Serialized) (ev30)`,
         `Membership Step 2.0 (Serialized) (ev24)`,
         `Membership Step 3.0 (Serialized) (ev25)`,
         `Membership Step 4.0 - Confirmation (Serialized) (ev26)`) %>% 
  filter(Day >= start_7_sun_start & Day <= end_7_sun_end) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

membership_fun_post_plot <- plot_ly() 
membership_fun_post_plot <- membership_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = membership_step_labels,
    x = membership_funnel_post, 
    textinfo = "value+percent initial",
    opacity = 0.8)
membership_fun_post_plot <- membership_fun_post_plot %>%
  layout(yaxis = list(categoryarray = membership_step_labels))


#--------------------------------------------------------------------------------

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
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
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
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  #scale_y_continuous(labels = dollar_format(prefix = "£")) +
  expand_limits(y=0)
shop_orders <- ggplotly(shop_orders) %>%
  layout(title = list(text = paste0(shop_orders_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Change Points Graphs                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

holidays_cp <- journey_data %>% 
  select(Day, journey_name, Visits) %>% 
  filter(journey_name == "Commercial: Holidays Checkout Steps 1-4") %>% 
  arrange((Day)) %>% 
  select(-journey_name) 

fit_changepoint_holidays = cpt.mean(holidays_cp$Visits)

# Return estimates
c(ints = param.est(fit_changepoint_holidays)$mean,
  cp = cpts(fit_changepoint_holidays))


shop_cp <- journey_data %>% 
  select(Day, journey_name, Visits) %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  arrange((Day)) %>% 
  select(-journey_name) 

fit_changepoint_shop = cpt.mean(shop_cp$Visits)

# Return estimates
c(ints = param.est(fit_changepoint_shop)$mean,
  cp = cpts(fit_changepoint_shop))

donate_cp <- journey_data %>% 
  select(Day, journey_name, Visits) %>% 
  filter(journey_name == "Commercial: Donate Checkout Steps 1-2") %>% 
  arrange((Day)) %>% 
  select(-journey_name) 

fit_changepoint_donate = cpt.mean(donate_cp$Visits)

# Return estimates
c(ints = param.est(fit_changepoint_donate)$mean,
  cp = cpts(fit_changepoint_donate))

renew_cp <- journey_data %>% 
  select(Day, journey_name, Visits) %>% 
  filter(journey_name == "Commercial: Renew Checkout Steps 1-3") %>% 
  arrange((Day)) %>% 
  select(-journey_name) 

fit_changepoint_renew = cpt.mean(renew_cp$Visits)

# Return estimates
c(ints = param.est(fit_changepoint_renew)$mean,
  cp = cpts(fit_changepoint_renew))

membership_cp <- journey_data %>% 
  select(Day, journey_name, Visits) %>% 
  filter(journey_name == "Commercial: Membership Checkout Steps 1-4") %>% 
  arrange((Day)) %>% 
  select(-journey_name) 

fit_changepoint_membership = cpt.mean(membership_cp$Visits)

# Return estimates
c(ints = param.est(fit_changepoint_membership)$mean,
  cp = cpts(fit_changepoint_membership))


# Internal Search Data Pull and Processing
# -----------------------------------------------------------------------------------------------------------------------
days <- seq(from=post_start_date_14, to=post_end_date, by='days')
days_count <- length(days)
search_datalist = list()
for ( i in seq_along(days)) {
  day_date <- c(as.Date(days[i]), as.Date(days[i]))
  search_term_data_main_site  <- get_search_data(day_date)
  search_term_data_main_site$day <- days[i]
  search_datalist[[i]] <- search_term_data_main_site
  pc = round((i/days_count)*100, 0)
  print(paste0("Search terms for day: ", i, " of ", days_count, " - Completed: ",days[i], " ", " - Progress: ",pc,"%"))
}
search_term_data_main_site <- data.table::rbindlist(search_datalist, fill = TRUE)

# Clean up table and rename/relocate columns into a better order.
search_term_data_main_site <- search_term_data_main_site %>% 
  rename(searches = evar13instances) %>% 
  rename(search_term = evar13) %>% 
  relocate(day, .before = search_term) %>% 
  mutate(search_term = tolower(search_term))

# Prep for calculations
# calculate total searches for a day, divide by number of total searches that contain a string/search term.

# Plots
# Sort the data by each term into a time series for plotting.

search_terms_trended_flex_table <- search_term_data_main_site %>% 
  arrange(desc(search_term), day) %>% 
  group_by(search_term) %>% 
  add_count() %>% 
  select(search_term, searches) %>% 
  mutate(total = sum(searches)) %>% 
  select(search_term, total) %>% 
  arrange(desc(total)) %>% 
  distinct() %>% 
  ungroup() %>% 
  slice(1:50) %>% 
  kable(col.names = c("Search Term", "Total Searches")) %>% 
  column_spec(2, color = "white", bold = T, background = spec_color(1:50)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

search_terms_trended_flex_table

