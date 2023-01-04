##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             Build the Daily Baseline Journey Changes Table          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pre_baseline <- journey_data %>% 
  filter(Day >= pre_end_date_30 & Day <= pre_end_date) %>% 
  filter(journey_type=="pre") %>%
  arrange(desc(journey_name), Day, journey_type) %>% 
  group_by(journey_name, .drop=FALSE) %>% add_count(journey_name, name = "day_count") %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max, median = median), .names = "{.col}_{.fn}")) %>% 
  add_column(journey_type = "pre", .after = "journey_name") %>% 
  mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>% 
  mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0))

post_baseline <- journey_data %>% 
  filter(Day >= last_valid_date-30 & Day <= last_valid_date) %>% # Tweak based on launch
  filter(journey_type=="post") %>% 
  arrange(desc(journey_name), Day, journey_type) %>% 
  group_by(journey_name, .drop=FALSE) %>% add_count(journey_name, name = "day_count") %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max, median = median), .names = "{.col}_{.fn}")) %>% 
  add_column(journey_type = "post", .after = "journey_name") %>% 
  mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>% 
  mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0))

 mid_baseline <- journey_data %>%
   filter(post_only == TRUE) %>%
   arrange(desc(journey_name), Day, journey_type, post_only) %>%
   group_by(journey_name, .drop=FALSE) %>% add_count(journey_name, name = "day_count") %>%
   summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max, median = median), .names = "{.col}_{.fn}")) %>%
   add_column(post_only = TRUE, .after = "journey_name") %>%
   mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>%
   mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0))

 last_week_mid_baseline <- journey_data %>%
   filter(Day >= previous_week_7_start & Day <= previous_week_7_end) %>% # Tweak based on launch
   filter(post_only == TRUE) %>%
   arrange(desc(journey_name), Day, journey_type, post_only) %>%
   group_by(journey_name, .drop=FALSE) %>% add_count(journey_name, name = "day_count") %>%
   summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max, median = median), .names = "{.col}_{.fn}")) %>%
   add_column(post_only = TRUE, .after = "journey_name") %>%
   mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>%
   mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0)) %>% 
   select(Visits_mean) %>% pull(Visits_mean)

 # Need more data, uncomment in week 2
  mid_baseline <- mid_baseline %>% add_column(Visits_mean_last_week = last_week_mid_baseline, .after = "Visits_mean")
 
baseline <- rbind(pre_baseline, post_baseline)

 baseline_flex_table <- baseline %>% 
   select(journey_name, journey_type, contains("Visits_mean")) %>% 
   arrange(journey_name)# %>% 
   #kbl() %>%
   #kable_styling(bootstrap_options = c("striped", "condensed"))

baseline_summary <- pre_baseline %>% full_join(post_baseline, by = c("journey_name" = "journey_name"), suffix =c(".a.pre", ".b.post")) %>% select(-journey_type.a.pre, -journey_type.b.post) %>% select(journey_name, order(colnames(.))) 
baseline_visits <- baseline_summary %>% select(journey_name, contains("Visits"))

baseline_post_start_date <- journey_data %>% 
  filter(Day >= last_valid_date-30 & Day <= last_valid_date) %>% # Tweak based on launch
  filter(journey_type=="post" & journey_name == "ALL Visits") %>% 
  arrange(desc(journey_name), Day, journey_type) %>% 
  select(Day) %>% arrange(Day) %>% slice(1:1) %>% pull(Day)

baseline_post_date_range = c(as.Date(baseline_post_start_date), as.Date(last_valid_date)) 
# Dynamic calculation of the true baseline date range.
# this is required so early in launch window e.g 10 days we are using the true data available 
# which would be ten days in the table for the calculation
# in the post period to show the correct date in markdown.
# The maximum would be the 30 days but this should update correctly going forward.

baseline_pre_date_range = c(as.Date(pre_end_date_30), as.Date(pre_end_date))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             Build the Anomaly Counts Table                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

anomaly_count <- anomaly_count %>% arrange(post_net)
# %>% # Run the merge line above before running this block for testing
  # kable(col.names = c("Journey Name", "Positive", "Negative", "Net", "Positive", "Negative", "Net")) %>% 
  # add_header_above(c(" " = 1, "Pre Launch Anomalies" = 3, "Post Launch Anomalies" = 3)) %>% 
  # column_spec(7, color = "white", bold = T,
  #             background = spec_color(1:journey_unique_count, end = 1, option = "viridis", direction = 1),
  #             popover = paste("am:", anomaly_count$post_net[1:journey_unique_count])) %>% 
  # kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")


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
##                               METRICS DATA                               ----                                                                     
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Shop =========================================================================
shop_events_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, `Shop - Order Step 1 - Basket (ev179)`,
         `Shop - Order Step 2 - Delivery Details (ev180)`,
         `Shop - Order Step 3 - Payment Details (ev181)`,
         `Shop - Order Step 4 - Order Confirmation (Serialized) (ev182)`,
         `Shop - Revenue`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(fourteen_days_ago,"%d")," - ", format(fourteen_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

shop_events_post <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, `Shop - Order Step 1 - Basket (ev179)`,
         `Shop - Order Step 2 - Delivery Details (ev180)`,
         `Shop - Order Step 3 - Payment Details (ev181)`,
         `Shop - Order Step 4 - Order Confirmation (Serialized) (ev182)`,
         `Shop - Revenue`) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(seven_days_ago,"%d")," - ", format(seven_days_ago+6,"%d %B"))) %>%  
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

shop_events <- rbind(shop_events_pre, shop_events_post)
shop_events <- shop_events %>% kable(col.names = c("Date Range", "Journey Name", "Step 1", "Step 2", "Step 3", "Step 4 - Confirm", "£ Revenue")) %>% kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")

# Membership =========================================================================
membership_events_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Membership Checkout Steps 1-4") %>% 
  select(Day, journey_name, `Membership Step 1.0 (Serialized) (ev30)`,
         `Membership Step 2.0 (Serialized) (ev24)`,
         `Membership Step 3.0 (Serialized) (ev25)`,
         `Membership Step 4.0 - Confirmation (Serialized) (ev26)`,
         `Membership Revenue (ev5)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(fourteen_days_ago,"%d")," - ", format(fourteen_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

membership_events_post <- journey_data %>% 
  filter(journey_name == "Commercial: Membership Checkout Steps 1-4") %>% 
  select(Day, journey_name, `Membership Step 1.0 (Serialized) (ev30)`,
         `Membership Step 2.0 (Serialized) (ev24)`,
         `Membership Step 3.0 (Serialized) (ev25)`,
         `Membership Step 4.0 - Confirmation (Serialized) (ev26)`,
         `Membership Revenue (ev5)`) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(seven_days_ago,"%d")," - ", format(seven_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

membership_events <- rbind(membership_events_pre, membership_events_post)
membership_events <- membership_events %>% 
  kable(col.names = c("Date Range", "Journey Name", "Step 1", "Step 2", "Step 3", "Step 4 - Confirm", "£ Revenue")) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")

# Renewals =========================================================================
renewals_events_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Renew Checkout Steps 1-3") %>% 
  select(Day, journey_name, `Renew Step 1.0 (ev71)`,
                            `Renew Step 2.0 (ev72)`,
         `Renew Step 3.0 - Confirmation - Serialized (ev76)`,
         `Renew Revenue - Serialized (ev79)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(fourteen_days_ago,"%d")," - ", format(fourteen_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

renewals_events_post <- journey_data %>% 
  filter(journey_name == "Commercial: Renew Checkout Steps 1-3") %>% 
  select(Day, journey_name, `Renew Step 1.0 (ev71)`,
         `Renew Step 2.0 (ev72)`,
         `Renew Step 3.0 - Confirmation - Serialized (ev76)`,
         `Renew Revenue - Serialized (ev79)`) %>%
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(seven_days_ago,"%d")," - ", format(seven_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

renewals_events <- rbind(renewals_events_pre, renewals_events_post)
renewals_events <- renewals_events %>% 
  kable(col.names = c("Date Range", "Journey Name", "Step 1", "Step 2", "Step 3 - Confirm", "£ Revenue")) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")

# Holidays =========================================================================
holidays_events_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Holidays Checkout Steps 1-4") %>% 
  select(Day, journey_name, `Holidays Booking Step 1.0 - Serialised (ev131)`,
         `Holidays Booking Step 2.0 - Serialised (ev132)`,
         `Holidays Booking Step 3.0 - Serialised (ev133)`,
         `Holidays Booking Step 4.0 - Confirmation - Serialised (ev134)`,
         `Holidays Booking Total Revenue (Serialised) (ev125)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(fourteen_days_ago,"%d")," - ", format(fourteen_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

holidays_events_post <- journey_data %>% 
  filter(journey_name == "Commercial: Holidays Checkout Steps 1-4") %>% 
  select(Day, journey_name, `Holidays Booking Step 1.0 - Serialised (ev131)`,
         `Holidays Booking Step 2.0 - Serialised (ev132)`,
         `Holidays Booking Step 3.0 - Serialised (ev133)`,
         `Holidays Booking Step 4.0 - Confirmation - Serialised (ev134)`,
         `Holidays Booking Total Revenue (Serialised) (ev125)`) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(seven_days_ago,"%d")," - ", format(seven_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

holidays_events <- rbind(holidays_events_pre, holidays_events_post)
holidays_events <- holidays_events %>% 
  kable(col.names = c("Date Range", "Journey Name", "Step 1", "Step 2", "Step 3", "Step 4 - Confirm", "£ Revenue")) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")

# Donate =========================================================================
donate_events_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Donate Checkout Steps 1-2") %>% 
  select(Day, journey_name, 
         `Donate Step 1.0 (Serialized) (ev115)`,
         `Donate Step 2.0 - Complete (Serialized) (ev116)`,
         `Donate Revenue (Serialized) (ev114)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(fourteen_days_ago,"%d")," - ", format(fourteen_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

donate_events_post <- journey_data %>% 
  filter(journey_name == "Commercial: Donate Checkout Steps 1-2") %>% 
  select(Day, journey_name, `Donate Step 1.0 (Serialized) (ev115)`,
         `Donate Step 2.0 - Complete (Serialized) (ev116)`,
         `Donate Revenue (Serialized) (ev114)`) %>%
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(journey_name, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(`Date Range` = paste0(format(seven_days_ago,"%d")," - ", format(seven_days_ago+6,"%d %B"))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  relocate(`Date Range`, .before = journey_name) %>% 
  slice (1:1)

donate_events <- rbind(donate_events_pre, donate_events_post)
donate_events <- donate_events %>% 
  kable(col.names = c("Date Range", "Journey Name", "Step 1", "Step 2 - Confirm", "£ Revenue")) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##              Build stats table for significance and normality              --
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


stats_list = list()

for (i in 1:nrow(journey_unique_names)) {
  journey_name_stats <- journey_unique_names$journey_name[i] 
  journey_subset <- journey_data %>% select(Day, journey_name, journey_type, Visits) %>% filter(journey_name == journey_name_stats)
  
  before_count_rows <- nrow(journey_subset %>% filter(journey_type == "pre") %>% arrange((Day)))
  after_count_rows <- nrow(journey_subset %>% filter(journey_type == "post") %>% arrange((Day)))
  cut_off_date <- pre_end_date - after_count_rows
  # Get the equivalent number of days in the pre period as in the post period 
  # This is necessary for the two sided test to have equal numbers on both pre and post.
  
  before <- journey_subset %>% 
    filter(journey_type == "pre") %>% 
    arrange((Day)) %>% 
    filter(Day > cut_off_date & Day <= pre_end_date) 
  
  after <- journey_subset %>% filter(journey_type == "post") %>% arrange((Day))
  
  # Welch's Test for unequal variances
  t_stat <- t.test(before$Visits, after$Visits, paired = TRUE) # Compare means between groups (pre/post)
  p_val <- t_stat[["p.value"]]
  # A p Value of less than 0.05 (typically <= 0.05 is statistically significant)
  
  # Preliminary test to check paired t-test assumptions
  # Shapiro-Wilk normality test.
  # Above p = 0.05 = data is of normal distribution
  
  d <- journey_subset %>% arrange((Day)) %>% filter(Day > cut_off_date & Day <= last_valid_date) 
  
  shaprio_data <- with(d, Visits[journey_type == "pre"] - Visits[journey_type == "post"])
  shapiro <- shapiro.test(shaprio_data)
  shap_p_value <- shapiro[["p.value"]]
  # Above p = 0.05 = data is of normal distribution
  
  # Plots
  # qqnorm(shaprio_data)   # QQPlot
  # truehist(shaprio_data) # Does the histogram look normally distributed?
  
  stats_table <- data.frame(journey_name_stats, p_val, shap_p_value)
  stats_list[[i]] <- stats_table
  
}

journey_stats = do.call(rbind, stats_list)

journey_stats <- journey_stats %>% 
  rename(journey_name = journey_name_stats) %>% 
  mutate(journey_sig = ifelse(p_val <= 0.05, "Significant", "Not Significant"),
         normality_sig = ifelse(shap_p_value >= 0.05, "Normal Distribution", "Non-Normal Distribution")) %>% 
  arrange(desc(journey_sig))


# A p Value of less than 0.05 (typically <= 0.05 is statistically significant)
# effectively indicates that the differences between old and new journeys are statistically significant.
# Reject the Null hypothesis of the no differences between old site and new site.
# Higher than 0.05 p indicates that no statistical difference.
# Retain the Null hypothesis that there is no difference in the journeys between the old site and new site.
# 
# Shapiro Test
# Above p = 0.05 = data is of normal distribution

# Summary Data
# 
# journey_data %>% select(Day, journey_name, journey_type, Visits) %>% 
#   filter(journey_name == "ALL Desktop") %>% 
#   group_by(journey_type) %>% 
#   summarize(
#     count = n(),
#     mean = mean(Visits, na.rm = TRUE),
#     sd = sd(Visits, na.rm = TRUE)
#   )
# 
# 
# library("ggpubr")
# md <- journey_data %>% select(Day, journey_name, journey_type, Visits) %>% 
#   filter(journey_name == "ALL Desktop")
# ggboxplot(md, x = "journey_type", y = "Visits", 
#           color = "journey_type", palette = c("#00AFBB", "#E7B800"),
#           order = c("pre", "post"),
#           ylab = "All Visits", xlab = "Journey Type")


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

db1 <- baseline %>% 
  select(journey_name, journey_type, Visits_mean) %>%
  pivot_wider(
    names_from = journey_type,
    values_from = 'Visits_mean') %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

post_only_plot <- mid_baseline %>% 
   select(journey_name, starts_with("Visits_")) %>%
   ungroup() %>% 
   arrange(Visits_max) %>% 
   mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
   droplevels()

all_journeys_before_after_plot <- left_join(db1, journey_data %>% dplyr::select(journey_name, category, sub_category), by = "journey_name") %>% 
        left_join(journey_stats, by = "journey_name") %>% 
        filter(category != "skip_summary") %>% 
        distinct() %>% 
        arrange(post) %>% 
        mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
        droplevels()

all_journeys_significant <- all_journeys_before_after_plot %>% filter(journey_sig == "Significant") %>% 
  mutate(diff = post - pre) %>%
  filter(diff < -1000) %>% 
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

all_browser_journeys <- all_journeys_before_after_plot %>% filter(sub_category == "browser") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

all_help_journeys <- all_journeys_before_after_plot %>% filter(sub_category == "help_centre") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels()

all_discovery_journeys <- all_journeys_before_after_plot %>% filter(category == "discovery") %>% 
  arrange(post) %>% 
  mutate(journey_name = factor(journey_name, levels=journey_name)) %>% 
  droplevels() 

metric_name <- 'Average Time Spent on Site (seconds)_mean'
title_text <- "Average Time Spent on Site (Sec.)"
time_on_site <- get_metric_plot(metric_name, title_text)

metric_name <- 'Bounces_mean'
title_text <- "Bounces"
bounces_mean <- get_metric_plot (metric_name, title_text)

metric_name <- 'Page Views_mean'
title_text <- "Page Views"
pageviews_mean <- get_metric_plot (metric_name, title_text)

metric_name <- 'Unique Visitors_mean'
title_text <- "Unique Visitors"
unique_visitors_mean <- get_metric_plot (metric_name, title_text)

metric_name <- 'New Visits_mean'
title_text <- "New Visits"
new_visits_mean <- get_metric_plot (metric_name, title_text)

metric_name <- 'Repeat Visits_mean'
title_text <- "Repeat Visits"
repeat_visits_mean <- get_metric_plot (metric_name, title_text)

metric_name <- 'Reloads_mean'
title_text <- "Reloads"
reloads_mean <- get_metric_plot (metric_name, title_text)

#temp <- baseline %>% select(journey_name, journey_type, contains('_mean'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Top/Bottom 10 Journeys By Improvement               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Top 10 Most improved Journeys from Baseline in New CMS
top_10_journeys <- db_plot_data %>% 
  mutate(journey_name = fct_reorder(journey_name, diff_vs_baseline)) %>% 
  arrange(desc(diff_vs_baseline)) %>% 
  slice(1:15)

top_journeys <- db_plot_data %>% 
  mutate(journey_name = fct_reorder(journey_name, diff_vs_baseline)) %>% 
  arrange(desc(diff_vs_baseline)) %>% 
  filter(change == 'Higher') %>% 
  slice(1:30)

top_10_only <- ggplot(top_journeys) +
  aes(
    x = journey_name, 
    fill = change, 
    weight = diff_vs_baseline*100) +
  geom_bar() +
  scale_fill_manual(values = c(Higher = "#406882")) +
  labs(
    x = "Journey Name", 
    y = "% Percentage increase over the baseline", 
    title = "Highest Performing Journeys", 
    fill = "Performance") +
  #scale_y_continuous(breaks = seq(-200, 200, by = 50)) +
  coord_flip() +
  ggthemes::theme_pander()

# Bottom 10 Journeys with Highest Differences to their Baseline
bot_10_journeys <- db_plot_data %>% filter(post > 0) %>% 
  mutate(journey_name = fct_reorder(journey_name, diff_vs_baseline, .desc = TRUE)) %>% 
  arrange(diff_vs_baseline) %>% 
  slice(1:15)

bot_journeys <- db_plot_data %>% filter(post > 0) %>% 
  mutate(journey_name = fct_reorder(journey_name, diff_vs_baseline, .desc = TRUE)) %>% 
  arrange(diff_vs_baseline) %>% 
  filter(change == 'Lower') %>% 
  slice(1:30)

bottom_10_only <- ggplot(bot_journeys) +
  aes(
    x = journey_name, 
    fill = change, 
    weight = diff_vs_baseline*100) +
  geom_bar() +
  scale_fill_manual(values = c(Lower = "#F05454")) +
  labs(
    x = "Journey Name", 
    y = "% Percentage decline over the baseline", 
    title = "Lowest Performing Journeys", 
    fill = "Performance") +
  #scale_y_continuous(breaks = seq(-200, 200, by = 50)) +
  coord_flip() +
  ggthemes::theme_pander()

top_bot_journeys <- rbind(top_10_journeys,bot_10_journeys)

top_bot <- ggplot(top_bot_journeys) +
 aes(
   x = journey_name, 
   fill = change, 
   weight = diff_vs_baseline*100) +
 geom_bar() +
 scale_fill_manual(values = c(Higher = "#406882", Lower = "#F05454")) +
 labs(
   x = "Journey Name", 
   y = "% Percentage inc./dec. over the baseline", 
 title = "Highest & Lowest Performing Journeys", 
 fill = "Performance") +
  #scale_y_continuous(breaks = seq(-200, 200, by = 50)) +
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
  filter(!is.na(change_0_1)) %>% # Remove post only journeys 
  mutate(Lower = sum(change_0_1 == 0)) %>% 
           mutate(Higher = sum(change_0_1 == 1)) %>% 
   select(-change_0_1) %>% distinct() %>%
  pivot_longer(
    everything()
    )

journey_high_count <- high_low_table %>% filter(name == "Higher") %>% pull(value)
journey_low_count <- high_low_table %>% filter(name == "Lower") %>% pull(value)

#high_low_table

# low_detail <- db_plot_data %>% select(journey_name, diff_vs_baseline, change) %>% 
#   mutate(diff_vs_baseline = round((diff_vs_baseline*100), digits = 0)) %>% 
#   filter(change == "Lower") %>% 
#   arrange(diff_vs_baseline) %>% 
#   kable(col.names = c("Journey Name", "% Diff. to Baseline", "Change")) %>% 
#   column_spec(2, color = "white", bold = T, background = spec_color(1:12)) %>% 
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) 
  
#low_detail

# high_detail <- db_plot_data %>% select(journey_name, diff_vs_baseline, change) %>% 
#   mutate(diff_vs_baseline = round((diff_vs_baseline*100), digits = 0)) %>% 
#   filter(change == "Higher") %>% 
#   arrange(desc(diff_vs_baseline)) %>% 
#   kable(col.names = c("Journey Name", "% Diff. to Baseline", "Change")) %>% 
#   #column_spec(2, color = "white", bold = T, background = spec_color(1:12)) %>% 
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) 

#high_detail


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  HEATMAP                                 ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

heatmap_visits_by_day <- journey_data %>% 
  filter(Day >= last_valid_date-90 & Day < last_valid_date) %>% 
  select(journey_name, Day, Visits) %>% 
  group_by(journey_name) %>% 
  arrange(desc(journey_name), Day) %>% 
  tidyr::pivot_wider(
    names_from = 'Day',
    values_from = 'Visits',
    names_sort = TRUE)
write_sheet(heatmap_visits_by_day, gsheet, "Heatmap")

# heatmap_anomalies_by_day <- anomaly_data %>%  
#   filter(day >= last_valid_date-180 & day < last_valid_date) %>% 
#   filter(metric == 'visits') %>% 
#   select(journey_name, day, dataAnomalyDetected_0_1) %>% 
#   group_by(journey_name) %>% 
#   arrange(desc(journey_name), day) %>% 
#   tidyr::pivot_wider(
#     names_from = 'day',
#     values_from = 'dataAnomalyDetected_0_1')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Commercial Funnel Plots                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Membership Funnel Plot
# Pre Plot
membership_step_labels <- c("Step 1 - Personal Details", 
                            "Step 2 - Payment Options", 
                            "Step 3 - Summary", 
                            "Step 4 - Confirmation")


# Add the Join Us Page to the top of the funnel
# Set to use the post page so rolling tracking of the funnel can be monitored.
add_start_page_value_pre <- journey_data %>% 
  filter(journey_name == "Membership: Join Us Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)


add_start_page_value_post <- journey_data %>% 
  filter(journey_name == "Membership: Join Us Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)

annotations = list( 
  list( 
    x = 0.2,  
    y = 1.0,  
    text = c(paste0(format(fourteen_days_ago, "%d-%b"), " to ", format(fourteen_days_ago+6, "%d-%b"))),  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.8,  
    y = 1,  
    text = c(paste0(format(seven_days_ago, "%d-%b"), " to ", format(seven_days_ago+6, "%d-%b"))),    
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ))

#================================================================================================

membership_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Membership Checkout Steps 1-4") %>% 
  select(Day, journey_name, 
         `Membership Step 1.0 (Serialized) (ev30)`,
         `Membership Step 2.0 (Serialized) (ev24)`,
         `Membership Step 3.0 (Serialized) (ev25)`,
         `Membership Step 4.0 - Confirmation (Serialized) (ev26)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)
 
membership_fun_pre_plot <- plot_ly() 
membership_fun_pre_plot <- membership_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = membership_step_labels,
    x = membership_funnel_pre, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
marker = list(color = c("#406882")))
membership_fun_pre_plot <- membership_fun_pre_plot %>%
  layout(yaxis = list(categoryarray = membership_step_labels))

# Build Pre with a start Page
membership_funnel_pre_start_values <- c()
membership_funnel_pre_start_values <- c(add_start_page_value_pre, membership_funnel_pre[1])
membership_step_labels_start <- c("Membership Page", membership_step_labels[1])

membership_fun_pre_plot_start <- plot_ly() 
membership_fun_pre_plot_start <- membership_fun_pre_plot_start %>%
  add_trace(
    type = "funnel",
    y = membership_step_labels_start,
    x = membership_funnel_pre_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
    )
membership_fun_pre_plot_start <- membership_fun_pre_plot_start %>% layout(yaxis = list(categoryarray = membership_step_labels_start))


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
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

membership_fun_post_plot <- plot_ly() 
membership_fun_post_plot <- membership_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = membership_step_labels,
    x = membership_funnel_post, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
membership_fun_post_plot <- membership_fun_post_plot %>%
  layout(yaxis = list(categoryarray = membership_step_labels))

# Build the side by side funnel plot
membership_pre_post_funnel <- subplot(membership_fun_pre_plot, membership_fun_post_plot, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)


# Build Post with a start Page
membership_funnel_post_start_values <- c()
membership_funnel_post_start_values <- c(add_start_page_value_post, membership_funnel_post[1])

membership_fun_post_plot_start <- plot_ly() 
membership_fun_post_plot_start <- membership_fun_post_plot_start %>%
  add_trace(
    type = "funnel",
    y = membership_step_labels_start,
    x = membership_funnel_post_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
  )
membership_fun_post_plot_start <- membership_fun_post_plot_start %>% layout(yaxis = list(categoryarray = membership_step_labels_start))

# Build the side by side funnel plot for the funnel with the start page
membership_pre_post_start_funnel <- subplot(membership_fun_pre_plot_start, membership_fun_post_plot_start, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)

#--------------------------------------------------------------------------------

# Shop Pre Funnel Plot

shop_step_labels <- c("Order Step 1 - Basket", 
                      "Order Step 2 - Delivery Details", 
                      "Order Step 3 - Payment Details", 
                      "Order Step 4 - Order Confirmation")

shop_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, contains("Shop - Order Step")) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

shop_fun_pre_plot <- plot_ly() 
shop_fun_pre_plot <- shop_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = shop_step_labels,
    x = shop_funnel_pre, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
shop_fun_pre_plot <- shop_fun_pre_plot %>%
  layout(yaxis = list(categoryarray = shop_step_labels))

# Add the Start Page to the top of the funnel
# Set to use the post page so rolling tracking of the funnel can be monitored.
add_start_page_value_pre <- journey_data %>% 
  filter(journey_name == "Shop: Any Shop Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)


# Build Pre with a start Page
shop_funnel_pre_start_values <- c()
shop_funnel_pre_start_values <- c(add_start_page_value_pre, shop_funnel_pre[1])
shop_step_labels_start <- c("Any Shop Page", shop_step_labels[1])

shop_fun_pre_plot_start <- plot_ly() 
shop_fun_pre_plot_start <- shop_fun_pre_plot_start %>%
  add_trace(
    type = "funnel",
    y = shop_step_labels_start,
    x = shop_funnel_pre_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
  )
shop_fun_pre_plot_start <- shop_fun_pre_plot_start %>% layout(yaxis = list(categoryarray = shop_step_labels_start))


add_start_page_value_post <- journey_data %>% 
  filter(journey_name == "Shop: Any Shop Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)

# Shop Post Funnel Plot
shop_funnel_post <- journey_data %>% 
  filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% 
  select(Day, journey_name, contains("Shop - Order Step")) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

shop_fun_post_plot <- plot_ly() 
shop_fun_post_plot <- shop_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = shop_step_labels,
    x = shop_funnel_post, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
shop_fun_post_plot <- shop_fun_post_plot %>%
  layout(yaxis = list(categoryarray = shop_step_labels))


# Build the side by side funnel plot
shop_pre_post_funnel <- subplot(shop_fun_pre_plot, shop_fun_post_plot, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)


# Build Post with a start Page
shop_funnel_post_start_values <- c()
shop_funnel_post_start_values <- c(add_start_page_value_post, shop_funnel_post[1])

shop_fun_post_plot_start <- plot_ly() 
shop_fun_post_plot_start <- shop_fun_post_plot_start %>%
  add_trace(
    type = "funnel",
    y = shop_step_labels_start,
    x = shop_funnel_post_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
  )
shop_fun_post_plot_start <- shop_fun_post_plot_start %>% layout(yaxis = list(categoryarray = shop_step_labels_start))


# Build the side by side funnel plot for the funnel with the start page
shop_pre_post_start_funnel <- subplot(shop_fun_pre_plot_start, shop_fun_post_plot_start, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)

#===============================================================================
# Holidays Pre Funnel Plots

holidays_step_labels <- c("Booking Step 1 - Personal Details", 
                          "Booking Step 2 - Payment Details", 
                          "Booking Step 3 - Summary", 
                          "Booking Step 4 - Booking Confirmation")


holidays_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Holidays Checkout Steps 1-4") %>% 
  select(Day, journey_name, 
         `Holidays Booking Step 1.0 - Serialised (ev131)`,
         `Holidays Booking Step 2.0 - Serialised (ev132)`,
         `Holidays Booking Step 3.0 - Serialised (ev133)`,
         `Holidays Booking Step 4.0 - Confirmation - Serialised (ev134)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

holidays_fun_pre_plot <- plot_ly() 
holidays_fun_pre_plot <- holidays_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = holidays_step_labels,
    x = holidays_funnel_pre, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
holidays_fun_pre_plot <- holidays_fun_pre_plot %>% layout(yaxis = list(categoryarray = holidays_step_labels))

# Add the Start Page to the top of the funnel
# Set to use the post page so rolling tracking of the funnel can be monitored.
add_start_page_value_pre <- journey_data %>% 
  filter(journey_name == "Holidays: Any Holidays Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)

# Build Pre with a start Page
holidays_funnel_pre_start_values <- c()
holidays_funnel_pre_start_values <- c(add_start_page_value_pre, holidays_funnel_pre[1])
holidays_step_labels_start <- c("Any Holidays Page", holidays_step_labels[1])

holidays_fun_pre_plot_start <- plot_ly() 
holidays_fun_pre_plot_start <- holidays_fun_pre_plot_start %>%
  add_trace(
    type = "funnel",
    y = holidays_step_labels_start,
    x = holidays_funnel_pre_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
  )
holidays_fun_pre_plot_start <- holidays_fun_pre_plot_start %>% layout(yaxis = list(categoryarray = holidays_step_labels_start))

# Holidays Post Funnel Plot
holidays_funnel_post <- journey_data %>% 
  filter(journey_name == "Commercial: Holidays Checkout Steps 1-4") %>% 
  select(Day, journey_name, 
         `Holidays Booking Step 1.0 - Serialised (ev131)`,
         `Holidays Booking Step 2.0 - Serialised (ev132)`,
         `Holidays Booking Step 3.0 - Serialised (ev133)`,
         `Holidays Booking Step 4.0 - Confirmation - Serialised (ev134)`) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

holidays_fun_post_plot <- plot_ly() 
holidays_fun_post_plot <- holidays_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = holidays_step_labels,
    x = holidays_funnel_post, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
holidays_fun_post_plot <- holidays_fun_post_plot %>% layout(yaxis = list(categoryarray = holidays_step_labels))

# Build the side by side funnel plot
holidays_pre_post_funnel <- subplot(holidays_fun_pre_plot, holidays_fun_post_plot, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)

add_start_page_value_post <- journey_data %>% 
  filter(journey_name == "Holidays: Any Holidays Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)

# Build Post with a start Page
holidays_funnel_post_start_values <- c()
holidays_funnel_post_start_values <- c(add_start_page_value_post, holidays_funnel_post[1])


holidays_fun_post_plot_start <- plot_ly() 
holidays_fun_post_plot_start <- holidays_fun_post_plot_start %>%
  add_trace(
    type = "funnel",
    y = holidays_step_labels_start,
    x = holidays_funnel_post_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
  )
holidays_fun_post_plot_start <- holidays_fun_post_plot_start %>% layout(yaxis = list(categoryarray = holidays_step_labels_start))

# Build the side by side funnel plot for the funnel with the start page
holidays_pre_post_start_funnel <- subplot(holidays_fun_pre_plot_start, holidays_fun_post_plot_start, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)


#===============================================================================

# Renewals Pre Funnel Plots

renew_step_labels <- c("Renew Step 1 - Find Membership", 
                       "Renew Step 2 - Details", 
                       "Renew Step 3 - Renewal Confirmation")


renew_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Renew Checkout Steps 1-3") %>% 
  select(Day, journey_name, `Renew Step 1.0 (ev71)`, 
                            `Renew Step 2.0 (ev72)`, 
                            `Renew Step 3.0 - Confirmation - Serialized (ev76)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

renew_fun_pre_plot <- plot_ly() 
renew_fun_pre_plot <- renew_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = renew_step_labels,
    x = renew_funnel_pre, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
renew_fun_pre_plot <- renew_fun_pre_plot %>% layout(yaxis = list(categoryarray = renew_step_labels))


# Add the Start Page to the top of the funnel
# Set to use the post page so rolling tracking of the funnel can be monitored.
add_start_page_value_pre <- journey_data %>% 
  filter(journey_name == "Membership: Renew or Join Us Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)

# Build Pre with a start Page
renew_funnel_pre_start_values <- c()
renew_funnel_pre_start_values <- c(add_start_page_value_pre, renew_funnel_pre[1])
renew_step_labels_start <- c("Join or Renew Page", renew_step_labels[1])

renew_fun_pre_plot_start <- plot_ly() 
renew_fun_pre_plot_start <- renew_fun_pre_plot_start %>%
  add_trace(
    type = "funnel",
    y = renew_step_labels_start,
    x = renew_funnel_pre_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
  )
renew_fun_pre_plot_start <- renew_fun_pre_plot_start %>% layout(yaxis = list(categoryarray = renew_step_labels_start))

# Renew Post Funnel Plot
renew_funnel_post <- journey_data %>% 
  filter(journey_name == "Commercial: Renew Checkout Steps 1-3") %>% 
  select(Day, journey_name, `Renew Step 1.0 (ev71)`, 
         `Renew Step 2.0 (ev72)`, 
         `Renew Step 3.0 - Confirmation - Serialized (ev76)`) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

renew_fun_post_plot <- plot_ly() 
renew_fun_post_plot <- renew_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = renew_step_labels,
    x = renew_funnel_post, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
renew_fun_post_plot <- renew_fun_post_plot %>% layout(yaxis = list(categoryarray = renew_step_labels))

# Build the side by side funnel plot
renew_pre_post_funnel <- subplot(renew_fun_pre_plot, renew_fun_post_plot, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)

add_start_page_value_post <- journey_data %>% 
  filter(journey_name == "Membership: Renew or Join Us Page") %>% 
  select(Day, journey_name, Visits) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>%
  c(., recursive=TRUE)


# Build Post with a start Page
renew_funnel_post_start_values <- c()
renew_funnel_post_start_values <- c(add_start_page_value_post, renew_funnel_post[1])

renew_fun_post_plot_start <- plot_ly() 
renew_fun_post_plot_start <- renew_fun_post_plot_start %>%
  add_trace(
    type = "funnel",
    y = renew_step_labels_start,
    x = renew_funnel_post_start_values, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#824068"))
  )
renew_fun_post_plot_start <- renew_fun_post_plot_start %>% layout(yaxis = list(categoryarray = renew_step_labels_start))
#renew_fun_post_plot_start


# Build the side by side funnel plot for the funnel with the start page
renew_pre_post_start_funnel <- subplot(renew_fun_pre_plot_start, renew_fun_post_plot_start, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)

#=========================================================================================================

# Renew - Email Only Funnel

renew_step_labels <- c()
renew_step_labels <- c("Renew Step 2 - Details", 
                       "Renew Step 3 - Renewal Confirmation")


renew_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Email Renew Checkout Steps 2-3") %>% 
  select(Day, journey_name, 
         `Renew Step 2.0 (ev72)`, 
         `Renew Step 3.0 - Confirmation - Serialized (ev76)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

renew_fun_pre_plot <- plot_ly() 
renew_fun_pre_plot <- renew_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = renew_step_labels,
    x = renew_funnel_pre, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
renew_fun_pre_plot <- renew_fun_pre_plot %>% layout(yaxis = list(categoryarray = renew_step_labels))

# Renew Post Funnel Plot
renew_funnel_post <- journey_data %>% 
  filter(journey_name == "Commercial: Email Renew Checkout Steps 2-3") %>% 
  select(Day, journey_name, 
         `Renew Step 2.0 (ev72)`, 
         `Renew Step 3.0 - Confirmation - Serialized (ev76)`) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

renew_fun_post_plot <- plot_ly() 
renew_fun_post_plot <- renew_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = renew_step_labels,
    x = renew_funnel_post, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
renew_fun_post_plot <- renew_fun_post_plot %>% layout(yaxis = list(categoryarray = renew_step_labels))

# Build the side by side funnel plot
renew_pre_post_email_funnel <- subplot(renew_fun_pre_plot, renew_fun_post_plot, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)

# --------------------------------------------------------------------------------------------------------------------------

# Donate Pre Funnel Plots

donate_step_labels <- c("Donate Step 1 - Your Details", 
                       "Donate Step 2 - Donate Confirmation")
# Last run date need to be added
donate_funnel_pre <- journey_data %>% 
  filter(journey_name == "Commercial: Donate Checkout Steps 1-2") %>% 
  select(Day, journey_name, `Donate Step 1.0 (Serialized) (ev115)`,
                            `Donate Step 2.0 - Complete (Serialized) (ev116)`) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)


donate_fun_pre_plot <- plot_ly() 
donate_fun_pre_plot <- donate_fun_pre_plot %>%
  add_trace(
    type = "funnel",
    y = donate_step_labels,
    x = donate_funnel_pre, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
donate_fun_pre_plot <- donate_fun_pre_plot %>% layout(yaxis = list(categoryarray = donate_step_labels))



# Donate Post Funnel Plot
donate_funnel_post <- journey_data %>% 
  filter(journey_name == "Commercial: Donate Checkout Steps 1-2") %>% 
  select(Day, journey_name, `Donate Step 1.0 (Serialized) (ev115)`,
         `Donate Step 2.0 - Complete (Serialized) (ev116)`) %>%
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% arrange(Day) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  c(., recursive=TRUE)

donate_fun_post_plot <- plot_ly() 
donate_fun_post_plot <- donate_fun_post_plot %>%
  add_trace(
    type = "funnel",
    y = donate_step_labels,
    x = donate_funnel_post, 
    textposition = "inside",
    textinfo = "value+percent initial",
    opacity = 0.8,
    marker = list(color = c("#406882")))
donate_fun_post_plot <- donate_fun_post_plot %>% layout(yaxis = list(categoryarray = donate_step_labels))


# Build the side by side funnel plot
donate_pre_post_funnel <- subplot(donate_fun_pre_plot, donate_fun_post_plot, widths = c(0.5, 0.5), shareY = TRUE) %>% 
  layout(annotations = annotations, showlegend = FALSE)

#=========================================================================================================

# Anomaly Plots for each journey with Ribbon and Pre/Post separations
shop_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Shop Checkout Steps 1-4")
shop_revenue_plot_title <- "Shop Revenue Trended"
plot_journey_name <- shop_funnel$journey_name    # Get the journey name
anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'revenue')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
shop_revenue <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
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
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
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

# Membership Revenue and Sales ----------------------------------------------------------------------------------------------------
membership_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Membership Checkout Steps 1-4")
membership_revenue_plot_title <- "Membership Sales Revenue Trended"
plot_journey_name <- membership_funnel$journey_name    # Get the journey name

anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event5')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
membership_revenue <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
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
membership_revenue <- ggplotly(membership_revenue) %>%
  layout(title = list(text = paste0(membership_revenue_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))



# Membership Sales Trended
membership_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Membership Checkout Steps 1-4")
membership_orders_plot_title <- "Membership Sales Trended"
plot_journey_name <- membership_funnel$journey_name    # Get the journey name
anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event26')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
membership_orders <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
  labs(title = plot_journey_name,
       caption = paste0('There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.')) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  ylab("Sales") +
  xlab("Day") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  #scale_y_continuous(labels = dollar_format(prefix = "£")) +
  expand_limits(y=0)
membership_sales <- ggplotly(membership_orders) %>%
  layout(title = list(text = paste0(membership_orders_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))

# Holidays Revenue and Sales ----------------------------------------------------------------------------------------------------
holidays_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Holidays Checkout Steps 1-4")
holidays_revenue_plot_title <- "Holidays Sales Revenue Trended"
plot_journey_name <- holidays_funnel$journey_name    # Get the journey name

anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event125')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
holidays_revenue <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
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
holidays_revenue <- ggplotly(holidays_revenue) %>%
  layout(title = list(text = paste0(holidays_revenue_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))



# holidays Sales Trended
holidays_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Holidays Checkout Steps 1-4")
holidays_orders_plot_title <- "Holidays Sales Trended"
plot_journey_name <- holidays_funnel$journey_name    # Get the journey name
anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event134')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
holidays_orders <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
  labs(title = plot_journey_name,
       caption = paste0('There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.')) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  ylab("Sales") +
  xlab("Day") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  #scale_y_continuous(labels = dollar_format(prefix = "£")) +
  expand_limits(y=0)
holidays_sales <- ggplotly(holidays_orders) %>%
  layout(title = list(text = paste0(holidays_orders_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))


# Donate Revenue and Sales ----------------------------------------------------------------------------------------------------
donate_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Donate Checkout Steps 1-2")
donate_revenue_plot_title <- "Donate Sales Revenue Trended"
plot_journey_name <- donate_funnel$journey_name    # Get the journey name

anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event114')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
donate_revenue <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
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
donate_revenue <- ggplotly(donate_revenue) %>%
  layout(title = list(text = paste0(donate_revenue_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))



# donate Sales Trended
donate_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Donate Checkout Steps 1-2")
donate_orders_plot_title <- "Donations Trended"
plot_journey_name <- donate_funnel$journey_name    # Get the journey name
anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event116')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
donate_orders <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
  labs(title = plot_journey_name,
       caption = paste0('There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.')) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  ylab("Donations") +
  xlab("Day") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  #scale_y_continuous(labels = dollar_format(prefix = "£")) +
  expand_limits(y=0)
donate_sales <- ggplotly(donate_orders) %>%
  layout(title = list(text = paste0(donate_orders_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))



# Renewals Revenue and Sales ----------------------------------------------------------------------------------------------------
renew_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Renew Checkout Steps 1-3")
renew_revenue_plot_title <- "Renew Sales Revenue Trended"
plot_journey_name <- renew_funnel$journey_name    # Get the journey name

anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event79')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
renew_revenue <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
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
renew_revenue <- ggplotly(renew_revenue) %>%
  layout(title = list(text = paste0(renew_revenue_plot_title,
                                    '<br>',
                                    '<sup>',
                                    'There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.',
                                    '</sup>')))



# Renewals Sales Trended
renew_funnel <- journey_unique_names %>% filter(journey_name == "Commercial: Renew Checkout Steps 1-3")
renew_orders_plot_title <- "Renewals Trended"
plot_journey_name <- renew_funnel$journey_name    # Get the journey name
anomaly_subset <- anomaly_data %>% filter(journey_name == plot_journey_name & metric == 'event76')     # Subset the anomaly data for journey and metric
plot_metric_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(metric)                       # Get the metric name from the first row
renew_orders <- anomaly_subset %>% dplyr::filter(metric == plot_metric_name & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot(aes_string(x = "day")) +
  geom_line(aes_string( y = 'data'), color="#406882", size = 0.8) +
  geom_point(data = anomaly_data %>% dplyr::filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name),
             aes_string(y ='data'),color="#F05454", size = 1.8) +
  geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "#F05454", linetype='dotted', lwd = .8, alpha=0.5) +
  labs(title = plot_journey_name,
       caption = paste0('There are ',nrow(anomaly_data %>% filter(metric == plot_metric_name & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.')) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  ylab("Renewals") +
  xlab("Day") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  #scale_y_continuous(labels = dollar_format(prefix = "£")) +
  expand_limits(y=0)
renew_sales <- ggplotly(renew_orders) %>%
  layout(title = list(text = paste0(renew_orders_plot_title,
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


## Search Analysis
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

# search_terms_trended_flex_table <- search_term_data_main_site %>%
#   arrange(desc(search_term), day) %>%
#   group_by(search_term) %>%
#   add_count() %>%
#   select(search_term, searches) %>%
#   mutate(total = sum(searches)) %>%
#   select(search_term, total) %>%
#   arrange(desc(total)) %>%
#   distinct() %>%
#   ungroup() %>%
#   slice(1:100) %>%
#   kable(col.names = c("Search Term", "Total Searches")) %>%
#   column_spec(2, color = "white", bold = T, background = spec_color(1:100)) %>%
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>% 
#   scroll_box(width = "100%", height = "400px")



# 
# t1 <- search_term_data_main_site %>%
#   arrange(desc(search_term), day) %>%
#   group_by(search_term) %>%
#   add_count() %>%
#   select(search_term, searches) %>%
#   mutate(total = sum(searches)) %>%
#   select(search_term, total) %>%
#   arrange(desc(total)) %>%
#   distinct() %>%
#   ungroup() %>%
#   slice(1:20)
# 
# t2 <- search_term_data_main_site %>%
#   arrange(desc(search_term), day) %>%
#   group_by(search_term) %>%
#   add_count() %>%
#   select(search_term, searches) %>%
#   mutate(total = sum(searches)) %>%
#   select(search_term, total) %>%
#   arrange(desc(total)) %>%
#   distinct() %>%
#   ungroup() %>%
#   slice(21:40)
# 
# t3 <- search_term_data_main_site %>%
#   arrange(desc(search_term), day) %>%
#   group_by(search_term) %>%
#   add_count() %>%
#   select(search_term, searches) %>%
#   mutate(total = sum(searches)) %>%
#   select(search_term, total) %>%
#   arrange(desc(total)) %>%
#   distinct() %>%
#   ungroup() %>%
#   slice(41:60)
# 
# kable(t1) %>%
#   kable_styling(full_width = FALSE, position = "left")
# kable(t2) %>%
#   kable_styling(full_width = FALSE, position = "center")
# kable(t3) %>%
#   kable_styling(full_width = FALSE, position = "right")

# kable(t1) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "float_left")
# kable(t2) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "float_right")

#search_terms_trended_flex_table


# Google Search Console Plots and Data -----------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------
#scr_auth()

days <- seq(from=post_start_date_30, to=post_end_date, by='days')
days_count <- length(days)
search_console_datalist = list()
for ( i in seq_along(days)) {
  search_console_data  <- get_search_console_data(days[i], days[i])
  search_console_data$day <- days[i]
  search_console_datalist[[i]] <- search_console_data
  pc = round((i/days_count)*100, 0)
  print(paste0("Search Console data for : ", i, " of ", days_count, " - Completed: ",days[i], " ", " - Progress: ",pc,"%"))
}
search_console_data <- data.table::rbindlist(search_console_datalist, fill = TRUE)
search_console_data <-  na.omit(search_console_data)

# Add a column to identify brand terms
data_brand <- search_console_data %>% mutate(brand = case_when(grepl("national trust|national.trust", query) ~ 'brand', TRUE ~ 'nonbrand'))

branded_search <- data_brand %>% group_by(day, brand) %>%
  summarize(clicks = sum(clicks))

br <- ggplot(branded_search) +
  geom_line(aes(day, clicks, color = brand)) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_bw() +
  labs(
    x = "Day",
    y = "Clicks",
    title = "National Trust Brand vs. Non Brand Clicks",
    subtitle = "Last 90 Days"
  )

# NT Product Type Segmentation
search_products <- search_console_data %>%
  mutate(product_type = case_when(grepl("mem|membership|member", query) ~ 'Membership',
                                  grepl("donation|donate|support", query) ~ 'Donation',
                                  grepl("shop|shopping", query) ~ 'Shop',
                                  grepl("holiday|hols|holidays", query) ~ 'Holidays',
                                  TRUE ~ 'Other'),
         brand = case_when(grepl("national trust|national.trust", query) ~ 'brand', TRUE ~ 'nonbrand')) %>% drop_na()

product_clicks <- search_products %>%
  group_by(day, product_type) %>%
  summarize(clicks = sum(clicks)) %>%
  filter(product_type != 'Other') %>% # Exclude Other
  ggplot() +
  geom_line(aes(day, clicks, color = product_type), size = 0.7) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal() +
  labs(
    x = "Day",
    y = "Clicks",
    title = "National Trust Product Clicks",
    subtitle = "Last 90 Days",
    color = "Product Type"
  )

# Aggregate Product Metrics by Product Type
search_product_table <- search_products %>%
  group_by(product_type) %>%
  summarise(clicks = sum(clicks),
            impressions = sum(impressions),
            position = mean(position)) %>%
  mutate(ctr = 100 * (clicks / impressions)) %>%
  arrange(desc(product_type)) %>%
  kable(col.names = c("Product Type", "Clicks", "Impressions", "Position", "CTR")) %>%
  column_spec(5, color = "white", bold = T, background = spec_color(1:5)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

#search_product_table

brand_non_branded <- search_products %>%
  ggplot() +
  geom_histogram(aes(ctr, fill = brand), binwidth = 0.01) +   # ADD FILL = BRAND
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = comma_format()) +
  labs(x = 'Click-through Rate',
       y = 'Count') +
  theme_bw()

search_console_date <- search_analytics(siteURL = website,
                                        startDate = post_start_date,
                                        endDate = post_end_date,
                                        dimensions = 'date',
                                        searchType = type)
search_console_devices <- search_analytics(siteURL = website,
                                           startDate = post_start_date,
                                           endDate = post_end_date,
                                           dimensions = 'device',
                                           searchType = type)
search_console_pages <- search_analytics(siteURL = website,
                                         startDate = post_start_date,
                                         endDate = post_end_date,
                                         dimensions = 'page',
                                         searchType = type)

keyword_utilisation <- search_products %>%
  ggplot() +
  stat_ecdf(aes(ctr, color = brand)) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = 'Click-through Rate',
       y = 'Keyword %') +
  theme_bw()

# Visualise the top 5 terms
search_console_vis <- search_console_data %>%
  filter(day >= post_start_date & day <= post_end_date) %>%
  group_by(query) %>%
  add_count() %>% filter(n > 2) %>%
  filter(str_detect(query, 'mem')) %>%
  mutate(query = str_remove_all(query, "national trust | national trust")) %>%
  group_by(day)

search_terms_facet_grid <- ggplot(search_console_vis) +
  aes(x = day, y = clicks, colour = query) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(x = "Day", y = "Number of National Trust Google Searches Made", title = "Searches containing 'mem'",
       subtitle = "Any search on Google contains the word 'mem'.", fill = "Search Term",
       caption = "Google Search Console Data") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(query), scales = "free_y")

# Plot Click Through Rate by Above Search Terms
search_term_by_ctr <- ggplot(search_console_vis) +
  aes(x = day, y = ctr, colour = query) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(x = "Day", y = "CTR", title = "Search Console CTR", subtitle = "Click Through Rate by Term",
       caption = "Search Console", color = "Search Term") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(query), scales = "free")

# Plot Impressions by Above Search Terms
search_term_by_impressions <- ggplot(search_console_vis) +
  aes(x = day, y = impressions, colour = query) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(x = "Day", y = "Impressions", title = "Search Impressions", subtitle = "Impressions by Term",
       caption = "Search Console", color = "Search Term") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(query), scales = "free")

# Plot Position by Above Search Terms
search_term_by_position <- ggplot(search_console_vis) +
  aes(x = day, y = position, colour = query) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(x = "Day", y = "Position", title = "Search Position", subtitle = "Position by Term",
       caption = "Search Console", color = "Search Term") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(query), scales = "free")

# Line Graph Trend of the NT Clicks from Google
search_term_clicks <- ggplot(search_console_date) +
  aes(x = date, y = clicks) +
  geom_line(size = 0.6, colour = "#46337E") +
  labs(
    x = "Day",
    y = "Clicks",
    title = "National Trust Search Console Clicks",
    subtitle = "Last 90 Days"
  ) +
  theme_light()

# Bar Chart - Clicks by Device Typescipen=999
search_by_device <- ggplot(search_console_devices) +
  aes(x = device, fill = device, weight = clicks) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(x = "Device", y = "Clicks", title = "Google Clicks by Device Type", subtitle = "Google Search Console",
       fill = "Device") +
  theme_minimal()

# Get Page Data metrics and page names
page_date_range <- c(last_valid_date - 7, last_valid_date)
page_segment_ids <- "s1957_6113b90d5f900636dbd9b2ff"
page_search_criteria <- c("(CONTAINS 'M|')")
page_metrics <- c("visits", 
                  "pageviews", 
                  "visitors",
                  "cm1957_5ceff32dc50fd94a13c2cca4", 
                  "cm1957_565885a3e4b074a4bacd8fd0",
                  "cm_bouncerate_defaultmetric",
                  "cm1957_618a4fda709efa77430796e5",
                  "cm_exit_rate_defaultmetric"
)

page_data <- get_page_data(page_segment_ids, page_metrics, page_date_range, page_search_criteria)

# Marketing Channels Data Pull and Clean up
marketing_channels <- get_marketing_channel_metrics()

marketing_channels_by_day <- marketing_channels %>% # Includes Daily breakdown
  rename(all_revenue = cm1957_5fc4b9a2b5895e0644b9120e) %>% 
  group_by(week = week(daterangeday-1), marketingchannel)

marketing_week <- marketing_channels_by_day %>% ungroup() %>% 
  select(daterangeday, week) %>% 
  group_by(week) %>% 
  mutate(week_start_sunday = min(daterangeday),
         week_end_saturday = max(daterangeday)) %>% 
  select(-daterangeday) %>% distinct() %>% 
  arrange(week) %>% 
  mutate(days_between = (week_end_saturday - week_start_sunday)+1) %>% 
  filter(days_between == "7") %>% 
  mutate(date_text = paste0(format(week_start_sunday,"%d-%b"),' to ',format(week_end_saturday, "%d-%b"))) %>% 
  select(week, date_text)

marketing_channels_by_week_channel <- marketing_channels_by_day %>% select(-daterangeday) %>% #Summarized by work week and channel name
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  right_join(marketing_week, by = 'week') %>% 
  rename("Week Num" = week) %>% 
  rename("Marketing Channel" = marketingchannel) %>% 
  rename("Visits" = visits) %>% 
  rename("Mem. Rev." = event5) %>% 
  rename("Shop Rev." = revenue) %>% 
  rename("Renew Rev." = event79) %>% 
  rename("Holiday Rev." = event125) %>% 
  rename("Donate Rev." = event114) %>% 
  rename("Total Rev." = all_revenue) %>% 
  rename("Date" = date_text) %>% 
  relocate("Date", .before = "Week Num")

paid_search_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Paid Search") %>% 
  arrange(`Week Num`) 
  
paid_social_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Paid Social") %>% 
  arrange(`Week Num`)

affiliates_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Affiliates") %>% 
  arrange(`Week Num`)

natural_search_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Natural Search") %>% 
  arrange(`Week Num`)

social_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Social") %>% 
  arrange(`Week Num`)

direct_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Direct") %>% 
  arrange(`Week Num`)

email_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Email") %>% 
  arrange(`Week Num`)

websites_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Other Websites") %>% 
  arrange(`Week Num`)

mobile_app_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Mobile App") %>% 
  arrange(`Week Num`)

print_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Print") %>% 
  arrange(`Week Num`)

display_last_week <- marketing_channels_by_week_channel %>% 
  filter(`Marketing Channel` == "Display") %>% 
  arrange(`Week Num`)

# Voice of the Supporter Table
voice_of_the_supporter_googlesheet$Date <- as.Date(voice_of_the_supporter_googlesheet$Date)
vos_comments <- voice_of_the_supporter_googlesheet %>% select(Date, Category, Feedback) %>% arrange(desc(Date))