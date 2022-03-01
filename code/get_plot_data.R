##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             Build the Daily Baseline Journey Changes Table          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pre_baseline <- journey_data %>% filter(journey_type=="pre") %>% 
  group_by(journey_name) %>% 
  summarise(across(c(`Visits`,`Page Views`, `Unique Visitors`, `Bounces`,`Average Time Spent on Site (seconds)` , `% New Visits`,	`% Repeat Visits`,	`New Visits`,	`Repeat Visits`), list(mean = mean), .names = "{.col}_{.fn}")) %>% 
  add_column(journey_type = "pre", .after = "journey_name") %>% 
  mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>% 
  mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0))


post_baseline <- journey_data %>% filter(journey_type=="post") %>% 
  group_by(journey_name) %>% 
  summarise(across(c(`Visits`,`Page Views`, `Unique Visitors`, `Bounces`,`Average Time Spent on Site (seconds)` ,`% New Visits`,	`% Repeat Visits`,	`New Visits`,	`Repeat Visits`), list(mean = mean), .names = "{.col}_{.fn}")) %>% 
  add_column(journey_type = "post", .after = "journey_name") %>% 
  mutate(`% New Visits_mean` = percent(`% New Visits_mean`, accuracy = 0.1)) %>% 
  mutate(`% Repeat Visits_mean` = percent(`% Repeat Visits_mean`, accuracy = 0.1)) %>% mutate(across(where(is.numeric), round, 0))

baseline <- rbind(pre_baseline, post_baseline)
baseline_flex_table <- baseline %>% arrange(journey_name) %>% 
  kable(col.names = c("Journey Name", "Journey Type", "Visits Avg.", "Page Views Avg.", "UV Avg.", "Bounces Avg.", "Avg. Time on Site", "Avg. % New Visits", "Avg. % Repeat Visits", "Avg New Visits", "Avg. Repeat Visits")) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"))


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

compare_yesterday <- compare_day %>% filter(Day == post_end_date & journey_type == "post") %>%
  mutate(visits_yest = mean(Visits)) %>% select(journey_name, visits_yest) %>% distinct()

compare_3_day <- compare_day %>% filter(Day >= post_start_date_3 & Day < post_end_date & journey_type == "post") %>%
  mutate(visits_3_da = mean(Visits)) %>% select(journey_name, visits_3_da) %>% distinct()

compare_7_day <- compare_day %>% filter(Day >= post_start_date_7 & Day < post_end_date & journey_type == "post") %>%
  mutate(visits_7_da = mean(Visits)) %>% select(journey_name, visits_7_da) %>% distinct()

compare_14_day <- compare_day %>% filter(Day >= post_start_date_14 & Day < post_end_date & journey_type == "post") %>%
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

anomaly_count <- anomaly_data %>% filter(dataAnomalyDetected == T & journey_type == "post") %>% count(journey_name, journey_desc, dataAnomalyDetected) %>% 
  select(-dataAnomalyDetected) %>% arrange(desc(n)) %>% 
  kable(col.names = c("Journey Name", "Journey Description", "No. of Anomalies (Post Launch)")) %>% kable_styling(bootstrap_options = c("striped", "condensed", full_width = F)) #%>% scroll_box(width = "100%", height = "400px")


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

