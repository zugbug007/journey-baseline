
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                The Big Loop                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Update the Data
if (file.exists("output/df_journey_data.rds")) {
  journey_data <- readRDS("output/df_journey_data.rds")
} 

if (file.exists("output/df_anomaly_data.rds")) {
  anomaly_data <- readRDS("output/df_anomaly_data.rds")
} 

if (file.exists("output/df_events.rds")) {
  df_events <- readRDS("output/df_events.rds")
} 

if (file.exists("output/df_events_daily.rds")) {
  df_events_daily <- readRDS("output/df_events_daily.rds")
}

journey_datalist = list()
anomaly_datalist = list()
anomaly_events_datalist = list()
anomaly_data_events <- data.frame()
anomaly_data_output <- data.frame()

# Testing
#journey_data <- readRDS("output/df_journey_data.rds")
#anomaly_data <- readRDS("output/df_anomaly_data.rds")

# Setup for Adobe Data
#aw_metrics <- aw_get_metrics()
#aw_dims <- aw_get_dimensions()
#aw_reportsuites <- aw_get_reportsuites()
#aw_calculatedmetrics <- aw_get_calculatedmetrics()
#aw_segments <- aw_get_segments()

last_valid_date <- c()
first_valid_date <- c()

if(exists("journey_data")) {
  last_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange(desc(Day)) %>% slice(1:1) %>% pull(Day))
  first_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange((Day)) %>% slice(1:1) %>% pull(Day))
  date_diff <- as.double(Sys.Date()-last_valid_date)
  
  if(date_diff > max_days) {
  message(paste("Last update was more than",max_days,"days ago - running delta update"))
  
  last_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange(desc(Day)) %>% slice(1:1) %>% pull(Day))
  first_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange((Day)) %>% slice(1:1) %>% pull(Day))
  
  last_valid_date_anomaly <- as.Date(anomaly_data %>% select(day) %>% arrange(desc(day)) %>% slice(1:1) %>% pull(day))
  first_valid_date_anomly <- as.Date(anomaly_data %>% select(day) %>% arrange((day)) %>% slice(1:1) %>% pull(day))
  
  date_range <- c(as.Date(last_valid_date+1), as.Date(Sys.Date() - 1))
  date_range
  anomaly_date_range <- c(as.Date(last_valid_date_anomaly+1),as.Date(Sys.Date()-1))
  anomaly_date_range
  update_type <- "Delta" # DELTA- update since last last refresh (Faster)
  
  # Backup and copy before emptying object
  journey_data_exists <- journey_data
  journey_data <- NULL
  
  anomaly_data_exists <- anomaly_data
  anomaly_data <- NULL
  
  post_journey_segments <- journey_segments %>% filter(journey_type == "post")
  post_journey_count <- nrow(post_journey_segments %>% filter(journey_type == "post"))
  
  for (i in 1:nrow(post_journey_segments)) {
    
    # Build list of metrics from row item (metric group) in the config table
    metrics_list <- journey_metrics_googlesheet %>% 
      dplyr::select(metric_group_id, id) %>% 
      filter(metric_group_id == post_journey_segments$metric_group[i]) %>% 
      pull(id)
    
    # Get the metric group number for later evaluation to determine if additional anomaly metrics are required.
    metrics_group_no <- journey_metrics_googlesheet %>% 
      dplyr::select(metric_group_id, id) %>% 
      filter(metric_group_id == post_journey_segments$metric_group[i]) %>% 
      slice(1:1) %>% pull(metric_group_id)
  
    
    # add the proxy metrics if the metric group is not group 1
    if (!'visits' %in% metrics_list) {
      metrics_list <- append(metrics_list, 'visits', after = 0)
      if (!'pageviews' %in% metrics_list){
        metrics_list <- append(metrics_list, 'pageviews', after = 1)
        if(!'visitors' %in% metrics_list){
          metrics_list <- append(metrics_list, 'visitors', after = 2)
        }
      }
    }
    
    
    segment_group = c()
    segment_group <- as.character(post_journey_segments$id[i])               # Pass the segment id of the first journey
    if(!is.na(post_journey_segments$secondary_segment_id[i])){               # This statement looks to see if a second segment is required to be applied
      secondary_seg <- post_journey_segments$secondary_segment_id[i]         # to the journey, if it is add it to the group to be sent to the data request
      secondary_seg_name <- post_journey_segments$secondary_segment_name[i]  # add name of the segment for readability
      segment_group <- c(segment_group, secondary_seg)                  # segments are AND'ed together as if they were added to the header of an Analysis Workspace panel. 
    }
    
    if(!is.na(post_journey_segments$third_segment_id[i])){                   # This statement looks to see if a third segment is required to be applied
      third_seg <- post_journey_segments$third_segment_id[i]                 # to the journey, if it is add it to the group to be sent to the data request
      third_seg_name <- post_journey_segments$third_segment_name[i]
      segment_group <- c(segment_group, third_seg)                      # segments are AND'ed together as if they were added to the header of an Analysis Workspace panel. 
    }
    
    segment_group <- c(segment_group)
    journey_data <- get_segment_data(segment_group, metrics_list, date_range)
    journey_data$journey_type <- post_journey_segments$journey_type[i]
    journey_data$journey_name <- post_journey_segments$journey_name[i]
    journey_data$journey_desc <- post_journey_segments$journey_desc[i]
    journey_data$segment_name_adobe <- post_journey_segments$segment_name_adobe[i]
    journey_data$include_level <- post_journey_segments$include_level[i]
    journey_data$category <- post_journey_segments$category[i]
    journey_data$sub_category <- post_journey_segments$sub_category[i]
    journey_data$journey_applied <- segment_group[1]
    journey_data$secondary_segment_1_name <- post_journey_segments$secondary_segment_name[i]
    journey_data$secondary_segment_2_name <- post_journey_segments$third_segment_name[i]
    journey_data$pre_only <- post_journey_segments$pre_only[i]
    journey_data$post_only <- post_journey_segments$post_only[i]
    journey_data$update_type <- update_type
    
    metrics_list <- journey_metrics_googlesheet %>% 
      dplyr::select(metric_group_id, id) %>% 
      filter(metric_group_id == post_journey_segments$metric_group[i]) %>% 
      pull(id)
    
    # Pull anomaly data set for visit metric
    # Each journey gets the anomaly data against visits
    anomaly_data <- get_anomaly_data(segment_group, "visits", anomaly_date_range)
    
    anomaly_data$journey_type <- post_journey_segments$journey_type[i]
    anomaly_data$journey_name <- post_journey_segments$journey_name[i]
    anomaly_data$journey_desc <- post_journey_segments$journey_desc[i]
    anomaly_data$category <- post_journey_segments$category[i]
    anomaly_data$sub_category <- post_journey_segments$sub_category[i]
    anomaly_data$journey_applied <- segment_group[1]
    anomaly_data$secondary_segment_1_name <- post_journey_segments$secondary_segment_name[i]
    anomaly_data$secondary_segment_2_name <- post_journey_segments$third_segment_name[i]
    
    if(metrics_group_no >= 5) {
      for(j in seq_along(metrics_list)) {
        anomaly_events_datalist = list()
        anomaly_data_events <- get_anomaly_data(segment_group, metrics_list[j], date_range)
        anomaly_data_events$journey_type <- post_journey_segments$journey_type[i]
        anomaly_data_events$journey_name <- post_journey_segments$journey_name[i]
        anomaly_data_events$journey_desc <- post_journey_segments$journey_desc[i]
        anomaly_data_events$category <- post_journey_segments$category[i]
        anomaly_data_events$sub_category <- post_journey_segments$sub_category[i]
        anomaly_data_events$journey_applied <- segment_group[1]
        anomaly_data_events$secondary_segment_1_name <- post_journey_segments$secondary_segment_name[i]
        anomaly_data_events$secondary_segment_2_name <- post_journey_segments$third_segment_name[i]
        anomaly_data_output <- rbind(anomaly_data_output, anomaly_data_events)
      }
      anomaly_events_datalist[[j]] <- anomaly_data_output
    }
    
    # Calculations for Daily Averages for Visit Data
    #journey_data <- calculate_means(journey_data)
    journey_datalist[[i]] <- journey_data # if this statement fails, something above this did not work :)
    anomaly_events_datalist[[i]] <- anomaly_data_events
    anomaly_datalist[[i]] <- anomaly_data
    
    segment_group <- NULL                           # Clear the segment group at the end of the loop
    metrics_list <- NULL
    message(paste0("Journey ", i, " of ", post_journey_count, " Completed: ",post_journey_segments$journey_name[i], " Type: ", post_journey_segments$journey_type[i]))
    print(paste0("Journey ", i, " of ", post_journey_count, " Completed: ",post_journey_segments$journey_name[i], " Type: ", post_journey_segments$journey_type[i]))
    
  }
  
  journey_new_data <- data.table::rbindlist(journey_datalist, fill = TRUE)
  table_list <- list(journey_data_exists, journey_new_data)
  journey_data <- data.table::rbindlist(table_list, fill = TRUE)
  
  
  anomaly_data_new <- data.table::rbindlist(anomaly_datalist, fill = TRUE)
  anomaly_table_list <- list(anomaly_data_exists, anomaly_data_new)
  anomaly_data <- data.table::rbindlist(anomaly_table_list, fill = TRUE)
  }
}


if(!exists("journey_data")) {
message("No Data exists - Running Full Data Update")
update_type <- "Full" # FULL for all journey data in time frame (Slower).

journey_count <- nrow(journey_segments)

message("Journey & Anomaly processing starting...")
for (i in 1:nrow(journey_segments)) {
  # Build list of metrics from row item (metric group) in the config table
  metrics_list <- journey_metrics_googlesheet %>% 
    dplyr::select(metric_group_id, id) %>% 
    filter(metric_group_id == journey_segments$metric_group[i]) %>% 
    pull(id)
  # Get the metric group number for later evaluation to determine if additional anomaly metrics are required.
  metrics_group_no <- journey_metrics_googlesheet %>% 
    dplyr::select(metric_group_id, id) %>% 
    filter(metric_group_id == journey_segments$metric_group[i]) %>% 
    slice(1:1) %>% pull(metric_group_id)
  
  # add the proxy metrics if the metric group is not group 1
  if (!'visits' %in% metrics_list) {
    metrics_list <- append(metrics_list, 'visits', after = 0)
    if (!'pageviews' %in% metrics_list){
      metrics_list <- append(metrics_list, 'pageviews', after = 1)
      if(!'visitors' %in% metrics_list){
        metrics_list <- append(metrics_list, 'visitors', after = 2)
      }
    }
  }
  # Select the correct date range for the journey. 
  # Get pre/post value from the row item in the config table and select the correct one to be fed into the data pull request.
  if(journey_segments$journey_type[i] == "pre") {
    date_range <- pre_date_range  
  }
  if(journey_segments$journey_type[i] =="post") {
    date_range <- post_date_range
  }
  segment_group = c()
  segment_group <- as.character(journey_segments$id[i])               # Pass the segment id of the first journey
  if(!is.na(journey_segments$secondary_segment_id[i])){               # This statement looks to see if a second segment is required to be applied
    secondary_seg <- journey_segments$secondary_segment_id[i]         # to the journey, if it is add it to the group to be sent to the data request
    secondary_seg_name <- journey_segments$secondary_segment_name[i]  # add name of the segment for readability
    segment_group <- c(segment_group, secondary_seg)                  # segments are AND'ed together as if they were added to the header of an Analysis Workspace panel. 
  }
  
  if(!is.na(journey_segments$third_segment_id[i])){                   # This statement looks to see if a third segment is required to be applied
    third_seg <- journey_segments$third_segment_id[i]                 # to the journey, if it is add it to the group to be sent to the data request
    third_seg_name <- journey_segments$third_segment_name[i]
    segment_group <- c(segment_group, third_seg)                      # segments are AND'ed together as if they were added to the header of an Analysis Workspace panel. 
  }
  
  segment_group <- c(segment_group)
  journey_data <- get_segment_data(segment_group, metrics_list, date_range)
  journey_data$journey_type <- journey_segments$journey_type[i]
  journey_data$journey_name <- journey_segments$journey_name[i]
  journey_data$journey_desc <- journey_segments$journey_desc[i]
  journey_data$segment_name_adobe <- journey_segments$segment_name_adobe[i]
  journey_data$include_level <- journey_segments$include_level[i]
  journey_data$category <- journey_segments$category[i]
  journey_data$sub_category <- journey_segments$sub_category[i]
  journey_data$journey_applied <- segment_group[1]
  journey_data$secondary_segment_1_name <- journey_segments$secondary_segment_name[i]
  journey_data$secondary_segment_2_name <- journey_segments$third_segment_name[i]
  journey_data$pre_only <- journey_segments$pre_only[i]
  journey_data$post_only <- journey_segments$post_only[i]
  journey_data$update_type <- update_type
  
  metrics_list <- journey_metrics_googlesheet %>% 
    dplyr::select(metric_group_id, id) %>% 
    filter(metric_group_id == journey_segments$metric_group[i]) %>% 
    pull(id)
  # Pull anomaly data set for visit metric
  # Each journey gets the anomaly data against visits
  anomaly_data <- get_anomaly_data(segment_group, "visits", date_range)
  anomaly_data$journey_type <- journey_segments$journey_type[i]
  anomaly_data$journey_name <- journey_segments$journey_name[i]
  anomaly_data$journey_desc <- journey_segments$journey_desc[i]
  anomaly_data$category <- journey_segments$category[i]
  anomaly_data$sub_category <- journey_segments$sub_category[i]
  anomaly_data$journey_applied <- segment_group[1]
  anomaly_data$secondary_segment_1_name <- journey_segments$secondary_segment_name[i]
  anomaly_data$secondary_segment_2_name <- journey_segments$third_segment_name[i]
  
  # If the metric group is greater than 5 because it is a commercial journey where there are more metrics required,
  # Check if the journey is one of these. If so, loop though each metric and pull the data one event at a time.
  # This is a an API limitation
  
  if(metrics_group_no >= 5) {
    for(j in seq_along(metrics_list)) {
      anomaly_events_datalist = list()
      anomaly_data_events <- get_anomaly_data(segment_group, metrics_list[j], date_range)
      anomaly_data_events$journey_type <- journey_segments$journey_type[i]
      anomaly_data_events$journey_name <- journey_segments$journey_name[i]
      anomaly_data_events$journey_desc <- journey_segments$journey_desc[i]
      anomaly_data_events$category <- journey_segments$category[i]
      anomaly_data_events$sub_category <- journey_segments$sub_category[i]
      anomaly_data_events$journey_applied <- segment_group[1]
      anomaly_data_events$secondary_segment_1_name <- journey_segments$secondary_segment_name[i]
      anomaly_data_events$secondary_segment_2_name <- journey_segments$third_segment_name[i]
      anomaly_data_output <- rbind(anomaly_data_output, anomaly_data_events)
    }
    anomaly_events_datalist[[j]] <- anomaly_data_output
  }
  
  # Calculations for Daily Averages for Visit Data
  #journey_data <- calculate_means(journey_data)
  journey_datalist[[i]] <- journey_data # if this statement fails, something above this did not work :)
  anomaly_events_datalist[[i]] <- anomaly_data_events
  anomaly_datalist[[i]] <- anomaly_data
  
  segment_group <- NULL                           # Clear the segment group at the end of the loop
  metrics_list <- NULL
  message(paste0("Journey ", i, " of ", journey_count, " Completed: ",journey_segments$journey_name[i], " Type: ", journey_segments$journey_type[i]))
  print(paste0("Journey ", i, " of ", journey_count, " Completed: ",journey_segments$journey_name[i], " Type: ", journey_segments$journey_type[i]))
  
}

journey_data <- data.table::rbindlist(journey_datalist, fill = TRUE)
anomaly_data <- data.table::rbindlist(anomaly_datalist, fill = TRUE)
anomaly_data <- rbind(anomaly_data,anomaly_data_output)
}
# Write out to a .rds for use if the script craps out and put a message to the console
write_rds(journey_data, "output/df_journey_data.rds")
write_rds(anomaly_data, "output/df_anomaly_data.rds")
rm(anomaly_datalist)
rm(journey_datalist)
rm(anomaly_events_datalist)
rm(anomaly_data_events)
rm(anomaly_data_exists)
rm(anomaly_data_new)
rm(anomaly_data_output)
rm(journey_data_exists)
rm(journey_new_data)
rm(table_list)
rm(anomaly_table_list)
message("Journey & Anomaly processing completed.")
print("Journey & Anomaly processing completed.")

# POST PROCESSING
# Process the journey data however it got loaded into the environment.
# Create the metrics and variables required for the next steps of processing pipeline.

journey_unique_names <- journey_segments %>% filter(category != 'skip_summary') %>%  select(journey_name) %>% distinct()
journey_unique_count <- nrow(journey_unique_names)
time.points <- as.Date(seq(pre_start_date, post_end_date, by = "day"))

# Organise column order for data clarity
journey_data <- journey_data %>%        
  relocate(journey_name, .after = Day) %>% 
  relocate(journey_type, .before = journey_name) %>%
  relocate(journey_desc, .after = journey_name) %>% 
  relocate(journey_applied, .after =journey_desc) %>%
  relocate(category, .after = journey_applied) %>%
  relocate(sub_category, .after = category) %>% 
  relocate(secondary_segment_1_name, .after = journey_applied) %>%
  relocate(secondary_segment_2_name, .after = secondary_segment_1_name)

anomaly_data <- anomaly_data %>% mutate(dataAnomalyDetected_0_1 = case_when(dataAnomalyDetected =="TRUE" ~ 1, 
                                                                            dataAnomalyDetected == "FALSE" ~ 0))
anomaly_data$anomalies_good <- ifelse(anomaly_data$dataAnomalyDetected == "TRUE",
                                      ifelse(anomaly_data$data > anomaly_data$dataUpperBound, 1, 0), 0)

anomaly_data$anomalies_bad <- ifelse(anomaly_data$dataAnomalyDetected == "TRUE",
                                     ifelse(anomaly_data$data < anomaly_data$dataLowerBound,  1, 0), 0)

# Check the data validity
# Find the last date in the data set and calculate back the 3, 7, 14 day baselines.
# Find the last valid date in the dataset and work back from there
last_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange(desc(Day)) %>% slice(1:1) %>% pull(Day))
first_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange((Day)) %>% slice(1:1) %>% pull(Day))

baseline_days_count <- as.double(difftime(last_valid_date,last_valid_date-30, units = "days"))

today <- last_valid_date
yesterday <- last_valid_date - 1
three_days_ago <- last_valid_date - 3
seven_days_ago <- last_valid_date - 7
fourteen_days_ago <- last_valid_date - 14


# journey_data <- journey_data %>% mutate(post_only = case_when(post_only == 0 ~ "FALSE",
#                                                               post_only == 1 ~ "TRUE"))

