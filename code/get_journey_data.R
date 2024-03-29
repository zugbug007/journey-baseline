
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                The Big Loop                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

journey_datalist = list()
anomaly_datalist = list()
anomaly_events_datalist = list()
anomaly_data_events <- data.frame()
anomaly_data_output <- data.frame()
journey_count <- nrow(journey_segments)
anomaly_data <- NULL

# Setup for Adobe Data
aw_metrics <- aw_get_metrics()
aw_dims <- aw_get_dimensions()
aw_reportsuites <- aw_get_reportsuites()
aw_calculatedmetrics <- aw_get_calculatedmetrics()
aw_segments <- aw_get_segments()

# Delete the line below for run
#i = 2
# ##########
 
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
  journey_data$category <- journey_segments$category[i]
  journey_data$sub_category <- journey_segments$sub_category[i]
  journey_data$journey_applied <- segment_group[1]
  journey_data$secondary_segment_1_name <- journey_segments$secondary_segment_name[i]
  journey_data$secondary_segment_2_name <- journey_segments$third_segment_name[i]
  
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
  journey_datalist[[i]] <- journey_data # if this statement fails, something above this did not work.
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

# Write out to a .csv for use if the script craps out and put a message to the console
write_rds(journey_data, "output/df_journey_data.rds")
write_rds(anomaly_data, "output/df_anomaly_data.rds")
rm(anomaly_datalist)
rm(journey_datalist)
rm(anomaly_events_datalist)
rm(anomaly_data_events)
rm(anomaly_data_output)
message("Journey & Anomaly processing completed.")
print("Journey & Anomaly processing completed.")

