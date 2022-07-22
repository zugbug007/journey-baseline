
# Process the journey data however it got loaded into the environment.
# Create the metrics and variables required for the next steps of processing pipeline.

journey_unique_names <- journey_segments %>% select(journey_name) %>% distinct()
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

anomaly_data <- anomaly_data %>% mutate(dataAnomalyDetected_0_1 = case_when(dataAnomalyDetected =="TRUE" ~ 1, dataAnomalyDetected == "FALSE" ~ 0))
anomaly_data$anomalies_good <- ifelse(anomaly_data$dataAnomalyDetected == "TRUE",
                                      ifelse(anomaly_data$data > anomaly_data$dataUpperBound, 1, 0), 0)

anomaly_data$anomalies_bad <- ifelse(anomaly_data$dataAnomalyDetected == "TRUE",
                                     ifelse(anomaly_data$data < anomaly_data$dataLowerBound,  1, 0), 0)


# Check the data validity
# Find the last date in the data set and calculate back the 3, 7, 14 day baselines.
# Find the last valid date in the dataset and work back from there
last_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange(desc(Day)) %>% slice(1:1) %>% pull(Day))
first_valid_date <- as.Date(journey_data %>% select(Day) %>% arrange((Day)) %>% slice(1:1) %>% pull(Day))

today <- last_valid_date
yesterday <- last_valid_date - 1
three_days_ago <- last_valid_date - 3
seven_days_ago <- last_valid_date - 7
fourteen_days_ago <- last_valid_date - 14

