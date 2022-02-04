##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            Function Call Setup                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_segment_data <- function(segment_ids, metrics, date_range) {
  adobeanalyticsr::aw_freeform_table(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                     date_range = date_range,
                                     dimensions = "daterangeday",
                                     segmentId = segment_ids, # catch passed comma separated vector segment IDs group to pull data against. 
                                     prettynames = TRUE,      # Don't change this as many following variables names depend on this naming
                                     metrics = metrics,       # catch the comma separated vector group of metrics specified for this journey
                                     debug = FALSE
  )
  
}

get_anomaly_data <- function(segment_ids, metrics, data_range){
  adobeanalyticsr::aw_anomaly_report(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                     date_range = date_range,
                                     metrics = "visits",
                                     segmentId = segment_ids,
                                     granularity = "day",
                                     quickView = FALSE,
                                     anomalyDetection = TRUE,
                                     countRepeatInstances = TRUE,
                                     debug = FALSE)
}

calculate_means <- function(journey_data) {
  journey_data %>% arrange(desc(Day)) %>%
    mutate(visit_change_day_pct_chg = ifelse(lead(Visits) < 1, 0, signif((((Visits))/lead(Visits))-1, 2))) %>%                          # Calculation creates new column and checks that value to be divided by is not 0, 
    mutate(visits_change_day_pct_chg = ifelse(lead(`Page Views`) < 1, 0, signif((((`Page Views`))/lead(`Page Views`))-1, 2))) %>%    # if zero then calculation result is set to 0 then rounds to 2 D.P.
    mutate(uniquevisitors_change_day_pct_chg = ifelse(lead(`Unique Visitors`) < 1, 0, signif((((`Unique Visitors`))/lead(`Unique Visitors`))-1, 2))) %>%
    mutate(visits_3_DA = round(zoo::rollmean(Visits, k = 3, fill = NA, align ='left'), digits = 0), # Calculate Daily Averages from main metrics data.
           visits_7_DA = round(zoo::rollmean(Visits, k = 7, fill = NA, align ='left'), digits = 0),
           visits_14_DA = round(zoo::rollmean(Visits, k = 14, fill = NA, align ='left'), digits = 0),
           visits_30_DA = round(zoo::rollmean(Visits, k = 30, fill = NA, align ='left'), digits = 0),
           visits_60_DA = round(zoo::rollmean(Visits, k = 60, fill = NA, align ='left'), digits = 0),
           visits_90_DA = round(zoo::rollmean(Visits, k = 90, fill = NA, align ='left'), digits = 0))  %>% ungroup()
}

get_events_daily <- function(eventids) {
  events_data <- aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                    rsid = rsid, 
                    date_range = date_range,
                    dimensions = "daterangeday",
                    metrics = c("event1","event2")) %>% # change to event ids after testing
                    pivot_longer(-daterangeday) %>%
                    arrange(daterangeday, name) %>% 
                    select(day = daterangeday, 
                    id = name, value)
}

journey_datalist = list()
anomaly_datalist = list()
journey_count <- nrow(journey_segments)

journey_unique_names <- journey_segments %>% select(journey_name) %>% distinct()
journey_unique_count <- nrow(journey_unique_names)
time.points <- as.Date(seq(pre_start_date, post_end_date, by = "day"))
