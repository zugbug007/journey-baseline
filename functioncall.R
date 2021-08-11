get_segment_data <- function(segment_id) {
  adobeanalyticsr::aw_freeform_table(date_range = date_range,
                    dimensions = "daterangeday",
                    segmentId = segment_id, # Segment Name: Membership Completion
                    prettynames = TRUE,
                    metrics = "visits")
  
}
  

calculate_means <- function(journey_data) {
  journey_data %>% arrange(desc(Day)) %>%
    mutate(Visits_3_DA = zoo::rollmean(Visits, k = 3, fill = NA, align ='right'), 
           Visits_7_DA = zoo::rollmean(Visits, k = 7, fill = NA, align ='right'), 
           Visits_14_DA = zoo::rollmean(Visits, k = 14, fill = NA, align ='right'),
            Visits_30_DA = zoo::rollmean(Visits, k = 30, fill = NA, align ='right'),
           Visits_60_DA = zoo::rollmean(Visits, k = 60, fill = NA, align ='right'),
           Visits_90_DA = zoo::rollmean(Visits, k = 90, fill = NA, align ='right'))  %>%
    mutate(Visit_change_day_pct_chg = ((Visits - lag(Visits)))/lag(Visits)) %>% ungroup()
}
