dat <- aw_anomaly_report(
  date_range = date_range,
  metrics = "pageviews",
  granularity = "day",
  segmentId = NA,
  quickView = TRUE,
  anomalyDetection = TRUE,
  countRepeatInstances = TRUE,
  debug = FALSE
)


anomaly_data <- aw_anomaly_report(
  date_range = date_range,
  metrics = "pageviews",
  granularity = "day",
  segmentId = NA,
  quickView = FALSE,
  anomalyDetection = TRUE,
  countRepeatInstances = TRUE,
  debug = FALSE
)

get_anomaly_report1 <- get_anomaly_report %>% tidyr::pivot_wider(names_from = 'metric', values_from = c('data', 'dataExpected', 'dataUpperBound', 'dataLowerBound', 'dataAnomalyDetected'))
#get_anomaly_report1 <- get_anomaly_report %>% tidyr::spread(key = 'metric', value ='data')


get_anom_report <-   adobeanalyticsr::aw_anomaly_report(date_range = date_range,
                                                        metrics = metrics_list,
                                                        segmentId = segment_ids,
                                                        granularity = "day",
                                                        quickView = TRUE,
                                                        anomalyDetection = TRUE,
                                                        countRepeatInstances = TRUE,
                                                        debug = FALSE
                                                        

                                                        