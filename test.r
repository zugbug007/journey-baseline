library(adobeanalyticsr)
df <- adobeanalyticsr::aw_anomaly_report(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                   date_range = date_range,
                                   metrics = "event179",
                                   segmentId = "s1957_61f29c94872031367baf6ed9",
                                   granularity = "day",
                                   quickView = FALSE,
                                   anomalyDetection = TRUE,
                                   countRepeatInstances = TRUE,
                                   debug = FALSE)


#get_anomaly_data <- function(segment_ids, metrics, data_range)
