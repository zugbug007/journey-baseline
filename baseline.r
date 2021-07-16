library(adobeanalyticsr)
library(tidyverse)
library(kableExtra)
library(zoo)
#Test Token has been refreshed and is upto date.
#aw_token()
rm(list = ls()) # Clear the environment before running the code
monitorStartTime_baseline <- Sys.time()
#delete the aa.auth file in WD if issues

#Set Date range for Adobe Data pull below

#Last x days starting from yesterday
date_range = c(Sys.Date() - 90, Sys.Date() - 1)

#Specific Start Date, End Date of yesterday.
#date_range = c(as.Date("2021-06-17"), Sys.Date() - 1)

# Setup for Adobe Data
aw_metrics <- aw_get_metrics()
aw_dims <- aw_get_dimensions()
aw_reportsuites <- aw_get_reportsuites()
aw_calculatedmetrics <- aw_get_calculatedmetrics()
 aw_segments <- aw_get_segments(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsids = Sys.getenv("AW_REPORTSUITE_ID"),
  segmentFilter = NA,
  name = NA,
  tagNames = NA,
  filterByPublishedSegments = "all",
  limit = 10000,
  page = 0,
  sortDirection = "DESC",
  sortProperty = "modified_date",
  expansion = NA,
  includeType = "all",
  debug = FALSE
)


pagestats <- aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = date_range,
  dimensions = c("evar26"),
  metrics = c("pageviews","visits", "visitors"),
  top = c(50),
  page = 0,
  filterType = "breakdown",
  #segmentId = "s1957_6076f28b40d50e441ab3f0bd", # Segment Name: Membership Completion
  metricSort = "desc",
  include_unspecified = FALSE,
  search = NA,
  prettynames = FALSE,
  debug = FALSE
)

# Output the results as a formatted table
#pagestats %>% kable()


visits_by_day <- aw_freeform_table(date_range = date_range,
                  dimensions = "daterangeday",
                  metrics = "visits") %>% arrange(daterangeday)

device_type <- aw_freeform_table(date_range = date_range,
                  dimensions = "mobiledevicetype",
                  metrics = c("visits", "pageviews", "visitors"),
                  prettynames = TRUE)

device_type_by_day <- aw_freeform_table(date_range = date_range,
                        dimensions = c("daterangeday", "mobiledevicetype"),
                        metrics = "visits",
                        top = c(0, 3)) %>% arrange(daterangeday)


device_types <- aw_freeform_table(date_range = date_range,
                                  dimensions = "mobiledevicetype",
                                  metrics = "visits",
                                  # The default of 5 is probably going to get all of them,
                                  # but set a higher cutoff just in case.
                                  top = 10)

browsers <- aw_freeform_table(date_range = date_range,
                              dimensions = "browser",
                              metrics = "visits",
                              # We want to get all of the browsers, so set top as a high 
                              # value rather than the default of "5"
                              top = 1000)


browsers_by_device <- aw_freeform_table(date_range = date_range,
                  dimensions = c("mobiledevicetype", "browser"),
                  metrics = "visits",
                  top = c(10, 1000))



journey_with_segment_visits <- aw_freeform_table(date_range = date_range,
                                          dimensions = "daterangeday",
                                          segmentId = "s1957_6076f28b40d50e441ab3f0bd", # Segment Name: Membership Completion
                                          metrics = "visits") %>% arrange(daterangeday)

journey_with_segment_visits <- journey_with_segment_visits %>% 
  arrange(desc(daterangeday)) %>%
  mutate(Visits_3_DA = zoo::rollmean(visits, k = 3, fill = NA), 
          Visits_7_DA = zoo::rollmean(visits, k = 7, fill = NA), 
          Visits_14_DA = zoo::rollmean(visits, k = 14, fill = NA), 
          Visits_30_DA = zoo::rollmean(visits, k = 30, fill = NA))  %>%
  mutate(Visit_change_day_pct_chg = 100*((visits - lag(visits)))/lag(visits)) %>% ungroup()



plot(journey_with_segment_visits$daterangeday, journey_with_segment_visits$visits)




monitorEndTime_baseline <- Sys.time()
# Write out to the console how long it took for the entire process to run.
lastrunTime_baseline <- paste0("This process took ",monitorEndTime_baseline - monitorStartTime_baseline," minutes to run.",sep=" ")
lastrunTime_baseline