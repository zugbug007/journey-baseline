library(adobeanalyticsr)
library(tidyverse)
library(kableExtra)
library(zoo)
library(CausalImpact)
library(openxlsx)
library(openxlsx)
require(ggplot2)

#Test Token has been refreshed and is upto date.
#aw_token()
#rm(list = ls()) # Clear the environment before running the code
monitorStartTime_baseline <- Sys.time()
#delete the aa.auth file in WD if issues
aw_segments <- aw_get_segments(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsids = Sys.getenv("AW_REPORTSUITE_ID"),
  segmentFilter = NA,
  name = "journey",
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

#Last x days starting from yesterday
date_range = c(Sys.Date() - 90, Sys.Date() - 1)
#Function Calls Setup
get_segment_data <- function(segment_id) {
  adobeanalyticsr::aw_freeform_table(date_range = date_range,
                                     dimensions = "daterangeday",
                                     segmentId = segment_id, # Segment Name: Membership Completion
                                     prettynames = FALSE,
                                     metrics = c("visits","pageviews","visitors"))
  
}
calculate_means <- function(journey_data) {
  journey_data %>% arrange(desc(daterangeday)) %>%
    mutate(visit_change_day_pct_chg = (((visits))/lead(visits))-1)  %>%
    mutate(pageviews_change_day_pct_chg = (((pageviews))/lead(pageviews))-1)  %>%
    mutate(uniquevisitors_change_day_pct_chg = (((visitors))/lead(visitors))-1)  %>%
    mutate(visits_3_DA = zoo::rollmean(visits, k = 3, fill = NA, align ='left'), 
           visits_7_DA = zoo::rollmean(visits, k = 7, fill = NA, align ='left'), 
           visits_14_DA = zoo::rollmean(visits, k = 14, fill = NA, align ='left'),
           visits_30_DA = zoo::rollmean(visits, k = 30, fill = NA, align ='left'),
           visits_60_DA = zoo::rollmean(visits, k = 60, fill = NA, align ='left'),
           visits_90_DA = zoo::rollmean(visits, k = 90, fill = NA, align ='left'))  %>% ungroup()
}
#Set Date range for Adobe Data pull below

#for loop test
page_segments <- aw_segments %>% 
  filter(owner == "200133838") %>% #my segment ID top x
  slice(1:5)

#output <- as.data.frame(matrix(0, nrow(a), ncol(a) - 1))
datalist = list()
for (i in 1:nrow(page_segments)) {
      journey_data <- get_segment_data(page_segments$id[i])
      journey_data$journey_name <- page_segments$name[i]
      journey_data$journey_desc <- page_segments$description[i]
      journey_data <- calculate_means(journey_data)
      datalist[[i]] <- journey_data
}
journey_data <- data.table::rbindlist(datalist)
journey_data <- journey_data %>% relocate(journey_name, .after = daterangeday) %>% relocate(journey_desc, .after = journey_name)
  # Move Journey Name for neatness

# Sort by Date (Messy because of journey name)

# Create Workbook Output 
#####################################################################################################################################
wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")

addWorksheet(wb, sheetName = "journey_data", gridLines = FALSE)
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, sheet = 1, x = journey_data, colNames = TRUE, rowNames = TRUE, tableStyle = "TableStyleLight9")

saveWorkbook(wb, "testx1.xlsx", overwrite = TRUE) ## save to working directory

# Process Timing
######################################################################################################################################
monitorEndTime_baseline <- Sys.time()
# Write out to the console how long it took for the entire process to run.
lastrunTime_baseline <- paste0("This process took ",monitorEndTime_baseline - monitorStartTime_baseline," minutes to run.",sep=" ")
lastrunTime_baseline

