library(adobeanalyticsr)
library(tidyverse)    
library(kableExtra)   # Build neat tables to display complex data
library(zoo)          # Time series manipulation and formatting
library(CausalImpact) # Understand the Causal impact from the data
library(openxlsx)     # Used for Exporting Excel docs
require(ggplot2)      # Build Graphics
library(googlesheets4)# Import and manipulate Google Sheets docs
library(ggalt)        # Extend GGPLOT with ability to do dumbell plots
library(flow)         # Draw Flow diagrams from the code
library(plantuml)     # For use with Flow as alternative engine
library(anomalize)    # Build Anomalies from the data
library(rpivotTable)  # Build Pivot Tables
#Test Token has been refreshed and is upto date.
#aw_token()
#rm(list = ls()) # Clear the environment before running the code
monitorStartTime_baseline <- Sys.time()
#delete the aa.auth file in WD if issues
aw_segments_temp <- aw_get_segments(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsids = Sys.getenv("AW_REPORTSUITE_ID"),
  segmentFilter = NA,
  name = NA,
  tagNames = NA,
  filterByPublishedSegments = "all",
  limit = 1000,
  page = 0,
  sortDirection = "DESC",
  sortProperty = "modified_date",
  expansion = NA,
  includeType = "all",
  debug = FALSE
)

#Last x days starting from yesterday
date_range = c(Sys.Date() - 30, Sys.Date() - 1)
#Function Calls Setup
#flow_view(aw_freeform_table, engine = "plantuml")

get_segment_data <- function(segment_id) {
  adobeanalyticsr::aw_freeform_table(date_range = date_range,
                                     dimensions = "daterangeday",
                                     segmentId = segment_id, # catch passed segment ID to pull data against. 
                                     prettynames = FALSE, # Don't change this as many following variables names depend on this naming
                                     metrics = c("visits","pageviews","visitors"),
                                     debug = FALSE)
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
#flow_view(calculate_means, engine = "plantuml")
journey_segments_googlesheet <- read_sheet("https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", range = "journey")
page_segments <- journey_segments_googlesheet %>% slice(1:3)
# test dual/multiple segment IDs - 
#page_segments2 <- read_sheet("https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", range = "test")
# Load Config Sheet - Journey tab

#output <- as.data.frame(matrix(0, nrow(a), ncol(a) - 1))
datalist = list()
for (i in 1:nrow(page_segments)) {
  segment_group = c()
      segment_group <- as.character(page_segments$id[i])            # Pass the segment id of the first journey
            if(!is.na(page_segments$secondary_segment_id[i])){      # This statement looks to see if a second segment is required to be applied
              secondary_seg <- page_segments$secondary_segment_id[i]# to the journey, if it is add it to the group to be sent to the data request
              segment_group <- c(segment_group, secondary_seg)      # segments are AND'ed together as if they were added to the header of an Analysis Workspace panel. 
            }
            
            if(!is.na(page_segments$third_segment_id[i])){      # This statement looks to see if a second segment is required to be applied
              third_seg <- page_segments$third_segment_id[i]# to the journey, if it is add it to the group to be sent to the data request
              segment_group <- c(segment_group, third_seg)      # segments are AND'ed together as if they were added to the header of an Analysis Workspace panel. 
            }
      
      segment_group <- c(segment_group)
      journey_data <- get_segment_data(segment_group)
      journey_data$journey_name <- page_segments$journey_name[i]
      journey_data$journey_desc <- page_segments$journey_desc[i]
      journey_data$journey_applied <- segment_group[1]
      journey_data$secondary_segment_1_id <- segment_group[2]
      segment_group <- NULL

      # Calculations for Daily Averages for Visit Data
      journey_data <- calculate_means(journey_data)
      datalist[[i]] <- journey_data # if this statement fails, something above this did not work.
}
journey_data <- data.table::rbindlist(datalist)
journey_data <- journey_data %>% relocate(journey_name, .after = daterangeday) %>% relocate(journey_desc, .after = journey_name) %>% relocate(journey_applied, .after =journey_name)
# Move Journey Name for neatness





rpivotTable(journey_data)


# Visualise the R Script
flow_view("baseline.r")

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

