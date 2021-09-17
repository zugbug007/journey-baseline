library(adobeanalyticsr)  # Pull data from Adobe Analytics API 2.0
library(tidyverse)        # Import the Tidyverse packages
library(tidyquant)        # Import Tidyquant and them for Dumbbell plot
library(kableExtra)   # Build neat tables to display complex data
library(zoo)          # Time series manipulation and formatting
library(CausalImpact) # Understand the Causal impact from the data
library(openxlsx)     # Used for Exporting Excel docs
require(ggplot2)      # Build Graphics
library(googlesheets4)# Import and manipulate Google Sheets docs
library(ggalt)        # Extend ggplot with ability to do dumbbell plots
library(flow)         # Draw Flow diagrams from the code
library(plantuml)     # For use with Flow as alternative engine
library(anomalize)    # Build Anomalies from the data
library(rpivotTable)  # Build Pivot Tables
library(forecast)     # Use the TSOutliers function to detect anomalies in the data set: https://robjhyndman.com/hyndsight/tsoutliers/
library(plotly)       # Plotly for interactive graphs

#Test Token has been refreshed and is upto date.
#aw_token()
#rm(list = ls()) # Clear the environment before running the code
monitorStartTime_baseline <- Sys.time()
#delete the aa.auth file in WD if issues
#aw_calculatedmetrics <- aw_get_calculatedmetrics(rsids = "nationaltrustmainsiteprod")
#Last x days starting from yesterday
pre_start_date <- Sys.Date() - 180
pre_end_date <- Sys.Date() - 90
pre_date_range = c(as.Date(pre_start_date), as.Date(pre_end_date))

# Post Launch Date Prep, Adjust date after launch date is known and ~90 days back.
post_start_date <- Sys.Date() - 90
post_end_date <- Sys.Date() - 1
post_date_range = c(as.Date(post_start_date), as.Date(post_end_date))


journey_segments_googlesheet <- read_sheet("https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", range = "journey")
journey_metrics_googlesheet <- read_sheet("https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", range = "metrics")

journey_segments <- journey_segments_googlesheet %>% slice(1:36)

#Function Calls Setup
get_segment_data <- function(segment_ids, metrics, date_range) {
  adobeanalyticsr::aw_freeform_table(date_range = date_range,
                                     dimensions = "daterangeday",
                                     segmentId = segment_ids, # catch passed comma separated vector segment IDs group to pull data against. 
                                     prettynames = TRUE,      # Don't change this as many following variables names depend on this naming
                                     metrics = metrics,       # catch the comma separated vector group of metrics specified for this journey
                                     debug = FALSE
  )
  
}

get_anomaly_data <- function(segment_ids, metrics, data_range){
  adobeanalyticsr::aw_anomaly_report(date_range = date_range,
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

journey_datalist = list()
anomaly_datalist = list()
journey_count <- nrow(journey_segments)
for (i in 1:nrow(journey_segments)) {
  # Build list of metrics from row item (metric group) in the config table
  metrics_list <- journey_metrics_googlesheet %>% dplyr::select(metric_group_id, id) %>% filter(metric_group_id == journey_segments$metric_group[i]) %>% pull(id)
  if(journey_segments$journey_type[i] == "pre") {
    date_range <- pre_date_range  
  }
  if(journey_segments$journey_type[i] =="post") {
    date_range <- post_date_range
  }
  # Select the correct date range for the journey. Get pre/post value from the row item in the config table and select the correct one to be fed into the data pull request.
  segment_group = c()
  segment_group <- as.character(journey_segments$id[i])                     # Pass the segment id of the first journey
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
  journey_data$journey_applied <- segment_group[1]
  journey_data$secondary_segment_1_name <- journey_segments$secondary_segment_name[i]
  journey_data$secondary_segment_2_name <- journey_segments$third_segment_name[i]
  
  anomaly_data <- get_anomaly_data(segment_group, metrics_list, date_range)
  anomaly_data$journey_type <- journey_segments$journey_type[i]
  anomaly_data$journey_name <- journey_segments$journey_name[i]
  anomaly_data$journey_desc <- journey_segments$journey_desc[i]
  anomaly_data$journey_applied <- segment_group[1]
  anomaly_data$secondary_segment_1_name <- journey_segments$secondary_segment_name[i]
  anomaly_data$secondary_segment_2_name <- journey_segments$third_segment_name[i]
  
  segment_group <- NULL                                                     # Clear the segment group at the end of the loop
  
  # Calculations for Daily Averages for Visit Data
  journey_data <- calculate_means(journey_data)
  journey_datalist[[i]] <- journey_data # if this statement fails, something above this did not work.
  anomaly_datalist[[i]] <- anomaly_data
  
  print(paste0("Journey ", i, " of ", journey_count, " Completed: ",journey_segments$journey_name[i]))
}
journey_data <- data.table::rbindlist(journey_datalist)
anomaly_data <- data.table::rbindlist(anomaly_datalist)
#journey_data

journey_data <- journey_data %>%        # Organise column order for data clarity
  relocate(journey_name, .after = Day) %>% 
  relocate(journey_type, .before = journey_name) %>%
  relocate(journey_desc, .after = journey_name) %>% 
  relocate(journey_applied, .after =journey_desc) %>%
  relocate(secondary_segment_1_name, .after = journey_applied) %>%
  relocate(secondary_segment_2_name, .after = secondary_segment_1_name)


# Calculations
test_mutate <- journey_data %>% filter(journey_type=="pre") %>% group_by(journey_name) %>% summarise(across(c(`Visits`,`Page Views`, `Unique Visitors`, `Bounces`,`Average Time Spent on Site (seconds)` , 
                                                                                                              `% New Visits`,	`% Repeat Visits`,	`New Visits`,	`Repeat Visits`), list(mean = mean), .names = "{.col}_{.fn}")) %>% add_column(journey_type = "pre", .after = "journey_name")
test_mutate2 <- journey_data %>% filter(journey_type=="post") %>% group_by(journey_name) %>% summarise(across(c(`Visits`,`Page Views`, `Unique Visitors`, `Bounces`,`Average Time Spent on Site (seconds)` , 
                                                                                                                `% New Visits`,	`% Repeat Visits`,	`New Visits`,	`Repeat Visits`), list(mean = mean), .names = "{.col}_{.fn}")) %>% add_column(journey_type = "post", .after = "journey_name")
test_mutate3 <- rbind(test_mutate, test_mutate2)

tm3 <- test_mutate3 %>% select(journey_name, journey_type, Visits_mean) %>% 
  pivot_wider(
    names_from = journey_type,
    values_from = 'Visits_mean'
  ) %>% arrange(desc(post)) %>%
  mutate(journey_name = fct_reorder(journey_name, post))

g1 <- tm3 %>% 
  ggplot(aes(x = pre, xend = post, y = journey_name)) +
  geom_dumbbell(
    colour="#a3c4dc",
    colour_xend="#0e668b",
    size=4.0,
    dot_guide=TRUE,
    dot_guide_size=0.15,
    dot_guide_colour = "grey60"
  )

g1
g2 <- g1 +
  labs(
    title = "Baseline Journey Comparison from Before & After CMS Launch",
    x="Pre vs.Post Baseline (Visits)", y = "Journey Name"
  ) +
  theme_tq() +
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(),
    axis.ticks=element_blank(),
    panel.border=element_blank()
  )
g2

# WAIT BEFORE WRITING TO GSHEET!
view(journey_data)
view(anomaly_data)
# WAIT BEFORE WRITING TO GSHEET!

# Create new table with all pre-change metrics 
#pre_journey_data <- journey_data %>% rename_all(paste0, "_pre")

#Outputs to the Google Sheet
write_sheet(journey_data, "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", sheet ="Journey_data")
write_sheet(anomaly_data, "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", sheet ="Anomaly_data")

#rpivotTable(journey_data)


library(dplyr)
library(ggplot2)
ggplot(journey_data) +
  aes(x = Day, y = Visits, group = journey_type) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(journey_name))

journey_data %>%
  filter(journey_type %in% "pre") %>%
  ggplot() +
  aes(x = Day, y = Visits, colour = journey_type) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(journey_name), scales = "free", ncol = 10L, nrow = 10L)

ggplot(journey_data) +
  aes(x = Day, y = Visits, colour = journey_type) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(journey_name), scales = "free", ncol = 10L, nrow = 10L)

journey_data %>%
  filter(journey_name %in% c("ALL Visits", "ALL Visits - New Site")) %>%
  ggplot() +
  aes(x = Day, y = Visits, colour = journey_type) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(journey_name), scales = "free_x", ncol = 10L, nrow = 10L)

journey_data %>%
  filter(journey_name %in% c("ALL Visits", "ALL Visits - New Site")) %>%
  ggplot() +
  aes(x = Day, y = visit_change_day_pct_chg, fill = journey_type) +
  geom_area(size = 1.5) +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(journey_name), scales = "free_x", 
             ncol = 10L, nrow = 10L)

anomaly_data <- aw_anomaly_report(
  date_range = post_date_range,
  metrics = "visits",
  granularity = "day",
  segmentId = NA,
  quickView = FALSE,
  anomalyDetection = TRUE,
  countRepeatInstances = TRUE,
  debug = FALSE
)

# Loop Required or Facet Wrap

anomaly_subset <- anomaly_data %>% filter(journey_name == "ALL Social" & metric == 'visits')     # Subset the anomaly data for journey and metric
plot_journey_name <- anomaly_subset %>% filter(row_number()==1) %>% pull(journey_name)        # Get the journey name from the first row
anomaly_subset %>% dplyr::filter(metric == 'visits' & journey_name == plot_journey_name) %>%  # Use the subset to build the anomaly chart
  ggplot2::ggplot(aes_string(x = "day")) +
  ggplot2::geom_line(aes_string( y = 'data')) +
  ggplot2::geom_point(data = anomaly_data %>% dplyr::filter(metric == 'visits' & dataAnomalyDetected == T & journey_name == plot_journey_name),
                      ggplot2::aes_string(y ='data')) +
  ggplot2::geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
  ggplot2::geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "red", linetype=4, lwd = .8, alpha=0.5) +
  ggplot2::labs(title = plot_journey_name,
                subtitle = paste0('There are ',nrow(anomaly_data %>% filter(metric == 'visits' & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.'),
                caption =paste0('There are ',nrow(anomaly_data %>% filter(metric == 'visits' & dataAnomalyDetected == T & journey_name == plot_journey_name)), ' anomalies.')) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::expand_limits(y=0) +
  ggplot2::facet_wrap(~journey_name, dir = "v")
# Visualise the R Script
#flow_view("baseline.r")


# Process Timing
#####################################################################################################################################
monitorEndTime_baseline <- Sys.time()
# Write out to the console how long it took for the entire process to run.
lastrunTime_baseline <- paste0("This process took ",monitorEndTime_baseline - monitorStartTime_baseline," minutes to run.",sep=" ")
lastrunTime_baseline


# Create Workbook Output 
# Excel Spreadsheet Output
#####################################################################################################################################
wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")

addWorksheet(wb, sheetName = "journey_data", gridLines = FALSE)
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, sheet = 1, x = journey_data, colNames = TRUE, rowNames = TRUE, tableStyle = "TableStyleLight9")

saveWorkbook(wb, "journey_data.xlsx", overwrite = TRUE) ## save to working directory

#writexl::write_xlsx(aw_calculatedmetrics, path = "cal_metrics.xlsx")

# date_range = c(Sys.Date() - 366, Sys.Date() - 1)
# Sam <- adobeanalyticsr::aw_freeform_table(date_range = date_range,
#                                    dimensions = "daterangeday",
#                                    segmentId = "s1957_611e66cab501e00d15ba4e3f", # catch passed segment IDs to pull data against. 
#                                    prettynames = TRUE, #  Don't change this as many following variables names depend on this naming
#                                    metrics = metrics_list,  #  catch the group of metrics specified for this journey
#                                    debug = FALSE)
# write_sheet(Sam, ss=NULL, sheet = NULL)
