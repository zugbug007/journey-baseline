update_data <- FALSE

if (update_data == TRUE) {
  print("Deleting Files")
  file.remove("output/df_journey_data.rds")
  file.remove("output/df_anomaly_data.rds")
  file.remove("output/df_events.rds")
  file.remove("output/df_events_daily.rds")
}

# Capturing the start time -- the final output will include how long
# it took for the process to run.
monitorStartTime_baseline <- Sys.time()

rsid = "nationaltrustmainsiteprod"

# Check for packages needed and then load the packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               tidyquant,
               zoo,
               purrr,
               CausalImpact,
               ggplot2,
               ggalt,
               lubridate,
               forecast,
               plotly,
               patchwork,
               gridExtra,
               htmltools,
               GGally,
               rpivotTable,
               plotly,
               adobeanalyticsr,
               googlesheets4,
               scales,
               htmltools,  
               knitr,
               RColorBrewer,
               anomalize,
               grafify,
               flextable,
               kableExtra,
               RColorBrewer,
               forcats,
               changepoint,
               searchConsoleR,
               prophet,
               vistime,
               DT,
               sparkline)

gs4_auth(email = params$google_account)
scr_auth(email = params$alt_google_account)
# Test Token has been refreshed and is up to date.
#library(adobeanalyticsr)
aw_auth_with('oauth') # Use with Rmarkdown
#aw_auth()            # Use to refresh authentication

# Check Adobe API Token Access Now
#aw_calculatedmetrics <- aw_get_calculatedmetrics(rsid = rsid)
#rm(aw_calculatedmetrics)

# Test GSheets Connections
gsheet = "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0"
journey_segments_googlesheet <- read_sheet(gsheet, range = "journey")

# Cutoff for total instances (or occurrences) below which will flag as "Minimal Data"
min_instances <- 100

# Max Days before Refreshing the data (delta update Kick off)
max_days <- 3

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                 Setup Date Ranges and Common Date Variables              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pre dates will be hard coded when new site launches
# Adjust post baseline calculation time frame at the same time **IMPORTANT**
# Build Plots line 15
# Change top of funnel start pages when new site launches
# Membership & Renew: 'M|membership'
# Donate
launch_date_start <- as.Date("2022-10-01")
#launch_date_start <- as.Date("2022-11-02") # GO LIVE!

year_start <- ymd("2022-01-01")
days_since_launch <- as.double(difftime(Sys.Date(),launch_date_start, units = "days"))
reporting_length <- 180

pre_start_date <- Sys.Date() - reporting_length #180
pre_end_date <- launch_date_start - 1
pre_end_date_7 <- pre_end_date - 7
pre_end_date_30 <- pre_end_date - 30
pre_date_range = c(as.Date(pre_start_date), as.Date(pre_end_date))


# Post Launch Date Prep, Adjust date after launch date is known and ~90 days back.
post_start_date <- launch_date_start # post start date will set to launch date of the new site
post_end_date <- Sys.Date() - 1
post_date_range = c(as.Date(post_start_date), as.Date(post_end_date))

# Post Launch Date Improvement Monitoring Dates. Use start dates below and post_end_date for comparison.
post_start_date_3 <- post_end_date - 3
post_start_date_7 <- post_end_date - 7
post_start_date_14 <- post_end_date - 14
post_start_date_30 <- post_end_date - 30
post_start_date_60 <- post_end_date - 60

# Last.. 7 Days, month Etc.
# Setup Dates for Last Month
last_month_start <- format(Sys.Date() - 30, '%Y-%m-01')
last_month_end <- as.character(as.Date(format(Sys.Date(), '%Y-%m-01')) - 1)

# Create a rolling window starting two weeks back and plus 7 days.
start_14_days_ago_days <- Sys.Date() - 15
end_7_days_ago <- Sys.Date() - 8
two_weeks_ago <- c(start_14_days_ago_days, end_7_days_ago)

# Get Date Starting on a Sunday Two Weeks Ago
start_14_sun_start <- floor_date(as.Date(start_14_days_ago_days, "%m/%d/%Y"), unit="week")
end_14_sun_end <- as.Date(start_14_sun_start) + 6
two_weeks_ago <- c(as.Date(start_14_sun_start), as.Date(end_14_sun_end)) #Starting on a Sunday

#Get Date Starting on a Sunday 1 Week Ago
start_7_sun_start <- floor_date(as.Date(end_7_days_ago, "%m/%d/%Y"), unit="week")
end_7_sun_end <- as.Date(start_7_sun_start) + 6
last_7_full_week <- c(start_7_sun_start, end_7_sun_end)

# Last two weeks
last_two_full_weeks <- c(two_weeks_ago[1], last_7_full_week[2])
last_week <- c(as.Date(start_7_sun_start), as.Date(end_7_sun_end)) # starting on a Sunday

# Previous Week
previous_week_7 <- lubridate::floor_date(Sys.Date(), "week")
previous_week_7_end <- previous_week_7
previous_week_7_start <- previous_week_7 - 7
previous_week <- c(previous_week_7_start, previous_week_7_end)

# Valid from data dates are set in the post-processing script to determine exact last valid and valid start dates for ranges

website <- "https://www.nationaltrust.org.uk"
download_dimensions <- c('query')
type <- c('web')

df_metrics <- aw_get_metrics(
    company_id = Sys.getenv("AW_COMPANY_ID"), 
    rsid = Sys.getenv("AW_REPORTSUITE_ID"))
      if("description" %in% colnames(df_metrics) == FALSE){
    df_metrics$description <- ""
      }

# A few different names get used, so add to this if you come across another one
session_refresh_names <- c("Session Refresh", "Internal", "Internal Refresh", "Session Timeout",
                           "Browser left open", "Internal Referrer")
						   
						   # Update the Data
source("code/get_g_sheets.R")
source("code/get_functions.R")

if (file.exists("output/df_journey_data.rds")) {
  journey_data <- readRDS("output/df_journey_data.rds")
} 

if (file.exists("output/df_anomaly_data.rds")) {
  anomaly_data <- readRDS("output/df_anomaly_data.rds")
} 

if (file.exists("output/df_events.rds")) {
	df_events <- readRDS("output/df_events.rds")
} 

if (file.exists("output/df_events_daily.rds")) {
	df_events_daily <- readRDS("output/df_events_daily.rds")
}

source("code/get_journey_data.R")
source("code/get_events_data.R")
source("code/build_plots.R")

monitorEndTime_baseline <- Sys.time()
# Write out to the console how long it took for the entire process to run.
last_run <- as.numeric(monitorEndTime_baseline - monitorStartTime_baseline, units = "mins")
last_run <- round(last_run)
lastrunTime_baseline <- paste0("This process took ",last_run," minutes to build this analysis.",sep=" ")

save.image(file = "output/baseline-workspace.RData")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                         Outputs to the Google Sheet                      ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#write_sheet(journey_data, gsheet, sheet ="Journey_data")
#write_sheet(anomaly_data, gsheet, sheet ="Anomaly_data")
#write_sheet(compare_to_day, gsheet, sheet ="compare_to_day")
write_sheet(baseline, gsheet, sheet = "Baseline")