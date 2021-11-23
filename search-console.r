library(searchConsoleR)
scr_auth()
## wait for authorisation in browser window

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                 Setup Date Ranges and Common Date Variables              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pre dates will be hard coded when new site launches
pre_start_date <- Sys.Date() - 180
pre_end_date <- Sys.Date() - 91
pre_date_range = c(as.Date(pre_start_date), as.Date(pre_end_date))

# Post Launch Date Prep, Adjust date after launch date is known and ~90 days back.
post_start_date <- Sys.Date() - 90 # post start date will e set to launch date of the new site
post_end_date <- Sys.Date() - 1
post_date_range = c(as.Date(post_start_date), as.Date(post_end_date))

# Post Launch Date Improvement Monitoring Dates. Use start dates below and post_end_date for comparison.
post_start_date_3 <- post_end_date - 3
post_start_date_7 <- post_end_date - 7
post_start_date_14 <- post_end_date - 14

website <- "https://www.nationaltrust.org.uk"
download_dimensions <- c('query')
type <- c('web')
#dfe = c("device==DESKTOP","country==GBR", "page!=/home", "query~~outdoors")
dfe = c("query~~national trust")
get_search_console_data <- function(start, end) {
  search_analytics(siteURL = website, 
                         startDate = start, 
                         endDate = end, 
                         dimensions = download_dimensions, 
                         searchType = type,
                         dimensionFilterExp = dfe)
}

days <- seq(from=post_start_date, to=post_end_date, by='days')
days_count <- length(days)
search_console_datalist = list()
for ( i in seq_along(days)) {
  search_console_data  <- get_search_console_data(days[i], days[i])
    search_console_data$day <- days[i]
  search_console_datalist[[i]] <- search_console_data
  pc = round((i/days_count)*100, 0)
  print(paste0("Search Console data for : ", i, " of ", days_count, " - Completed: ",days[i], " ", " - Progress: ",pc,"%"))
}
search_console_data <- data.table::rbindlist(search_console_datalist, fill = TRUE)


