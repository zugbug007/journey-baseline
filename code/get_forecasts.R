
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Build Forecast for every Journey                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(scipen = 999)

journey_unique_names <- journey_segments %>% select(journey_name) %>% distinct()
journey_unique_count <- nrow(journey_unique_names)

# Predictor = Control Group (Pre Launch Data)
forecast_datalist = list()
for (i in 1:nrow(journey_unique_names)) {
  forecast_data <- journey_data %>%
    select(Day, journey_name, Visits) %>%
    group_by(journey_name) %>%
    arrange(desc(journey_name), Day) %>%
    filter(journey_name == journey_unique_names$journey_name[i]) %>%
    filter(Day >= pre_start_date & Day <= post_end_date)
  
  # Get the first and last row dates of the available data
  forecast_start_date = head(forecast_data$Day, n = 1)
  forecast_end_date = tail(forecast_data$Day, n = 1)
  forecast_pre_date_range = c(as.Date(forecast_start_date), as.Date(pre_end_date))
  forecast_post_date_range = c(as.Date(post_start_date), as.Date(forecast_end_date))
  
  time.points <- as.Date(seq(forecast_start_date, forecast_end_date, by = "day"))
  
  data <- zoo(cbind(forecast_data$Visits), time.points)
  forecast_data <- CausalImpact(data, as.Date(forecast_pre_date_range), as.Date(forecast_post_date_range), model.args = list(season.duration = 1))
  
  gg <- plot(forecast_data, c("original", "pointwise", "cumulative"))
  plotname <- paste0(journey_unique_names$journey_name[i])
  
  print(gg + ggplot2::ggtitle(plotname))
  
  forecast_datalist[[i]] <- forecast_data
}

# Write out to a .csv for use if the script craps out and put a message to the console
message("Forecast processing completed.")
