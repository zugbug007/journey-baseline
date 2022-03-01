library(searchConsoleR)
library(tidyverse)
library(stringr)
library(plotly)
library(scales)
library(prophet)
library(ggplot2)
library(lubridate)
scr_auth()
scipen=999
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
post_start_date_30 <- post_end_date - 30
post_start_date_60 <- post_end_date - 60

website <- "https://www.nationaltrust.org.uk"
download_dimensions <- c('query')
type <- c('web')
#dfe = c("device==DESKTOP","country==GBR", "page!=/home", "query~~outdoors")
#dfe = c("query~~national trust")
get_search_console_data <- function(start, end) {
  search_analytics(siteURL = website, 
                         startDate = start, 
                         endDate = end, 
                         dimensions = download_dimensions, 
                         searchType = type)
}

days <- seq(from=post_start_date_14, to=post_end_date, by='days')
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

# Add a column to identify brand terms
data_brand <- search_console_data %>% mutate(brand = case_when(grepl("national trust|national.trust", query) ~ 'brand', TRUE ~ 'nonbrand')) 

branded_search <- data_brand %>% group_by(day, brand) %>%
  summarize(clicks = sum(clicks)) 

br <- ggplot(branded_search) +
  geom_line(aes(day, clicks, color = brand)) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_bw() +
    labs(
      x = "Day",
      y = "Clicks",
      title = "National Trust Brand vs. Non Brand Clicks",
      subtitle = "Last 90 Days"
    )
ggplotly(br)


# NT Product Type Segmentation
search_products <- search_console_data %>%
  mutate(product_type = case_when(grepl("membership|member", query) ~ 'Membership',
                                  grepl("donation|donate|support", query) ~ 'Donation',
                                  grepl("shop|shopping", query) ~ 'Shop',
                                  grepl("holiday|hols|holidays", query) ~ 'Holidays',
                                  TRUE ~ 'Other'),
         brand = case_when(grepl("national trust|national.trust", query) ~ 'brand', TRUE ~ 'nonbrand')) %>% drop_na()

search_products %>%
  group_by(day, product_type) %>%
  summarize(clicks = sum(clicks)) %>%
  filter(product_type != 'Other') %>% # Exclude Other
  ggplot() +
  geom_line(aes(day, clicks, color = product_type), size = 0.7) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal() +
  labs(
    x = "Day",
    y = "Clicks",
    title = "National Trust Product Clicks",
    subtitle = "Last 90 Days",
    color = "Product Type"
  )

# Aggregate Product Metrics by Product Type
search_product_table <- search_products %>%
  group_by(product_type) %>%
  summarise(clicks = sum(clicks),
           impressions = sum(impressions),
           position = mean(position)) %>%
  mutate(ctr = 100 * (clicks / impressions)) %>%
  arrange(desc(product_type)) 
flextable::flextable(search_product_table)

search_products %>%
  ggplot() +
  geom_histogram(aes(ctr, fill = brand), binwidth = 0.01) +   # ADD FILL = BRAND
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = comma_format()) +
  labs(x = 'Click-through Rate',
       y = 'Count') +
  theme_bw()

search_products %>%
  ggplot() +
  stat_ecdf(aes(ctr, color = brand)) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = 'Click-through Rate',
       y = 'Keyword %') +
  theme_bw()

search_console_date <- search_analytics(siteURL = website, 
                                        startDate = post_start_date, 
                                        endDate = post_end_date, 
                                        dimensions = 'date', 
                                        searchType = type)
search_console_devices <- search_analytics(siteURL = website, 
                                           startDate = post_start_date, 
                                           endDate = post_end_date, 
                                           dimensions = 'device', 
                                           searchType = type)
search_console_pages <- search_analytics(siteURL = website, 
                                         startDate = post_start_date, 
                                         endDate = post_end_date, 
                                         dimensions = 'page', 
                                         searchType = type)

# Visualise the top 5 terms
search_console_vis <- search_console_data %>% 
  filter(day >= post_start_date & day <= post_end_date) %>% 
  group_by(query) %>% 
  add_count() %>% filter(n > 2) %>% 
   filter(str_detect(query, 'mem')) %>% 
  mutate(query = str_remove_all(query, "national trust | national trust")) %>%
  group_by(day)
  
ggplot(search_console_vis) +
  aes(x = day, y = clicks, colour = query) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(x = "Day", y = "Number of National Trust Google Searches Made", title = "Searches containing 'mem'",  
       subtitle = "Any search on Google contains the word 'mem'.", fill = "Search Term",
       caption = "Google Search Console Data") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(query), scales = "free_y")

# Plot Click Through Rate by Above Search Terms
ggplot(search_console_vis) +
 aes(x = day, y = ctr, colour = query) +
 geom_line(size = 0.5) +
 scale_color_hue(direction = 1) +
 labs(x = "Day", y = "CTR", title = "Search Console CTR", subtitle = "Click Through Rate by Term", 
 caption = "Search Console", color = "Search Term") +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_wrap(vars(query), scales = "free")

# Plot Impressions by Above Search Terms
ggplot(search_console_vis) +
  aes(x = day, y = impressions, colour = query) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(x = "Day", y = "Impressions", title = "Search Impressions", subtitle = "Impressions by Term", 
       caption = "Search Console", color = "Search Term") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(query), scales = "free")

# Plot Position by Above Search Terms
ggplot(search_console_vis) +
 aes(x = day, y = position, colour = query) +
 geom_line(size = 0.5) +
 scale_color_hue(direction = 1) +
  labs(x = "Day", y = "Position", title = "Search Position", subtitle = "Position by Term", 
       caption = "Search Console", color = "Search Term") +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_wrap(vars(query), scales = "free")

# High level Overview Charts
# Clicks Trend 90 Days
# Line Graph Trend of the NT Clicks from Google
ggplot(search_console_date) +
  aes(x = date, y = clicks) +
  geom_line(size = 0.6, colour = "#46337E") +
  labs(
    x = "Day",
    y = "Clicks",
    title = "National Trust Search Console Clicks",
    subtitle = "Last 90 Days"
  ) +
  theme_light()

# Bar Chart - Clicks by Device Typescipen=999
ggplot(search_console_devices) +
 aes(x = device, fill = device, weight = clicks) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 labs(x = "Device", y = "Clicks", title = "Google Clicks by Device Type", subtitle = "Google Search Console", 
 fill = "Device") +
 theme_minimal()

search_console_pages_table <- search_console_pages %>% slice(1:25) %>% flextable::flextable()
search_console_pages_table

# Build Forecasting Model using Prophet
# Non Branded Traffic Prediction
data_time_series <- data_brand %>%
  filter(brand == 'nonbrand') %>%   # Change for Branded/Non branded Search Types
  select(ds = day, 
         y = clicks) %>%    # RENAME COLUMNS AS REQUIRED BY PROPHET
  group_by(ds) %>%
  summarize(y = sum(y)) %>% drop_na()

m <- prophet(data_time_series, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 14)  # Change future prediction window
forecast <- predict(m, future)

#plot(m, forecast)   # Check basic plot

forecast <- forecast %>% as_tibble()    # Transform to Tidy format

forecast$ds <- ymd(forecast$ds)     # Change to Time Series Format

forecast_clean <- forecast %>%
  select(ds, yhat, yhat_upper, yhat_lower) %>%
  left_join(data_time_series) %>%
  rename(date = ds, 
         actual = y,
         forecast = yhat,
         forecast_low = yhat_lower,
         forecast_high = yhat_upper)

pf <-   ggplot(forecast_clean) +
  geom_point(aes(date, actual), color = 'steelblue') +
  geom_line(aes(date, actual), color = 'steelblue') +
  geom_ribbon(aes(date, ymin = forecast_low, ymax = forecast_high), 
              fill = '#69b3a2', alpha = 0.2) +
  scale_y_continuous(labels = comma_format()) +
  expand_limits(y = 0) + 
  labs(x = "Month",
       y = "Non Branded Search Traffic") +
  theme_bw()
ggplotly(pf)+
  ggsave("Non Branded Search Traffic Prediction.png", path = "search_console_plots/")


# Build Forecasting Model using Prophet
# Branded Traffic Prediction
data_time_series <- data_brand %>%
  filter(brand == 'brand') %>%   # Change for Branded/Non branded Search Types
  select(ds = day, 
         y = clicks) %>%    # RENAME COLUMNS AS REQUIRED BY PROPHET
  group_by(ds) %>%
  summarize(y = sum(y)) %>% drop_na()

m <- prophet(data_time_series, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 14)  # Change future prediction window
forecast <- predict(m, future)

#plot(m, forecast)   # Check basic plot

forecast <- forecast %>% as_tibble()    # Transform to Tidy format

forecast$ds <- ymd(forecast$ds)     # Change to Time Series Format

forecast_clean <- forecast %>%
  select(ds, yhat, yhat_upper, yhat_lower) %>%
  left_join(data_time_series) %>%
  rename(date = ds, 
         actual = y,
         forecast = yhat,
         forecast_low = yhat_lower,
         forecast_high = yhat_upper)

pf <-   ggplot(forecast_clean) +
  geom_point(aes(date, actual), color = 'steelblue') +
  geom_line(aes(date, actual), color = 'steelblue') +
  geom_ribbon(aes(date, ymin = forecast_low, ymax = forecast_high), 
              fill = '#69b3a2', alpha = 0.2) +
  scale_y_continuous(labels = comma_format()) +
  expand_limits(y = 0) + 
  labs(x = "Month",
       y = "Branded Search Traffic") +
  theme_bw()
ggplotly(pf)+
  ggsave("Branded Search Traffic Prediction.png", path = "search_console_plots/")

# Build Forecasting Model using Prophet
# Membership Traffic Prediction
data_time_series <- search_products %>%
  filter(product_type == 'Membership') %>%   # Change for Branded/Non branded Search Types
  select(ds = day, 
         y = clicks) %>%    # RENAME COLUMNS AS REQUIRED BY PROPHET
  group_by(ds) %>%
  summarize(y = sum(y)) %>% drop_na()

m <- prophet(data_time_series, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 14)  # Change future prediction window
forecast <- predict(m, future)

#plot(m, forecast)   # Check basic plot

forecast <- forecast %>% as_tibble()    # Transform to Tidy format

forecast$ds <- ymd(forecast$ds)     # Change to Time Series Format

forecast_clean <- forecast %>%
  select(ds, yhat, yhat_upper, yhat_lower) %>%
  left_join(data_time_series) %>%
  rename(date = ds, 
         actual = y,
         forecast = yhat,
         forecast_low = yhat_lower,
         forecast_high = yhat_upper)

pf <-   ggplot(forecast_clean) +
  geom_point(aes(date, actual), color = 'steelblue') +
  geom_line(aes(date, actual), color = 'steelblue') +
  geom_ribbon(aes(date, ymin = forecast_low, ymax = forecast_high), 
              fill = '#69b3a2', alpha = 0.2) +
  scale_y_continuous(labels = comma_format()) +
  expand_limits(y = 0) + 
  labs(x = "Month",
       y = "Membership Search Traffic Prediction Model") +
  theme_bw()
ggplotly(pf)+
  ggsave("Membership Traffic Prediction.png", path = "search_console_plots/")

# Build Forecasting Model using Prophet
# Holidays Traffic Prediction
data_time_series <- search_products %>%
  filter(product_type == 'Holidays') %>%   # Change for Branded/Non branded Search Types
  select(ds = day, 
         y = clicks) %>%    # RENAME COLUMNS AS REQUIRED BY PROPHET
  group_by(ds) %>%
  summarize(y = sum(y)) %>% drop_na()

m <- prophet(data_time_series, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 14)  # Change future prediction window
forecast <- predict(m, future)

#plot(m, forecast)   # Check basic plot

forecast <- forecast %>% as_tibble()    # Transform to Tidy format

forecast$ds <- ymd(forecast$ds)     # Change to Time Series Format

forecast_clean <- forecast %>%
  select(ds, yhat, yhat_upper, yhat_lower) %>%
  left_join(data_time_series) %>%
  rename(date = ds, 
         actual = y,
         forecast = yhat,
         forecast_low = yhat_lower,
         forecast_high = yhat_upper)

pf <-   ggplot(forecast_clean) +
  geom_point(aes(date, actual), color = 'steelblue') +
  geom_line(aes(date, actual), color = 'steelblue') +
  geom_ribbon(aes(date, ymin = forecast_low, ymax = forecast_high), 
              fill = '#69b3a2', alpha = 0.2) +
  scale_y_continuous(labels = comma_format()) +
  expand_limits(y = 0) + 
  labs(x = "Month",
       y = "Holidays Search Traffic Prediction Model") +
  theme_bw()
ggplotly(pf)+
  ggsave("Holidays Traffic Prediction.png", path = "search_console_plots/")

# Build Forecasting Model using Prophet
# Donations Traffic Prediction
data_time_series <- search_products %>%
  filter(product_type == 'Donation') %>%   # Change for Branded/Non branded Search Types
  select(ds = day, 
         y = clicks) %>%    # RENAME COLUMNS AS REQUIRED BY PROPHET
  group_by(ds) %>%
  summarize(y = sum(y)) %>% drop_na()

m <- prophet(data_time_series, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 14)  # Change future prediction window
forecast <- predict(m, future)

#plot(m, forecast)   # Check basic plot

forecast <- forecast %>% as_tibble()    # Transform to Tidy format

forecast$ds <- ymd(forecast$ds)     # Change to Time Series Format

forecast_clean <- forecast %>%
  select(ds, yhat, yhat_upper, yhat_lower) %>%
  left_join(data_time_series) %>%
  rename(date = ds, 
         actual = y,
         forecast = yhat,
         forecast_low = yhat_lower,
         forecast_high = yhat_upper)

pf <-   ggplot(forecast_clean) +
  geom_point(aes(date, actual), color = 'steelblue') +
  geom_line(aes(date, actual), color = 'steelblue') +
  geom_ribbon(aes(date, ymin = forecast_low, ymax = forecast_high), 
              fill = '#69b3a2', alpha = 0.2) +
  scale_y_continuous(labels = comma_format()) +
  expand_limits(y = 0) + 
  labs(x = "Month",
       y = "Donation Search Traffic Prediction Model") +
  theme_bw()
ggplotly(pf)+
  ggsave("Donation Traffic Prediction.png", path = "search_console_plots/")

# Build Forecasting Model using Prophet
# NT Shop Traffic Prediction
data_time_series <- search_products %>%
  filter(product_type == 'Shop') %>%   # Change for Branded/Non branded Search Types
  select(ds = day, 
         y = clicks) %>%    # RENAME COLUMNS AS REQUIRED BY PROPHET
  group_by(ds) %>%
  summarize(y = sum(y)) %>% drop_na()

m <- prophet(data_time_series, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 14)  # Change future prediction window
forecast <- predict(m, future)

#plot(m, forecast)   # Check basic plot

forecast <- forecast %>% as_tibble()    # Transform to Tidy format

forecast$ds <- ymd(forecast$ds)     # Change to Time Series Format

forecast_clean <- forecast %>%
  select(ds, yhat, yhat_upper, yhat_lower) %>%
  left_join(data_time_series) %>%
  rename(date = ds, 
         actual = y,
         forecast = yhat,
         forecast_low = yhat_lower,
         forecast_high = yhat_upper)

pf <-   ggplot(forecast_clean) +
  geom_point(aes(date, actual), color = 'steelblue') +
  geom_line(aes(date, actual), color = 'steelblue') +
  geom_ribbon(aes(date, ymin = forecast_low, ymax = forecast_high), 
              fill = '#69b3a2', alpha = 0.2) +
  scale_y_continuous(labels = comma_format()) +
  expand_limits(y = 0) + 
  labs(x = "Month",
       y = "Shop Search Traffic Prediction Model") +
  theme_bw()
ggplotly(pf)+
ggsave("Shop Traffic Prediction.png", path = "search_console_plots/")

