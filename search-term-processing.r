library(tidyverse)
library(tidytext)
library(lubridate)
library(adobeanalyticsr)
library(ggplot2)

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

#keyword <- "member"

get_search_data <- function(date_range) {
  adobeanalyticsr::aw_freeform_table(
    company_id = Sys.getenv("AW_COMPANY_ID"),
    rsid = Sys.getenv("AW_REPORTSUITE_ID"),
    date_range = date_range,
    dimensions = c("evar13"),
    metrics = c("evar13instances"),
    top = c(200),
    page = 0,
    filterType = "breakdown",
    segmentId = "s1957_619389fabb66f417bb8adc2f",
    metricSort = "desc",
    include_unspecified = TRUE,
    search = c("CONTAINS 'member'"),
    prettynames = FALSE,
    debug = FALSE
  )
}

days <- seq(from=post_start_date, to=post_end_date, by='days')
days_count <- length(days)
search_datalist = list()
for ( i in seq_along(days)) {
  day_date <- c(as.Date(days[i]), as.Date(days[i]))
  search_term_data_main_site  <- get_search_data(day_date)
  search_term_data_main_site$day <- days[i]
  search_datalist[[i]] <- search_term_data_main_site
  pc = round((i/days_count)*100, 0)
  print(paste0("Search terms for day: ", i, " of ", days_count, " - Completed: ",days[i], " ", " - Progress: ",pc,"%"))
}
search_term_data_main_site <- data.table::rbindlist(search_datalist, fill = TRUE)

# Clean up table and rename/relocate columns into a better order.
search_term_data_main_site <- search_term_data_main_site %>% 
  rename(searches = evar13instances) %>% 
  rename(search_term = evar13) %>% 
  relocate(day, .before = search_term) %>% 
  mutate(search_term = tolower(search_term))

# Prep for calculations
# calculate total searches for a day, divide by number of total searches that contain a string/search term.

# Plots
# Sort the data by each term into a time series for plotting.

search_terms_trended <- search_term_data_main_site %>% 
    arrange(desc(search_term), day) %>% 
    group_by(search_term) %>% 
    add_count() # %>% 
   # filter(str_detect(search_term, keyword)) %>% 
    #filter(n > 50)
search_terms_trended_flex_table <- search_terms_trended %>% 
  select(search_term, searches) %>% 
  mutate(total = sum(searches)) %>% 
  select(search_term, total) %>% 
  distinct() %>%  
  arrange(desc(total)) %>%
  flextable::flextable() 
search_terms_trended_flex_table

# total_queries <- search_terms_trended %>% mutate(grand_total = sum(searches)) %>% select(grand_total) %>% flextable::flextable()
# total_queries

# Single Item Trended
ggplot(search_terms_trended) +
 aes(x = day, y = searches, colour = search_term) +
 geom_line(size = 0.5) +
 scale_color_hue(direction = -1) +
 labs(x = "Day", y = "Number of Searches Made", title = "Search Term Contains: 'member'", 
 color = "Search Term") +
 theme_minimal() # +
 # theme(legend.position = "none")

# Facet wrap Line
ggplot(search_terms_trended) +
  aes(x = day, y = searches, colour = search_term) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(search_term), scales = "free")

# Bar
# ggplot(search_terms_trended) +
#  aes(x = day, fill = search_term, weight = searches) +
#  geom_bar() +
#  scale_fill_brewer(palette = "Dark2", 
#  direction = -1) +
#  labs(x = "Day", y = "Number of Searches Made", title = "Searches containing 'dog'", 
#  subtitle = "Any search on the NT Main Site which contains the word 'dog'", fill = "Search Term") +
#  theme_bw()

# Bar Variant 2
ggplot(search_terms_trended) +
    aes(x = day, fill = search_term, weight = searches) +
    geom_bar()+
    scale_fill_viridis_d(option = "viridis", direction = 1) +
    labs(x = "Day", y = "Number of Searches Made", title = "Searches containing 'member'",  
    subtitle = "Any search on the NT Main Site which contains the word 'member'.", fill = "Search Term") +
    theme_bw()

 