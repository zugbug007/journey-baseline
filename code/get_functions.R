##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            Function Call Setup                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hex Colour Reference
# Blue = "#406882" OR rgb(64, 104, 130)
# Red = "#f05454" OR rgb(240, 84, 84)
# Purple = "#824068" OR rgb(130, 64, 104)
# Green ="#688240" OR rgb(104, 130, 64)
# Dark Blue = "#4d4082" OR rgb(77, 64, 130)

get_segment_data <- function(segment_ids, metrics, date_range) {
  adobeanalyticsr::aw_freeform_table(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                     date_range = date_range,
                                     dimensions = "daterangeday",
                                     top = 0,
                                     segmentId = segment_ids, # catch passed comma separated vector segment IDs group to pull data against. 
                                     prettynames = TRUE,      # Don't change this as many following variables names depend on this naming
                                     metrics = metrics,       # catch the comma separated vector group of metrics specified for this journey
                                     debug = FALSE
  )
  
}

get_page_data <- function(segment_ids, metrics, date_range, search_criteria) {
  adobeanalyticsr::aw_freeform_table(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                     date_range = date_range,
                                     dimensions = "evar26",
                                     segmentId = segment_ids,          # catch passed comma separated vector segment IDs group to pull data against. 
                                     prettynames = TRUE,      # Don't change this as many following variables names depend on this naming
                                     metrics = metrics,       # catch the comma separated vector group of metrics specified for this journey
                                     include_unspecified = TRUE,
                                     filterType = "breakdown",
                                     metricSort = "desc",
                                     search = search_criteria,
                                     top = c(500),
                                     page = 0,
                                     debug = FALSE
  ) %>%  
    mutate(`Bounce Rate` = percent(`Bounce Rate`, accuracy = 0.1)) %>% 
    mutate(`Exit Rate` = percent(`Exit Rate`, accuracy = 0.1)) %>% 
    mutate(across(where(is.numeric), round, 1))
  
}

get_anomaly_data <- function(segment_ids, metrics, data_range){
  adobeanalyticsr::aw_anomaly_report(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                     company_id = Sys.getenv("AW_COMPANY_ID"),
                                     date_range = date_range,
                                     metrics = metrics,
                                     segmentId = segment_ids,
                                     granularity = "day",
                                     quickView = FALSE,
                                     anomalyDetection = TRUE,
                                     countRepeatInstances = TRUE,
                                     debug = FALSE
  )
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

get_events_daily <- function(eventids) {
  events_data <- aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                    rsid = rsid, 
                    top = 0,
                    date_range = date_range,
                    dimensions = "daterangeday",
                    metrics = c("event1","event2")) %>% # change to event ids after testing
                    pivot_longer(-daterangeday) %>%
                    arrange(daterangeday, name) %>% 
                    select(day = daterangeday, 
                    id = name, value)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      SCATTER PLOT CHANNEL CONVERSION                     ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_scatter_plot <- function(date_range_conversion, metric, segment_name, funnel_name) {
  df <- adobeanalyticsr::aw_freeform_table(
    date_range = date_range_conversion,
    dimensions = c("evar38"),
    metrics = c(metric,"evar38instances"),
    top = c(50000),
    page = 0,
    filterType = "breakdown",
    segmentId = segment_name, 
    metricSort = "desc",
    include_unspecified = FALSE,
    search = NA,
    prettynames = FALSE,
    debug = FALSE
  )
  options(warn = -1)
  df %>% dplyr::rename("path" = 1, 
                       "conversion" = 2,
                       "path_count" = 3) %>% 
    plotly::plot_ly(
      type = 'scatter',
      mode = 'markers',
      size =~conversion,
      color=~conversion,
      marker = list(size =~conversion*3),
      y=~conversion,
      x=~path_count,
      text=~paste("Channel:", path, '<br>Conversions:', conversion)
    ) %>% layout(
      #title = paste0(funnel_name," Conversions by Channel: ", date_range_conversion[1]," - ", date_range_conversion[2]),
      xaxis = list(type="log",title=paste0(funnel_name, " - Incoming Channels")),
      yaxis = list(type="log",title=paste0(funnel_name, " - Number of Conversions"))
    ) %>% colorbar(
      title = "Rate"
    )
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          CONVERSION CHANNEL FLOW                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_conversion_flow <- function(date_range_sankey, metric_sankey, segment_name_sankey, funnel_name_sankey){
  #Prep the Sankey Title based on Date and Channel
  sankey_title <- paste(funnel_name_sankey, "Conversion Flow ", date_range_sankey[1], "-", date_range_sankey[2])
  
  df_sankey <- adobeanalyticsr::aw_freeform_table(
    date_range = date_range_sankey,
    dimensions = c("evar38"),
    metrics = c(metric_sankey,"evar38instances"),
    top = c(50000),
    page = 0,
    filterType = "breakdown",
    segmentId = segment_name_sankey, 
    metricSort = "desc",
    include_unspecified = FALSE,
    search = NA,
    prettynames = FALSE,
    debug = FALSE
  )
  df_sankey <- df_sankey %>% dplyr::rename("path" = 1, 
                                           "conversion" = 2,
                                           "path_count" = 3)
  df_sankey$path_list = strsplit(x=df_sankey$path, split = ">")
  
  depth = 4
  
  #Generate node labels and label length vectors
  node_labels=rep(list(list()),depth)
  label_length = list()
  for(i in 1:depth){
    for(j in 1:length(df_sankey$path)){
      if(!is.na(df_sankey$path_list[j][[1]][i]))
        node_labels[[i]][j] = df_sankey$path_list[j][[1]][i]
    }
    node_labels[[i]] = unique(unlist(node_labels[[i]]))
    node_labels[[i]] = node_labels[[i]][order(node_labels[[i]])]
    label_length[[i]] = length(node_labels[[i]])
  }
  node_labels = unlist(node_labels)
  label_length = unlist(label_length)
  
  #Build a data frame to fill out with each path view
  combos = NULL
  for(i in 1:(depth-1)){
    for(j in (1 + sum(label_length[1:i-1])):(label_length[i] + sum(label_length[1:i-1]))){
      for(k in (1 + label_length[i] + sum(label_length[1:i-1])):(label_length[i+1] + label_length[i] + sum(label_length[1:i-1]))){
        combos = rbind(combos, c(i,j,k,0))
      } 
    }
  }
  combos = as.data.frame(combos)
  names(combos) = c("step","source","target","value")
  
  #Populate the combo table
  for(i in 1:(dim(combos)[1])){
    for(j in 1:(dim(df_sankey)[1])){
      combos$value[i] = sum(combos$value[i], ifelse(
        (node_labels[combos$source[i]] == df_sankey$path_list[j][[1]][combos$step[i]]) &
          (node_labels[combos$target[i]] == df_sankey$path_list[j][[1]][combos$step[i]+1]),
        df_sankey$path_count[j],0), na.rm = TRUE)
    }
  }
  #OK to here
  #Add a node to populate with conversion values
  uniques = unique(c(combos$source,combos$target))
  
  converts = as.data.frame(list("step"=rep(0,length(uniques)), "source"=uniques, "target"=rep(max(uniques)+1,length(uniques)), 
                                "value"=rep(0,length(uniques))))
  combos = rbind(combos,converts)
  for(i in 1:(dim(df_sankey)[1])){
    stack_depth = min(depth,length(df_sankey$path_list[i][[1]]))
    index_val = which(combos$step==0 & combos$source==(which(node_labels == df_sankey$path_list[i][[1]][stack_depth]) + 
                                                         ifelse(stack_depth>1, sum(label_length[1:(stack_depth-1)]),0)))
    combos$value[index_val] = combos$value[index_val] + df_sankey$conversion[i]
  }
  #Populate the conversion node values
  display_node_labels = node_labels
  for(i in 1:length(label_length)){
    for(j in 1:label_length[i]){
      display_node_labels[j+ifelse(i==1,0,sum(label_length[1:(i-1)]))] = paste0(i,":",node_labels[j+ifelse(i==1,0,sum(label_length[1:(i-1)]))])
    }
  }
  display_node_labels = c(display_node_labels, "Conversion")
  #Generate Sankey diagram
  s2 <- plot_ly(
    type = "sankey",
    orientation = "h",
    
    node = list(
      label = display_node_labels,
      #color = node_colors,
      pad = 10,
      alpha = 0.9,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.1
      )
    ),
    
    link = list(
      source = combos$source-1, # convert to zero index
      target = combos$target-1, # convert to zero index
      value = combos$value, #size of connection
      color = 'rgba(0,0,0,0.1)'
      #color = combos$color #add colors for each link if desired
    )
  ) %>% 
    layout(
      title = sankey_title,
      font = list(
        size = 10
      )
    )
  return (s2)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      Dumb Bell Plot for Journey Summary                   ---
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



get_journey_plot <- function(data_journey_plot){
  
  journey_before_after_plot <- plotly::plot_ly(
    data = data_journey_plot, mode = 'markers') %>% 
    add_segments(
      x = ~pre, y = ~journey_name,
      xend = ~post, yend = ~journey_name, 
      color = I("gray"), line = list(
        color = 'rgb(192,192,192)',
        width = 7
      ), showlegend = FALSE
    ) %>%
    add_markers(
      x = ~pre, y = ~journey_name, 
      marker = list(
        color = 'rgb(64, 104, 130)',
        size = 10,
        line = list(
          color = 'rgb(192,192,192)',
          width = 0.1
        )
      ),
      name = "Pre"
    ) %>%
    add_markers(
      x = ~post, y = ~journey_name,
      marker = list(
        color = 'rgb(240, 84, 84)',
        size = 10,
        line = list(
          color = 'rgb(128,128,128)',
          width = 0.1
        )
      ),
      name  = "Post"
    ) %>%
    layout(xaxis = list(title = "Pre vs. Post Baseline (Visits)")) %>% 
    layout(yaxis = list(title = "Journey Name"))
  
  return (journey_before_after_plot)
}

get_post_only_journey_plot <- function(data_journey_plot){
  journey_before_after_plot <- plotly::plot_ly(
    data = data_journey_plot, mode = 'markers') %>% 
    add_segments(
      x = ~Visits_mean, y = ~journey_name,
      xend = ~Visits_mean_last_week, yend = ~journey_name, 
      color = I("gray"), line = list(
        color = 'rgb(192,192,192)',
        width = 7
      ), showlegend = FALSE
    ) %>%
    add_markers(
      x = ~Visits_mean_last_week, y = ~journey_name, 
      marker = list(
        color = 'rgb(64, 104, 130)', # Blue
        size = 10,
        line = list(
          color = 'rgb(192,192,192)',
          width = 0.1
        )
      ),
      name = "Last Week"
    ) %>%
    # add_markers(
    #   x = ~Visits_min, y = ~journey_name, 
    #   marker = list(
    #     color = 'rgb(130, 64, 104)', # Purple
    #     size = 10,
    #     line = list(
    #       color = 'rgb(192,192,192)',
    #       width = 0.1
    #     )
    #   ),
    #   name = "Min"
    # ) %>%
    # add_markers(
    #   x = ~Visits_max, y = ~journey_name, 
    #   marker = list(
    #     color = 'rgb(77, 64, 130)', # Dark Blue
    #     size = 10,
    #     line = list(
    #       color = 'rgb(192,192,192)',
    #       width = 0.1
    #     )
    #   ),
    #   name = "Max"
    # ) %>%
    # add_markers(
    #   x = ~Visits_median, y = ~journey_name, 
    #   marker = list(
    #     color = 'rgb(104, 130, 64)', # Green
    #     size = 10,
    #     line = list(
    #       color = 'rgb(192,192,192)',
    #       width = 0.1
    #     )
    #   ),
    #   name = "Median"
    # ) %>%
    add_markers(
      x = ~Visits_mean, y = ~journey_name,
      marker = list(
        color = 'rgb(240, 84, 84)', # Red
        size = 10,
        line = list(
          color = 'rgb(128,128,128)',
          width = 0.1
        )
      ),
      name  = "Today"
    ) %>%
    layout(xaxis = list(title = "Last Week vs. Today Baseline (Visits)")) %>% 
    layout(yaxis = list(title = "Journey Name"))
  
  return (journey_before_after_plot)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      Internal Search Functions                            ---
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_search_data <- function(date_range) {
  adobeanalyticsr::aw_freeform_table(
    company_id = Sys.getenv("AW_COMPANY_ID"),
    rsid = Sys.getenv("AW_REPORTSUITE_ID"),
    date_range = date_range,
    dimensions = c("evar13"),
    metrics = c("evar13instances"),
    top = c(5000),
    page = 0,
    filterType = "breakdown",
    segmentId = "s1957_619389fabb66f417bb8adc2f",
    metricSort = "desc",
    include_unspecified = TRUE,
    # search = c("CONTAINS 'mem'"),
    search = NA,
    prettynames = FALSE,
    debug = FALSE
  )
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      Google Search Console Functions                      ---
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_search_console_data <- function(start, end) {
  search_analytics(siteURL = website, 
                   startDate = start, 
                   endDate = end, 
                   dimensions = download_dimensions, 
                   searchType = type)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        Insight Generator Function                         ---
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_insights <- function(journey_name_insight, insight_metric){
#Testing
#insight_metric <- "Page Views"
#journey_name_insight <- "Commercial: Membership Checkout Steps 1-4"
#use_metric <- "Holidays Booking Total Revenue (Serialized) (ev125)"
#journey_name.insight <- "Commercial: Holidays Checkout Steps 1-4"
  
journey.insight <- data.frame()  

metric <- insight_metric
metric

insight.data <- journey_data %>% 
  filter(journey_name == journey_name_insight) %>% 
  filter(Day >= first_valid_date & Day < last_valid_date) %>% 
  select(journey_name, journey_type, Day, !!as.name(metric)) %>% 
  group_by(journey_name) %>% 
  arrange(desc(journey_name), Day) %>% 
  mutate(trend.avg.3da = round(zoo::rollmean(metric, k = 3, fill = NA),digits = 0),
         trend.avg.5da = round(zoo::rollmean(metric, k = 5, fill = NA),digits = 0),
         trend.avg.7da = round(zoo::rollmean(metric, k = 7, fill = NA),digits = 0),
         trend.avg.15da = round(zoo::rollmean(metric, k = 15, fill = NA),digits = 0),
         trend.avg.21da = round(zoo::rollmean(metric, k = 21, fill = NA),digits = 0),
         trend.avg.30da = round(zoo::rollmean(metric, k = 30, fill = NA),digits = 0),
         day.x = as.numeric(Day) # Add a column to get the date as a numeric value for the LM model
  ) %>% 
  replace(is.na(.), 0) %>% 
  mutate(trend.slope.test = abs(lm(day.x ~ !!as.name(metric))$coefficients[2])) %>% 
  mutate(trend.slope = cor(day.x, !!as.name(metric))) %>% 
  mutate(metric_name = metric) %>% 
  relocate(metric_name, .after = journey_name)

# Build the summary table of key metrics for the insight generator.

journey.insight <- insight.data %>% group_by(journey_name) %>% 
  summarize(min.metric = min(!!as.name(metric)), min.metric.Date.Name = Day[which.min(!!as.name(metric))],
            max.metric = max(!!as.name(metric)), max.metric.Date.Name = Day[which.max(!!as.name(metric))],
            trend.avg.3da = round(mean(trend.avg.3da), digits = 0),
            trend.avg.5da = round(mean(trend.avg.5da), digits = 0),
            trend.avg.7da = round(mean(trend.avg.7da), digits = 0),
            trend.avg.15da = round(mean(trend.avg.15da), digits = 0),
            trend.avg.21da = round(mean(trend.avg.21da), digits = 0),
            trend.avg.30da = round(mean(trend.avg.30da), digits = 0),
            avg.metric = round(mean(!!as.name(metric)), digits = 0),
            sd.metric = round(sd(!!as.name(metric)), digits = 0),
            sd.1.metric = round(sd.metric+avg.metric, digits = 0),
            sd.2.metric = round((2*sd.metric)+avg.metric, digits = 0),
            trend.slope = signif(mean(trend.slope),digits = 2),
            trend.slope.test = signif(mean(trend.slope.test),digits = 2)
  ) %>% 
  mutate(min.avg.diff = round((min.metric/avg.metric -1) * 100, digits = 1),
         trend.slope.correlation = ifelse(sign(trend.slope) == -1, "NEGATIVE",
                                          ifelse(sign(trend.slope) == 1, "POSITIVE",
                                                 ifelse(sign(trend.slope) == 0, "FLAT")))
  ) %>% 
  mutate(metric_name = metric) %>% 
  relocate(metric_name, .after = journey_name)

# Find the date and amount of the highest most significant spike that is at least 2 SD above the mean.
journey.spike.most.recent.date <- data.frame()

journey.spike.most.recent.date <- insight.data %>% 
  select(journey_name, Day, !!as.name(metric)) %>%
  group_by(journey_name) %>%
  mutate(sd.2.spike = !!as.name(metric) > journey.insight$sd.2.metric) %>%
  mutate(sd.1.spike = !!as.name(metric) > journey.insight$sd.1.metric) %>%
  filter(sd.2.spike == TRUE) %>%
  arrange(desc(Day)) %>%
  rename(metric.spike = !!as.name(metric)) %>% 
  slice(1:1)  

#journey.spike.most.recent.date
# If no spikes are found fill the data table with appropriate info.
if (nrow(journey.spike.most.recent.date) == 0) {
  print("No Data for Spikes")
  journey.spike.most.recent.date <- insight.data %>% 
    select(journey_name, Day, !!as.name(metric)) %>%
    group_by(journey_name) %>%
    mutate(sd.2.spike = "No Spikes Detected",
           sd.1.spike = "No Spikes Detected",
           Day = NA) %>% slice(1:1) 
}
# Previous Week
trend.7 <-  insight.data %>% select(journey_name, Day, !!as.name(metric), day.x) %>% group_by(journey_name) %>% 
  filter(Day >= seven_days_ago & Day <= seven_days_ago+6) %>% 
  summarize(trend.slope.7 = cor(day.x, !!as.name(metric))) %>% 
  mutate(correlation.7 = ifelse(sign(trend.slope.7) == -1, "NEGATIVE",
                                ifelse(sign(trend.slope.7) == 1, "POSITIVE",
                                       ifelse(sign(trend.slope.7) == 0, "FLAT")))
  )


# Last Week
trend.14 <- insight.data %>% select(journey_name, Day, !!as.name(metric), day.x) %>% group_by(journey_name) %>% 
  filter(Day >= fourteen_days_ago & Day <= fourteen_days_ago+6) %>% 
  summarize(trend.slope.14 = cor(day.x, !!as.name(metric))) %>% 
  mutate(correlation.14 = ifelse(sign(trend.slope.14) == -1, "NEGATIVE",
                                 ifelse(sign(trend.slope.14) == 1, "POSITIVE",
                                        ifelse(sign(trend.slope.14) == 0, "FLAT")))
  )
# Merge the two tables together to build a trend comparison over the last two weeks.
trend <- merge(x = trend.14, y = trend.7, by = 'journey_name')
# Merge into the main journey insight table 
journey.insight <- merge(x = journey.insight, y = trend, by= 'journey_name')
# Calculate the change values in slope.
journey.insight <- journey.insight %>% mutate(trend.change = round((trend.slope.7 / trend.slope.14), digits = 1),
                                              trend.change.relative = round(((trend.slope.14 - trend.slope.7) / trend.slope.14), digits = 1))

journey.insight <- merge(x = journey.insight, y = journey.spike.most.recent.date, by = 'journey_name')

rm(trend.7)
rm(trend.14)

return(journey.insight)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Build Events Trend Graphs                        ---
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_events <- function(event_ids){
  end_date <- Sys.Date() - 1
  start_date <- Sys.Date() - 180
  date_range_local <- c(start_date, end_date)
  df <- aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                          rsid = Sys.getenv("AW_REPORTSUITE_ID"), 
                          date_range = date_range_local,
                          dimensions = "daterangeday",
                          top = 0,
                          metrics = event_ids) %>%
    pivot_longer(-daterangeday) %>%
    arrange(daterangeday, name) %>% 
    select(day = daterangeday, id = name, value)
}


get_marketing_channel_metrics <- function(){
# Debug
# end_date <- Sys.Date() - 1
# start_date <- Sys.Date() - 30
  
  current_week_no <- week(last_valid_date)
  last_complete_week <- current_week_no-1
  last_week_end <- year_start+last_complete_week*weeks()
  
  start_week_no <- last_complete_week-4
  start_date <- year_start+start_week_no*weeks()+1
  
  date_range_local <- c(start_date, last_week_end)
  df <- aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                          rsid = Sys.getenv("AW_REPORTSUITE_ID"), 
                          date_range = date_range_local,
                          dimensions = c("marketingchannel","daterangeday"),
                          page = 0,
                          top = c(15, 30),
                          metrics = c('visits', 'event5','revenue','event79', 'event125', 'cm1957_5fc4b9a2b5895e0644b9120e')
  )
}


get_forecast <- function(journey_name_forecast, forecast_metric){
#options(scipen = 999)
# Debug
# journey_name_forecast <- "Commercial: Renew Checkout Steps 1-3"
# forecast_metric <- "Visits"

forecast_data <- journey_data %>% 
  filter(journey_name == journey_name_forecast) %>% 
  filter(Day >= first_valid_date & Day < last_valid_date) %>% 
  select(journey_name, Day, !!as.name(forecast_metric)) %>% 
  group_by(journey_name) %>% 
  arrange(desc(journey_name), Day)
  
  prophet_data <- forecast_data %>% ungroup() %>% select(Day, !!as.name(forecast_metric))
  df <- prophet_data
  #tail(df)
  
  names(df) <- c('ds', 'y') 
  m <- prophet(df)
  future <- make_future_dataframe(m, periods=60)
  forecast <- predict(m, future)
  #tail(forecast)
  
  gg <- plot(m, forecast, xlab = "Day", ylab = forecast_metric) + add_changepoints_to_plot(m)
  #prophet_plot_components(m, forecast)
  plotname <- paste0(journey_name_forecast)
  gg <- gg + ggplot2::ggtitle(plotname)
  
  return(gg)
}

# Time on Site ##################################################################
  
get_time_onsite <- function(device_segment_id, start_date, end_date, plot_title) {
# # # Debug:
#  start_date <- Sys.Date() - 90
#  end_date <- Sys.Date()
#  device_segment_id <- 's1957_6113b90d5f900636dbd9b2ff'
#  plot_title <- "Time on Site: All Main Site, All Devices"
# # # End Debug

  date_range_tos <- c(start_date, end_date)
  segment_group_tos = c()
  segment_group_tos <- c(device_segment_id, "s1957_611e5647d17b6b04401e42d2") # static segment ID for NT Main site + device segment

    time_on_site <- adobeanalyticsr::aw_anomaly_report(
                                    rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                    date_range = date_range_tos,
                                    metrics = "averagetimespentonsite",
                                    granularity = "day",
                                    quickView = FALSE,
                                    countRepeatInstances = TRUE,
                                    segmentId = segment_group_tos,
                                    anomalyDetection = TRUE,
                                    debug = FALSE) %>% 
    mutate(data = round(data, 0)) %>% 
    arrange(day)
  
  avg.tos.two.weeks.ago <- time_on_site %>% 
    filter(day >= fourteen_days_ago & day <= fourteen_days_ago+6) %>% 
    arrange(day) %>% select(data) %>% 
    summarize(mean(data)) %>% 
    pull()
  avg.tos.two.weeks.ago  <- lubridate::duration(round(avg.tos.two.weeks.ago,0))
   
  avg.tos.last.week <- time_on_site %>% 
    filter(day >= seven_days_ago & day <= seven_days_ago+6) %>% 
    arrange(day) %>% select(data) %>% 
    summarize(mean(data)) %>% 
    pull()
  avg.tos.last.week <- lubridate::duration(round(avg.tos.last.week,0))
  tos.diff <- avg.tos.two.weeks.ago-avg.tos.last.week
  
  
  p2 <- time_on_site %>%  # Use the subset to build the anomaly chart
    ggplot(aes_string(x = "day")) +
    geom_line(aes_string( y = 'data'), color="#69b3a2", size = 0.8) +
    geom_point(data = time_on_site %>% dplyr::filter(metric == 'averagetimespentonsite' & dataAnomalyDetected == T),
               aes_string(y ='data'), color="red", size = 1.8) +
    geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
    geom_vline(xintercept = as.numeric(as.Date(post_start_date)), color = "red", linetype='dotted', lwd = .8, alpha = 0.5) +
    geom_hline(yintercept = mean(time_on_site$data), color = "blue", linetype='dotted', lwd = .8, alpha = 0.5) +
    labs(title = plot_title,
         subtitle = paste0('The average time last week was ',avg.tos.last.week,'. Difference of ',tos.diff,' over the prior week.'),
         caption =paste0('There are ',nrow(time_on_site %>% filter(metric == 'averagetimespentonsite' & dataAnomalyDetected == T)), ' anomalies.')) +
    theme_bw() +
    theme(axis.title.y = element_text(face = "bold", angle = 90), axis.title.x = element_text(face = "bold", angle = 0), axis.text.x = element_text(angle = 90)) +
    scale_x_date(date_breaks = "week" , date_labels = "%d-%b")+
    scale_y_continuous(labels = scales::comma, trans = 'log10') +
    expand_limits(y=0) +
    ylab("Avg. Time on Site (sec)")+
    xlab("Day")
  #p2
  return (p2)
}

get_cms_page_metrics <- function(page_name_exact, date_range) {
  search_page <- paste0("MATCH '",page_name_exact,"'")
   aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                      rsid = Sys.getenv("AW_REPORTSUITE_ID"), 
                      date_range = date_range,
                      dimensions = c("page", "daterangeday"),
                      metrics = c("visits"),
                      top = c(180),
                      page = 0,
                      filterType = "breakdown",
                      segmentId = NA,
                      include_unspecified = TRUE,
                      search = search_page,
                      prettynames = FALSE,
                      debug = FALSE,
                      check_components = TRUE)
}

get_cms_segment_metrics <- function(segment_id, date_range) {
  # get segment id and journey to secure site
  aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                      rsid = Sys.getenv("AW_REPORTSUITE_ID"), 
                      date_range = date_range,
                      dimensions = c("daterangeday"),
                      metrics = c("visits"),
                      top = c(180),
                      page = 0,
                      filterType = "breakdown",
                      segmentId = segment_id,
                      include_unspecified = TRUE,
                      search = NA,
                      prettynames = FALSE,
                      debug = FALSE,
                      check_components = TRUE)
}

get_cms_summary <- function(product_name){
# Merge the data frames
  cms_to_secure <- merge(x = cms_page_metrics, y = cms_segment_metrics, by = "daterangeday")
  cms_to_secure <- cms_to_secure %>% rename(Day = "daterangeday",
                                            page_visits = visits.x, 
                                            step_1_visits = visits.y)
  
  # calculate the conversion to the secure site
  cms_to_secure_day <<- cms_to_secure %>% 
    mutate(conv_to_secure = round(step_1_visits / page_visits, 4))
                      
  cms_to_secure_pre <- cms_to_secure_day %>% 
    filter(Day >= pre_end_date_30 & Day <= pre_end_date) %>% 
    summarise(across(where(is.numeric), list(sum = sum, mean = mean), .names = "{.col}_{.fn}")) %>% 
    mutate(period = "pre_baseline",
           product = product_name) %>% 
    relocate(period, .before = page_visits_sum) %>% 
    relocate(product, .before = period) %>% 
    mutate(conversion = step_1_visits_sum / page_visits_sum)
  
  cms_to_secure_post <- cms_to_secure_day %>% 
    filter(Day >= last_valid_date-30 & Day <= last_valid_date) %>% 
    summarise(across(where(is.numeric), list(sum = sum, mean = mean), .names = "{.col}_{.fn}")) %>% 
    mutate(period = "post_baseline",
           product = product_name) %>% 
    relocate(period, .before = page_visits_sum) %>% 
    relocate(product, .before = period) %>% 
    mutate(conversion = step_1_visits_sum / page_visits_sum)
  
  cms_to_secure_summary <- rbind(cms_to_secure_pre, cms_to_secure_post)
  return(cms_to_secure_summary)
  rm(cms_segment_metrics)
  rm(cms_page_metrics)
  rm(cms_to_secure_post)
  rm(cms_to_secure_pre)
}

get_marketing_channels <- function(start_date_mc, end_date_mc) {
  #date_range_mc <- c(fourteen_days_ago, fourteen_days_ago+6) #Debug
  date_range_mc <- c(as.Date(start_date_mc), as.Date(end_date_mc))
  ## Marketing Channels Data Extract
  total_visits <- aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                                    rsid = Sys.getenv("AW_REPORTSUITE_ID"), 
                                    date_range = date_range_mc,
                                    dimensions = "daterangeyear",
                                    metrics = "visits") %>% pull(visits) %>% sum()
  
  # Get the marketing channel data
  df_marketing_channels <<- aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
                                             rsid = Sys.getenv("AW_REPORTSUITE_ID"), 
                                             date_range = date_range_mc,
                                             dimensions = "marketingchannel",
                                             metrics = "visits",
                                             top = c(15)
                                             )
  
  # Calculate the % of total visits
  df_marketing_channels <- df_marketing_channels %>% 
    mutate(pct_visits = round(visits/total_visits, 4)) %>% 
    # Convert to a factor for ordering in the plot
    mutate(marketingchannel = factor(marketingchannel, levels = rev(marketingchannel)))
  
  # How many channels have pretty low traffic to the site
  mc_low <<- paste0("**", df_marketing_channels %>% 
                     filter(pct_visits < 0.01) %>% 
                     nrow(), " channels** each account for **less than 1% of total visits** to the site.")
  
  mc_none <<- if(df_marketing_channels %>% filter(marketingchannel == "None") %>% nrow() == 0){
    paste0("**'None'** does not show up at all for marketing channels. This is great!")
  } else if (df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) < 0.01) {
    paste0("**'None'** appears as a marketing channel, but it is **less than 1% of total traffic**, so this is of low concern.")
  } else if (df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) < 0.05) {
    paste0("**'None'** appears as a marketing channel accounting for **", df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) %>% percent(accuracy = 0.1)," of traffic**, which is **somewhat concerning**.")
  } else {
    paste0("**'None'** appears as a marketing channel accounting for **", df_marketing_channels %>% filter(marketingchannel == "None") %>% pull(pct_visits) %>% percent(accuracy = 0.1)," of traffic**, which is **very concerning**.")
  }
    
   # Find "the one" that is the name used in this case. Get a df with just that one row
  df_session_refresh <- df_marketing_channels %>% 
    filter(marketingchannel %in% session_refresh_names)
  
  session_refresh_name <- case_when(
    nrow(df_session_refresh) == 0 ~ "None Found",
    nrow(df_session_refresh) == 1 ~ as.character(df_session_refresh$marketingchannel[1]),
    # Having 2 or more rows match would be a surprise. For now, just using the first
    # row that appears if that happens, but could change later to get fancier
    TRUE ~ as.character(df_session_refresh$marketingchannel[1])   
  )
  
  
  mc_session_refresh <<- if(session_refresh_name == "None Found"){ 
    paste("**Session Refresh** does not appear to show up at all for marketing channels. If such a channel exists and",
          "is properly configured, this is great! If the channel exists, but with a slightly different name that we",
          "were not able to detect, check the chart below to confirm that it is not showing up more than expected.")
  } else {
    case_when(
      df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% pull(pct_visits) < 0.01 ~
        paste0("**", session_refresh_name,"** appears as a marketing channel, but it is **less than 1% of total traffic**, so this is of low concern."),
      
      df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% pull(pct_visits) < 0.05 ~
        paste0("**", session_refresh_name,"** appears as a marketing channel accounting for **",
               df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% 
                 pull(pct_visits) %>% percent(accuracy = 0.1),
               " of traffic**, which is **somewhat concerning**."),
      TRUE ~ paste0("**", session_refresh_name,"** appears as a marketing channel accounting for **", 
                    df_marketing_channels %>% filter(marketingchannel == session_refresh_name) %>% 
                      pull(pct_visits) %>% percent(accuracy = 0.1),
                    " of traffic**, which is **very concerning**."))
  }
  
  gg_marketing_channels <- ggplot(df_marketing_channels, aes(x = marketingchannel, y = visits, 
                                                             label = paste0(format(visits, big.mark = ",", trim = TRUE), " (",
                                                                            percent(pct_visits, accuracy = 0.1), ")"))) +
    geom_bar(stat = "identity", fill = "#406882") +
    geom_text(aes(y = visits + max(df_marketing_channels$visits) * 0.02), hjust = 0, size = 2.5, color = "gray30") +
    coord_flip() +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(df_marketing_channels$visits) * 1.25)) +
    labs(title = "Visits by Marketing Channel",
         subtitle = paste0(rsid, ": ", start_date_mc, " to ", end_date_mc),
         caption = paste("Source: Adobe Analytics - RSID:", rsid, "-", start_date_mc, "to", end_date_mc)) +
    theme_minimal() +
    theme(plot.title.position = "plot",
          plot.title = element_text(size = 11, color = "gray20"),
          plot.subtitle = element_text(size = 10, face = "italic", color = "gray40"),
          plot.caption = element_text(face = "italic", color = "gray40"),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank())
  
  return(gg_marketing_channels)
}

