##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            Function Call Setup                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_segment_data <- function(segment_ids, metrics, date_range) {
  adobeanalyticsr::aw_freeform_table(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                     date_range = date_range,
                                     dimensions = "daterangeday",
                                     segmentId = segment_ids, # catch passed comma separated vector segment IDs group to pull data against. 
                                     prettynames = TRUE,      # Don't change this as many following variables names depend on this naming
                                     metrics = metrics,       # catch the comma separated vector group of metrics specified for this journey
                                     debug = FALSE
  )
  
}

get_anomaly_data <- function(segment_ids, metrics, data_range){
  adobeanalyticsr::aw_anomaly_report(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
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
  
  depth = 5
  
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
  filter(Day >= start_7_sun_start & Day <= end_7_sun_end) %>% 
  summarize(trend.slope.7 = cor(day.x, !!as.name(metric))) %>% 
  mutate(correlation.7 = ifelse(sign(trend.slope.7) == -1, "NEGATIVE",
                                ifelse(sign(trend.slope.7) == 1, "POSITIVE",
                                       ifelse(sign(trend.slope.7) == 0, "FLAT")))
  )


# Last Week
trend.14 <- insight.data %>% select(journey_name, Day, !!as.name(metric), day.x) %>% group_by(journey_name) %>% 
  filter(Day >= start_14_sun_start & Day <= end_14_sun_end) %>% 
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
  start_date <- Sys.Date() - 90
  date_range_local <- c(start_date, end_date)
  df <- aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), rsid = Sys.getenv("AW_REPORTSUITE_ID"), date_range = date_range_local,
                          dimensions = "daterangeday",
                          metrics = event_ids) %>%
    pivot_longer(-daterangeday) %>%
    arrange(daterangeday, name) %>% 
    select(day = daterangeday, id = name, value)
}
