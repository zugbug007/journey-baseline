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
