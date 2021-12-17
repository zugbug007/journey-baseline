library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)
library(plotly)
library(httr)
library(jsonlite)
library(here)
library(dplyr)
library(googlesheets4)# Import and manipulate Google Sheets docs
gs4_auth()

# WAIT Until auth completed.

options(scipen=10000)
API_KEY = Sys.getenv("YOUTUBE_API_KEY")
channel_id <- Sys.getenv("YOUTUBE_CHANNEL_ID") 

user_id <- "nationaltrustcharity"
base <- "https://www.googleapis.com/youtube/v3/"

# Note: The statistics.dislikeCount property was made private as of December 13, 2021. 
# Construct the API call
api_params <- 
  paste(paste0("key=", API_KEY), 
        paste0("id=", channel_id), 
        "part=snippet,contentDetails,statistics",
        sep = "&")
api_call <- paste0(base, "channels", "?", api_params)
api_result <- GET(api_call)
json_result <- content(api_result, "text", encoding="UTF-8")

# Process the raw data into a data frame
channel.json <- fromJSON(json_result, flatten = T)
channel.df <- as.data.frame(channel.json)

playlist_id <- channel.df$items.contentDetails.relatedPlaylists.uploads

# temporary variables
nextPageToken <- ""
upload.df <- NULL
pageInfo <- NULL

# Loop through the playlist while there is still a next page
while (!is.null(nextPageToken)) {
  # Construct the API call
  api_params <- 
    paste(paste0("key=", API_KEY), 
          paste0("playlistId=", playlist_id), 
          "part=snippet,contentDetails,status",
          "maxResults=50",
          sep = "&")
  
  # Add the page token for page 2 onwards
  if (nextPageToken != "") {
    api_params <- paste0(api_params,
                         "&pageToken=",nextPageToken)
  }
  
  api_call <- paste0(base, "playlistItems", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  upload.json <- fromJSON(json_result, flatten = T)
  
  nextPageToken <- upload.json$nextPageToken
  pageInfo <- upload.json$pageInfo
  
  curr.df <- as.data.frame(upload.json$items)
  if (is.null(upload.df)) {
    upload.df <- curr.df
  } else {
    upload.df <- bind_rows(upload.df, curr.df)
  }
}

video.df<- NULL
# Loop through all uploaded videos
for (i in 1:nrow(upload.df)) {
  # Construct the API call
  video_id <- upload.df$contentDetails.videoId[i]
  api_params <- 
    paste(paste0("key=", API_KEY), 
          paste0("id=", video_id), 
          "part=id,statistics,contentDetails",
          sep = "&")
  
  api_call <- paste0(base, "videos", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  video.json <- fromJSON(json_result, flatten = T)
  
  curr.df <- as.data.frame(video.json$items)
  
  if (is.null(video.df)) {
    video.df <- curr.df
  } else {
    video.df <- bind_rows(video.df, curr.df)
  }
}  

# Combine all video data frames
video.df$contentDetails.videoId <- video.df$id
video_final.df <- merge(x = upload.df, 
                        y = video.df,
                        by = "contentDetails.videoId")

yt_data <- video_final.df %>% 
  select(snippet.publishedAt, snippet.title, snippet.description, snippet.position, starts_with("statistics")) %>% 
  arrange(desc(snippet.publishedAt)) %>% mutate(data.last.updated = Sys.time())

# Convert column types
yt_data$snippet.publishedAt <- as.Date(as.character(yt_data$snippet.publishedAt))
yt_data$snippet.position <- as.numeric(as.character(yt_data$snippet.position))
yt_data$statistics.viewCount <- as.numeric(as.character(yt_data$statistics.viewCount))
yt_data$statistics.favoriteCount <- as.numeric(as.character(yt_data$statistics.favoriteCount))
yt_data$statistics.commentCount <- as.numeric(as.character(yt_data$statistics.commentCount))
write_sheet(yt_data, "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", sheet ="YouTube_data")

channel_stats <- channel.df %>% select(items.snippet.title, items.snippet.description, items.snippet.customUrl, starts_with("items.statistics"))
channel_stats$items.statistics.viewCount <- as.numeric(as.character(channel_stats$items.statistics.viewCount))
channel_stats$items.statistics.subscriberCount <- as.numeric(as.character(channel_stats$items.statistics.subscriberCount))
channel_stats$items.statistics.videoCount <- as.numeric(as.character(channel_stats$items.statistics.videoCount))
channel_stats <- channel_stats %>% mutate(data.last.updated = Sys.time())

write_sheet(channel_stats, "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", sheet ="YouTube_Channel_Stats")


#####################################################

adobe_video_stats <- adobeanalyticsr::aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(Sys.Date() - 768, Sys.Date() - 1),
  dimensions = c("evar27"),
  metrics = c("event10", "event12", "event13","event14", "event15", "cm1957_601d669663a03a772d6a2556", "event11"),
  top = c(20000),
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = TRUE,
  search = NA,
  prettynames = TRUE,
  debug = FALSE
)
adobe_video_stats <- adobe_video_stats %>% mutate(data.last.updated = Sys.time())
write_sheet(adobe_video_stats, "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", sheet ="Adobe_Video_Stats")
