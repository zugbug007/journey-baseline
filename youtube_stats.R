library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)
library(adobeanalyticsr)
library(plotly)
library(tuber)
library(purrr)
library(lubridate)
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
client_id <- Sys.getenv("YOUTUBE_CLIENT_ID")
client_secret <- Sys.getenv("YOUTUBE_CLIENT_SECRET")

# WAIT Until auth completed.

yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')
# WAIT Until auth completed.

user_id <- "nationaltrustcharity"
base <- "https://www.googleapis.com/youtube/v3/"

# Note: The statistics.dislikeCount property was made private as of December 13, 2021. 
# Construct the API call - https://developers.google.com/youtube/v3/docs
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
write_sheet(video_final.df, "https://docs.google.com/spreadsheets/d/18RlnfFeR1rlsqRLEtdHWz7pE82LKdzmwq-Pioa0gc40/edit?usp=sharing", sheet ="FULL_API_DUMP")

yt_data <- video_final.df %>% 
  select(snippet.publishedAt, contentDetails.videoId, contentDetails.duration, snippet.title, snippet.description, snippet.position, starts_with("statistics")) %>% 
  arrange(desc(snippet.publishedAt)) %>% mutate(data.last.updated = Sys.time()) %>% rowwise() %>% mutate(contentDetails.duration.sec = as.numeric(as.duration(contentDetails.duration)))

# Convert column types
yt_data$snippet.publishedAt <- as.Date(as.character(yt_data$snippet.publishedAt))
yt_data$snippet.position <- as.numeric(as.character(yt_data$snippet.position))
yt_data$statistics.viewCount <- as.numeric(as.character(yt_data$statistics.viewCount))
yt_data$statistics.favoriteCount <- as.numeric(as.character(yt_data$statistics.favoriteCount))
yt_data$statistics.commentCount <- as.numeric(as.character(yt_data$statistics.commentCount))
write_sheet(yt_data, "https://docs.google.com/spreadsheets/d/18RlnfFeR1rlsqRLEtdHWz7pE82LKdzmwq-Pioa0gc40/edit?usp=sharing", sheet ="YouTube_Data_API")

channel_stats <- channel.df %>% select(items.snippet.title, items.snippet.description, items.snippet.customUrl, starts_with("items.statistics"))
channel_stats$items.statistics.viewCount <- as.numeric(as.character(channel_stats$items.statistics.viewCount))
channel_stats$items.statistics.subscriberCount <- as.numeric(as.character(channel_stats$items.statistics.subscriberCount))
channel_stats$items.statistics.videoCount <- as.numeric(as.character(channel_stats$items.statistics.videoCount))
channel_stats <- channel_stats %>% mutate(data.last.updated = Sys.time())

write_sheet(channel_stats, "https://docs.google.com/spreadsheets/d/18RlnfFeR1rlsqRLEtdHWz7pE82LKdzmwq-Pioa0gc40/edit?usp=sharing", sheet ="YouTube_Channel_Stats")


#####################################################

adobe_video_stats <- adobeanalyticsr::aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(Sys.Date() - 730, Sys.Date() - 1),
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
write_sheet(adobe_video_stats, "https://docs.google.com/spreadsheets/d/18RlnfFeR1rlsqRLEtdHWz7pE82LKdzmwq-Pioa0gc40/edit?usp=sharing", sheet ="Adobe_Video_Stats_API")

video_data_combined <- merge(x = yt_data, y = adobe_video_stats, by.x = "snippet.title", by.y = "Video Name (v27)") 

video_data_combined <- video_data_combined %>% 
  select(-starts_with("data.last.updated")) %>% 
  rename(YT_Views = statistics.viewCount) %>% 
  rename(Adobe_Views = `Video Start (ev10)`) %>% 
  mutate(total_views = YT_Views + Adobe_Views)%>% 
  mutate(data.last.updated = Sys.time())

write_sheet(video_data_combined, "https://docs.google.com/spreadsheets/d/18RlnfFeR1rlsqRLEtdHWz7pE82LKdzmwq-Pioa0gc40/edit?usp=sharing", sheet ="Adobe_YouTube_Combined")

# Setup Dates for Last Month
last_month_start <- format(Sys.Date() - 30, '%Y-%m-01')
last_month_end <- as.character(as.Date(format(Sys.Date(), '%Y-%m-01')) - 1)
# Organic Channels pull, using segment
video_by_channel_organic <- adobeanalyticsr::aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(last_month_start, last_month_end),
  dimensions = c("evar27"),
  metrics = c("event10"),
  top = c(20000),
  page = 0,
  filterType = "breakdown",
  segmentId = "s1957_61c09c58b4c96777691e4226",
  metricSort = "desc",
  include_unspecified = TRUE,
  search = NA,
  prettynames = FALSE,
  debug = FALSE
)
video_by_channel_organic <- video_by_channel_organic %>% rename('Channel: Organic Traffic Views' = event10) # Rename column

# Paid Channels pull, using segment
video_by_channel_paid <- adobeanalyticsr::aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(last_month_start, last_month_end),
  dimensions = c("evar27"),
  metrics = c("event10"),
  top = c(20000),
  page = 0,
  filterType = "breakdown",
  segmentId = "s1957_61c09e772e23a36df73bd975",
  metricSort = "desc",
  include_unspecified = TRUE,
  search = NA,
  prettynames = FALSE,
  debug = FALSE
)
video_by_channel_paid <- video_by_channel_paid %>% rename('Channel: Paid Search & Paid Social Views' = event10) # Rename column

# Join the two data pulls based on the video name
video_by_channel <- merge(x = video_by_channel_organic, y = video_by_channel_paid, by = "evar27")
video_by_channel <- video_by_channel %>% rename('Video Title' = evar27)

# write out to Channels Last Month tab in Google sheet
write_sheet(video_by_channel, "https://docs.google.com/spreadsheets/d/18RlnfFeR1rlsqRLEtdHWz7pE82LKdzmwq-Pioa0gc40/edit?usp=sharing", sheet ="Adobe_Channels_Last_Month")


# Get Video Comments


# Filter the list of Videos by statistics.commentCount => 1 & is not NA as tuber library does not like comments disabled videos
filter_videos <- video_final.df %>% 
  filter(!is.na(statistics.commentCount)) %>% 
  filter(statistics.commentCount >= 1)

# Convert to Vector
nt_video_ids <- as.vector(filter_videos$contentDetails.videoId)

# Pull Comments for each video
video_comments_raw <- purrr::map_df(.x = nt_video_ids, .f = tuber::get_all_comments)

# Pull Comments for each video
# video_category <- purrr::splice(.x = nt_video_ids, .f = tuber::list_videocats(c(region_code = "GB")))

# Merge the name of the video with the Video ID
YouTube_Video_Comments <- merge(x =yt_data , y = video_comments_raw, by.x = "contentDetails.videoId", by.y = "videoId") 
YouTube_Video_Comments$publishedAt <- ymd(as.Date(YouTube_Video_Comments$publishedAt))    # Change Date/Time to Time Series Format
YouTube_Video_Comments$updatedAt <- ymd(as.Date(YouTube_Video_Comments$updatedAt))    
YouTube_Video_Comments <- YouTube_Video_Comments %>% 
  select(contentDetails.videoId, textDisplay, authorDisplayName, likeCount, publishedAt, id, 
         parentId, snippet.publishedAt, snippet.publishedAt, snippet.title, statistics.commentCount, data.last.updated)
YouTube_Video_Comments <- YouTube_Video_Comments %>% 
  rename(video_published = snippet.publishedAt) %>% 
  rename(comment_published = publishedAt)

write_sheet(YouTube_Video_Comments, "https://docs.google.com/spreadsheets/d/18RlnfFeR1rlsqRLEtdHWz7pE82LKdzmwq-Pioa0gc40/edit?usp=sharing", sheet ="YouTube_Video_Comments")

