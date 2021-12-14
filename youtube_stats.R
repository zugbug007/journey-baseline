library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)
library(plotly)
library(googlesheets4)# Import and manipulate Google Sheets docs

options(scipen=10000)
API_KEY = Sys.getenv("YOUTUBE_API_KEY")
channel_id <- Sys.getenv("YOUTUBE_CHANNEL_ID") 

user_id <- "nationaltrustcharity"
base <- "https://www.googleapis.com/youtube/v3/"

required_packages <- c("httr", "jsonlite", "here", "dplyr")
for(i in required_packages) {
  if(!require(i, character.only = T)) {
    #  if package is not existing, install then load the package
    install.packages(i, dependencies = T, repos = "http://cran.us.r-project.org")
    # install.packages(i, dependencies = T, repos = "https://cran.stat.upd.edu.ph/")
    require(i, character.only = T)
  }
}

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

yt_data <- video_final.df %>% select(snippet.publishedAt, snippet.title, snippet.description, snippet.position, starts_with("statistics"))

# Convert column types
yt_data$snippet.publishedAt <- as.Date(as.character(yt_data$snippet.publishedAt))
yt_data$snippet.position <- as.numeric(as.character(yt_data$snippet.position))
yt_data$statistics.viewCount <- as.numeric(as.character(yt_data$statistics.viewCount))
yt_data$statistics.likeCount <- as.numeric(as.character(yt_data$statistics.likeCount))
yt_data$statistics.dislikeCount <- as.numeric(as.character(yt_data$statistics.dislikeCount))
yt_data$statistics.favoriteCount <- as.numeric(as.character(yt_data$statistics.favoriteCount))
yt_data$statistics.commentCount <- as.numeric(as.character(yt_data$statistics.commentCount))
# Check Column Types
sapply(yt_data, class)
write_sheet(yt_data, "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", sheet ="YouTube_data")


channel_stats <- channel.df %>% select(items.snippet.title, items.snippet.description, items.snippet.customUrl, starts_with("items.statistics"))
channel_stats$items.statistics.viewCount <- as.numeric(as.character(channel_stats$items.statistics.viewCount))
channel_stats$items.statistics.subscriberCount <- as.numeric(as.character(channel_stats$items.statistics.subscriberCount))
channel_stats$items.statistics.videoCount <- as.numeric(as.character(channel_stats$items.statistics.videoCount))
sapply(channel_stats, class)

write_sheet(channel_stats, "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0", sheet ="YouTube_Channel_Stats")




#######################################################################
# 
# top_10_videos <- yt_data %>% 
#   filter(snippet.publishedAt > "2021-10-01") %>% 
#   arrange(snippet.publishedAt)
# 
# p <- ggplot(top_10_videos) +
#  aes(x = snippet.publishedAt, y = statistics.viewCount) +
#  geom_point(shape = "circle", colour = "#4682B4") +
#  geom_smooth(method = glm, span = 0.9) +
#  #scale_y_continuous(trans = "log10") +
#  labs(x = "Published Date", y = "Views", title = "National Trust YouTube Videos", subtitle = "2020 - 2021") +
#  theme_minimal() +
#  theme(legend.position = "none")
# p + geom_label_repel(aes(label = top_10_videos$snippet.title),
#                      box.padding   = 0.35, 
#                      point.padding = 0.5,
#                      segment.color = 'grey50') +
#   theme_classic()
# 
# ggplotly(p)
