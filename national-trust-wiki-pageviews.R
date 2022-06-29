library(wikipediatrend)
library(ggplot2)

trend_data <- 
  wp_trend(
    page = c("National_Trust"), 
    lang = c("en"), 
    from = "2017-01-01",
    to   = Sys.Date()
  )

avg.daily.views <- trend_data %>% 
  filter(date > '2022-01-01') %>% 
  summarize(mean(views))

nt.wiki <- ggplot(trend_data) +
 aes(x = date, y = views) +
 geom_line(size = 0.5, colour = "#46337E") +
 labs(x = "Date", 
 y = "Views", title = "National Trust Wikipedia Page Views") +
 theme_bw()
nt.wiki

trend_data <- 
  wp_trend(
    page = c("English_Heritage"), 
    lang = c("en"), 
    from = "2017-01-01",
    to   = Sys.Date()
  )

avg.daily.views <- trend_data %>% 
  filter(date > '2022-01-01') %>% 
  summarize(mean(views))


eh.wiki <- ggplot(trend_data) +
  aes(x = date, y = views) +
  geom_line(size = 0.5, colour = "#46337E") +
  labs(x = "Date", 
       y = "Views", title = "English Heritage Wikipedia Page Views") +
  theme_bw()
eh.wiki

