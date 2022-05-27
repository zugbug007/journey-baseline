library(dplyr)
library(fuzzyjoin)
data(misspellings)

misspellings
library(qdapDictionaries)
words <- tbl_df(DICTIONARY)
words

set.seed(2016)
sub_misspellings <- misspellings %>%
  sample_n(1000)

joined <- sub_misspellings %>% stringdist_inner_join(words, by = c(misspelling = "word"), max_dist = 1)

joined %>% count(misspelling, correct)

which_correct <- joined %>% group_by(misspelling, correct) %>% summarize(guesses = n(), one_correct = any(correct == word))

mean(which_correct$one_correct)
sum(which_correct$guesses == 1 & which_correct$one_correct)

joined_dists <- sub_misspellings %>% stringdist_inner_join(words, by = c(misspelling = "word"), max_dist = 2, distance_col = "distance")
joined_dists

closest <- joined_dists %>%
  group_by(misspelling) %>%
  top_n(1, desc(distance)) %>%
  ungroup()

closest
closest %>%
  count(distance)

left_joined <- sub_misspellings %>% stringdist_left_join(words, by = c(misspelling = "word"), max_dist = 1)

left_joined

library(dplyr)
library(ggplot2)
data(diamonds)

d <- data_frame(approximate_name = c("Idea", "Premiums", "Premioom",
                                     "VeryGood", "VeryGood", "Faiir"),
                type = 1:6)

# no matches when they are inner-joined:
diamonds %>%
  inner_join(d, by = c(cut = "approximate_name"))

# but we can match when they're fuzzy joined
diamonds %>%
  stringdist_inner_join(d, by = c(cut = "approximate_name"))



library(dplyr)
library(data.table)
library(ggplot2)
library(cusumcharter)
library(ggExtra)


# make the link dynamic
part1 <- "https://www.opendata.nhs.scot/dataset/"
part2 <- "b318bddf-a4dc-4262-971f-0ba329e09b87/"
part3 <- "resource/427f9a25-db22-4014-a3bc-893b68243055/"
part4 <- "download/trend_ca_"
part5 <- ".csv"
today <- gsub('-','',as.character(Sys.Date()))

link <- paste0(part1, part2, part3, part4, today, part5, sep = '')


dates <- seq.Date(as.Date(Sys.Date()-27), as.Date(Sys.Date()), by = '1 day')

DT <- data.table::fread(link)
DT[, Date := as.character(Date)]
DT[, Date := as.IDate(Date, format = "%Y%m%d")]
positives <- DT[Date >= as.Date(Sys.Date() -28),.(Date, CAName, DailyPositive)][]


ggplot(positives,aes(Date, DailyPositive)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ CAName, ncol = 4, scales = "free_y") + 
  theme_minimal() + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()



p <- positives %>% 
  group_by(CAName) %>% 
  group_modify(~ cusum_control(.$DailyPositive), .keep = TRUE) %>% 
  ungroup() %>% 
  group_by(CAName) %>% 
  mutate(Date = dates) %>% 
  ungroup() %>% 
  cusum_control_plot(.,
                     xvar = Date,
                     facet_var = CAName, 
                     facet_scales = 'free_y',
                     title_text = "CUSUM Rolling 28 Day Positive Cases")

p <- p + facet_wrap(~CAName, ncol = 4, scales = 'free_y')
print(p)