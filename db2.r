# Day Comparison Plot
compare_to_day <- compare_to_day %>% arrange(desc(baseline_pre)) %>%
  mutate(journey_name = fct_reorder(journey_name, baseline_pre))

db2 <- compare_to_day %>%
  ggplot(aes(x = visits_14_da, xend = visits_yest, y = journey_name)) +
  geom_dumbbell(colour="#a3c4dc", 
                colour_xend="#0e668b", 
                size=4.0, dot_guide=TRUE, 
                dot_guide_size=0.15, 
                dot_guide_colour = "grey60")+
  labs(title = "Baseline Journey Comparison from Before & After CMS Launch", x="14 Day Visits Average Compared to Yesterday", y = "Journey Name") +
  theme_tq() +
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(),
    axis.ticks=element_blank(),
    panel.border=element_blank()
  ) +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))
db2


db_plot_data <- baseline %>% select(journey_name, journey_type, Visits_mean) %>% 
  pivot_wider(
    names_from = journey_type,
    values_from = 'Visits_mean') %>% 
  arrange(desc(post)) %>%
  mutate(journey_name = fct_reorder(journey_name, post)) %>% 
  mutate(diff_vs_baseline = ((post - pre)/pre)) %>%
  mutate(rank = order(order(diff_vs_baseline))) %>% 
  mutate(change = ifelse(post > pre, "Higher", "Lower"))

db1 <- db_plot_data %>% 
  ggplot(aes(x = pre, xend = post, y = journey_name)) +
  geom_dumbbell(colour="#a3c4dc", 
                colour_xend="#0e668b", 
                size=4.0, dot_guide=TRUE, 
                dot_guide_size=0.15, 
                dot_guide_colour = "grey60")+
  labs(title = "Baseline Journey Comparison from Before & After CMS Launch", x="Pre vs. Post Baseline (Visits)", y = "Journey Name") +
  theme_tq() +
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(),
    axis.ticks=element_blank(),
    panel.border=element_blank()
  ) +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"))
db1