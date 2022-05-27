relative_change %>%
 filter(journey_name %in% c("ALL Mobile", "ALL Visits", "ALL Tablet")) %>%
 ggplot() +
  aes(x = Day, y = diff_to_mean, colour = journey_name) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "90 Days Pre & 90 Days Post",
    y = "% Change Relative to Baseline",
    title = "Percentage Change Relative to Baseline",
    subtitle = "Baseline Pre & Post CMS Launch",
    caption = "% Change Relative to Baseline"
  ) +
  theme_bw() +
  facet_wrap(vars(journey_name), scales = "free", ncol = 1L)
  
  relative_change %>%
 filter(!(journey_name %in% c("ALL Tablet", "ALL Social", "ALL Shop", "ALL PPC", "ALL Paid Social", 
"ALL Holidays", "ALL Email", "ALL Affiliates"))) %>%
 ggplot() +
  aes(x = journey_name, y = Visits, fill = journey_type) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Journey Name",
    y = "Visits",
    title = "Journey Pre & Post Comparison",
    subtitle = "Box Plot"
  ) +
  coord_flip() +
  theme_bw()
  
  
  relative_change %>%
 filter(journey_name %in% c("ALL Social", "ALL Paid Social", "ALL Holidays", "ALL Email", 
"ALL Affiliates", "ALL Shop")) %>%
 ggplot() +
  aes(
    x = Day,
    y = diff_to_mean,
    fill = sub_category,
    colour = sub_category
  ) +
  geom_line(size = 0.5) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_bw() +
  facet_wrap(vars(sub_category))
  
  relative_change %>%
 filter(journey_name %in% c("ALL Social", "ALL Paid Social", "ALL Holidays", "ALL Email", 
"ALL Affiliates", "ALL Shop")) %>%
 ggplot() +
  aes(
    x = Day,
    y = diff_to_mean,
    fill = sub_category,
    colour = sub_category
  ) +
  geom_line(size = 0.5) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Day",
    y = "% from Mean Baseline",
    title = "% Difference to Baseline Mean",
    subtitle = "By Journey TrendedTrended",
    color = "Journey Name"
  ) +
  theme_bw() +
  facet_wrap(vars(sub_category), scales = "free")
  
  
  
  ggplot(db_plot_data) +
  aes(x = change, group = journey_name) +
  geom_bar(fill = "#4682B4") +
  labs(
    x = "Higher or Lower than Baseline",
    y = "Journey Count",
    title = "Number of Journeys Higher or Lower than Baseline",
    subtitle = "Journey Performance"
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )