aw_freeform_table(company_id = Sys.getenv("AW_COMPANY_ID"), 
					rsid = rsid, 
					date_range = date_range,
                  dimensions = "daterangeday",
                  metrics = "event1") %>%
    pivot_longer(-daterangeday) %>%
    arrange(daterangeday, name) %>% 
    select(day = daterangeday, id = name, value)