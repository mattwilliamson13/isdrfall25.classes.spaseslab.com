cal <- readr::read_csv("data/schedule.csv")
date_shift <- as.Date("2025-08-25") - as.Date("2023-08-21")
updated_tbl <- cal %>%
  mutate(date = date + date_shift)

write_csv(cal, "data/schedule.csv")


cal <- readr::read_csv("data/schedule.csv")

updated_tbl <- cal %>% 
  mutate(group = case_when(
    date <= as.Date("2025-09-11") ~ "Getting Started",
    date > as.Date("2025-09-11") & date <= as.Date("2025-10-1") ~ "Fundamentals of Spatial Data",
    date > as.Date("2025-10-1") & date <= as.Date("2025-10-22") ~ "Describing Spatial Patterns",
    date > as.Date("2025-10-22") & date <= as.Date("2025-11-23") ~ "Explaining Spatial Patterns",
    date > as.Date("2025-11-23") & date <= as.Date("2025-11-30") ~ "Fall Break",
    date > as.Date("2025-11-30") & date <= as.Date("2025-12-08") ~ "Predicting Spatial Patterns",
    TRUE ~ group
  ))
