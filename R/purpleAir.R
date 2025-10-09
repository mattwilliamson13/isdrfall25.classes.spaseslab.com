library(PurpleAir)
library(tidyverse)
library(sf)
library(lubridate)

# Replace PurpleAir functions ---------------------------------------------

#The API endpoints are wrong in the PurpleAir package, fix them here
get_sensor_history <- function(sensor_index,
                               fields,
                               start_timestamp,
                               end_timestamp,
                               average = c("10min", "30min", "60min",
                                           "6hr", "1day", "1week",
                                           "1month", "1year", "real-time"),
                               purple_air_api_key = Sys.getenv("PURPLE_AIR_API_KEY"),
                               read_key = NULL) {
  if (!rlang::is_integer(as.integer(sensor_index))) cli::cli_abort("sensor_index must be an integer")
  if (!rlang::is_character(fields)) cli::cli_abort("fields must be a character")
  avg <- rlang::arg_match(average)
  avg_int <- as.integer(
    c(
      "real-time" = 0, "10min" = 10, "30min" = 30, "60min" = 60,
      "6hr" = 360, "1day" = 1440, "1week" = 10800, "1month" = 43200, "1year" = 525600
    )[avg]
  )
  resp <-
    purple_air_request(
      resource = "sensor_history",
      success_code = as.integer(200),
      sensor_index = as.integer(sensor_index),
      start_timestamp = as.integer(start_timestamp),
      end_timestamp = as.integer(end_timestamp),
      average = avg_int,
      fields = fields,
      read_key = read_key
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  out <-
    purrr::map(resp$data, stats::setNames, resp$fields) |>
    purrr::modify(as.data.frame) |>
    purrr::list_rbind() |>
    tibble::as_tibble()
  out$time_stamp <- as.POSIXct.numeric(out$time_stamp)
  return(out)
  ## md <- purrr::discard_at(resp, c("fields", "data"))
}

purple_air_request <- function(purple_air_api_key = Sys.getenv("PURPLE_AIR_API_KEY"),
                               resource = c("keys", "organization", "sensors", "sensor_history"),
                               sensor_index = NULL,
                               success_code,
                               ...) {
  if (!rlang::is_integer(success_code)) cli::cli_abort("success_code must be an integer")
  resource <- rlang::arg_match(resource)
  req <-
    httr2::request("https://api.purpleair.com/v1") |>
    httr2::req_user_agent("PurpleAir package for R (https://github.com/cole-brokamp/PurpleAir)") |>
    httr2::req_headers("X-API-Key" = purple_air_api_key, .redact = "X-API-Key") |>
    httr2::req_error(
      is_error = \(resp) httr2::resp_status(resp) != success_code,
      body = \(resp) glue::glue_data(httr2::resp_body_json(resp), "{error}: {description} (API version: {api_version})")
    ) |>
    httr2::req_url_query(!!!list(...), .multi = "comma") |>
    httr2::req_retry(max_seconds = as.numeric(Sys.getenv("PURPLE_AIR_API_RETRY_MAX_TIME", "45")))
  if (resource == "keys") req <- httr2::req_url_path_append(req, "keys")
  if (resource == "organization") req <- httr2::req_url_path_append(req, "organization")
  if (resource == "sensors") {
    req <- httr2::req_url_path_append(req, "sensors")
    if (!is.null(sensor_index)) {
      req <- httr2::req_url_path_append(req, sensor_index)
    }
  }
  if (resource == "sensor_history") req <- httr2::req_url_path_append(req, "sensors", sensor_index, "history")
  return(req)
}


# download longest running censor data ------------------------------------


st <- tigris::states()

study_area <- st %>% 
  filter(., STUSPS %in% c("ID", "OR", "WA", "MT")) %>% 
  st_bbox() %>% 
  st_transform(., crs= "EPSG:4326")

sa_sensors <- get_sensors_data(study_area,
                               fields = c("name", "location_type","uptime", 
                                          "date_created","latitude", "longitude"),
                               location_type = "outside") %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(., st_crs(st))

sa_sensors <- sa_sensors %>% 
  mutate(date_up = as_datetime(date_created))

study_area_join <- st_join(x = sa_sensors, y = st, join=st_covered_by) %>% 
  mutate(., STUSPS = if_else(is.na(STUSPS), "WA", STUSPS)) %>% 
  st_drop_geometry() %>% 
  select(sensor_index, name, date_up, uptime, STUSPS) %>% 
  filter(year(date_up) < 2020)%>% 
  group_by(STUSPS) %>% 
  slice_sample(n=50)
  

sa_download <- map(study_area_join$sensor_index,
                   function(x) try(get_sensor_history(sensor_index = x,
                                               fields = c("pm2.5_cf_1", "pm2.5_atm", "temperature", "humidity"),
                                               average = "1month",
                                               start_timestamp =as.POSIXct("2020-01-01"),
                                               end_timestamp = as.POSIXct("2025-08-30"))))

row_counts <- sapply(sa_download, function(x) {
  if (is.data.frame(x)) nrow(x) else 0
})

names(sa_download) <- study_area_join$sensor_index
sa_subset <- sa_download[row_counts > 0]  

subset_df <- bind_rows(sa_subset, .id = "id") %>% 
  mutate(id = as.integer(id)) 

%>% 
  left_join(., study_area_join, by = join_by(id == sensor_index)) 


subset_df %>% 
  filter(STUSPS == "ID") %>% 
  ggplot(data = ., 
         mapping = aes(x = time_stamp, y=pm2.5_atm, color=name))+
  geom_line() +
  scale_color_brewer()+
  scale_x_datetime(date_minor_breaks = "1 month")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal")

by_state <- subset_df %>% 
  group_nest(STUSPS) %>% 
  mutate(path = str_glue("data/original/purpleair_{STUSPS}.csv"))

walk2(by_state$data, by_state$path, write_csv)
