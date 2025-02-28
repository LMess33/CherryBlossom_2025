# Load Libraries
library(tidyverse)
library(lubridate)
library(rvest)
library(dplyr)
library(xgboost)


get_weather_table <- function(url)
  read_html(url) %>% 
  html_nodes("div.monthly-calendar") %>% 
  html_text2() %>%
  str_replace("N/A", "N/A N/A") %>%
  str_remove_all("°|Hist. Avg. ") %>%
  str_split(" ", simplify = TRUE) %>%
  parse_number() %>%
  matrix(ncol = 3, 
         byrow = TRUE,
         dimnames = list(NULL, c("day", "tmax", "tmin"))) %>%
  as_tibble() %>%
  filter(
    row_number() %in%
      (which(diff(day) < 0) %>% (function(x) if(length(x) == 1) seq(1, x[1], 1) else seq(x[1] + 1, x[2], 1))))

liestal <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/ch/liestal/311994/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/311994?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)  

newyork <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/new-york/10021/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/349727?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)

washington <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/washington/20006/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/18-327659_1_al?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)  

vancouver <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/vancouver/98661/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/331419?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)


# --- 1. Load & Combine Bloom Data ---
bloom_data <- read.csv("~/Desktop/STAT 490/Peak-Bloom-Prediction/data/washingtondc.csv") |> 
  bind_rows(read.csv("~/Desktop/STAT 490/Peak-Bloom-Prediction/data/liestal.csv")) |> 
  bind_rows(read.csv("~/Desktop/STAT 490/Peak-Bloom-Prediction/data/kyoto.csv")) |> 
  bind_rows(read.csv("~/Desktop/STAT 490/Peak-Bloom-Prediction/data/vancouver.csv")) |> 
  bind_rows(read.csv("~/Desktop/STAT 490/Peak-Bloom-Prediction/data/nyc.csv")) |> 
  mutate(bloom_date = as.Date(bloom_date), 
         bloom_doy = yday(bloom_date))  # Convert bloom date to DOY

# --- 2. Load & Combine Temperature Data ---

temp_data <- bind_rows(
  mutate(liestal, location = "liestal"),
  mutate(newyork, location = "newyork"),
  mutate(vancouver, location = "vancouver"),
  mutate(washington, location = "washington")
) |>
  mutate(date=as.Date(date))



base_temp <- 50  # Base temperature for cherry blossoms
temp_data <- temp_data |> 
  mutate(GDD = pmax(0, (tmax + tmin) / 2 - base_temp)) |>  # GDD formula
  group_by(location, year) |> 
  arrange(date) |> 
  mutate(cum_GDD = cumsum(GDD))  # Cumulative sum of GDD

bloom_data$location <- recode(bloom_data$location, "washingtondc" = "washington")
bloom_data$location <- recode(bloom_data$location, "newyorkcity" = "newyork")


# Step 1: Prepare Training Data (Exclude 2025)
train_data <- bloom_data %>%
  select(year, lat, long, alt, bloom_doy) %>%
  filter(year < 2025)  

# Convert to matrix format for XGBoost
train_matrix <- as.matrix(train_data %>% select(-bloom_doy))
train_labels <- train_data$bloom_doy

# Train XGBoost Model
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)

params <- list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eta = 0.1,
  max_depth = 5,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = 0)

# Step 2: Predict Bloom DOY for Liestal & Kyoto Directly
predict_bloom_doy <- function(location_name) {
  location_future <- bloom_data %>%
    filter(location == location_name, year == 2024) %>%
    mutate(year = 2025) %>%
    select(year, lat, long, alt)
  
  location_matrix <- as.matrix(location_future)
  location_bloom_doy <- predict(xgb_model, location_matrix)
  
  as.Date(lubridate::ymd("2025-01-01") + location_bloom_doy)
}

liestal_bloom_date <- predict_bloom_doy("liestal")
kyoto_bloom_date <- predict_bloom_doy("kyoto")

print(paste("Predicted Bloom Date for Liestal:", liestal_bloom_date))
print(paste("Predicted Bloom Date for Kyoto:", kyoto_bloom_date))

# Step 3: Predict Bloom Dates for Other Locations Using GDD
future_data <- bloom_data %>%
  filter(year == 2024) %>%
  mutate(year = 2025) %>%
  select(year, lat, long, alt)

future_matrix <- as.matrix(future_data)
predicted_bloom_doy <- predict(xgb_model, future_matrix)

# GDD Threshold Approach (For Non-Liestal & Non-Kyoto Locations)
gdd_threshold <- median(predicted_bloom_doy)

predicted_dates <- temp_data %>%
  filter(year == 2025, !location %in% c("liestal", "kyoto")) %>%
  group_by(location) %>%
  filter(cum_GDD >= gdd_threshold) %>%
  summarize(predicted_bloom_date = min(date))

# Add Liestal & Kyoto Predictions
predicted_dates <- predicted_dates %>%
  bind_rows(data.frame(location = "liestal", predicted_bloom_date = liestal_bloom_date)) %>%
  bind_rows(data.frame(location = "kyoto", predicted_bloom_date = kyoto_bloom_date))

print(predicted_dates)
