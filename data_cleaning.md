Data Cleaning
================

### Weather Condition

``` r
noaa = read.csv("data/weather.csv")

noaa_df = noaa %>%
  janitor::clean_names() %>% 
  select(date, starts_with("hourly")) %>% 
  mutate(
    date = str_replace_all(date, "T", "-")
  ) %>% 
  separate(date, c("year", "month", "day", "hour", "minute", "second")) %>% 
  filter(minute == "51") %>% 
  select(-minute, -second)
```

### COVID

``` r
covid = 
  GET("https://data.cityofnewyork.us/resource/rc75-m7u3.csv") %>% 
  content("parsed") 

covid_df = covid %>% 
  select(date_of_interest, case_count) %>% 
  filter(date_of_interest < "2022-02-01" & date_of_interest >= "2021-11-01") %>% 
  separate(date_of_interest, c("year", "month", "day"))
```

### Delay

``` r
read_csv_function = function(path) {
  
  df = read_csv(path, 
                skip = 7, 
                cols(.default = "c")) %>%
    janitor::row_to_names(1) %>% 
    janitor::clean_names()
  
  df
  
}

full_delay_df = 
  tibble(
    files = list.files("data/delay/"),
    path = str_c("data/delay/", files)
  ) %>% 
  mutate(data = map(path, read_csv_function)) %>% 
  unnest(data) %>% 
  select(-files, -path) %>%
  mutate(
    airline_name = case_when(
      carrier_code == "9E" ~ "Endeavor Air",
      carrier_code == "AA" ~ "American Airlines",
      carrier_code == "AS" ~ "Alaska Airlines",
      carrier_code == "B6" ~ "JetBlue Airways",
      carrier_code == "DL" ~ "Delta Air Lines",
      carrier_code == "UA" ~ "United Air Lines",
      carrier_code == "YX" ~ "Republic Airways",
      TRUE      ~ ""
    ),
    scheduled_hour = str_left(scheduled_departure_time, 2)
  ) %>% 
  separate(date_mm_dd_yyyy, into = c("month", "date", "year"), sep = "/") %>% 
  relocate(scheduled_hour, .before = scheduled_departure_time)
```
