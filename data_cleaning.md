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
