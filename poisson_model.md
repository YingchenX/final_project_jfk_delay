Poisson model
================

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ stringr 1.4.1
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.2
    ## ✔ readr   2.1.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggridges)
library(patchwork)

knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))


cancel_raw = read_csv("tidied_data/cancel.csv")
```

    ## Rows: 1441 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): airline_name, flight_number, destination_airport, scheduled_hour
    ## dbl  (4): month, day, year, scheduled_elapsed_time_minutes
    ## date (1): date
    ## time (1): scheduled_departure_time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
covid = read_csv("tidied_data/covid.csv")
```

    ## Rows: 92 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (4): year, month, day, case_count
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
daily_weather = read_csv("tidied_data/daily_weather.csv")
```

    ## Rows: 92 Columns: 24
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (5): daily_peak_wind_direction, daily_precipitation, daily_snowfall, d...
    ## dbl  (18): year, month, day, daily_average_dew_point_temperature, daily_aver...
    ## date  (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Outcome: mutate count cancellation data

Count numbers of cancellation by date

``` r
cancel_date <- cancel_raw %>% 
  mutate(number = 1)%>% 
  mutate(number = as.numeric(number)) %>% 
  select(-flight_number,-destination_airport,-scheduled_hour,-scheduled_departure_time,-scheduled_elapsed_time_minutes) %>% 
  group_by(date) %>% 
  mutate(cancel_count = sum(number)) %>% 
  distinct
```

Count numbers of cancellation by airline and date

``` r
cancel_airline <- cancel_raw %>% 
  mutate(number = 1)%>% 
  mutate(number = as.numeric(number)) %>% 
  select(-flight_number,-destination_airport,-scheduled_hour,-scheduled_departure_time,-scheduled_elapsed_time_minutes) %>% 
  group_by(date, airline_name) %>% 
  mutate(cancel_by_airline = sum(number)) %>% 
  distinct
```

``` r
cancel <- cancel_date %>%
 inner_join(cancel_airline, by = c("airline_name", "date", "month", "day", "year", "number")) %>% 
 select(-number) 
```

# Merge outcome and predictors:

weather dataset: select `daily_average_dry_bulb_temperature`,
`daily_average_relative_humidity`, `daily_peak_wind_speed` predictors

``` r
weather <- daily_weather %>% 
  mutate(
    temperature = daily_average_dry_bulb_temperature,
    humidity = daily_average_relative_humidity,
    windspeed = daily_peak_wind_speed
         )  %>% 
  select(date, year, month, day, temperature, humidity, windspeed) 
```

merge dataset

``` r
# merge weather and covid dataset
weather_covid <- weather %>% left_join(covid, by = c("date", "month", "day", "year"))

# merge cancel and weather_covid dataset
cancel_tidy <- weather_covid %>% 
  left_join(cancel, by = c("date", "month", "day", "year")) %>% 
  mutate(
    temperature = as.numeric(temperature),
    humidity = as.numeric(humidity),
    windspeed = as.numeric(windspeed),
    case_count = as.numeric(case_count),
    airline_name = as_factor(airline_name),
    month = ifelse(month == 11, "November", 
                        ifelse(month == 12, "December", "January"))) 
```

# Poisson model

``` r
poisson = glm(cancel_count ~ temperature + humidity + windspeed + case_count + airline_name,family = "poisson",data=cancel_tidy)

summary(poisson)
```

    ## 
    ## Call:
    ## glm(formula = cancel_count ~ temperature + humidity + windspeed + 
    ##     case_count + airline_name, family = "poisson", data = cancel_tidy)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -11.311   -3.701   -1.241    2.167   14.413  
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                   2.202e+00  8.680e-02  25.368  < 2e-16 ***
    ## temperature                  -1.204e-01  1.875e-03 -64.188  < 2e-16 ***
    ## humidity                      5.744e-02  1.101e-03  52.197  < 2e-16 ***
    ## windspeed                     3.815e-02  1.321e-03  28.873  < 2e-16 ***
    ## case_count                    8.978e-06  8.448e-07  10.626  < 2e-16 ***
    ## airline_nameAlaska Airlines   2.368e-01  4.103e-02   5.772 7.82e-09 ***
    ## airline_nameJetBlue Airways  -4.970e-02  3.893e-02  -1.277  0.20165    
    ## airline_nameDelta Air Lines   2.063e-01  4.002e-02   5.156 2.53e-07 ***
    ## airline_nameEndeavor Air      2.020e-01  4.177e-02   4.835 1.33e-06 ***
    ## airline_nameRepublic Airways  1.245e-01  4.177e-02   2.980  0.00288 ** 
    ## airline_nameUnited Air Lines  4.329e-01  4.514e-02   9.591  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 15063.0  on 215  degrees of freedom
    ## Residual deviance:  5012.7  on 205  degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## AIC: 5990.4
    ## 
    ## Number of Fisher Scoring iterations: 6

# Poisson model Nested by month

``` r
cancel_tidy %>% 
  nest(df = -month) %>%
  mutate(
    models = map(.x = df, ~ glm(cancel_count ~ temperature + humidity + windspeed + case_count + airline_name,family = "poisson", data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  unnest(results) %>% 
  select(month, term, estimate) %>% 
  mutate(term = fct_inorder(term)) %>% 
  pivot_wider(
    names_from = term, 
    values_from = estimate) %>% 
  knitr::kable(digits = 3)
```

| month    | (Intercept) | temperature | humidity | windspeed | case_count | airline_nameAlaska Airlines | airline_nameJetBlue Airways | airline_nameDelta Air Lines | airline_nameEndeavor Air | airline_nameRepublic Airways | airline_nameUnited Air Lines |
|:---------|------------:|------------:|---------:|----------:|-----------:|----------------------------:|----------------------------:|----------------------------:|-------------------------:|-----------------------------:|-----------------------------:|
| November |       0.182 |       0.054 |   -0.040 |     0.027 |          0 |                       0.213 |                       0.029 |                       0.070 |                    0.040 |                        0.139 |                       -0.135 |
| December |      -1.169 |       0.005 |    0.032 |     0.046 |          0 |                      -0.104 |                      -0.362 |                      -0.041 |                   -1.884 |                       -1.609 |                        0.085 |
| January  |       2.337 |      -0.130 |    0.062 |     0.033 |          0 |                       0.227 |                      -0.057 |                       0.190 |                    0.223 |                        0.146 |                        0.422 |
