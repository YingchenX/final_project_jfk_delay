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
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
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
    ## chr  (4): date, daily_precipitation, daily_snowfall, daily_weather
    ## dbl (20): year, month, day, daily_average_dew_point_temperature, daily_avera...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Outcome: mutate count cancellation data

Count numbers of cancellation by date

``` r
cancel <- cancel_raw %>% 
  mutate(number = 1)%>% 
  mutate(number = as.numeric(number)) %>% 
  group_by(date) %>% 
  mutate(cancel_count = sum(number)) %>% 
  select(-flight_number,-destination_airport,-scheduled_hour,-scheduled_departure_time,-scheduled_elapsed_time_minutes, -number) %>%
  distinct
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
weather_covid <- weather %>% left_join(covid, by = c("month", "day", "year"))
```

``` r
# merge cancel and weather_covid dataset
cancel_tidy <- weather_covid %>% 
  left_join(cancel, by = c("month", "day", "year")) 

cancel_tidy <- cancel_tidy%>% 
  mutate(
    temperature = as.numeric(temperature),
    humidity = as.numeric(humidity),
    windspeed = as.numeric(windspeed),
    case_count = as.numeric(case_count),
    airline_name = as_factor(airline_name),
    month = ifelse(month == 11, "November", 
                        ifelse(month == 12, "December", "January")),
    month2 = ifelse(month == "November", "Nov", 
                        ifelse(month == "December", "Dec", "Jan")),
    year_month = paste(year, month2, sep="-")) %>% 
    filter(!is.na(cancel_count))  %>% 
    select(-month2)
```

# Poisson model

``` r
poisson = glm(cancel_count ~ temperature + humidity + windspeed + case_count,family = "poisson",data=cancel_tidy)

summary(poisson)
```

    ## 
    ## Call:
    ## glm(formula = cancel_count ~ temperature + humidity + windspeed + 
    ##     case_count, family = "poisson", data = cancel_tidy)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -10.834   -3.818   -1.062    1.687   12.682  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  2.257e+00  8.342e-02   27.06   <2e-16 ***
    ## temperature -1.240e-01  1.852e-03  -66.97   <2e-16 ***
    ## humidity     5.970e-02  1.080e-03   55.29   <2e-16 ***
    ## windspeed    3.868e-02  1.313e-03   29.46   <2e-16 ***
    ## case_count   1.017e-05  8.435e-07   12.06   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 15063.0  on 215  degrees of freedom
    ## Residual deviance:  5171.4  on 211  degrees of freedom
    ## AIC: 6137.1
    ## 
    ## Number of Fisher Scoring iterations: 6

# Poisson model Nested by month

``` r
cancel_tidy %>% 
  nest(df = -month) %>%
  mutate(
    models = map(.x = df, ~ glm(cancel_count ~ temperature + humidity + windspeed + case_count,family = "poisson", data = .x)),
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

| month    | (Intercept) | temperature | humidity | windspeed | case_count |
|:---------|------------:|------------:|---------:|----------:|-----------:|
| November |       0.073 |       0.058 |   -0.042 |     0.029 |          0 |
| December |      -0.902 |       0.006 |    0.026 |     0.035 |          0 |
| January  |       2.407 |      -0.133 |    0.064 |     0.034 |          0 |

# Poisson model nested by airline

## Descriptive table and plot

``` r
cancel_airline <- cancel_tidy  %>%
  group_by(year_month, airline_name) %>%
  mutate(Total_number_of_cancellation = sum(cancel_count)) 

cancel_airline %>%
  select(year_month, airline_name, Total_number_of_cancellation) %>%
  distinct %>% 
  pivot_wider(
  names_from = airline_name, 
  values_from = Total_number_of_cancellation) %>%
  head() %>% knitr::kable(digits = 2)
```

| year_month | American Airlines | Alaska Airlines | JetBlue Airways | Delta Air Lines | Endeavor Air | Republic Airways | United Air Lines |
|:-----------|------------------:|----------------:|----------------:|----------------:|-------------:|-----------------:|-----------------:|
| 2021-Nov   |                42 |              23 |              11 |               8 |            3 |                8 |                1 |
| 2021-Dec   |               174 |             143 |             252 |             226 |           10 |                5 |              171 |
| 2022-Jan   |              1047 |             977 |            1121 |            1039 |         1058 |             1047 |              672 |

``` r
plot_cancel_airline <- cancel_airline %>%
  ggplot(aes(x = year_month, y = Total_number_of_cancellation, fill = airline_name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Total Number of Cancellation by Airline",
    x = "Year and Month",
    y = "Total Number of Cancellation"
  ) +
  theme(legend.position="right", legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 5)) +
  facet_grid(. ~ airline_name)

  
ggplotly(plot_cancel_airline)
```

<img src="poisson_model_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

## Nesting airline

``` r
poisson_by_airline = cancel_tidy %>%
  nest(data = -airline_name) %>% 
  mutate(
    models = map(.x = data, ~glm(cancel_count ~ temperature + humidity + windspeed + case_count, family = "poisson", data = .x)),
    results = map(models, broom::tidy)
    ) %>% 
  select(airline_name, results) %>% 
  unnest(results) %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(airline_name, term, OR, CI_lower,CI_upper, p.value) 

poisson_by_airline %>% 
  filter(term != "(Intercept)" ) %>% 
  knitr::kable(digits = 20, align = "llccc", col.names = c("Airline Name", "Terms", "Estimated adjusted OR", "CI lower bound", "CI upper bound", "P-value"))
```

| Airline Name      | Terms       | Estimated adjusted OR | CI lower bound | CI upper bound | P-value      |
|:------------------|:------------|:---------------------:|:--------------:|:--------------:|:-------------|
| American Airlines | temperature |       0.8846483       |   0.8775611    |   0.8917927    | 0.000000e+00 |
| American Airlines | humidity    |       1.0686233       |   1.0630732    |   1.0742023    | 0.000000e+00 |
| American Airlines | windspeed   |       1.0521990       |   1.0456680    |   1.0587709    | 0.000000e+00 |
| American Airlines | case_count  |       1.0000157       |   1.0000116    |   1.0000199    | 1.342065e-13 |
| Alaska Airlines   | temperature |       0.8956385       |   0.8876480    |   0.9037009    | 0.000000e+00 |
| Alaska Airlines   | humidity    |       1.0471660       |   1.0411269    |   1.0532401    | 0.000000e+00 |
| Alaska Airlines   | windspeed   |       1.0377509       |   1.0304065    |   1.0451476    | 0.000000e+00 |
| Alaska Airlines   | case_count  |       1.0000057       |   1.0000013    |   1.0000101    | 1.073996e-02 |
| JetBlue Airways   | temperature |       0.8714847       |   0.8639014    |   0.8791346    | 0.000000e+00 |
| JetBlue Airways   | humidity    |       1.0738661       |   1.0686825    |   1.0790750    | 0.000000e+00 |
| JetBlue Airways   | windspeed   |       1.0356740       |   1.0294890    |   1.0418961    | 0.000000e+00 |
| JetBlue Airways   | case_count  |       1.0000128       |   1.0000088    |   1.0000169    | 5.776996e-10 |
| Delta Air Lines   | temperature |       0.8984078       |   0.8887831    |   0.9081367    | 0.000000e+00 |
| Delta Air Lines   | humidity    |       1.0463897       |   1.0409742    |   1.0518334    | 0.000000e+00 |
| Delta Air Lines   | windspeed   |       1.0355927       |   1.0285581    |   1.0426754    | 0.000000e+00 |
| Delta Air Lines   | case_count  |       1.0000019       |   0.9999978    |   1.0000060    | 3.601724e-01 |
| Endeavor Air      | temperature |       0.8601985       |   0.8495072    |   0.8710243    | 0.000000e+00 |
| Endeavor Air      | humidity    |       1.0861519       |   1.0772882    |   1.0950886    | 0.000000e+00 |
| Endeavor Air      | windspeed   |       1.0129569       |   1.0047147    |   1.0212668    | 2.012441e-03 |
| Endeavor Air      | case_count  |       1.0000182       |   1.0000131    |   1.0000233    | 3.949415e-12 |
| Republic Airways  | temperature |       0.8739409       |   0.8649762    |   0.8829984    | 0.000000e+00 |
| Republic Airways  | humidity    |       1.0633295       |   1.0566389    |   1.0700623    | 0.000000e+00 |
| Republic Airways  | windspeed   |       1.0487196       |   1.0414215    |   1.0560688    | 0.000000e+00 |
| Republic Airways  | case_count  |       1.0000145       |   1.0000097    |   1.0000194    | 4.098106e-09 |
| United Air Lines  | temperature |       0.9287536       |   0.9158573    |   0.9418316    | 0.000000e+00 |
| United Air Lines  | humidity    |       1.0275640       |   1.0208801    |   1.0342917    | 3.170600e-16 |
| United Air Lines  | windspeed   |       1.0463292       |   1.0342887    |   1.0585098    | 1.729727e-14 |
| United Air Lines  | case_count  |       0.9999957       |   0.9999912    |   1.0000001    | 5.706210e-02 |

## Plot

Create a plot showing the estimated ORs and CIs for each airline

``` r
poisson_by_airline %>% 
  mutate(airline_name = fct_reorder(airline_name, OR)) %>%
  ggplot(aes(x = airline_name, y = OR, color = airline_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Airline", y = "Estimated OR with CI")
```

<img src="poisson_model_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

``` r
poisson_by_airline %>%
  mutate(airline_name = fct_reorder(airline_name, OR)) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = airline_name, y = OR, color = term)) + 
  geom_point(show.legend = FALSE, aes()) +
  geom_errorbar(aes(ymin = CI_lower, 
                    ymax = CI_upper)) +
  labs(
    title = "Estimated OR with 95% CI in Cancellation Count Data by Airline",
    x = "Airline",
    y = "Estimated OR with CI"
  ) +
  ylim(0, 5) +
  theme(legend.position="right", legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8)) + 
  facet_grid(. ~ term)
```

<img src="poisson_model_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />
