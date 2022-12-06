Regression_delay
================
Fengyi Ma

## Data Import

``` r
delay = read.csv("./tidied_data/delay.csv") %>% 
  janitor::clean_names() 

h_weather = read.csv("./tidied_data/hourly_weather.csv") %>% 
  janitor::clean_names() 
```

## clean dataset ‘delay’

Check how many airlines

``` r
unique(delay$airline_name)
```

    ## [1] "Endeavor Air"      "American Airlines" "Alaska Airlines"  
    ## [4] "JetBlue Airways"   "Delta Air Lines"   "United Air Lines" 
    ## [7] "Republic Airways"

*7* -\> ok, keep

Check how many destinations

``` r
unique(delay$destination_airport)
```

    ##  [1] "MSP" "DTW" "RDU" "BTV" "BNA" "ROC" "IND" "RIC" "CLE" "SYR" "BUF" "CHS"
    ## [13] "PWM" "SAV" "CVG" "BGR" "PIT" "LAX" "SFO" "MIA" "ORD" "CLT" "SNA" "DCA"
    ## [25] "PHX" "SAT" "FLL" "DFW" "AUS" "SEA" "SAN" "PDX" "PBI" "ABQ" "SLC" "MCO"
    ## [37] "DEN" "BOS" "TPA" "SRQ" "SJU" "RSW" "ONT" "BUR" "MSY" "SJC" "ORH" "JAX"
    ## [49] "LAS" "BQN" "ATL" "PSP" "IAH" "EYW" "IAD" "STT" "SMF" "BWI" "ORF" "PHL"
    ## [61] "CMH" "RNO" "JAC" "EGE" "PSE" "BZN"

*66* -\> too many, remove

Keep variables of interest and `date` for merge purpose (which will be
removed later)

``` r
delay = delay %>% 
  mutate(
    airline = airline_name,
    hour = scheduled_hour,
    delay_min = delay_minutes,
    carrierd_min = delay_carrier_minutes,
    extrmwd_min = delay_weather_minutes,
    nasd_min = delay_national_aviation_system_minutes,
    securityd_min = delay_security_minutes,
    latarrd_min = delay_late_aircraft_arrival_minutes) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  select(airline, date, month, hour, delay_min, carrierd_min, extrmwd_min, nasd_min, securityd_min, latarrd_min)
```

check ‘NA’

``` r
sum(is.na(delay))
```

*0* -\> good

## clean dataset ‘h_weather’

About the measure of temperature: Since the dry bulb temperature is the
ambient air temperature measured by regular thermometers, that is, the
temperature often mentioned in our general weather forecast. Thus, we
decide to use the variable `hourly_dry_bulb_temperature` to represent
temperature.

``` r
h_weather = h_weather %>% 
  mutate(
    temperature = hourly_dry_bulb_temperature,
    humidity = hourly_relative_humidity,
    visibility = hourly_visibility,
    wind_s = hourly_wind_speed,
    hour = as.numeric(hour)) %>% 
      select(date, month, hour, temperature, humidity, visibility, wind_s)
```

check ‘NA’

``` r
sum(is.na(h_weather))
```

*0* -\> good

## merge datasets ‘delay’ and ‘hourly_weather’

``` r
raw_df = merge(x = delay, y = h_weather, by = c("date", "month", "hour"),
               all.x = TRUE)
```

check ‘NA’

``` r
sum(is.na(raw_df))
```

*0* -\> good

To this step, we have our raw dataset for doing association analysis.
Since the outcome variable `delay_min` is a continuous variavle, we
would do linear regression. However, there might be too many variables
so far (ignoring `date` we still got 12 potential predictors), so next
step will be fitting the model.

By intuition, I would set:

`airline` -\> categorical `month` -\> categorical `hour` -\> categorical
(need further categorization) and for the rest -\> continuous

Let’s first check how each variable roughly distribute.

### For categorical variables

``` r
cat_sum = raw_df %>% 
  select(airline, month, hour) %>% 
  mutate(
    airline = as.factor(airline),
    month = as.factor(month),
    hour = as.factor(hour)
    ) %>% 
  summary(maxsum = 24)

cat_sum
```

    ##               airline      month      hour     
    ##  Alaska Airlines  :  888   1 : 9931   5 : 165  
    ##  American Airlines: 3976   11:10042   6 :1180  
    ##  Delta Air Lines  : 5513   12: 9752   7 :2035  
    ##  Endeavor Air     : 3711              8 :3311  
    ##  JetBlue Airways  :10199              9 :2065  
    ##  Republic Airways : 5094              10: 987  
    ##  United Air Lines :  344              11:1503  
    ##                                       12:1280  
    ##                                       13:1697  
    ##                                       14:1780  
    ##                                       15:2212  
    ##                                       16:1689  
    ##                                       17:1795  
    ##                                       18:1871  
    ##                                       19:2148  
    ##                                       20:1815  
    ##                                       21:1487  
    ##                                       22: 619  
    ##                                       23:  86

### For continuous variables

``` r
con_sum_df = raw_df %>% 
  select(-date, -airline, -month, -hour)

con_sum = skim(con_sum_df) %>%
  dplyr::select(-n_missing, -complete_rate) %>% 
  mutate(
    mean = numeric.mean,
    sd = numeric.sd,
    histogram = numeric.hist,
    var = skim_variable,
    min = numeric.p0,
    max = numeric.p100,
    median = numeric.p50,
    q1 = numeric.p25,
    q3 = numeric.p75
  ) %>% 
  dplyr::select(-numeric.mean, -numeric.sd, -numeric.hist, -skim_variable, -numeric.p0, -numeric.p100, -numeric.p50, -numeric.p25, -numeric.p75) %>% 
  relocate(var, mean, min, q1, median, q3, max, sd, histogram)

con_sum
```

    ## # A tibble: 10 × 10
    ##    var              mean   min    q1 median    q3   max    sd histogram skim_t…¹
    ##    <chr>           <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>     <chr>   
    ##  1 delay_min     11.7      -27    -5     -2     9  1252 45.8  ▇▁▁▁▁     numeric 
    ##  2 carrierd_min   7.31       0     0      0     0  1225 32.3  ▇▁▁▁▁     numeric 
    ##  3 extrmwd_min    0.614      0     0      0     0   850 12.2  ▇▁▁▁▁     numeric 
    ##  4 nasd_min       2.63       0     0      0     0   731 11.4  ▇▁▁▁▁     numeric 
    ##  5 securityd_min  0.0447     0     0      0     0   137  1.48 ▇▁▁▁▁     numeric 
    ##  6 latarrd_min    3.07       0     0      0     0   714 19.0  ▇▁▁▁▁     numeric 
    ##  7 temperature   41.4       10    35     42    49    68 10.8  ▁▃▇▆▂     numeric 
    ##  8 humidity      59.6       16    45     56    73   100 18.5  ▁▇▇▅▃     numeric 
    ##  9 visibility     9.42       0    10     10    10    10  1.95 ▁▁▁▁▇     numeric 
    ## 10 wind_s        11.9        0     7     11    16    32  6.22 ▅▇▇▃▁     numeric 
    ## # … with abbreviated variable name ¹​skim_type
