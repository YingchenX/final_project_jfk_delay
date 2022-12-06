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
    delay = delay_minutes,
    carrierd = delay_carrier_minutes,
    extrmwd = delay_weather_minutes,
    nasd = delay_national_aviation_system_minutes,
    securityd = delay_security_minutes,
    latarrd = delay_late_aircraft_arrival_minutes) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  select(airline, date, month, hour, delay, carrierd, extrmwd, nasd, securityd, latarrd)
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
Since the outcome variable `delay` is a continuous variable, we would do
linear regression. However, there might be too many variables so far
(ignoring `date` we still got 12 potential predictors), so next step
will be fitting the model.

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
    ##    var            mean   min    q1 median    q3   max    sd histogram skim_type
    ##    <chr>         <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>     <chr>    
    ##  1 delay       11.7      -27    -5     -2     9  1252 45.8  ▇▁▁▁▁     numeric  
    ##  2 carrierd     7.31       0     0      0     0  1225 32.3  ▇▁▁▁▁     numeric  
    ##  3 extrmwd      0.614      0     0      0     0   850 12.2  ▇▁▁▁▁     numeric  
    ##  4 nasd         2.63       0     0      0     0   731 11.4  ▇▁▁▁▁     numeric  
    ##  5 securityd    0.0447     0     0      0     0   137  1.48 ▇▁▁▁▁     numeric  
    ##  6 latarrd      3.07       0     0      0     0   714 19.0  ▇▁▁▁▁     numeric  
    ##  7 temperature 41.4       10    35     42    49    68 10.8  ▁▃▇▆▂     numeric  
    ##  8 humidity    59.6       16    45     56    73   100 18.5  ▁▇▇▅▃     numeric  
    ##  9 visibility   9.42       0    10     10    10    10  1.95 ▁▁▁▁▇     numeric  
    ## 10 wind_s      11.9        0     7     11    16    32  6.22 ▅▇▇▃▁     numeric

Next, as mentioned above, we want to further categorize variable `hour`.

*Motivation:*

To increase the power of our model by reducing the number of parameters
involved and to be more efficient and concise.

*Rationale for the categorization of `hour`:*

Based on the previous inspection of variable `hour`, we could see that:
except for 5, 8, 10, 22, and 23, the frequencies of the other classes
are roughly even (between 1000-2000). Take this into consideration, our
rationale for classification will be a combination of convention and the
desire to achieve a uniform distribution.

Thus, we would categorize `hour` into the following 4 categories:

*morning*: 5-8

*noon*: 9-13

*afternoon*: 14-17

*night*:18-23

Now, we can start categorize `hour`

``` r
raw_df = raw_df %>% 
  mutate(hour_c = cut(hour, breaks = c(4, 8, 13, 17, 24),
                      labels = c("morning","noon","afternoon","night"))) %>% 
  select(-hour)
```

As usual, check if it was done properly

``` r
summary(as.factor(raw_df$hour_c))
```

    ##   morning      noon afternoon     night 
    ##      6691      7532      7476      8026

``` r
sum(is.na(raw_df))
```

    ## [1] 0

*0* ‘NA’ and the distribution looks good.

Now, since we are not yet able to decide the variable type of the rest
of the predictors, we decide to do univariate linear regression for each
variable first.

## Univariate linear regression

- temperature

``` r
lrTemp = lm(delay~temperature, data = raw_df)
summary(lrTemp) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic  p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>       <int> <dbl>
    ## 1   0.00696       0.00693  45.7      208. 4.60e-47     1       29723 29725

``` r
summary(lrTemp) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate   p.value
    ##   <chr>          <dbl>     <dbl>
    ## 1 (Intercept)   26.4   2.00e-137
    ## 2 temperature   -0.355 4.60e- 47

- humidity

``` r
lrHum = lm(delay~humidity, data = raw_df)
summary(lrHum) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic  p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>       <int> <dbl>
    ## 1   0.00285       0.00282  45.8      85.0 3.16e-20     1       29723 29725

``` r
summary(lrHum) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate  p.value
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)    3.80  2.26e- 5
    ## 2 humidity       0.133 3.16e-20

- visibility

``` r
lrVis = lm(delay~visibility, data = raw_df)
summary(lrVis) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic  p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>       <int> <dbl>
    ## 1   0.00663       0.00660  45.7      198. 6.70e-45     1       29723 29725

``` r
summary(lrVis) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate   p.value
    ##   <chr>          <dbl>     <dbl>
    ## 1 (Intercept)    29.7  1.45e-113
    ## 2 visibility     -1.91 6.70e- 45

- wind speed

``` r
lrWin = lm(delay~wind_s, data = raw_df)
summary(lrWin) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic  p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>       <int> <dbl>
    ## 1  0.000404      0.000370  45.8      12.0 0.000529     1       29723 29725

``` r
summary(lrWin) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate  p.value
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)    9.94  1.34e-66
    ## 2 wind_s         0.148 5.29e- 4

- carrier delay

``` r
lrCar = lm(delay~carrierd, data = raw_df)
summary(lrCar) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>       <int> <dbl>
    ## 1     0.654         0.654  27.0    56271.       0     1       29723 29725

``` r
summary(lrCar) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate  p.value
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)     3.31 3.30e-94
    ## 2 carrierd        1.15 0

- extreme weather delay

``` r
lrExw = lm(delay~extrmwd, data = raw_df)
summary(lrExw) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>       <int> <dbl>
    ## 1    0.0805        0.0805  44.0     2603.       0     1       29723 29725

``` r
summary(lrExw) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate p.value
    ##   <chr>          <dbl>   <dbl>
    ## 1 (Intercept)    11.1        0
    ## 2 extrmwd         1.07       0

- NAS delay

``` r
lrNas = lm(delay~nasd, data = raw_df)
summary(lrNas) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic   p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>       <int> <dbl>
    ## 1    0.0299        0.0299  45.2      917. 2.05e-198     1       29723 29725

``` r
summary(lrNas) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate   p.value
    ##   <chr>          <dbl>     <dbl>
    ## 1 (Intercept)    9.89  6.55e-290
    ## 2 nasd           0.694 2.05e-198

- security delay

``` r
lrSec = lm(delay~securityd, data = raw_df)
summary(lrSec) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic  p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>       <int> <dbl>
    ## 1   0.00204       0.00200  45.8      60.7 6.92e-15     1       29723 29725

``` r
summary(lrSec) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value) 
```

    ## # A tibble: 2 × 3
    ##   term        estimate  p.value
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)    11.6  0       
    ## 2 securityd       1.40 6.92e-15

- late arrival delay

``` r
lrLat = lm(delay~latarrd, data = raw_df)
summary(lrLat) %>% broom::glance()
```

    ## # A tibble: 1 × 8
    ##   r.squared adj.r.squared sigma statistic p.value    df df.residual  nobs
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>       <int> <dbl>
    ## 1     0.310         0.310  38.1    13339.       0     1       29723 29725

``` r
summary(lrLat) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate   p.value
    ##   <chr>          <dbl>     <dbl>
    ## 1 (Intercept)     7.57 3.65e-246
    ## 2 latarrd         1.35 0
