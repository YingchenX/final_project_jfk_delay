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
