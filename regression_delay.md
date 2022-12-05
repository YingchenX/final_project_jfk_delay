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

### hypothesized predictors

At 100% relative humidity, the wet-bulb temperature is equal to the air
temperature (dry-bulb temperature); at lower humidity the wet-bulb
temperature is lower than dry-bulb temperature because of evaporative
cooling.

The wet-bulb temperature is defined as the temperature of a parcel of
air cooled to saturation (100% relative humidity) by the evaporation of
water into it, with the latent heat supplied by the parcel.
