Visualization and EDA
================

``` r
delay %>% 
  group_by(date, airline_name) %>% 
  summarize(
    count = n()
  ) %>% 
  ggplot(aes(x = date, y = count, color = airline_name)) +
  geom_line()
```

    ## `summarise()` has grouped output by 'date'. You can override using the
    ## `.groups` argument.

<img src="viz_and_eda_files/figure-gfm/unnamed-chunk-1-1.png" width="90%" />

``` r
delay %>% 
  filter(delay_minutes < 120, airline_name == "JetBlue Airways") %>% 
  ggplot(aes(x = as.factor(month), y = delay_minutes)) +
  geom_boxplot()
```

<img src="viz_and_eda_files/figure-gfm/unnamed-chunk-1-2.png" width="90%" />

``` r
delay %>% 
  filter(delay_minutes < 0) %>% 
  ggplot(aes(x = as.factor(month), y = delay_minutes)) +
  geom_boxplot()
```

<img src="viz_and_eda_files/figure-gfm/unnamed-chunk-1-3.png" width="90%" />

``` r
cancel_line = cancel %>% 
  group_by(date, airline_name) %>% 
  summarize(
    count = n()
  ) %>% 
  ggplot(aes(x = date, y = count, color = airline_name)) +
  geom_line()
```

    ## `summarise()` has grouped output by 'date'. You can override using the
    ## `.groups` argument.

``` r
covid_line = covid %>% 
  ggplot(aes(x = date, y = case_count)) +
  geom_line()

cancel_line + covid_line
```

<img src="viz_and_eda_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />
