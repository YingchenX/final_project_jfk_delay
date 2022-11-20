Scraping Data (Restaurants)
================

## Restaurants

Function.

``` r
scrape_rest = function(url){

  air_html = read_html(url)
  
  air_rest_vec = 
    air_html %>% 
    html_nodes(css = ".company-link") %>% 
    html_text()

  air_rest_rate_vec = 
    air_html %>% 
    html_nodes(css = ".ratings") %>% 
    html_text()
  
  air_df = 
    tibble(
      rest_name = air_rest_vec,
      rating = air_rest_rate_vec
    ) %>% 
    mutate(
      rating = as.numeric(str_extract(rating, "\\d+\\.*\\d*"))
    )
    
  air_df
  
}
```

### JFK

Official website of [JFK International
Airport](https://www.jfkairport.com/at-airport/shops-restaurants-and-services).

[Unofficial website](https://jfkfly.com/dining-drinking).

Information obtained from [unofficial website for
JKF](https://www.airport-jfk.com/restaurants-and-food.php).

``` r
jfk_url = "https://www.airport-jfk.com/restaurants-and-food.php"

jfk_df = scrape_rest(jfk_url) %>% 
  mutate(airport = "JFK")
```

### LGA

Information obtained from [unofficial website for
LGA](https://www.laguardia-airport.com/restaurants-and-food.php).

``` r
lga_url = "https://www.laguardia-airport.com/restaurants-and-food.php"

lga_df = scrape_rest(lga_url) %>% 
  mutate(airport = "LGA")
```

### EWR

Information obtained from [unofficial website for
EWR](https://www.airport-ewr.com/newark-restaurants-and-food.php).

``` r
ewr_url = "https://www.airport-ewr.com/newark-restaurants-and-food.php"

ewr_df = scrape_rest(ewr_url) %>% 
  mutate(airport = "EWR")
```

``` r
rest_df = bind_rows(jfk_df, lga_df, ewr_df) %>% 
  mutate(
    airport = fct_reorder(airport, rating, na.rm = TRUE, .desc = TRUE)
  ) 

rest_df %>% 
  ggplot(aes(x = airport, y = rating, color = airport)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Accent") +
  labs(
    x = "Airport",
    y = "Rating",
    color = "Airport"
  )
```

<img src="scraping_data_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
rest_df %>%
  ggplot(aes(x = rating, y = airport, fill = airport)) +
  geom_density_ridges(alpha = .3) +
  scale_x_continuous(
    breaks = 1:5
  ) +
  scale_fill_brewer(palette = "Accent") +
  labs(
    x = "Rating",
    y = "Airport",
    fill = "Airport"
  )
```

<img src="scraping_data_files/figure-gfm/unnamed-chunk-6-2.png" width="90%" />