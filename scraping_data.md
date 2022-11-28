Scraping Data for JFK Services and Facilities
================

First, write a scraping function.

``` r
scrape_rating = function(url){

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

### Restaurants

Information obtained from [unofficial website for
JKF](https://www.airport-jfk.com/restaurants-and-food.php).

``` r
jfk_url = "https://www.airport-jfk.com/restaurants-and-food.php"

jfk_rest = scrape_rating(jfk_url) %>% 
  mutate(category = "Restaurants")
```

### Shops and Stores

Information obtained from [unofficial website for
JFK](https://www.airport-jfk.com/shops-and-stores.php).

``` r
jfk_url = "https://www.airport-jfk.com/shops-and-stores.php"
```

``` r
jfk_shops = scrape_rating(jfk_url) %>% 
  mutate(category = "Shops")
```

### Lounges

Information obtained from [unofficial website for
JFK](https://www.airport-jfk.com/lounge-night-club.php).

``` r
jfk_url = "https://www.airport-jfk.com/lounge-night-club.php"
```

``` r
jfk_lounges = scrape_rating(jfk_url) %>% 
  mutate(category = "Lounges")
```

### Other Services
