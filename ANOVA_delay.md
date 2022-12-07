ANOVA_delay
================
Fengyi Ma

# ANOVA Test

In order to go more about how `month`, `airplane`, and `hour` are
associated with the time of delay. We would like to do ANOVA for those
categorical predictors

## month

H0: Mean delay time are not different across months

H1: Mean delay time are different across months

``` r
anova_delay_m = lm(delay ~ month, data = raw_df)

anova(anova_delay_m) %>% knitr::kable(caption = "One way anova of delay time and month")
```

|           |    Df |     Sum Sq |    Mean Sq |  F value | Pr(\>F) |
|:----------|------:|-----------:|-----------:|---------:|--------:|
| month     |     2 |   586699.4 | 293349.678 | 140.9119 |       0 |
| Residuals | 29722 | 61875115.2 |   2081.795 |       NA |      NA |

One way anova of delay time and month

p-value is extremely small, thus we reject the null hypothesis and
conclude that mean delay times are difference across 3 different months
(Nov, Dec, Jan)

## airline

H0: Mean delay time are not different across airlines

H1: Mean delay time are different across airlines

``` r
anova_delay_a = lm(delay ~ airline, data = raw_df)

anova(anova_delay_a) %>% knitr::kable(caption = "One way anova of delay time and airline")
```

|           |    Df |   Sum Sq |    Mean Sq |  F value | Pr(\>F) |
|:----------|------:|---------:|-----------:|---------:|--------:|
| airline   |     6 |  2296320 | 382719.990 | 189.0398 |       0 |
| Residuals | 29718 | 60165495 |   2024.547 |       NA |      NA |

One way anova of delay time and airline

p-value is extremely small, thus we reject the null hypothesis and
conclude that mean delay times are difference across 7 different
airlines.

## hour_c

H0: Mean delay time are not different across hours

H1: Mean delay time are different across hours

``` r
anova_delay_h = lm(delay ~ hour_c, data = raw_df)

anova(anova_delay_h) %>% knitr::kable(caption = "One way anova of delay time and hours")
```

|           |    Df |     Sum Sq |    Mean Sq |  F value | Pr(\>F) |
|:----------|------:|-----------:|-----------:|---------:|--------:|
| hour_c    |     3 |   583395.7 | 194465.247 | 93.40416 |       0 |
| Residuals | 29721 | 61878418.8 |   2081.976 |       NA |      NA |

One way anova of delay time and hours

p-value is extremely small, thus we reject the null hypothesis and
conclude that mean delay times are difference across different hours in
a day
