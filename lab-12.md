Lab 12 - Smoking during pregnancy
================
Ben Waggener
4/11/2025

### Load packages and data

``` r
library(tidyverse) 
```

    ## Warning: package 'tidyverse' was built under R version 4.4.3

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    ## Warning: package 'purrr' was built under R version 4.4.3

``` r
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.4.3

    ## Warning: package 'broom' was built under R version 4.4.3

    ## Warning: package 'dials' was built under R version 4.4.3

    ## Warning: package 'infer' was built under R version 4.4.3

    ## Warning: package 'modeldata' was built under R version 4.4.3

    ## Warning: package 'parsnip' was built under R version 4.4.3

    ## Warning: package 'recipes' was built under R version 4.4.3

    ## Warning: package 'rsample' was built under R version 4.4.3

    ## Warning: package 'tune' was built under R version 4.4.3

    ## Warning: package 'workflows' was built under R version 4.4.3

    ## Warning: package 'workflowsets' was built under R version 4.4.3

    ## Warning: package 'yardstick' was built under R version 4.4.3

``` r
library(openintro)
```

    ## Warning: package 'openintro' was built under R version 4.4.3

    ## Warning: package 'airports' was built under R version 4.4.3

    ## Warning: package 'cherryblossom' was built under R version 4.4.3

    ## Warning: package 'usdata' was built under R version 4.4.3

setting a seed for reproducibility

``` r
set.seed(04102025)
```

loading data

``` r
ncbirths <- openintro::ncbirths
```

### Exercise 1

cases are individual births such that it includes information aboout the
child, the mother, and the father. There are 1000 cases in this sample.

### Exercise 2

filter to keep only the white mothers

Mean weight of babies from white moms : 7.25

``` r
ncbirths_white <- ncbirths %>%
  filter(whitemom == "white")

summary(ncbirths_white)
```

    ##       fage            mage               mature        weeks      
    ##  Min.   :14.00   Min.   :15.00   mature mom :103   Min.   :22.00  
    ##  1st Qu.:26.00   1st Qu.:23.00   younger mom:611   1st Qu.:38.00  
    ##  Median :31.00   Median :27.50                     Median :39.00  
    ##  Mean   :30.69   Mean   :27.65                     Mean   :38.51  
    ##  3rd Qu.:35.00   3rd Qu.:32.00                     3rd Qu.:40.00  
    ##  Max.   :50.00   Max.   :50.00                     Max.   :45.00  
    ##  NA's   :77                                        NA's   :2      
    ##        premie        visits            marital        gained     
    ##  full term:616   Min.   : 0.0   not married:203   Min.   : 0.00  
    ##  premie   : 96   1st Qu.:10.0   married    :510   1st Qu.:22.00  
    ##  NA's     :  2   Median :12.0   NA's       :  1   Median :30.00  
    ##                  Mean   :12.3                     Mean   :30.98  
    ##                  3rd Qu.:15.0                     3rd Qu.:40.00  
    ##                  Max.   :30.0                     Max.   :85.00  
    ##                  NA's   :4                        NA's   :20     
    ##      weight      lowbirthweight    gender          habit          whitemom  
    ##  Min.   : 1.00   low    : 68    female:367   nonsmoker:617   not white:  0  
    ##  1st Qu.: 6.63   not low:646    male  :347   smoker   : 96   white    :714  
    ##  Median : 7.44                               NA's     :  1                  
    ##  Mean   : 7.25                                                              
    ##  3rd Qu.: 8.13                                                              
    ##  Max.   :11.75                                                              
    ## 

``` r
obs_mean <- ncbirths_white %>%
  summarize(mean_weight = mean(weight)) 
```

### Exercise 3

the criteria for conducting simulation-based inference are satisfied.
The data has a lot of observations (1000), they are randomly sampled,
and these observations are independent of each other.

\##Exercise 4

``` r
sim_ncbirth_white <- ncbirths_white %>%
  specify(response = weight) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "mean")
summary(sim_ncbirth_white)
```

    ##    replicate           stat      
    ##  Min.   :  1.00   Min.   :7.120  
    ##  1st Qu.: 25.75   1st Qu.:7.213  
    ##  Median : 50.50   Median :7.242  
    ##  Mean   : 50.50   Mean   :7.243  
    ##  3rd Qu.: 75.25   3rd Qu.:7.271  
    ##  Max.   :100.00   Max.   :7.434

``` r
obs_mean_value <- obs_mean$mean_weight

# Calculate shift value
shift_amount <- obs_mean_value - 7.43

# Shift bootstrap distribution
null_dist <- sim_ncbirth_white %>%
  mutate(stat = stat - shift_amount)
```

``` r
visualize(null_dist) +
  geom_vline(xintercept = obs_mean_value, color = "red") +
  labs(title = "Null Distribution Centered at 7.43",
       x = "Sample Mean Birth Weight (lbs)",
       y = "Count")
```

![](lab-12_files/figure-gfm/plot-1.png)<!-- -->

``` r
p_value <- null_dist %>%
  get_p_value(obs_stat = obs_mean_value, direction = "two_sided")
```

    ## Warning: Please be cautious in reporting a p-value of 0. This result is an approximation
    ## based on the number of `reps` chosen in the `generate()` step.
    ## ℹ See `get_p_value()` (`?infer::get_p_value()`) for more information.

``` r
p_value
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1       0

Add exercise headings as needed.
