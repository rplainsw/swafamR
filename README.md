
<!-- README.md is generated from README.Rmd. Please edit that file -->

# swafamR

<!-- badges: start -->
<!-- badges: end -->

The goal of swafamR is to …

## Installation

You can install the released version of swafamR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("swafamR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rplainsw/swafamR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(swafamR)

mod_inputs <- assign_mod_inputs(dat_pack, 10)

mod_inputs$flight_schedule <- query_teradata(
  con_tera,
  flight_schedule_query(),
  rep('2021-06-15', 2)
) %>% 
  clean_teradata_schedule(minute_bin = 10)

#Sqlite
mod_inputs$load_factors <- tbl(dat_pack, "LOAD_FACTORS") %>% 
  filter(between(date_of_dep, '2019-06-01', '2019-06-30')) %>% 
  collect() %>%  
  update_new_stations() %>% 
  clean_load_factors() %>% 
  summarise(
    load_factor = mean(load_factor, na.rm = TRUE),
    orig_factor = mean(orig_factor, na.rm = TRUE),
    .groups = 'drop'
  )

mod_inputs$bag_factors <- dplyr::tbl(dat_pack, "BAG_FACTORS") %>%
    group_by(orig, dest, dep_hour, wday) %>%
    summarise(
      avg_bags = mean(avg_bags, na.rm = TRUE),
      chk_ratio = mean(chk_ratio, na.rm = TRUE)
    ) %>%
    collect() %>%
    ungroup() %>%
    rename(avg_bag_factor = avg_bags,
           chk_ratio_factor = chk_ratio)

df <- main_model(mod_inputs, minute_bin = 10)

```
