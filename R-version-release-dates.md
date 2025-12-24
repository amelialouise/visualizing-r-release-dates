Analysis of R version release dates
================

# Analysis of R version release dates

``` r
library(here)
```

    ## here() starts at /Users/amelia/test-stuff/rstudio-git-clone

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(fpp3)
```

    ## Registered S3 method overwritten by 'tsibble':
    ##   method               from 
    ##   as_tibble.grouped_df dplyr

    ## ── Attaching packages ──────────────────────────────────────────── fpp3 1.0.2 ──

    ## ✔ tibble      3.3.0     ✔ tsibbledata 0.4.1
    ## ✔ tidyr       1.3.2     ✔ feasts      0.4.2
    ## ✔ lubridate   1.9.4     ✔ fable       0.4.1
    ## ✔ tsibble     1.1.6

    ## ── Conflicts ───────────────────────────────────────────────── fpp3_conflicts ──
    ## ✖ lubridate::date()    masks base::date()
    ## ✖ dplyr::filter()      masks stats::filter()
    ## ✖ tsibble::intersect() masks base::intersect()
    ## ✖ tsibble::interval()  masks lubridate::interval()
    ## ✖ dplyr::lag()         masks stats::lag()
    ## ✖ tsibble::setdiff()   masks base::setdiff()
    ## ✖ tsibble::union()     masks base::union()

``` r
library(ggrepel)

source(here("src", "get_latest_r_versions_data.R"))
release_dates <- readRDS(here("data", paste0(Sys.Date(), "_rversions.RDS")))
```

The `release_dates` data is sourced from the {rversions} package. Run
the `get_latest_r_versions_data.R` script in *src/* to get the latest
data.

``` r
release_dates
```

    ## # A tibble: 162 × 12
    ##    major_version minor_version version date       minor_release_start
    ##    <chr>         <chr>         <chr>   <date>     <date>             
    ##  1 R 0.0         R 0.0         0.0     1995-06-20 1995-06-20         
    ##  2 R 0.1         R 0.1         0.1     1996-02-12 1996-02-12         
    ##  3 R 0.2         R 0.2         0.2     1996-03-14 1996-03-14         
    ##  4 R 0.3         R 0.3         0.3     1996-03-22 1996-03-22         
    ##  5 R 0.4         R 0.4         0.4     1996-04-01 1996-04-01         
    ##  6 R 0.5         R 0.5         0.5     1996-05-13 1996-05-13         
    ##  7 R 0.6         R 0.6         0.6     1996-05-17 1996-05-17         
    ##  8 R 0.7         R 0.7         0.7     1996-05-28 1996-05-28         
    ##  9 R 0.8         R 0.8         0.8     1996-05-31 1996-05-31         
    ## 10 R 0.9         R 0.9         0.9     1996-06-07 1996-06-07         
    ## # ℹ 152 more rows
    ## # ℹ 7 more variables: minor_release_year <dbl>, patch_updates <dbl>,
    ## #   minor_release_end <date>, minor_releases <dbl>, major_release_start <date>,
    ## #   major_release_year <dbl>, major_release_end <date>

Which versions have had the most patch_updates?

``` r
release_dates %>% 
  filter(date > as.Date("2000-02-07")) %>% 
  distinct(minor_version, patch_updates) %>% 
  arrange(desc(patch_updates)) %>% 
  slice(1:5)
```

    ## # A tibble: 5 × 2
    ##   minor_version patch_updates
    ##   <chr>                 <dbl>
    ## 1 R 3.2                     5
    ## 2 R 4.0                     5
    ## 3 R 3.4                     4
    ## 4 R 1.2                     3
    ## 5 R 2.15                    3

# Can we guess how many more patch_updates the latest R version will have?

At the initial time of writing this, R 4.3 only had one update. Upon
returning to this, R 4.4.0 was just released.

We’ll build a `tsibble` to take a look at patch_updates by initial
release date for each minor R version.

``` r
major_release_ts <- 
  release_dates %>% 
  mutate(
    initial_release_week = yearweek(major_release_start),
    month = yearmonth(major_release_start),
    dev_end_week = yearweek(major_release_end)
  ) %>% 
  distinct(initial_release_week, major_version, minor_releases, dev_end_week) %>%
  as_tsibble(key = major_version, index = initial_release_week)

minor_release_ts <- 
  release_dates %>% 
  mutate(
    initial_release_week = yearweek(minor_release_start),
    month = yearmonth(minor_release_start),
    dev_end_week = yearweek(minor_release_end)
  ) %>% 
  distinct(initial_release_week, minor_version, patch_updates, dev_end_week) %>% 
  as_tsibble(key = minor_version, index = initial_release_week)
```

And on to the graph…

Major R releases

``` r
autoplot(major_release_ts, minor_releases) + 
  geom_point() + 
  geom_label_repel(aes(label = major_version, vjust = 0.6), direction = "y", segment.color = "blue") + 
  labs(x = NULL, y = "# of minor releases", caption = "Label positions indicate initial release date for major R version") + 
  ggtitle("How many minor releases should be expected for the latest major version of R?") + 
  scale_x_yearmonth(date_breaks = "2 years", date_label = "%Y") + 
  scale_y_continuous(limits = c(0, 15), expand = c(0.02, 0), breaks = seq(0, 15, by = 2)) + 
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = 2)
  )
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

    ## Warning: ggrepel: 5 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](R-version-release-dates_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave(
  here("publish", paste0(Sys.Date(), "_r_major_version_ts.png")),
  width = 15,
  height = 6
  )
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

Minor releases..

``` r
autoplot(minor_release_ts, patch_updates) + 
  geom_point() + 
  geom_label_repel(aes(label = minor_version, vjust = 0.6), direction = "y", segment.color = "blue") + 
  labs(x = NULL, y = "# of patch updates", caption = "Label positions indicate initial release date for minor R version") + 
  ggtitle("How many R 4.4 patch updates should be expected this year?") + 
  scale_x_yearmonth(date_breaks = "1 year", date_label = "%Y") + 
  scale_y_continuous(limits = c(0, 5.25), expand = c(0.01, 0)) + 
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = 2)
  )
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

    ## Warning: ggrepel: 30 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](R-version-release-dates_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggsave(
  here("publish", paste0(Sys.Date(), "_r_minor_version_ts.png")),
  width = 15,
  height = 6
  )
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

    ## Warning: ggrepel: 18 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

# What months have typically had the most patch_updates?

We can look at the total number of patch_updates that have occurred in
each month. We’re currently in August 2023, so this might suggest when
we can expect patch_updates to occur for the latest R version.

``` r
release_dates %>% 
  mutate(
    month = yearmonth(date),
    mo = lubridate::month(date, label = TRUE),
    yr = lubridate::year(date) %>% as.factor()
  ) %>% 
  group_by(mo) %>% 
  mutate(patch_updates = n()-1) %>% 
  distinct(mo, patch_updates) %>% 
  ggplot(aes(x = mo, y = patch_updates)) +
  geom_col() + 
  theme_classic() + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = NULL, y = "total # of R releases", title = "End of year patch_updates are more likely to happen in October and December")
```

![](R-version-release-dates_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave(
  here("publish", paste0(Sys.Date(), "_minor_release_months.png")),
  width = 7,
  height = 5
  )
```

# What months have new R versions been initially released in?

``` r
minor_release_ts %>% 
  mutate(month = month(initial_release_week, label = TRUE)) %>% 
  count(month, sort = TRUE)
```

    ## # A tibble: 11 × 2
    ##    month     n
    ##    <ord> <int>
    ##  1 Apr      24
    ##  2 Oct       9
    ##  3 May       6
    ##  4 Jun       5
    ##  5 Dec       5
    ##  6 Feb       4
    ##  7 Nov       4
    ##  8 Mar       3
    ##  9 Sep       3
    ## 10 Aug       2
    ## 11 Jul       1

Interesting, so no new R versions have ever been released in January or
July!
