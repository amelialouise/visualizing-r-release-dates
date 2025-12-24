Analysis of R version release dates
================

We’ll use the `r_versions()` function from {rversions} to get past and
present versions in a dataframe.

Since R 1.4.1, version numbers consist of three numbers: major, minor
and patch.

So we’ll start with R 1.4.1.

The `get_latest_r_versions_data.R` script in */src* will format names
and add date variables used for plots.

``` r
library(here)
library(dplyr)
library(ggplot2)
library(rversions)
library(tsibble)
library(ggrepel)

# custom script to create and save tidy data in local sesh
source(here("src", "get_latest_r_versions_data.R"))

# load tidy data
release_dates <- readRDS(here("data", paste0(Sys.Date(), "_rversions.RDS")))
```

Take a…

``` r
glimpse(release_dates)
```

    ## Rows: 29
    ## Columns: 10
    ## $ major_version       <chr> "R 1.0", "R 1.0", "R 1.0", "R 1.0", "R 1.0", "R 1.…
    ## $ minor_version       <chr> "R 1.4", "R 1.5", "R 1.6", "R 1.7", "R 1.8", "R 1.…
    ## $ minor_release_start <date> 2002-01-30, 2002-04-29, 2002-10-01, 2003-04-16, 2…
    ## $ minor_release_year  <dbl> 2002, 2002, 2002, 2003, 2003, 2004, 2004, 2005, 20…
    ## $ patch_updates       <dbl> 0, 1, 2, 1, 1, 1, 1, 18, 1, 1, 1, 1, 2, 2, 1, 2, 3…
    ## $ minor_release_end   <date> 2002-01-30, 2002-06-17, 2003-01-10, 2003-06-16, 2…
    ## $ minor_releases      <dbl> 5, 5, 5, 5, 5, 5, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6,…
    ## $ major_release_start <date> 2002-01-30, 2002-01-30, 2002-01-30, 2002-01-30, 2…
    ## $ major_release_year  <dbl> 2002, 2002, 2002, 2002, 2002, 2002, 2004, 2004, 20…
    ## $ major_release_end   <date> 2004-06-21, 2004-06-21, 2004-06-21, 2004-06-21, 2…

Which versions have had the most patch updates?

``` r
release_dates %>% 
  distinct(minor_version, patch_updates) %>% 
  arrange(desc(patch_updates)) %>% 
  slice(1:5)
```

    ## # A tibble: 5 × 2
    ##   minor_version patch_updates
    ##   <chr>                 <dbl>
    ## 1 R 2.1                    18
    ## 2 R 3.2                     5
    ## 3 R 4.0                     5
    ## 4 R 3.4                     4
    ## 5 R 3.0                     3

# Can we guess how many more patch updates the latest R version will have?

At the initial time of writing this, R 4.5 had 2 updates.

We’ll use a `tsibble` to take a look at updates by initial release date
for each major and then minor R version.

``` r
plot_majors <- 
  release_dates %>% 
  distinct(major_release_start, major_version, minor_releases) %>% 
  as_tsibble(index = major_release_start) %>% 
  ggplot(aes(x = major_release_start, y = minor_releases)) + 
  geom_line() + 
  geom_label_repel(aes(label = major_version, vjust = 0.6), direction = "y", segment.color = "blue") + 
  labs(x = NULL, y = "# of minor releases", caption = "Label positions indicate initial release date for major R version") + 
  ggtitle("How many minor releases should be expected for the latest major version of R?") + 
  scale_x_yearmonth(date_breaks = "2 years", date_label = "%Y") + 
  scale_y_continuous(limits = c(0, 12), expand = c(0.02, 0), breaks = seq(0, 12, by = 2)) + 
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = 2)
  )

plot_majors
```

![](R-version-release-dates_files/figure-gfm/major-versions-plot-1.png)<!-- -->

And we save it…

``` r
ggsave(
  here("images", paste0(Sys.Date(), "_r_major_version_ts.png")),
  width = 15,
  height = 6
  )
```

<figure>
<img src="images/r_major_plot.png" alt="R major versions" />
<figcaption aria-hidden="true">R major versions</figcaption>
</figure>

Minor releases graph version

``` r
plot_minors <- 
  release_dates %>% 
  distinct(minor_release_start, minor_version, patch_updates) %>% 
  as_tsibble(index = minor_release_start) %>% 
  ggplot(aes(x = minor_release_start, y = patch_updates)) + 
  geom_line() + 
  geom_label_repel(aes(label = minor_version, vjust = 0.6), direction = "y", segment.color = "blue") + 
  labs(x = NULL, y = "# of patch updates", caption = "Label positions indicate initial release date for minor R version") + 
  ggtitle("How many R 4.5 patch updates should be expected this year?") + 
  scale_x_yearmonth(date_breaks = "1 year", date_label = "%Y") + 
  scale_y_continuous(limits = c(0, 19), expand = c(0.01, 0), breaks = seq(0, 20, by = 2)) + 
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = 2)
  )

plot_minors
```

![](R-version-release-dates_files/figure-gfm/minor-versions-plot-1.png)<!-- -->

``` r
ggsave(
  here("images", paste0(Sys.Date(), "_r_minor_version_ts.png")),
  width = 15,
  height = 6
  )
```

<figure>
<img src="images/r_minor_plot.png" alt="R minor versions" />
<figcaption aria-hidden="true">R minor versions</figcaption>
</figure>

# What months have typically had the most patch updates?

We can look at the total number of patch_updates that have occurred in
each month. We’re currently in August 2023, so this might suggest when
we can expect patch_updates to occur for the latest R version.

``` r
release_dates %>% 
  mutate(
    month = yearmonth(minor_release_start),
    mo = lubridate::month(minor_release_start, label = TRUE),
    yr = lubridate::year(minor_release_start) %>% as.factor()
  ) %>% 
  group_by(mo) %>% 
  mutate(patch_updates = n()-1) %>% 
  distinct(mo, patch_updates) %>% 
  ggplot(aes(x = mo, y = patch_updates)) +
  geom_col() + 
  theme_classic() + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = NULL, y = "total # of R releases", title = "End of year patch updates are more likely to happen in October")
```

![](R-version-release-dates_files/figure-gfm/patch-update-months-1.png)<!-- -->

``` r
ggsave(
  here("images", paste0(Sys.Date(), "_minor_release_months.png")),
  width = 7,
  height = 5
  )
```

<figure>
<img src="images/release_months_plot.png" alt="R release months" />
<figcaption aria-hidden="true">R release months</figcaption>
</figure>

That’s all for now.
