library(rversions)

rversions <-
  r_versions() %>% 
  mutate(
    date = as.Date(date),
    major_version = 
      paste("R", stringr::str_extract(version, "(0\\.[1-9])|(\\d*)")) %>% 
      if_else(stringr::str_detect(., "\\."), ., paste0(., ".0")),
    minor_version = paste("R", stringr::str_extract(version, "(\\d*\\.{1}\\d*)"))
  ) %>% 
  select(major_version, minor_version, version, date) %>% 
  group_by(minor_version) %>% 
  mutate(
    minor_release_start = first(date),
    minor_release_year = first(date) %>% lubridate::year(),
    patch_updates = n()-1,
    minor_release_end = last(date)
  ) %>% 
  ungroup() %>% 
  group_by(major_version) %>% 
  mutate(
    minor_releases = n_distinct(minor_version)-1,
    major_release_start = first(date),
    major_release_year = first(date) %>% lubridate::year(),
    major_release_end = last(date)
  ) %>% 
  ungroup()

saveRDS(rversions, here("data", paste0(Sys.Date(), "_rversions.RDS")))
