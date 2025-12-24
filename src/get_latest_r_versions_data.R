library(rversions)

rversions <-
  r_versions() %>% 
  filter(version >= "1.4.1") %>% 
  mutate(
    date = as.Date(date),
    major_version = paste0("R ", stringr::str_extract(version, "(^\\d)"), ".0"),
    minor_version = paste0("R ", stringr::str_extract(version, "(\\d\\.{1}\\d)"))
  ) %>% 
  select(major_version, minor_version, date) %>% 
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
  ungroup() %>% 
  select(-date) %>% 
  distinct()

saveRDS(rversions, here("data", paste0(Sys.Date(), "_rversions.RDS")))
