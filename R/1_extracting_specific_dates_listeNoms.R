library(tidyverse)
library(sf)

full_dates_studied <- st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1829') %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1833')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1839')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1842')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1845')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1850')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1855')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1860')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1864')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1871')) %>% 
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1875')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1880')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1885')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1891')) %>% 
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1896')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1901')) %>%
  bind_rows(st_read(dsn="data/init_datasets/directories-ListNoms.gpkg", layer = '1907'))

write_rds(x = full_dates_studied, file = "data/intermediary_datasets/studied_data_2.rds", compress = 'gz')
