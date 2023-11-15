library(tidyverse)
library(sf)
library(data.table)
library(patchwork)

#### data init and data binding creation ####
initial_data <- read_rds(file = "data/intermediary_datasets/data_extraction.rds")

##### data counting difference before and after review by hand #####
review_automatic <- read.csv(file = "data/group_block_review.csv", header = TRUE, stringsAsFactors = FALSE)

review_automatic <- review_automatic %>%
  # see manual identification review comments
  mutate(NAICS = case_when(
    freq_max == 'papiers peints' ~ 'Wholesalers Trade',
    freq_max == 'vins-' ~ 'Full-Service Restaurants',
    TRUE ~ NAICS
  )) %>%
  select(-manual_identification_review) %>%
  mutate(activity_grouping = 'automatic') %>%
  group_by(activity_grouping, NAICS) %>%
  summarise(N = sum(size)) %>%
  mutate(NAICS = if_else(N == 39410, 'NA', NAICS))

review_by_hand <- initial_data %>%
  st_drop_geometry() %>%
  mutate(activity_grouping = 'manual') %>%
  filter(!is.na(NAICS)) %>%
  group_by(activity_grouping, NAICS) %>%
  summarise(N = n()) %>%
  mutate(NAICS = if_else(N == 13697, 'NA', NAICS))

review_automatic %>%
  bind_rows(review_by_hand) %>%
  ggplot(mapping = aes(x = N, y = reorder(NAICS, N))) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) +
  ylab('NAICS-inspired category') +
  labs(caption = 'Group of activities, including 1829. Automatic is based on phonetic index and manual is the review of automatic data') +
  facet_wrap(~activity_grouping)

ggsave(filename = "outputs/grouping_analysis.png", width = 28, height = 22, units = 'cm', dpi = 300)

#### rank-size plots #####
## rank-size activities and number of entries for each book
b <- initial_data %>%
  st_drop_geometry() %>%
  group_by(source.book, source.publication_date, freq_max) %>%
  summarise(size = n()) %>%
  filter(!is.na(freq_max)) %>%
  mutate(rank = rank(x=desc(size))) %>%
  ggplot(mapping = aes(x=rank, y=size, color = reorder(source.book, source.publication_date))) +
  geom_point(size=0.4) +
  ggthemes::scale_color_tableau(palette = 'Tableau 20') +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_y_continuous(trans = 'log10', minor_breaks = seq(1, 10000, 1000)) +
  scale_x_continuous(trans = 'log10', minor_breaks = seq(1, 10000, 100)) +
  labs(subtitle = 'Directories')

# overview rank-size general
a <- initial_data %>%
  st_drop_geometry() %>%
  group_by(freq_max) %>%
  summarise(size = n()) %>%
  filter(!is.na(freq_max)) %>%
  mutate(rank = rank(x=desc(size))) %>%
  ggplot(mapping = aes(x=rank, y=size)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(trans = 'log10', minor_breaks = seq(1, 100000, 10000)) +
  scale_x_continuous(trans = 'log10', minor_breaks = seq(1, 10000, 1000)) +
  labs(subtitle = 'All directories')

a + b + plot_annotation(caption = 'Activites after applying soundex index')
ggsave(filename = 'outputs/overview_rank_size_activites.png', width = 26, height = 18, units = 'cm', dpi = 300)
rm(a,b)

# count data without NA
initial_data %>%
  st_drop_geometry() %>%
  filter(!is.na(freq_max)) %>%
  nrow()


#### adding link dates with population ####
initial_data <- initial_data %>%
  mutate(date_join = case_when(
    source.publication_date == 1833 ~ 1831,
    source.publication_date == 1839 ~ 1836,
    source.publication_date == 1842 ~ 1841,
    source.publication_date == 1845 ~ 1846,
    source.publication_date == 1850 ~ 1851,
    source.publication_date == 1855 ~ 1856,
    source.publication_date == 1860 ~ 1861,
    source.publication_date == 1864 ~ 1866,
    source.publication_date == 1871 ~ 1872,
    source.publication_date == 1875 ~ 1876,
    source.publication_date == 1880 ~ 1881,
    source.publication_date == 1885 ~ 1886,
    source.publication_date == 1896 ~ 1896,
    source.publication_date == 1901 ~ 1901,
    source.publication_date == 1907 ~ 1906,
  ))

# population of Paris
pop_paris <- read.csv(file = "data/pop_paris_long_1831_1906.csv")

##### output data with population for general accessibility (open access) ####
openxlsx::write.xlsx(x = initial_data %>% 
                       st_drop_geometry() %>% 
                       filter(!is.na(manual_identification_review)) %>% 
                       left_join(y = pop_paris, by = c('date_join' = 'date')) %>% 
                       rename(date_recensement = date_join), file = "data/data_extraction_with_population.xlsx")

# same but as a geopackage (spatialized data)
st_write(obj = initial_data %>% 
           filter(!is.na(manual_identification_review)) %>% 
           left_join(y = pop_paris, by = c('date_join' = 'date')) %>% 
           rename(date_recensement = date_join), dsn = "data/data_extraction_with_population.gpkg")
