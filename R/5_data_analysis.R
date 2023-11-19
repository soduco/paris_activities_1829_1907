library(tidyverse)
library(sf)
library(data.table)
library(ineq)
library(patchwork)
library(latex2exp)
source(file = "R/functions.R")

#### data init ####
initial_data <- st_read(dsn = "data/data_extraction_with_population.gpkg") %>%
  rename(date_join = date_recensement) %>%
  select(-pop)
# total is 990,108

#### Rank clocks on all dates ####
table_for_clock <- initial_data %>%
  filter(!is.na(NAICS)) %>%
  filter(NAICS != '') %>%
  st_drop_geometry() %>%
  group_by(NAICS, source.publication_date) %>%
  summarise(size=n()) %>%
  group_by(source.publication_date) %>%
  mutate(rank=frankv(x = size, order=-1))

##### analyzing with qual. change ####
table_for_clock <- table_for_clock %>%
  group_by(NAICS) %>%
  mutate(NAICS = str_remove_all(NAICS, 'Manufacturers/Retailers|manufacturers/retailers|Retailers')) %>%
  mutate(type_of_change = case_when(
    # max(rank)-min(rank) == 1 ~ 'a. no change',
    max(rank)-min(rank) < 4 & max(rank)-min(rank) > -1 ~ 'a. minor change',
    max(rank)-min(rank) > 7 ~ 'c. major change',
    TRUE ~ 'b. slight change'
  ))

##### patchwork visual ####
table_for_clock_vis <- table_for_clock %>%
  mutate(NAICS = case_when(
    NAICS == 'Cosmetics, Beauty Supplies, and Perfume ' ~ 'Cosmetics, Beauty Supplies,\nand Perfume',
    NAICS == 'Clothing and clothing accessories ' ~ 'Clothing and clothing\naccessories',
    NAICS == 'Arts, Entertainment, and Recreation' ~ 'Arts, Entertainment,\nand Recreation',
    NAICS == 'Administrative and Legal Services' ~ 'Administrative\nand Legal Services',
    NAICS == 'No Activity, Living of Income' ~ 'No Activity,\nLiving of Income',
    NAICS == 'Construction and Public Works' ~ 'Construction and\n Public Works',
    TRUE ~ NAICS
  ))

a <- table_for_clock_vis %>%
  filter(type_of_change == 'a. minor change') %>%
  ggplot(aes(x = source.publication_date, y = rank, group = NAICS, color = NAICS)) +
  geom_line(linewidth=0.7) +
  scale_y_continuous(limits = c(0,21)) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.title = element_blank(), axis.text.x = element_text(size=10),
        plot.margin=unit(c(.1,.1,.4,.9),"cm"), legend.text = element_text(size=8), 
        legend.position = 'bottom',legend.margin=margin(-10,0,0,0)) +
  scale_x_continuous(breaks = seq(1830, 1900, 10)) +
  coord_polar() +
  expand_limits(x = 1909) +
  labs(subtitle = "(a)") +
  guides(color=guide_legend(ncol=2,byrow=TRUE))

b <- table_for_clock_vis %>%
  filter(type_of_change == 'b. slight change') %>%
  ggplot(aes(x = source.publication_date, y = rank, group = NAICS, color = NAICS)) +
  geom_line(linewidth=0.7) +
  scale_y_continuous(limits = c(0,21)) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.title = element_blank(), axis.text.x = element_text(size=10),
        plot.margin=unit(c(.1,.1,.4,.9),"cm"),
        legend.text = element_text(size=8), legend.position = 'bottom',legend.margin=margin(-10,0,0,0)) +
  scale_x_continuous(breaks = seq(1830, 1900, 10)) +
  coord_polar() +
  expand_limits(x = 1909) +
  labs(subtitle = "(b)") +
  guides(color=guide_legend(ncol=2,byrow=TRUE))

c <- table_for_clock_vis %>%
  filter(type_of_change == 'c. major change') %>%
  ggplot(aes(x = source.publication_date, y = rank, group = NAICS, color = NAICS)) +
  geom_line(linewidth=0.7) +
  scale_y_continuous(limits = c(0,21)) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.title = element_blank(), axis.text.x = element_text(size=10),
        plot.margin=unit(c(.1,.1,.4,.9),"cm"),
        legend.text = element_text(size=8), legend.position = 'bottom',legend.margin=margin(-10,0,0,0)) +
  scale_x_continuous(breaks = seq(1830, 1900, 10)) +
  coord_polar() +
  expand_limits(x = 1909) +
  labs(subtitle = "(c)") +
  guides(color=guide_legend(ncol=2,byrow=TRUE))

a + b + c + patchwork::plot_layout(ncol = 1, nrow = 3)

ggsave(filename = 'outputs/rank_clock_general.pdf', width = 14, height = 35, units = 'cm', dpi = 400)

##### Gini on all dates ####
table_gini <- table_for_clock %>%
  group_by(source.publication_date) %>%
  summarise(gini = ineq(x = size, type = 'Gini'))

d <- table_gini %>%
  ggplot(aes(x = source.publication_date, y = gini)) +
  geom_line() +
  theme_bw() +
  theme(axis.title = element_blank(), axis.text = element_text(size=7)) +
  scale_y_continuous(limits = c(0.4,0.55)) +
  labs(subtitle = "Gini coefficient")

e <- table_for_clock %>%
  group_by(source.publication_date) %>%
  ggplot(aes(size, group=source.publication_date, color=source.publication_date)) +
  gglorenz::stat_lorenz(linewidth = 0.2) +
  scale_color_viridis_c() +
  geom_abline(linetype = "dashed") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  labs(x = 'cumulative frequency of activity categories',
       y = TeX(r"(cumulative frequency of $N_a$)"),
       subtitle = 'Lorenz curves')

e + d + plot_layout(widths = c(4,3))
ggsave(filename = 'outputs/lorenz_and_gini.png', width = 18, height = 13, units = 'cm', dpi = 300)

rm(a,b,c,d,e)

#### Na for some activity categories: all dates ####
# all for SI
table_for_clock %>%
  ggplot(aes(x = source.publication_date, y = size, color=NAICS)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text = element_text(size=9), legend.spacing = unit(0.3, 'cm'),
        legend.text = element_text(size=7), legend.title = element_blank(), legend.position = 'bottom',
        legend.margin=margin(-10,0,0,0), plot.margin=unit(c(.1,.4,.1,.1),"cm")) +
  scale_y_log10(name = TeX(r"($N_a$)"), minor_breaks = seq(10, 25000, 1000),
                labels = scales::label_math(format = log10), limits = c(10,25000))

ggsave(filename = "outputs/evo_Na_all.png", width = 24, height = 18, units = 'cm', dpi = 300)

# choice in main paper
table_choices_act <- table_for_clock %>%
  filter(NAICS %in% c('Food stores', 'Educational Services', 'Engineering Services', 
                      'Administrative and Legal Services', 'Health Practitioners', 
                      'Trade Agents and Brokers', 'Finance and Insurance',
                      'Clothing and clothing accessories ', 'Public Administration', 'Manufacturing'))

table_choices_act %>%
  ggplot(aes(x = source.publication_date, y = size, color=NAICS)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text = element_text(size=16), legend.spacing = unit(0.3, 'cm'),
        legend.text = element_text(size=14), legend.title = element_blank(), legend.position = 'bottom',
        legend.margin=margin(-10,0,0,0), plot.margin=unit(c(.1,.4,.1,.1),"cm"), axis.title.y = element_text(size=16)) +
  scale_y_log10(name = TeX(r"($N_a$)"),
                labels = scales::label_math(format = log10),
                     limits = c(50,25000),
                     minor_breaks = seq(100, 25000, 1000)) +
  guides(color=guide_legend(nrow=4,byrow=TRUE))

ggsave(filename = "outputs/evo_Na_selected_activities.pdf", width = 25, height = 18, units = 'cm', dpi = 400)

### Na and pop 1833-1907 ####
## removing 1829 because population census is 1817 and then 1831
initial_data_no_1829 <- initial_data %>%
  filter(source.publication_date != 1829) # deleting 1829 for co-evolution analysis
# total is 966,783 entries

## removing NA NAICS data
initial_data_no_1829 <- initial_data_no_1829 %>%
  filter(!is.na(NAICS)) %>%
  filter(NAICS != '')
# total is 951,367

## population data
pop_paris <- read.csv(file = "data/pop_paris_long_1831_1906.csv")


##### overview ####
data_na_naics <- initial_data %>%
  filter(!is.na(NAICS)) %>%
  filter(NAICS != '') %>%
  st_drop_geometry() %>%
  group_by(source.book, source.publication_date, NAICS, date_join) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(source.publication_date)

## data Na all
data_na_global <- initial_data %>%
  filter(!is.na(NAICS)) %>%
  filter(NAICS != '') %>%
  st_drop_geometry() %>%
  group_by(source.book, source.publication_date, date_join) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(source.publication_date)

data_na_global <- data_na_global %>%
  f_pente(.)

data_na_naics_liste <- data_na_naics %>%
  group_by(NAICS) %>%
  group_split()

data_na_naics_liste <- lapply(data_na_naics_liste, f_pente)
data_na_naics <- rbindlist(l = data_na_naics_liste) %>%
  as_tibble()

data_na_naics_pa_mean <- data_na_naics %>%
  group_by(source.publication_date) %>%
  summarise(Pa_mean = mean(Pa, na.rm = TRUE))

data_na_naics <- data_na_naics %>%
  left_join(y = data_na_naics_pa_mean, by = "source.publication_date") %>%
  mutate(Pa_ecart = Pa - Pa_mean) %>%
  group_by(NAICS) %>%
  mutate(time = row_number())

#### visualization
data_na_naics %>%
  ggplot(aes(x = time, y = Pa_ecart, color = NAICS)) +
  geom_line(linewidth=0.3) +
  theme_bw() +
  scale_y_continuous(name = TeX(r"($P_a - \bar{P_a}$)")) +
  scale_x_continuous(name = 't') +
  theme(legend.text = element_text(size = 6)) +
  ggtitle(TeX(r"($P_a = \frac{\Delta{N_a}}{\Delta{t}}$)")) +
  labs(caption = "Na is the normalized number of City directory entries")

ggsave(filename = "outputs/reprise_data_na_Pa_ecart_mean_Pa.png", width = 28, height = 16, units = 'cm', dpi = 300)

##### resumed ####
resume <- data_na_naics %>%
  summarise(Pa_ecart_mean = mean(Pa_ecart, na.rm = TRUE), sd_Pa_ecart = sd(Pa_ecart, na.rm = TRUE)) %>%
  arrange(-Pa_ecart_mean)

write.csv(x = resume, file = "outputs/Pa_resumed.csv", row.names = FALSE)

write.csv(x = data_na_naics, file = "outputs/Pa_general.csv", row.names = FALSE)

#### log10: NAICS and population #####
data_na_naics <- data_na_naics %>%
  left_join(y = pop_paris, by = c('date_join' = 'date'))

data_na_naics %>%
  mutate(NAICS = str_remove_all(NAICS, 'Manufacturers/Retailers|manufacturers/retailers|Retailers')) %>%
  ggplot(mapping = aes(x = pop, y = n, group = NAICS, color=NAICS)) +
  ggpmisc::stat_poly_line(se = FALSE, show.legend = FALSE) +
  ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                          after_stat(rr.label), sep = "*\", \"*")),
                        size = 2, label.x = "right", show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  scale_x_log10(name = 'Population', limits = c(750000,3000000), minor_breaks = seq(500000, 3000000, 100000)) +
  scale_y_log10(name = TeX(r"($N_a$)"), minor_breaks = seq(100, 15000, 1000), 
                labels = scales::label_math(format = log10)) +
  theme_bw() +
  theme(strip.text = element_text(size = 6), axis.text.x = element_text(size=7)) +
  facet_wrap(~NAICS)

ggsave(filename = "outputs/Na_vs_pop.png", width = 28, height = 22, units = 'cm', dpi = 300)

#### resumed of log10 relation
data_na_naics_liste <- data_na_naics %>%
  group_split()

relation_data_naics <- lapply(data_na_naics_liste, f_relationship)
relation_data_naics <- rbindlist(relation_data_naics) %>%
  as_tibble()

write.csv(x = relation_data_naics %>% arrange(beta), file = "outputs/Na_Pop_resumed.csv", row.names = FALSE)

##### variation through time #####
###### all distinct activities ####
general_slope <- f_slope_naics(pente_tibble = data_na_naics)

general_slope %>%
  mutate(NAICS = str_remove_all(NAICS, 'Manufacturers/Retailers|manufacturers/retailers|Retailers')) %>%
  mutate(t = row_number()+1, date = as.numeric(str_sub(time_period, start=6, end=10))) %>%
  mutate(diff_slope = (slope_t - slope_period)/abs(slope_period)) %>%
  ggplot(aes(x=date, y=diff_slope, color=NAICS)) +
  geom_line(linewidth=0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  ylab(label = TeX(r"($D_t$)"))

ggsave(filename = "outputs/diff_slope_t.png", width = 28, height = 14, units = 'cm', dpi = 300)

# ###### Dt in main paper ####
# # all activities as one
# # both in one graphic
# a <- general_slope %>%
#   mutate(NAICS = str_remove_all(NAICS, 'Manufacturers/Retailers|manufacturers/retailers|Retailers')) %>%
#   mutate(t = row_number()+1, date = as.numeric(str_sub(time_period, start=6, end=10))) %>%
#   mutate(diff_slope = (slope_t - slope_period)/abs(slope_period))
# 
# b <- a %>%
#   group_by(time_period, date) %>%
#   summarise(diff_slope = mean(diff_slope)) 
# 
# a %>%
#   select(date, diff_slope) %>%
#   mutate(colorp = 'a') %>%
#   bind_rows(
#     b %>%
#       select(date, diff_slope) %>%
#       mutate(colorp = 'b')
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x=date, y=diff_slope, group=NAICS, color = colorp)) +
#   geom_line(show.legend = FALSE) +
#   scale_color_manual(values = c('grey80', 'black')) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(), plot.margin=unit(c(.1,.4,.1,.1),"cm")) +
#   ylab(label = TeX(r"($D_t$)"))
# 
# ggsave(filename = "outputs/mean_dt.pdf", width = 14, height = 11, units = 'cm', dpi = 300)

###### Ft for each activity ####
cv_mean_slope <- general_slope %>%
  group_by(NAICS) %>% # already group, just for code expressiveness
  mutate(time_period_n = 1) %>%
  reframe(variance = sum((slope_t-slope_period)^2)/sum(time_period_n), slope_period = slope_period) %>%
  unique() %>%
  mutate(cv = round(sqrt(variance)/abs(slope_period), digits = 3))

write.csv(cv_mean_slope, 'outputs/slope_ft_naics.csv')

##### scaling exponent NAICS main paper ####
visu_arcaute <- relation_data_naics %>%
  left_join(y = cv_mean_slope, by = "NAICS") %>%
  # for better visualization
  mutate(NAICS = str_remove_all(NAICS, 'Manufacturers/Retailers|manufacturers/retailers|Retailers'))

visu_arcaute %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = reorder(NAICS, beta), y = beta, ymin = beta_ci_min, ymax=beta_ci_max, color=cv)) +
  scale_color_viridis_c(direction = -1, name = TeX(r"($F_t$)"), trans = 'log10') +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_text(size=14),
        legend.title = element_text(size=14), axis.text = element_text(size=16)) +
  ggrepel::geom_text_repel(data = visu_arcaute %>%
                             mutate(NAICSlabel = if_else(NAICS %in% c('Arts, Entertainment, and Recreation', 'No Activity, Living of Income',
                                                                      'Publishing Industries', 'Other retailers', 'Building and Car Rental',
                                                                      'Manufacturing',
                                                                      'Cosmetics, Beauty Supplies, and Perfume ', 'Construction and Public Works'),
                                                         '', NAICS)), 
                           aes(x = reorder(NAICS, beta), y = beta, label=NAICSlabel), 
                           size = 5, color='grey30', nudge_y = -0.4, nudge_x = 1, segment.size=0.2) +
  ylab(label = TeX(r"(Scaling exponent $\beta$)"))

ggsave(filename = "outputs/beta_scaling_gal.png", width = 18, height = 15, units = 'cm', dpi = 300)
ggsave(filename = "outputs/beta_scaling_gal.pdf", width = 25, height = 18, units = 'cm', dpi = 400)

##### haussmann and Commune period variation ####
naics_haussmann <- f_slope_naics_periodchoice(pente_tibble = data_na_naics, date_begin = 1855, date_ending = 1875) %>%
  mutate(Dt = round((slope_t - slope_period)/abs(slope_period), digits = 2))

write.csv(x = naics_haussmann, file = 'outputs/slope_haussmann_period.csv', row.names = FALSE)

naics_commune <- f_slope_naics_periodchoice(pente_tibble = data_na_naics, date_begin = 1864, date_ending = 1871) %>%
  mutate(Dt = round((slope_t - slope_period)/abs(slope_period), digits = 2))

write.csv(x = naics_commune, file = 'outputs/slope_commune_period_begin.csv', row.names = FALSE)

naics_commune <- f_slope_naics_periodchoice(pente_tibble = data_na_naics,date_begin = 1871, date_ending = 1875) %>%
  mutate(Dt = round((slope_t - slope_period)/abs(slope_period), digits = 2))

write.csv(x = naics_commune, file = 'outputs/slope_commune_period_after.csv', row.names = FALSE)

naics_newdelim <- f_slope_naics_periodchoice(pente_tibble = data_na_naics,date_begin = 1855, date_ending = 1860) %>%
  mutate(Dt = round((slope_t - slope_period)/abs(slope_period), digits = 2))

write.csv(x = naics_newdelim, file = 'outputs/slope_new_delim_paris.csv', row.names = FALSE)

rm(naics_haussmann, naics_commune, naics_newdelim)

#### Food differentiation ####
data_food <- initial_data %>%
  filter(!is.na(NAICS)) %>%
  filter(NAICS != '') %>%
  filter(NAICS == "Food stores") %>%
  st_drop_geometry() %>%
  # changing freq_max elements from manual analysis (see 3_manual_editing)
  mutate(freq_max = case_when(
    freq_max == 'produits chimiques' ~ 'comestibles', # activity is commerce alimentaire
    freq_max == 'bijoutier en or' ~ 'boucher',
    freq_max == 'bois et charbons' ~ 'patissier', # activity is biscuits
    freq_max == 'brocanteur' & str_detect(string = act_new, pattern = "biere") ~ 'vins', # very few
    freq_max == 'brocanteur' & str_detect(string = act_new, pattern = "beurre") ~ 'beurre et oeufs', # very few
    freq_max == 'pharmacien' & str_detect(string = act_new, pattern = "promages") ~ 'fromages', # very few
    freq_max == 'pharmacien' & str_detect(string = act_new, pattern = "primceur") ~ 'fruitier', # activity is primeur (very few, same meaning as fruitier)
    freq_max == 'broderies' ~ 'beurre et oeufs', # activity is beurre
    freq_max == 'cafe du commerce' ~ 'epicier', # activity is bad ocr of epicier
    freq_max == 'chemisier' ~ 'comestibles', # activity is conserve
    freq_max == 'couteurs et vernis' ~ 'vins', # activity is cidre
    freq_max == 'depute du nord' ~ 'epicier', # activity is thé
    freq_max == 'robes et manteaux' ~ 'epicier', # activity is bad ocr of epicier
    freq_max == "secretaire d'ambassade" ~ 'epicier', # activity is sucre
    freq_max == "traiteur" ~ 'comestibles', # activity is bad phonetic index for truitier (appeared at the end of the century)
    # all elements containing 'vins' are not wholesalers or restaurants
    str_detect(freq_max, 'vins') ~ 'vins',
    # diversity inside bad phonetic index of march. de meubles
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "beurre") ~ 'beurre et oeufs',
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "biere") ~ 'vins',
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "ca |cafc|cafe|sel|the") ~ 'epicier',
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "legum|pomme") ~ 'fruitier', # activity is legume and pomme de terre; less than 10
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "vins") ~ 'vins',
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "poiss") ~ 'comestibles', # activity is poissons; at the end of the period: less than 10
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "sauciss|abat") ~ 'charcutier',
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "volail|volait") ~ 'boucher',
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "fromage") ~ 'fromages',
    freq_max == 'march. de meubles' & str_detect(string = act_new, pattern = "scargo") ~ 'comestibles', # activity is escargot; at the end of the period: less than 5
    # diversity inside bad phonetic index of maroquinerie
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "beurre") ~ 'beurre et oeufs',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "biere") ~ 'vins',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "ca |cafc|cafe|sel|the") ~ 'epicier',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "legum|pomme") ~ 'fruitier',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "vins") ~ 'vins',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "poiss") ~ 'comestibles',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "sauciss|abat") ~ 'charcutier',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "volail|volait") ~ 'boucher',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "fromage") ~ 'fromages',
    freq_max == 'maroquinerie' & str_detect(string = act_new, pattern = "scargo") ~ 'comestibles',
    TRUE ~ freq_max
  )) %>%
  # changing the name from ocr and (case of fromages) the bad specification of max frequency of phonetic index
  # (see end of 3_manual_editing)
  mutate(freq_max = case_when(
    freq_max == 'boutanger' ~ 'boulanger', # bad ocr
    freq_max %in% c('cpicier', 'cpicter', 'epicter') ~ 'epicier', # idem
    freq_max == 'cremicr' ~ 'cremier', # idem
    freq_max == 'uins' ~ 'vins', # idem
    freq_max == 'fromages en gros' ~ 'fromages',
    TRUE ~ freq_max
  )) %>%
  group_by(source.book, source.publication_date, NAICS, date_join, freq_max) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(source.publication_date)

data_food <- data_food %>%
  left_join(y = pop_paris, by = c('date_join' = 'date'))

data_food %>%
  # translation french/english
  left_join(y = readxl::read_excel(path = "data/food_translation.xlsx"), by = c('freq_max' = 'french')) %>%
  mutate(freq_max = paste0(freq_max, '\n', translation)) %>%
  ggplot(mapping = aes(x = pop, y = n, group = freq_max)) +
  ggpmisc::stat_poly_line(se = FALSE, show.legend = FALSE, color='grey30') +
  ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                          after_stat(rr.label), sep = "*\", \"*")),
                        size = 2, label.x = "right", show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  scale_x_log10(name = 'Population', limits = c(750000,3000000), minor_breaks = seq(500000, 3000000, 100000)) +
  scale_y_log10(name = TeX(r"($N_a$)"), minor_breaks = seq(1, 10000, 1000), labels = scales::label_math(format=log10)) +
  theme_bw() +
  facet_wrap(~freq_max)

ggsave(filename = "outputs/food_stores.png",  width = 28, height = 22, units = 'cm', dpi = 300)


#### visu as Arcaute et al and slope
food_slope <- f_slope_in_naics(pente_tibble = data_food)

cv_mean_slope <- food_slope %>%
  mutate(time_period_n = 1) %>%
  reframe(variance = sum((slope_t-slope_period)^2)/sum(time_period_n), slope_period = slope_period) %>%
  unique() %>%
  mutate(cv = sqrt(variance)/abs(slope_period))

# calculs relation log10/log10
data_food_liste <- data_food %>%
  group_by(freq_max) %>%
  group_split()

data_food_liste <- lapply(data_food_liste, f_relationship_in_naic)
data_food_relationship <- rbindlist(l = data_food_liste) %>%
  as_tibble()

data_food_relationship <- data_food_relationship %>%
  left_join(y = readxl::read_excel(path = "data/food_translation.xlsx"), by = c('freq_max' = 'french'))

#### visualisation as Arcaute et al
visu_arcaute <- data_food_relationship %>%
  left_join(y = cv_mean_slope, by = "freq_max")

visu_arcaute %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = reorder(translation, beta), y = beta, ymin = beta_ci_min, ymax=beta_ci_max, color=cv)) +
  scale_color_viridis_c(direction = -1, name = TeX(r"($F_t$)"), trans = 'log10') +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_text(size=14),
        legend.title = element_text(size=14), axis.text = element_text(size=16)) +
  ggrepel::geom_text_repel(data = visu_arcaute, 
                           aes(x = reorder(translation, beta), y = beta, label=translation), 
                           size = 5, color='grey30', nudge_y = -0.4, nudge_x = 1, segment.size=0.2) +
  ylab(label = TeX(r"(Scaling exponent $\beta$)"))

ggsave(filename = "outputs/beta_scaling_food_stores.png", width = 18, height = 15, units = 'cm', dpi = 300)
ggsave(filename = "outputs/beta_scaling_food_stores.pdf", width = 25, height = 18, units = 'cm', dpi = 400)

# visu of slope
food_slope %>%
  # choice of activities considering previous analysis
  left_join(y = readxl::read_excel(path = "data/food_translation.xlsx"), by = c('freq_max' = 'french')) %>%
  filter(translation %in% c('seed retailer', 'cheeses', 'confectioner', 'grocer', 'wines', 'butter and eggs', 'creamer', 'edible products')) %>%
  mutate(t = row_number()+1, date = as.numeric(str_sub(time_period, start=6, end=10))) %>%
  mutate(diff_slope = (slope_t - slope_period)/abs(slope_period)) %>%
  rename(activity = translation) %>%
  ggplot(aes(x=date, y=diff_slope, color=activity)) +
  ggthemes::scale_color_colorblind() +
  geom_line(linewidth=0.4) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  ylab(label = TeX(r"($D_t$)"))

ggsave(filename = "outputs/diff_slope_t_food_stores.png", width = 15, height = 13, units = 'cm', dpi = 300)

#### Maps ####
library(osmdata) # extract Paris current limit
library(MASS) # for kerdel density 2d
library(reshape2) # For melt function
# download current boundary of Paris
osmdataparis <- getbb(place_name = "Paris") %>% 
  opq() %>% 
  add_osm_feature(key = "admin_level", value = 8) %>% # https://wiki.openstreetmap.org/wiki/FR:Key:admin_level
  osmdata_sf()

osmdataparis <- osmdataparis$osm_multipolygons[1,] %>%
  dplyr::select(osm_id, name) %>%
  st_transform(crs = 2154)

boundingboxparis <- st_bbox(obj = osmdataparis %>% # addin buffer to expand limits
                              st_buffer(dist = 1000)) %>% 
  st_as_sfc() %>% 
  st_as_sf()

# filter data with geometry inside bounding box of paris
initial_data_sf <- initial_data %>%
  mutate(geomempty = st_is_empty(.)) %>%
  filter(geomempty == FALSE) %>%
  st_filter(x = ., y = boundingboxparis, .predicate = st_within)

# local: if too large
# rm(initial_data)

###### KDE visualization of spatialized data ######
kde_data <- initial_data_sf %>%
  st_cast(to = 'POINT') # if multipoint

# sf data to x;y data with informations
kde_data <- do.call(rbind, st_geometry(kde_data)) %>% 
  as_tibble() %>%
  setNames(c("x","y")) %>%
  bind_cols(kde_data %>% st_drop_geometry())

ndate_geoloc <- kde_data %>%
  group_by(source.publication_date) %>%
  count()

kde_data <- kde_data %>%
  left_join(y = ndate_geoloc, by = "source.publication_date") %>%
  mutate(date_facet = paste0(source.publication_date, ', N=', n))


#### visual with 3 different boundaries of Paris
admin_before_1860 <- st_read(dsn = "data/init_datasets/Vasserot_district/Vasserots_quartiers_v.shp") %>%
  st_union()

admin_after_1860 <- st_read(dsn = "data/init_datasets/quartiers_paris_post_1860.shp") %>%
  st_union()

# viridis
ggplot() +
  geom_sf(data = osmdataparis, linewidth = 0.2, fill='grey50')+
  geom_density_2d_filled(data = kde_data, 
                         mapping = aes(x=x, y=y), contour_var = "ndensity", alpha=0.8, bins = 20) +
  ggdark::dark_theme_linedraw() +
  geom_sf(data = admin_after_1860, linewidth = 0.2, color='black', alpha=0)+
  geom_sf(data = admin_before_1860, linewidth = 0.2, color='black', alpha=0)+
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
  facet_wrap(~ date_facet) +
  labs(title = 'Two-Dimensional Kernel density esimation', fill = 'ndensity level')

ggsave(filename = 'outputs/KDE_spatialized_data.png', width = 30, height = 26, units = 'cm', dpi = 300)

######  Calculate and plot relative difference between 1829 and 1885 ###### 
# Calculate the common x and y range for 2 dates datasets
sf1829 <- kde_data %>% filter(source.publication_date == 1829)
sf1885 <- kde_data %>% filter(source.publication_date == 1885)
  
xrng <- range(c(sf1829$x, sf1885$x))
yrng <- range(c(sf1829$y, sf1885$y))

# Calculate the 2d density estimate over the common range
d1829 <- kde2d(sf1829$x, sf1829$y, lims=c(xrng, yrng), n=200)
d1885 <- kde2d(sf1885$x, sf1885$y, lims=c(xrng, yrng), n=200)

# Confirm that the grid points for each density estimate are identical
identical(d1829$x, d1885$x)
identical(d1829$y, d1885$y)

# Calculate the difference between the 2d density estimates
diff_2dates <- d1829
diff_2dates$z <- d1885$z-d1829$z

## Melt data into long format
rownames(diff_2dates$z) <- diff_2dates$x
colnames(diff_2dates$z) <- diff_2dates$y

# Now melt it to long format
diff_2datesmelt <- melt(diff_2dates$z, id.var=rownames(diff_2dates))
names(diff_2datesmelt) <- c("x","y","z")

# Plot difference
a <- ggplot() +
  geom_tile(data = diff_2datesmelt, aes(x=x, y=y, z=z, fill=z)) +
  stat_contour(data = diff_2datesmelt, aes(x=x, y=y, z=z, colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
  scale_colour_gradient2(low=scales::muted("red"), mid="white", high=scales::muted("blue"), midpoint=0) +
  geom_sf(data = osmdataparis, linewidth = 0.2, color='grey50', alpha=0)+
  geom_sf(data = admin_after_1860, linewidth = 0.2, color='grey50', alpha=0)+
  geom_sf(data = admin_before_1860, linewidth = 0.2, color='grey50', alpha=0)+
  theme_bw() +
  labs(fill = TeX(r"($KDE_{t+1}-KDE_t$)"), subtitle = '1829-1885') +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size=7)) +
  guides(colour=FALSE)

###### Same between 1850 and 1860 ###### 
# Calculate the common x and y range for 2 dates datasets
sf1850 <- kde_data %>% filter(source.publication_date == 1850)
sf1860 <- kde_data %>% filter(source.publication_date == 1860)

xrng <- range(c(sf1850$x, sf1860$x))
yrng <- range(c(sf1850$y, sf1860$y))

# Calculate the 2d density estimate over the common range
d1850 <- kde2d(sf1850$x, sf1850$y, lims=c(xrng, yrng), n=200)
d1860 <- kde2d(sf1860$x, sf1860$y, lims=c(xrng, yrng), n=200)

# Confirm that the grid points for each density estimate are identical
identical(d1850$x, d1860$x)
identical(d1850$y, d1860$y)

# Calculate the difference between the 2d density estimates
diff_2dates2 <- d1850
diff_2dates2$z <- d1860$z - d1850$z

## Melt data into long format
rownames(diff_2dates2$z) <- diff_2dates2$x
colnames(diff_2dates2$z) <- diff_2dates2$y

# Now melt it to long format
diff_2datesmelt2 <- melt(diff_2dates2$z, id.var=rownames(diff_2dates2))
names(diff_2datesmelt2) <- c("x","y","z")

# Plot difference
b <- ggplot() +
  geom_tile(data = diff_2datesmelt2, aes(x=x, y=y, z=z, fill=z)) +
  stat_contour(data = diff_2datesmelt2, aes(x=x, y=y, z=z, colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
  scale_colour_gradient2(low=scales::muted("red"), mid="white", high=scales::muted("blue"), midpoint=0) +
  geom_sf(data = osmdataparis, linewidth = 0.2, color='grey50', alpha=0)+
  geom_sf(data = admin_after_1860, linewidth = 0.2, color='grey50', alpha=0)+
  geom_sf(data = admin_before_1860, linewidth = 0.2, color='grey50', alpha=0)+
  theme_bw() +
  labs(fill = TeX(r"($KDE_{t+1}-KDE_t$)"), subtitle = '1850-1860') +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size=7)) +
  guides(colour=FALSE)

a/b

ggsave(filename = 'outputs/KDE_differences.png', width = 18, height = 20, units = 'cm', dpi = 300)


##### Difference between initial data and geolocated data ####
full_data_date <- table_for_clock %>%
  group_by(source.publication_date) %>%
  summarise(nall = sum(size))

full_data_date_geoloc <- initial_data_sf %>%
  st_drop_geometry() %>%
  group_by(source.publication_date) %>%
  count()

tibble_geoloc <- full_data_date %>%
  left_join(y = full_data_date_geoloc, by = 'source.publication_date') %>%
  mutate(Frequency = n/nall)

tibble_geoloc %>%
  ggplot(aes(x=source.publication_date, y = Frequency)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(axis.title.x = element_blank())

ggsave(filename = 'outputs/frequency_geolocated_entries.png', width = 14, height = 10, units = 'cm', dpi = 300)
