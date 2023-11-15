#### ni: inverse of in for selection of elements
`%ni%` <- Negate(`%in%`)

#### max frequency function
max_freq <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#### function for times derivations:t1 to t2, etc
f_pente <- function(pente_tibble){
  n_global <- sum(pente_tibble$n)
  result <- pente_tibble %>%
    # mutate(n_global = n+lag(n)) %>%
    mutate(Pa = (n/n_global - lag(n)/n_global)/(source.publication_date - lag(source.publication_date))) %>%
    mutate(n_normalized = n/n_global)
  return(result)
}

#### function for times derivations difference with general slope of NAICS:t1 to t2, etc
f_slope_naics <- function(pente_tibble){
  general_slope <- pente_tibble %>%
    filter(source.publication_date %in% c(1829, 1907)) %>%
    group_by(NAICS) %>%
    mutate(slope_period = (n-lag(n))/(1907-1829)) %>%
    filter(source.publication_date != 1829) %>%
    select(NAICS, slope_period)
  
  specific_slope_time <- pente_tibble %>%
    group_by(NAICS) %>%
    mutate(slope_t = (n-lag(n))/(source.publication_date - lag(source.publication_date))) %>% 
    mutate(time_period = paste0(lag(source.publication_date), '-', source.publication_date)) %>%
    filter(!is.na(slope_t)) %>% 
    select(time_period, date_join, NAICS, slope_t)
  
  slope <- specific_slope_time %>%
    left_join(y = general_slope, by = 'NAICS')
  return(slope)
}

#### function for times derivations difference with general slope in NAICS category
f_slope_in_naics <- function(pente_tibble){
  general_slope <- pente_tibble %>%
    filter(date_join %in% c(1831, 1906)) %>%
    group_by(freq_max) %>%
    mutate(slope_period = (n-lag(n))/(1907-1833)) %>%
    filter(source.publication_date != 1833) %>%
    select(freq_max, slope_period)
  
  specific_slope_time <- pente_tibble %>%
    group_by(NAICS, freq_max) %>%
    mutate(slope_t = (n-lag(n))/(source.publication_date - lag(source.publication_date))) %>% 
    mutate(time_period = paste0(lag(source.publication_date), '-', source.publication_date)) %>%
    filter(!is.na(slope_t)) %>% 
    select(time_period, date_join, NAICS, freq_max, slope_t)
  
  slope <- specific_slope_time %>%
    left_join(y = general_slope, by = 'freq_max')
  return(slope)
}


#### Derivations difference with general slope of NAICS: choice of dates
f_slope_naics_periodchoice <- function(pente_tibble, date_begin, date_ending){
  general_slope <- pente_tibble %>%
    filter(source.publication_date %in% c(1829, 1907)) %>%
    group_by(NAICS) %>%
    mutate(slope_period = round(x = (n-lag(n))/(1907-1829), digits = 3)) %>%
    filter(source.publication_date != 1829) %>%
    select(NAICS, slope_period)
  
  specific_slope_time <- pente_tibble %>%
    filter(source.publication_date %in% c(date_begin, date_ending)) %>%
    group_by(NAICS) %>%
    mutate(slope_t = round(x = (n-lag(n))/(date_ending-date_begin), digits = 3)) %>% 
    mutate(time_period = paste0(lag(source.publication_date), '-', source.publication_date)) %>%
    filter(!is.na(slope_t)) %>% 
    select(time_period, date_join, NAICS, slope_t)
  
  slope <- specific_slope_time %>%
    left_join(y = general_slope, by = 'NAICS')
  return(slope)
}

#### function for resumed of log10/log10 relation
f_relationship <- function(input_tibble){
  formula_log10 <- lm(log10(x = input_tibble$n) ~ log10(x = input_tibble$pop))
  summary_data <- summary(formula_log10)
  output_tibble <- tibble(
    y = paste0(round(summary_data$coefficients[1], 3), ' + ', round(summary_data$coefficients[2], 3), 'x'),
    beta = round(summary_data$coefficients[2], 3), beta_stderror = round(summary_data$coefficients[4], 3),
    beta_ci_min = round(summary_data$coefficients[2], 3) - round(summary_data$coefficients[4], 3),
    beta_ci_max = round(summary_data$coefficients[2], 3) + round(summary_data$coefficients[4], 3),
    r2 = round(summary_data$r.squared, 3), residuals_stderror_sigma = round(summary_data$sigma, 3),
    NAICS = input_tibble$NAICS[1])
  return(output_tibble)
}

#### function for resumed of log10/log10 relation in naic
f_relationship_in_naic <- function(input_tibble){
  formula_log10 <- lm(log10(x = input_tibble$n) ~ log10(x = input_tibble$pop))
  summary_data <- summary(formula_log10)
  output_tibble <- tibble(
    y = paste0(round(summary_data$coefficients[1], 3), ' + ', round(summary_data$coefficients[2], 3), 'x'),
    beta = round(summary_data$coefficients[2], 3), beta_stderror = round(summary_data$coefficients[4], 3),
    beta_ci_min = round(summary_data$coefficients[2], 3) - round(summary_data$coefficients[4], 3),
    beta_ci_max = round(summary_data$coefficients[2], 3) + round(summary_data$coefficients[4], 3),
    r2 = round(summary_data$r.squared, 3), residuals_stderror_sigma = round(summary_data$sigma, 3),
    NAICS = input_tibble$NAICS[1], freq_max = input_tibble$freq_max[1])
  return(output_tibble)
}
