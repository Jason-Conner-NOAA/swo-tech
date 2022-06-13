# swo basic tables 
# ben.williams@noaa.gov
# 2022-06

# load ----
library(tidyverse)
library(tidytable)

# globals ----
region = 'ai'

# data ----
spec <- vroom::vroom(here::here('data', 'species_ai.csv'))

# total and avg sexed per haul sample size
lfreq <- vroom::vroom(here::here('data', 'lfreq_ai.csv'))

lfreq %>% 
  left_join.(spec) %>% 
  mutate.(haul_n = sum(frequency),
          .by = c(year, species_code, hauljoin, sex)) %>% 
  group_by(year, species_code, sex, species, haul_n) %>%
  dplyr::distinct(hauljoin) %>% 
  mutate.(total_n = sum(haul_n),
          .by = c(year, species_code)) %>% 
  group_by(year, species_code, species) %>%
  dplyr::distinct(total_n) -> sp_tot

lfreq %>% 
  left_join.(spec) %>% 
  mutate.(haul_n = sum(frequency),
          .by = c(year, species_code, hauljoin, sex)) %>% 
  group_by(year, species_code, sex, species, haul_n) %>%
  dplyr::distinct(hauljoin) %>% 
  filter.(sex <= 2) %>% 
  mutate.(haul_sx_n = sum (haul_n),
          .by = c(year, species_code, hauljoin)) %>% 
  group_by(year, species_code, hauljoin, species) %>%
  dplyr::distinct(haul_sx_n) %>% 
  mutate.(mu_hl_sx_n = mean(haul_sx_n),
          max_hl_sx_n = max(haul_sx_n),
          .by = c(year, species_code, species)) %>% 
  group_by(year, species_code, species) %>%
  dplyr::distinct(mu_hl_sx_n, max_hl_sx_n) %>% 
  left_join.(sp_tot) -> ss_specs

vroom::vroom_write(ss_specs, file = here::here("output", region, paste0("ss_specs.csv")), delim = ",")


# table of sexed samples saved

lfreq %>% 
  left_join.(spec) %>% 
  filter.(sex <= 2) %>% 
  mutate.(haul_n = sum(frequency),
          .by = c(year, species_code, hauljoin)) %>% 
  group_by(year, species_code, hauljoin, species) %>%
  dplyr::distinct(haul_n) %>% 
  mutate.(s50 = ifelse(haul_n > 50, haul_n - 50, 0)) %>% 
  mutate.(s75 = ifelse(haul_n > 75, haul_n - 75, 0)) %>% 
  mutate.(s100 = ifelse(haul_n > 100, haul_n - 100, 0)) %>% 
  mutate.(s125 = ifelse(haul_n > 125, haul_n - 125, 0)) %>% 
  mutate.(s150 = ifelse(haul_n > 150, haul_n - 150, 0)) %>% 
  mutate.(s175 = ifelse(haul_n > 175, haul_n - 175, 0)) %>% 
  pivot_longer.(cols = c(s50, s75, s100, s125, s150, s175), names_to = 'id') %>% 
  filter.(value >0) %>% 
  mutate.(mu_hl = mean(value), tot_hl = sum(value),
          .by = c(year, species_code, id)) %>% 
  group_by(year, species_code, species, id, mu_hl) %>%
  dplyr::distinct(tot_hl) -> ss_saved

vroom::vroom_write(ss_saved, file = here::here("output", region, paste0("ss_saved.csv")), delim = ",")



