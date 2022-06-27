# swo basic figures 
# ben.williams@noaa.gov
# 2022-05

# load ----
library(tidyverse)
library(tidytable)
library(scico)
library(extrafont)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()
loadfonts(device="win")

# if you get a lot of messages like 'C:\Windows\Fonts\ALGER.TTF : No FontName. Skipping.'
# then load this package and then run font_import
# remotes::install_version("Rttf2pt1", version = "1.3.8")

# add fonts to all text (last line)
ggplot2::theme_set(
  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA),
      text = element_text(family = "Times New Roman")
    )
)


# globals ----
region = 'BS'

# data ----
spec <- vroom::vroom(here::here('data', 'species_code_name.csv'))
s50 <- vroom::vroom(here::here('output', region, 'ss50_ess_sz.csv')) %>% 
  mutate.(id = 50) 
s75 <- vroom::vroom(here::here('output', region, 'ss75_ess_sz.csv')) %>% 
  mutate.(id = 75)
s100 <- vroom::vroom(here::here('output', region, 'ss100_ess_sz.csv')) %>% 
  mutate.(id = 100) 
s125 <- vroom::vroom(here::here('output', region, 'ss125_ess_sz.csv')) %>% 
  mutate.(id = 125) 
s150 <- vroom::vroom(here::here('output', region, 'ss150_ess_sz.csv')) %>% 
  mutate.(id = 150)
s175 <- vroom::vroom(here::here('output', region, 'ss175_ess_sz.csv')) %>% 
  mutate.(id = 175)
base <- vroom::vroom(here::here('output', region, 'base_ess_sz.csv')) %>% 
  mutate.(id = 'base')


# length data 
bind_rows.(s50, s75, s100, s125, s150, s175) %>% 
  left_join.(spec) %>% 
    mutate.(id = factor(id, c('50', '75', '100', '125', '150', '175'))) -> df

df<- df %>% filter(species!="Pacific ocean perch")

samples <- factor(c('50', '75', '100', '125', '150', '175'))
base_plots <- base %>%
  left_join.(spec) %>% 
  filter(species!="Pacific ocean perch") %>%
  uncount(6) %>%
  mutate(id = rep(samples, 54000)) %>%
  tidytable::mutate.(ess = replace(ess, ess == "ess_f", "Female"),
                     ess = replace(ess, ess == "ess_m", "Male"),
                     ess = replace(ess, ess == "ess_t", "Total"))
  
  
# plots ----

#years 2017-2019
# ess boxplot ----
df %>% filter(year %in% 2017:2019) %>%
  tidytable::mutate.(ess = replace(ess, ess == "ess_f", "Female"),
                     ess = replace(ess, ess == "ess_m", "Male"),
                     ess = replace(ess, ess == "ess_t", "Total")) %>% 
  ggplot(aes(id, value, fill = factor(ess))) + 
    geom_boxplot(outlier.size = 0, alpha = 0.7, outlier.shape = NA) +
    geom_boxplot(data=base_plots, aes(fill=NULL, color=factor(ess)), outlier.size = 0, alpha = 0, outlier.shape = NA, size=.4) +
    coord_cartesian(ylim = c(0, 5000)) +
    facet_wrap(~species, scales = 'free') + 
    scico::scale_fill_scico_d("Sex", palette = 'roma') +
    ylab('Length composition effective sample size\n') +
    xlab("\nSubsample size case") -> ess_bp
png("ess_length_bs1719_alt.png",width=13,height=10,units="in",res=300)
print(ess_bp)
dev.off()

