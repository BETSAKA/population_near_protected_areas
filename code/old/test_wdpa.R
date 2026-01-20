library(wdpar)
library(tidyverse)

wdpa <- wdpa_read("data/WDPA_Oct2025_Public.zip")

wdpa_bdi <- wdpa %>%
  filter(ISO3 == "BDI") %>%
  filter(STATUS %in% c('Designated', 'Established', 'Inscribed')) %>%
  filter(DESIG_ENG != 'UNESCO-MAB Biosphere Reserve') %>%
  filter(MARINE != '2') %>%
  mutate(type = case_when(
    IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III') ~ 'strict', 
    IUCN_CAT %in% c('IV', 'V', 'VI') ~ 'multiple_use',
    TRUE ~ 'not_assigned')
  
wdpa_bdi_2000 <- wdpa_bdi %>%
  filter(STATUS_YR <= 2000 | is.na(STATUS_YR)) %>%
  group_by(type) %>%
  summarize(n = n())

