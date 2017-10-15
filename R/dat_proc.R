library(readxl)
library(tidyverse)
library(lubridate)

# prepping SLE data
sl_dat <- read_excel('data/SE03_Chl_WQ.xlsx') %>% 
  mutate(
    yr = year(Date)
  ) %>% 
  rename(
    chl = `CHLOROPHYLL-A`,
    sal = SALINITY
  ) %>% 
  filter(yr < 2017 & yr > 1990) %>% 
  select(Date, chl, sal)

save(sl_dat, file = 'data/sl_dat.RData', compress = 'xz')
