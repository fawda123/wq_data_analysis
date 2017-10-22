library(readxl)
library(tidyverse)
library(lubridate)

# prepping SLE data
sl_dat <- read_excel('data/SE03_Chl_WQ.xlsx') %>% 
  mutate(
    yr = year(Date), 
    Date = as.Date(Date)
  ) %>% 
  rename(
    chl = `CHLOROPHYLL-A`,
    sal = SALINITY,
    tn = `KJELDAHL NITROGEN, TOTAL`,
    tss = `TOTAL SUSPENDED SOLIDS`,
    nh = `AMMONIA-N`
  ) %>% 
  filter(yr < 2015 & yr > 1990) %>% 
  select(Date, chl, sal, nh, tn, tss)

save(sl_dat, file = 'data/sl_dat.RData', compress = 'xz')

# prepping SLE flow data
sl_fldat <- read_excel('data/SLE_freshwaterFlow_salinity_1989to2015.xlsx') %>% 
  mutate(
    Date = as.Date(Date),
    q = `Flow (cfs)`, 
    q = q * (0.3048^3) # cf/s to m3/s
  ) %>% 
  select(Date, q)

save(sl_fldat, file = 'data/sl_fldat.RData', compress = 'xz')

