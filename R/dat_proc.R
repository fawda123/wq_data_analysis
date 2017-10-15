library(readxl)
library(tidyverse)

# prepping SLE data
sl_dat <- read_excel('data/SE03_Chl_WQ.xlsx') %>% 
  select(Date, `CHLOROPHYLL-A`, SALINITY) %>% 
  rename(
    chl = `CHLOROPHYLL-A`,
    sal = SALINITY
  )

save(sl_dat, file = 'data/sl_dat.RData', compress = 'xz')
