library(tidyverse)
library(lubridate)
library(mgcv)  
library(plotly)
library(WRTDStidal)
library(gridExtra)
library(grid)
source('R/funcs.R')

# flow data, left moving window average of 30 days
data(sl_fldat)

fl_dat <- sl_fldat %>% 
  rename(date = Date) %>% 
  mutate(
    qsm = stats::filter(q, rep(1, 30)/30, sides = 1, method = 'convolution')
  )

# format the data to model
data(sl_dat)
sl_mod <- sl_dat %>%
  rename(date = Date) %>% 
  mutate(
    doy = yday(date), 
    dec_time = decimal_date(date), 
    yr = year(date),
    yr = factor(yr),
    mo = month(date, label = T)
  ) %>% 
  left_join(fl_dat, by = 'date') %>% 
  mutate(
    flo = log(qsm),
    chl = log(1 + chl)
  ) %>% 
  select(-q, -qsm, -sal)

# smooths to evaluate
smths <- c(
  "s(dec_time, bs = 'tp')",  
  "s(doy, bs = 'cc')",
  "te(dec_time, doy, bs = c('tp', 'cc'))"
)

# get all combinations of smoothers to model, one to many
frms <- list()
for(i in seq_along(smths)){
  
  frm <- combn(smths, i) %>%
    apply(2, function(x){
      paste(x, collapse = ' + ') %>% 
        paste('chl ~ ', .) %>% 
        formula
    }) 
  
  frms <- c(frms, frm)
  
}

# create models from smooth formula combinations
mods <- map(frms, function(frm){
  
  gam(frm, 
      knots = list(doy = c(1, 366)),
      data = sl_mod, 
      na.action = na.exclude
  )
  
})
names(mods) <- map(frms, ~ as.character(.x)[3] %>% gsub('\\"', "'", .))

# prediction data
pred_dat <- data.frame(
  dec_time = seq(min(sl_mod$dec_time), max(sl_mod$dec_time), length = 1000)
) %>% 
  mutate(
    date = date_decimal(dec_time), 
    date = as.Date(date),
    mo = month(date, label = TRUE), 
    doy = yday(date), 
    yr = year(date)
  ) %>% 
  left_join(., fl_dat[, c('date', 'qsm')]) %>% 
  mutate(flo = log(qsm)) %>% 
  select(-qsm)

# predictions
nms <- names(mods)
sl_res <- map(mods, function(x){
  pred_dat %>% 
    mutate(
      pred = predict(x, newdata = pred_dat)
    )
}) %>% 
  enframe('mods') %>% 
  unnest %>% 
  mutate(mods = factor(mods, levels = nms, 
                       labels = c('yr', 'seas', 'yr x seas', 'yr + seas', 
                                  'yr + yr x seas', 'seas + yr x seas', 'yr + seas + yr x seas')))

# plot
p <- ggplot(sl_res, aes(x = date)) + 
  geom_point(data = sl_mod, aes(y = chl)) + 
  geom_line(aes(y = pred, colour = mods), size = 1) + 
  theme_bw() + 
  scale_y_continuous(ylab, limits = c(1, 3.5)) +
  theme(
    legend.text.align = 1,
    legend.title = element_blank(), 
    axis.title.x = element_blank()
  )
pleg <- g_legend(p)
p <- p + theme(legend.position = 'none')

png('pres_figs/yrseas_sl.png', height = 2, width = 9, units = 'in', res = 300)
print(p)
dev.off()
png('pres_figs/yrseasleg_sl.png', height = 2.5, width = 2, units = 'in', res = 300)
grid.arrange(pleg)
dev.off()

ylab <- expression(paste("ln-chl (", italic(mu), "g ", L^-1, ")"))
p <- ggplot(sl_res, aes(x = doy, group = factor(yr), colour = yr)) + 
  geom_line(aes(y = pred), size = 1) + 
  theme_bw() + 
  theme(
    strip.text.x = element_text(size = 16), 
    legend.position = c(0.75, 0.1)
  ) + 
  scale_y_continuous(ylab) +
  scale_x_continuous('Day of year') +
  facet_wrap(~ mods, ncol = 2)

png('pres_figs/yrseasfac_sl.png', height = 8, width = 7, units = 'in', res = 300)
print(p)
dev.off()

data(sl_mods2)

# best model with only season, year
best1 <- map(mods, ~ summary(.x)$r.sq) %>% 
  unlist %>% 
  which.max %>% 
  mods[[.]]

# best model with season, year, flow
best2 <- map(mods2, ~ summary(.x)$r.sq) %>% 
  unlist %>% 
  which.max %>% 
  mods2[[.]] 

best <- list(best1 = best1, best2 = best2)

# predictions
sl_res2 <- map(best, function(x){
  pred_dat %>% 
    mutate(
      pred = predict(x, newdata = pred_dat)
    )
}) %>% 
  enframe('mods') %>% 
  unnest %>% 
  mutate(mods = factor(mods, 
                       levels = c('best1', 'best2'), 
                       labels = c('yr, seas', 'yr, seas, flow'))
  )

# plot
p <- ggplot(sl_res2, aes(x = date)) + 
  geom_point(data = sl_mod, aes(y = chl)) + 
  geom_line(aes(y = pred, colour = mods), size = 1) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(), 
    legend.text.align = 1, 
    axis.title.x = element_blank()
  ) + 
  scale_y_continuous(ylab, limits = c(1, 3.5))
pleg <- g_legend(p)
p <- p + theme(legend.position = 'none')

png('pres_figs/withflo_sl.png', height = 2, width = 9, units = 'in', res = 300)
print(p)
dev.off()
png('pres_figs/withfloleg_sl.png', height = 1, width = 2, units = 'in', res = 300)
grid.arrange(pleg)
dev.off()

ptheme <- theme(
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  strip.text.x = element_text(size = 12)
)
cols <- 'Spectral'
pb1 <- dynagam(best1, pred_dat, ncol = 1, col_vec = cols, month = c(9:12)) + 
  ptheme + 
  theme(legend.position = 'none')
pb2 <- dynagam(best2, pred_dat, ncol = 1, col_vec = cols, month = c(9:12)) + 
  ptheme + 
  theme(legend.position = 'right') + 
  guides(colour = guide_colourbar(barwidth = 1)) 
pleg <- g_legend(pb2)
pb2 <- pb2 + 
  theme(legend.position = 'none')

ylab <- expression(paste("ln-chl ( ", italic(mu), "g ", L^-1, ")"))

png('pres_figs/sldyna.png', height = 4, width = 5, units = 'in', res = 300)
grid.arrange(
  arrangeGrob(pb1, pb2, ncol = 2, bottom = 'lnQ', left = grid::textGrob(ylab, rot = 90)), 
  pleg,
  ncol = 2, 
  widths = c(1, 0.15)
)
dev.off()

strt <- best2$formula %>% 
  as.character

smths <- c(
  "s(nh, bs = 'tp')",  
  "s(tss, bs = 'tp')",
  "te(nh, tss, bs = c('tp', 'tp'))"
)

# get all combinations of smoothers to model, one to many
frmstab <- list()
frms <- list()
for(i in seq_along(smths)){
  
  # for the summary table  
  frmtab <- combn(smths, i) %>%
    apply(2, function(x){
      paste(x, collapse = ' + ')
    })
  
  # for the model
  frm <- sapply(frmtab, function(x){  
    paste('chl ~', strt[3], '+', x) %>% 
      formula
  })
  
  frmstab <- c(frmstab, frmtab)
  frms <- c(frms, frm)
  
}

# create models from smooth formula combinations
mods3 <- map(frms, function(frm){
  
  gam(frm, 
      knots = list(doy = c(1, 366)),
      data = sl_mod, 
      na.action = na.exclude
  )
  
})
names(mods3) <- paste0('mod', seq_along(mods3))

best3 <- map(mods3, ~ summary(.x)$r.sq) %>% 
  unlist %>% 
  which.max %>% 
  mods3[[.]] 

best <- list(best1 = best1, best2 = best2, best3 = best3)

sl_res3 <- map(best, function(x){
  sl_mod %>% 
    mutate(
      pred = predict(x)
    )
}) %>% 
  enframe('mods') %>% 
  unnest %>% 
  mutate(mods = factor(mods, levels = c('best1', 'best2', 'best3'), labels = c('yr, seas', 'plus flow', 'plus NH4, TSS')))

# plot
p <- ggplot(sl_res3, aes(x = date)) + 
  geom_point(data = sl_mod, aes(y = chl)) + 
  geom_line(aes(y = pred, colour = mods), size = 1) + 
  theme_bw() + 
  theme(
    legend.justification = 'top',
    legend.title = element_blank(), 
    axis.title.x = element_blank(), 
    legend.text = element_text(size = 12)
  ) + 
  scale_y_continuous(ylab, limits = c(1, 3.5))

png('pres_figs/withnuts_sl.png', height = 3.5, width = 8, units = 'in', res = 300)
p
dev.off()
