#Sondes & Ponds Revised Methods Figure Development
#Author: Jess Briggs
#Date created: 2025-03-24

library(tidyverse)
library(ggpubr)
library(cowplot)
library(scales)
library(ggeffects)
library(ggnewscale)
library(ggforce)

setwd('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Intermediate Data Products')

#To add in superscripts - use expression function

#Read in all required data######
level <- read.csv('./Corrected Max Water Levels Strickers and Tiedemans 2022 to 2024.csv') %>%
  filter(!(pond == 'Strickers' & year == '2022' & doy >= 182)) %>%
  filter(!(pond == 'Tiedemans' & year == '2023' & doy <= 165)) %>%
  group_by(pond,year,doy) %>%
  summarize(mndepth_m = mean(maxdepth_m, na.rm = T)) %>%
  ungroup() %>%
  complete(pond, year, doy, fill = list(mndepth_m = 0))

level_all <- read.csv('./Corrected Max Water Levels Strickers and Tiedemans 2022 to 2024.csv') %>%
  filter(!(pond == 'Strickers' & year == '2022' & doy >= 182)) %>%
  filter(!(pond == 'Tiedemans' & year == '2023' & doy <= 165)) %>%
  mutate(datetime = as.POSIXct(datetime))

precip <- read.csv('C:/Users/Jessica Briggs/Box/CAREER Grant/Data/Meteo Data/CoCoRaHS Data Clean.csv') %>%
  filter(is.na(multiday)) %>%
  #summarize into useful averages for Strickers and Tied
  filter(station %in% c('WI-DA-46','WI-DA-60')) %>% # Only closest stations
  select(date, precip_mm) %>%
  group_by(date) %>%
  summarize(precip_mm = mean(precip_mm, na.rm = T)) %>%
  mutate(date = as.POSIXct(date),
         doy = yday(date),
         year = year(date))

stormresp <- read.csv('./Quantitative Pigment Responses and Potential Explanatory Variables.csv') %>%
  mutate(stormdate = as.POSIXct(paste(stormdoy,year), format = '%j %Y'))

allstorms <-  read.csv('./Storms Above Thresholds.csv') %>%
  mutate(stormdate = as.POSIXct(paste(yday(risestarttime),year), format = '%j %Y'))

exo_night_storms <- read.csv('./EXO Nightly Averages with Storm Numbers.csv')

exo <- read.csv('C:/Users/Jessica Briggs/Box/CAREER Grant/Data/Sensors/EXO/EXO Full dataset.csv') %>% 
  mutate(datetime = as.POSIXct(datetime),
         year = year(datetime),
         doy = yday(datetime),
         dayfrac = yday(datetime) + (hour(datetime)/24) + (minute(datetime)/(60*24)))

exo_night <- exo %>% filter(hour(datetime) < 5) %>%
  group_by(pond, year, doy) %>%
  summarize(across(chl_rfu:pc_ugl, mean)) %>%
  ungroup() %>%
  mutate(date = as.POSIXct(paste(doy, year), format = '%j %Y'))

#Figure 2 - Hydrograph and Hyetographs######

#Plot all to get sense of ranges needed
ggplot() +
  geom_area(data = level, aes(x = doy, y = mndepth_m)) +
  facet_grid(pond ~ year) +
  theme_bw()


ggplot() +
  geom_col(data = precip, aes(x = doy, y = precip_mm/100), color = 'cornflowerblue') +
  scale_y_reverse() +
  facet_wrap(~year) +
  theme_bw()

#Strickers 2022
s22_stormresp <- stormresp %>% filter(pond == 'Strickers' & year == '2022')
s22_allstorms <- allstorms %>% filter(pond == 'Strickers' & year == '2022')

s22_level <- level_all %>% filter(pond == 'Strickers' & year =='2022') %>%
  ggplot() +
  geom_vline(data = s22_stormresp, aes(xintercept = stormdate), color = 'red') +
  geom_ribbon(aes(x = datetime, ymax = maxdepth_m, ymin = 0.7), fill = 'cornflowerblue') +
  scale_x_datetime(limits = c(as.POSIXct('2022-05-21'), as.POSIXct('2022-07-01')), expand = c(0,0), date_break = '2 weeks', labels = date_format('%b %d')) +
  scale_y_continuous(limits = c(0.7,2), expand = c(0,0)) +
  ylab('Water Depth (m)') + ggtitle('Stricker\'s 2022') +
  theme_cowplot(font_size = 11) +
  theme(panel.border = element_rect(color = 'black', fill = 'NA'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

s22_precip <- precip %>% filter(year == '2022') %>%
  ggplot() +
  geom_col(aes(x = date, y = precip_mm), color = 'black', fill = 'black') +
  scale_y_reverse(limits = c(150,0), position = 'right', expand = c(0,0)) +
  scale_x_datetime(limits = c(as.POSIXct('2022-05-21'), as.POSIXct('2022-07-01')), expand = c(0,0), date_break = '2 weeks', labels = date_format('%b %d')) +
  ylab('Daily Precipitation (mm)') + xlab('') + 
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

s22_aligned <- align_plots(s22_level, s22_precip, align = 'hv', axis = 'tblr')
s22_wlppt_plot <- ggdraw(s22_aligned[[1]]) + draw_plot(s22_aligned[[2]])
s22_wlppt_plot 

#Strickers 2023
s23_stormresp <- stormresp %>% filter(pond == 'Strickers' & year == '2023')
s23_allstorms <- allstorms %>% filter(pond == 'Strickers' & year == '2023')

s23_level <- level_all %>% filter(pond == 'Strickers' & year =='2023') %>%
  ggplot() +
  geom_vline(data = s23_stormresp, aes(xintercept = stormdate), linewidth = 0.5, color = 'red') +
  geom_ribbon(aes(x = datetime, ymax = maxdepth_m, ymin = 0.7), fill = 'cornflowerblue') +
  scale_x_datetime(limits = c(as.POSIXct('2023-06-17'), as.POSIXct('2023-10-06')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  scale_y_continuous(limits = c(0.7,2), expand = c(0,0)) +
  ylab('Water Depth (m)') + xlab('') + ggtitle('Stricker\'s 2023') +
  theme_cowplot(font_size = 11) +
  theme(panel.border = element_rect(color = 'black', fill = 'NA'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

s23_precip <- precip %>% filter(year == '2023') %>%
  ggplot() +
  geom_col(aes(x = date, y = precip_mm), color = 'black', fill = 'black') +
  scale_y_reverse(limits = c(150,0), position = 'right', expand = c(0,0)) +
  scale_x_datetime(limits = c(as.POSIXct('2023-06-17'), as.POSIXct('2023-10-06')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  ylab('Daily Precipitation (mm)') + xlab('') +
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

s23_aligned <- align_plots(s23_level, s23_precip, align = 'hv', axis = 'tblr')
s23_wlppt_plot <- ggdraw(s23_aligned[[1]]) + draw_plot(s23_aligned[[2]])
s23_wlppt_plot 

#Strickers 2024
s24_stormresp <- stormresp %>% filter(pond == 'Strickers' & year == '2024')
s24_allstorms <- allstorms %>% filter(pond == 'Strickers' & year == '2024')

s24_level <- level_all %>% filter(pond == 'Strickers' & year =='2024') %>%
  ggplot() +
  geom_vline(data = s24_stormresp, aes(xintercept = stormdate), color = 'red') +
  geom_vline(data = s24_allstorms, aes(xintercept = stormdate), color = 'red', linetype = 'dashed') +
  geom_ribbon(aes(x = datetime, ymax = maxdepth_m, ymin = 0.7), fill = 'cornflowerblue') +
  scale_x_datetime(limits = c(as.POSIXct('2024-03-30'), as.POSIXct('2024-11-10')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  scale_y_continuous(limits = c(0.7,2), expand = c(0,0)) +
  ylab('Water Depth (m)') + xlab('') + ggtitle('Stricker\'s 2024') +
  theme_cowplot(font_size = 11) +
  theme(panel.border = element_rect(color = 'black', fill = 'NA'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

s24_precip <- precip %>% filter(year == '2024') %>%
  ggplot() +
  geom_col(aes(x = date, y = precip_mm), color = 'black', fill = 'black') +
  scale_y_reverse(limits = c(150,0), position = 'right', expand = c(0,0)) +
  scale_x_datetime(limits = c(as.POSIXct('2024-03-30'), as.POSIXct('2024-11-10')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  ylab('Daily Precipitation (mm)') + xlab('') +
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

s24_aligned <- align_plots(s24_level, s24_precip, align = 'hv', axis = 'tblr')
s24_wlppt_plot <- ggdraw(s24_aligned[[1]]) + draw_plot(s24_aligned[[2]])
s24_wlppt_plot 

#Tiedemans 2023
t23_stormresp <- stormresp %>% filter(pond == 'Tiedemans' & year == '2023')
t23_allstorms <- allstorms %>% filter(pond == 'Tiedemans' & year == '2023')

t23_level <- level_all %>% filter(pond == 'Tiedemans' & year =='2023') %>%
  ggplot() +
  geom_vline(data = t23_stormresp, aes(xintercept = stormdate), color = 'red') +
  geom_vline(data = t23_allstorms, aes(xintercept = stormdate), color = 'red', linetype = 'dashed') +
  geom_ribbon(aes(x = datetime, ymax = maxdepth_m, ymin = 0.7), fill = 'cornflowerblue') +
  scale_x_datetime(limits = c(as.POSIXct('2023-06-17'), as.POSIXct('2023-10-06')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  scale_y_continuous(limits = c(0.7,2), expand = c(0,0)) +
  ylab('Water Depth (m)') + xlab('') + ggtitle('Tiedeman\'s 2023') +
  theme_cowplot(font_size = 11) +
  theme(panel.border = element_rect(color = 'black', fill = 'NA'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

t23_precip <- precip %>% filter(year == '2023') %>%
  ggplot() +
  geom_col(aes(x = date, y = precip_mm), color = 'black', fill = 'black') +
  scale_y_reverse(limits = c(150,0), position = 'right', expand = c(0,0)) +
  scale_x_datetime(limits = c(as.POSIXct('2023-06-17'), as.POSIXct('2023-10-06')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  ylab('Daily Precipitation (mm)') + xlab('') +
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

t23_aligned <- align_plots(t23_level, t23_precip, align = 'hv', axis = 'tblr')
t23_wlppt_plot <- ggdraw(t23_aligned[[1]]) + draw_plot(t23_aligned[[2]])
t23_wlppt_plot 

#Tiedemans 2024
t24_stormresp <- stormresp %>% filter(pond == 'Tiedemans' & year == '2024')
t24_allstorms <- allstorms %>% filter(pond == 'Tiedemans' & year == '2024')

t24_level <- level_all %>% filter(pond == 'Tiedemans' & year =='2024') %>%
  ggplot() +
  geom_vline(data = t24_stormresp, aes(xintercept = stormdate),  color = 'red') +
  geom_vline(data = t24_allstorms, aes(xintercept = stormdate), color = 'red', linetype = 'dashed') +
  geom_ribbon(aes(x = datetime, ymax = maxdepth_m, ymin = 0.7), fill = 'cornflowerblue') +
  scale_x_datetime(limits = c(as.POSIXct('2024-03-30'), as.POSIXct('2024-11-10')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  scale_y_continuous(limits = c(0.7,2), expand = c(0,0)) +
  ylab('Water Depth (m)') + xlab('') + ggtitle('Tiedeman\'s 2024') +
  theme_cowplot(font_size = 11) +
  theme(panel.border = element_rect(color = 'black', fill = 'NA'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

t24_precip <- precip %>% filter(year == '2024') %>%
  ggplot() +
  geom_col(aes(x = date, y = precip_mm), color = 'black', fill = 'black') +
  scale_y_reverse(limits = c(150,0), position = 'right', expand = c(0,0)) +
  scale_x_datetime(limits = c(as.POSIXct('2024-03-30'), as.POSIXct('2024-11-10')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  ylab('Daily Precipitation (mm)') + xlab('') +
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

t24_aligned <- align_plots(t24_level, t24_precip, align = 'hv', axis = 'tblr')
t24_wlppt_plot <- ggdraw(t24_aligned[[1]]) + draw_plot(t24_aligned[[2]])
t24_wlppt_plot 

#Combine plots into one pane
#Create empty for fill
fill <- ggplot() +theme_minimal()

ggarrange(s22_wlppt_plot, fill, s23_wlppt_plot, t23_wlppt_plot,s24_wlppt_plot, t24_wlppt_plot, nrow = 3, ncol = 2)
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Hydrograph and Hyetographs.pdf', width = 6.5, height = 6, units = 'in', dpi = 300)



#Figure 2 - Chlorophyll time series######

#2022 Strickers
schl22 <- exo_night %>% filter(pond == 'Strickers', year == '2022') %>%
  complete(doy = full_seq(doy, period = 1), fill = list(chl_ugl = 0, pc_ugl = 0)) %>% 
  mutate(year = '2022',
         date = as.POSIXct(paste(doy, year), format = '%j %Y')) %>%
  ggplot() +
  geom_vline(data = s22_stormresp, aes(xintercept = stormdate), ) +
  geom_vline(data = s22_allstorms, aes(xintercept = stormdate), linetype = 'dashed') +
  geom_area(aes(x = date, y = chl_ugl), fill = '#74C676') +
  scale_x_datetime(limits = c(as.POSIXct('2022-05-21'), as.POSIXct('2022-07-01')), expand = c(0,0)) +
  scale_y_continuous(limits =c(0,400), expand = c(0,0)) +
  ylab('Chlorophyll (\u00b5g/L)') +
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        panel.border = element_rect(color = 'black', fill = 'NA'))
schl22

#2023 Strickers
schl23 <- exo_night %>% filter(pond == 'Strickers', year == '2023') %>%
  complete(doy = full_seq(doy, period = 1), fill = list(chl_ugl = 0, pc_ugl = 0)) %>% 
  mutate(year = '2023',
         date = as.POSIXct(paste(doy, year), format = '%j %Y')) %>%
  ggplot() +
  geom_vline(data = s23_stormresp, aes(xintercept = stormdate), ) +
  geom_vline(data = s23_allstorms, aes(xintercept = stormdate), linetype = 'dashed') +
  geom_area(aes(x = date, y = chl_ugl), fill = '#74C676') +
  scale_x_datetime(limits = c(as.POSIXct('2023-06-17'), as.POSIXct('2023-10-06')), expand = c(0,0)) +
  scale_y_continuous(limits =c(0,400), expand = c(0,0)) +
  ylab('Chlorophyll (\u00b5g/L)') +
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        panel.border = element_rect(color = 'black', fill = 'NA'))
schl23

#2023 Tiedemans
tchl23 <- exo_night %>% filter(pond == 'Tiedemans', year == '2023') %>%
  complete(doy = full_seq(doy, period = 1), fill = list(chl_ugl = 0, pc_ugl = 0)) %>%
  mutate(year = '2023',
         date = as.POSIXct(paste(doy, year), format = '%j %Y')) %>%
  ggplot() +
  geom_vline(data = t23_stormresp, aes(xintercept = stormdate), ) +
  geom_vline(data = t23_allstorms, aes(xintercept = stormdate), linetype = 'dashed') +
  geom_area(aes(x = date, y = chl_ugl), fill = '#74C676') +
  scale_x_datetime(limits = c(as.POSIXct('2023-06-17'), as.POSIXct('2023-10-06')), expand = c(0,0)) +
  scale_y_continuous(limits =c(0,400), expand = c(0,0)) +
  ylab('Chlorophyll (\u00b5g/L)') +
  theme_cowplot(font_size = 11) +
  theme(axis.title = element_blank(),
        panel.border = element_rect(color = 'black', fill = 'NA'))
tchl23

#2024 Strickers
schl24 <- exo_night %>% filter(pond == 'Strickers', year == '2024') %>%
  complete(doy = full_seq(doy, period = 1), fill = list(chl_ugl = 0, pc_ugl = 0)) %>% 
  mutate(year = '2024',
         date = as.POSIXct(paste(doy, year), format = '%j %Y')) %>%
  ggplot() +
  geom_vline(data = s24_stormresp, aes(xintercept = stormdate), ) +
  geom_vline(data = s24_allstorms, aes(xintercept = stormdate), linetype = 'dashed') +
  geom_area(aes(x = date, y = chl_ugl), fill = '#74C676') +
  scale_x_datetime(limits = c(as.POSIXct('2024-03-30'), as.POSIXct('2024-11-10')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  scale_y_continuous(limits =c(0,400), expand = c(0,0)) +
  ylab('Chlorophyll (\u00b5g/L)') +
  theme_cowplot(font_size = 11) +
  theme(axis.title.x = element_blank(),
        panel.border = element_rect(color = 'black', fill = 'NA'))
schl24

#2024 Tiedemans
tchl24 <- exo_night %>% filter(pond == 'Tiedemans', year == '2024') %>%
  complete(doy = full_seq(doy, period = 1), fill = list(chl_ugl = 0, pc_ugl = 0)) %>%
  mutate(year = '2024',
         date = as.POSIXct(paste(doy, year), format = '%j %Y')) %>%
  ggplot() +
  geom_vline(data = t24_stormresp, aes(xintercept = stormdate), ) +
  geom_vline(data = t24_allstorms, aes(xintercept = stormdate), linetype = 'dashed') +
  geom_area(aes(x = date, y = chl_ugl), fill = '#74C676') +
  scale_x_datetime(limits = c(as.POSIXct('2024-03-30'), as.POSIXct('2024-11-10')), expand = c(0,0), date_break = '1 month', labels = date_format('%b')) +
  scale_y_continuous(limits =c(0,400), expand = c(0,0)) +
  ylab('Chlorophyll (\u00b5g/L)') +
  theme_cowplot(font_size = 11) +
  theme(axis.title = element_blank(),
        panel.border = element_rect(color = 'black', fill = 'NA'))
tchl24

ggarrange(schl22, fill, schl23, tchl23,schl24, tchl24, nrow = 3, ncol = 2, widths = c(0.52,0.48))
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Hydrograph and Hyetographs.pdf', width = 6.5, height = 6, units = 'in', dpi = 300)

#Figure 2 - Combine these plots#####

chl22 <- ggarrange(schl22,fill, fill,fill, nrow = 1, ncol = 4, widths = c(0.47,0.035,0.445,0.05))
wl22 <- ggarrange(s22_wlppt_plot, fill, nrow = 1, ncol = 2)

ts22 <- ggarrange(wl22, chl22, ncol = 1, nrow = 2, align = 'h', heights = c(0.66,0.33))
ts22

chl23 <- ggarrange(schl23,fill, tchl23,fill, nrow = 1, ncol = 4, widths = c(0.47,0.035,0.445,0.05))
wl23 <- ggarrange(s23_wlppt_plot, t23_wlppt_plot, nrow = 1, ncol = 2)

ts23 <- ggarrange(wl23, chl23, ncol = 1, nrow = 2, align = 'h', heights = c(0.66,0.33))
ts23

chl24 <- ggarrange(schl24,fill, tchl24,fill, nrow = 1, ncol = 4, widths = c(0.47,0.035,0.445,0.05))
wl24 <- ggarrange(s24_wlppt_plot, t24_wlppt_plot, nrow = 1, ncol = 2)

ts24 <- ggarrange(wl24, chl24, ncol = 1, nrow = 2, align = 'h', heights = c(0.66,0.33))

ggarrange(ts22, ts23, ts24, nrow = 3, ncol =1)
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Hydrograph and Hyetographs and Chlorophyll TS.pdf', width = 9.25, height = 10.75, units = 'in', dpi = 300)

#Figure 3 - All algal response timeseries#####

#Calculate median responses at each timestep
exo_night_storms <- exo_night_storms %>%
  mutate(deltachl_ugl = chl_ugl - pre_chl_ugl,
         deltapc_ugl = pc_ugl - pre_pc_ugl,
         percch_chl = deltachl_ugl/pre_chl_ugl *100,
         percch_pc = deltapc_ugl/pre_pc_ugl *100)

exo_flush <- exo_night_storms %>%
  filter(doy <= riseoverdoy +1)

exo_delay <- exo_night_storms %>%
  filter(doy >= riseoverdoy & timesincestormstart <= 10)

#Get end of rising limb data as pre data to align delayed responses
exo_night_endrise <- exo_night_storms %>% 
  filter(doy == riseoverdoy) %>%
  select(stormnum, chl_ugl, pc_ugl) %>%
  rename(endrise_chl_ugl = chl_ugl,
         endrise_pc_ugl = pc_ugl)

delayresp_plotinput <- left_join(exo_delay, exo_night_endrise) %>%
  mutate(dayssinceriseend = doy - riseoverdoy,
         delay_deltachl = (chl_ugl - endrise_chl_ugl) )%>%
  filter(timesincestormstart <= 10 & dayssinceriseend >= 0)

exo_night_storms_sum <- exo_night_storms %>%
  group_by(pond, timesincestormstart) %>%
  summarize(avg_deltachl = median(deltachl_ugl, na.rm = T),
            sd_deltachl = sd(deltachl_ugl, na.rm = T),
            avg_deltapc = median(deltapc_ugl, na.rm = T),
            sd_deltapc = sd(deltapc_ugl, na.rm = T),
            avg_percch_chl = median(percch_chl, na.rm = T),
            sd_percch_chl = sd(percch_chl, na.rm = T),
            avg_percch_pc = median(percch_pc, na.rm = T),
            sd_percch_pc = sd(percch_pc, na.rm = T),
            obsnum= n())

exo_flush_sum <- exo_flush %>%
  group_by(pond, timesincestormstart) %>%
  summarize(avg_deltachl = median(deltachl_ugl, na.rm = T),
            sd_deltachl = sd(deltachl_ugl, na.rm = T),
            avg_deltapc = median(deltapc_ugl, na.rm = T),
            sd_deltapc = sd(deltapc_ugl, na.rm = T),
            avg_percch_chl = median(percch_chl, na.rm = T),
            sd_percch_chl = sd(percch_chl, na.rm = T),
            avg_percch_pc = median(percch_pc, na.rm = T),
            sd_percch_pc = sd(percch_pc, na.rm = T),
            obsnum= n())

exo_delay_sum <- delayresp_plotinput %>%
  group_by(pond, timesincestormstart) %>%
  summarize(avg_deltachl = median(delay_deltachl, na.rm = T),
            sd_deltachl = sd(delay_deltachl, na.rm = T),
            obsnum= n())

#Add labeller object
pond_names <- c('Strickers' = 'Stricker\'s', 'Tiedemans' = 'Tiedeman\'s')

#Figure dev
fig3_full <- ggplot(exo_night_storms) +
  geom_ribbon(data = exo_night_storms_sum, aes(x = timesincestormstart, ymin = (avg_deltachl - sd_deltachl), ymax = (avg_deltachl + sd_deltachl)), alpha = 0.2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = timesincestormstart, y = chl_ugl - pre_chl_ugl, color = as.factor(year), group = stormnum), linewidth = 0.75, alpha = 0.65) +
  geom_line(data = exo_night_storms_sum, aes(x=timesincestormstart, y = avg_deltachl), linewidth = 1) +
  facet_wrap(~pond, labeller = as_labeller(pond_names)) +
  scale_x_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10)) +
  ylab(bquote('Change in Chlorophyll (\u03bcg'~L^-1~')')) + xlab('Days Since Storm Start') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  theme_bw()+
  theme(axis.title = element_text(size = 8))
        #legend.box.background = element_rect(color = 'black'),
        #legend.position = "inside",
        #legend.position.inside = c(0.9, 0.7))
fig3_full

fig3_flush <- ggplot(exo_flush) +
  #geom_ribbon(data = exo_flush_sum, aes(x = timesincestormstart, ymin = (avg_deltachl - sd_deltachl), ymax = (avg_deltachl + sd_deltachl)), alpha = 0.2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = timesincestormstart, y = chl_ugl - pre_chl_ugl, color = as.factor(year), group = stormnum), linewidth = 0.75, alpha = 0.65) +
  #geom_line(data = exo_flush_sum, aes(x=timesincestormstart, y = avg_deltachl), linewidth = 2) +
  facet_wrap(~pond, labeller = as_labeller(pond_names)) +
  scale_x_continuous(limits = c(0,5)) +
  ylab(bquote('Change in Chlorophyll (\u03bcg'~L^-1~')')) + xlab('Days Since Storm Start') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  theme_bw() +
  theme(axis.title = element_text(size = 8))
  #theme(legend.box.background = element_rect(color = 'black'),
        #legend.position = "inside",
        #legend.position.inside = c(0.9, 0.7))
fig3_flush

fig3_delay <- ggplot(delayresp_plotinput) +
  #geom_ribbon(data = exo_delay_sum, aes(x = timesincestormstart, ymin = (avg_deltachl - sd_deltachl), ymax = (avg_deltachl + sd_deltachl)), alpha = 0.2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = timesincestormstart, y = delay_deltachl, color = as.factor(year), group = stormnum), linewidth = 0.75, alpha = 0.65) +
  #geom_line(data = exo_delay_sum, aes(x=timesincestormstart, y = avg_deltachl), linewidth = 2) +
  facet_wrap(~pond, labeller = as_labeller(pond_names)) +
  scale_x_continuous(limits = c(0,10)) +
  ylab(bquote('Change in Chlorophyll (\u03bcg'~L^-1~')')) + xlab('Days Since Storm Start') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  theme_bw() +
  theme(axis.title = element_text(size = 8))
#theme(legend.box.background = element_rect(color = 'black'),
#legend.position = "inside",
#legend.position.inside = c(0.9, 0.7))
fig3_delay


#Boxplot of storm responses for 3rd column
boxplot_in <- stormresp %>%
  select(stormnum, pond, year, rise_chl_percch, post10_chl_percch, delay_chl_slope) %>%
  pivot_longer(cols = rise_chl_percch:delay_chl_slope, names_to = 'metric', values_to = 'value') %>%
  mutate(metric_type = ifelse(metric == 'delay_chl_slope', 'slope','percch'),
         metric = ifelse(metric == 'delay_chl_slope', 'Delayed Period',
                         ifelse(metric == 'post10_chl_percch', 'Full Period','Immediate Period')))

box_full <- boxplot_in %>% filter(metric == 'Full Period')
box_flush <- boxplot_in %>% filter(metric == 'Immediate Period')
box_delay <- boxplot_in %>% filter(metric == 'Delayed Period')

#Full boxplot with all metrics
ggplot(boxplot_in) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(x = metric, y = value), size = 1) +
  geom_point(aes(x = metric, y = value, color = as.factor(year), shape = pond), position = 'jitter', size = 3, alpha = 0.8) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  facet_row(~metric_type, space = 'free', scales = 'free', strip.position = "left", 
            labeller = as_labeller(c(percch = "Percent Change Response", slope = "Slope Response") )) +
  scale_shape_discrete(name = 'Pond')  +
  theme_bw() +
  theme(axis.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        strip.text = element_text(size = 10),
        legend.position = 'bottom')

#Only full response
box_full_plot <- ggplot(box_full) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(x = metric, y = value), size = 1) +
  geom_point(aes(x = metric, y = value, color = as.factor(year), shape = pond), position = 'jitter', size = 2, alpha = 0.8) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  facet_row(~metric_type, space = 'free', scales = 'free', strip.position = "left", 
            labeller = as_labeller(c(percch = "Percent Change in Chlorophyll \n during the Full Period", slope = "Slope Response") )) +
  scale_shape_discrete(name = 'Pond', labels = c('Stricker\'s', 'Tiedeman\'s'))  +
  theme_bw() +
  theme(axis.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        strip.text = element_text(size = 8),
        legend.position = 'bottom')
box_full_plot


ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Fig3 Boxplot Legend.png', width = 6.5, height = 6, units = 'in', dpi = 300)

box_flush_plot <- ggplot(box_flush) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(x = metric, y = value), size = 1) +
  geom_point(aes(x = metric, y = value, color = as.factor(year), shape = pond), position = 'jitter', size = 2, alpha = 0.8) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  facet_row(~metric_type, space = 'free', scales = 'free', strip.position = "left", 
            labeller = as_labeller(c(percch = "Percent Change in Chlorophyll \n during the Immediate Period", slope = "Slope Response") )) +
  scale_shape_discrete(name = 'Pond', labels = c('Stricker\'s', 'Tiedeman\'s'))  +
  theme_bw() +
  theme(axis.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        strip.text = element_text(size = 8),
        legend.position = 'bottom')
box_flush_plot

box_delay_plot <- ggplot(box_delay) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(x = metric, y = value), size = 1) +
  geom_point(aes(x = metric, y = value, color = as.factor(year), shape = pond), position = 'jitter', size = 2, alpha = 0.8) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  facet_row(~metric_type, space = 'free', scales = 'free', strip.position = "left", 
            labeller = label_bquote(rows = "Rate of Change in Chlorophyll \n during the Delayed Period (\u03bcg/L/day")) +
  scale_shape_discrete(name = 'Pond',  labels = c('Stricker\'s', 'Tiedeman\'s'))  +
  theme_bw() +
  theme(axis.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        strip.text = element_text(size = 8))
box_delay_plot

fig3_plotlabs <- c("A",'B','C','D','E','F')

ggarrange(fig3_flush, box_flush_plot, fig3_delay, box_delay_plot, fig3_full, box_full_plot, ncol = 2, nrow = 3, common.legend = T, labels =fig3_plotlabs, label.x = 0.03, widths = c(0.7, 0.3), legend = 'bottom')
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/All Chlorophyll Response Timeseries.pdf', width = 6.5, height = 7, units = 'in')

#Figure 4 - Flushing Response Explanatory Variables#####
#Input vars: Event rainfall, days since prev storm, cum precip
flushresp <- stormresp %>%
  select(stormnum, year, pond, rise_chl_percch, precip_mm, dayssinceprevstorm, cumprecip) %>%
  mutate(precip_std = as.numeric(scale(precip_mm)),
         dayssinceprevstorm_std = as.numeric(scale(dayssinceprevstorm)),
         cumprecip_std = as.numeric(scale(cumprecip)))

flushmod <- lm(rise_chl_percch ~ precip_std + dayssinceprevstorm_std + cumprecip_std, data = flushresp)
flushpreds_precip <- ggpredict(flushmod, terms = c('precip_std'))
flushpreds_time <- ggpredict(flushmod, terms = c('dayssinceprevstorm_std'))
flushpreds_cumprecip <- ggpredict(flushmod, terms = c('cumprecip_std'))

#Event rainfall panel
flush_precip_plot <- ggplot(flushresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_ribbon(data = flushpreds_precip, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_point(aes(x = precip_std, y = rise_chl_percch, color = as.factor(year), shape = pond), size = 3) +
  geom_line(data = flushpreds_precip, aes(x=x, y = predicted), linewidth = 1) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  scale_shape_discrete(name = 'Pond') +
  ylab("Percent Change in Chlorophyll \n during the Immediate Period") + xlab("Standardized Event Rainfall") +
  theme_bw()

#Days since previous storm panel
flush_time_plot <- ggplot(flushresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  #geom_ribbon(data = flushpreds_time, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_point(aes(x = dayssinceprevstorm_std, y = rise_chl_percch, color = as.factor(year), shape = pond), size = 3) +
  #geom_line(data = flushpreds_time, aes(x=x, y = predicted), linewidth = 1) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  scale_shape_discrete(name = 'Pond') +
  ylab("Percent Change in Chlorophyll \n during the Immediate Period") + xlab("Standardized Days Since Previous Storm") +
  theme_bw()

#Cumulative Precip panel
flush_cumprecip_plot <- ggplot(flushresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_ribbon(data = flushpreds_cumprecip, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_point(aes(x = cumprecip_std, y = rise_chl_percch, color = as.factor(year), shape = pond), size = 3) +
  geom_line(data = flushpreds_cumprecip, aes(x=x, y = predicted), linewidth = 1) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  scale_shape_discrete(name = 'Pond') +
  ylab("Percent Change in Chlorophyll \n during the Immediate Period") + xlab("Standardized Cumulative Rainfall") +
  theme_bw()

fig4_plotlabs <- c("A",'B','C')
ggarrange(flush_precip_plot, flush_time_plot, flush_cumprecip_plot, nrow = 2, ncol = 2, labels = fig4_plotlabs, common.legend = T, legend = 'right')

ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Flushing Response Predictors.pdf', width = 9, height = 6, units = 'in')

#Figure 5 - Delayed Response Explanatory Variables######
#Input vars: Event rainfall, days since prev storm, cum precip
delayresp <- stormresp %>%
  select(stormnum, year, pond, delay_chl_slope, rise_chl_percch, dayssinceprevstorm) %>%
  mutate(flush_std = as.numeric(scale(rise_chl_percch)),
         dayssinceprevstorm_std = as.numeric(scale(dayssinceprevstorm)))

delaymod <- lm(delay_chl_slope ~ flush_std * dayssinceprevstorm_std, data = delayresp)
delaypreds_flush <- ggpredict(delaymod, terms = c('flush_std'))
delaypreds_time <- ggpredict(delaymod, terms = c('dayssinceprevstorm_std'))
delaypreds_int <- predict_response(delaymod, terms = c('flush_std','dayssinceprevstorm_std [-1,0,1,2]'))

#Days since previous storm panel
delay_time_plot <- ggplot(delayresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  #geom_ribbon(data = delaypreds_time, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_point(aes(x = dayssinceprevstorm_std, y = delay_chl_slope, color = as.factor(year), shape = pond), size = 3) +
  #geom_line(data = delaypreds_time, aes(x=x, y = predicted), linewidth = 1) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  scale_shape_discrete(name = 'Pond') +
  ylab("Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period") + xlab("Standardized Days Since Previous Storm") +
  theme_bw()

#Flushing resposne panel
delay_flush_plot <- ggplot(delayresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_ribbon(data = delaypreds_flush, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_point(aes(x = flush_std, y = delay_chl_slope, color = as.factor(year), shape = pond), size = 3) +
  geom_line(data = delaypreds_flush, aes(x=x, y = predicted), linewidth = 1) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  scale_shape_discrete(name = 'Pond') +
  ylab("Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period") + xlab("Standardized Flushing Response") +
  theme_bw()

#Interaction Term Plot
#Actual model equation for sanity: y = intercept + (flush slope * flush) + (time slope * time) + (int slope * flush * time)
#Want to plot: Delayed resp vs flush @ various dayssincestorm
delay_int_plot <- ggplot(delayresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(x = flush_std, y = delay_chl_slope, color = dayssinceprevstorm_std), size = 3) +
  scale_color_distiller(palette = 'BrBG') +
  theme(legend.direction = ) +
  new_scale_color() +
  #geom_ribbon(data = delaypreds_int, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group, color = group), alpha = 0.1) +
  geom_line(data = delaypreds_int, aes(x=x, y = predicted, color = group), linewidth = 2) +
  scale_color_brewer(palette = 'BrBG', direction = -1, name = 'Standardized Days Since Previous Storm') +
  #scale_fill_brewer(palette = 'BrBG', direction = -1) +
  ylab("Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period") + xlab("Standardized Flushing Response") +
  theme_bw() 
delay_int_plot
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Delayed Response Interaction Term.jpeg', width = 5, height = 5, units = 'in')


fig5_plotlabs <- c("A",'B','C')
ggarrange(delay_time_plot, delay_flush_plot, delay_int_plot, nrow = 2, ncol = 2, labels = fig5_plotlabs, label.x = 0.1, common.legend = T, legend = 'right')
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Delayed Response Predictors.pdf', width = 10, height = 7, units = 'in')

#Figure 6 - Chemistry for Mechanism#####

doc <- ggplot(stormresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(y = delay_chl_slope, x = post_doc_mgl, color = as.factor(year), shape = pond), size = 4) +
  xlab('Post Storm DOC (mg/L)') + ylab('Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  theme_bw() +
  theme(legend.position = 'none')
doc

deltadoc <- ggplot(stormresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(y = delay_chl_slope, x = delta_doc, color = as.factor(year), shape = pond), size = 4) +
  xlab('Change in DOC (mg/L)') + ylab('Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  theme_bw() +
  theme(legend.position = 'none')
deltadoc

g440 <- ggplot(stormresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(y = delay_chl_slope, x = post_g440, color = as.factor(year), shape = pond), size = 4) +
  xlab('Post Storm g440') + ylab('Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  theme_bw() +
  theme(legend.position = 'none')
g440

delta_g440 <- ggplot(stormresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(y = delay_chl_slope, x = delta_g440, color = as.factor(year), shape = pond), size = 4) +
  xlab('Change in g440') + ylab('Delayed Response') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  theme_bw() +
  theme(legend.position = 'none')
delta_g440

srp <- ggplot(stormresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(y = delay_chl_slope, x = post_srp_ugl, color = as.factor(year), shape = pond), size = 4) +
  xlab('Post Storm SRP (\u00b5g/L)') + ylab('Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  scale_y_continuous(limits = c(-30,35)) +
  scale_x_continuous(limits = c(0,30)) +
  theme_bw() +
  theme(legend.position = 'none')
srp

delta_srp <- ggplot(stormresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(y = delay_chl_slope, x = delta_srp, color = as.factor(year), shape = pond), size = 4) +
  xlab('Change in SRP (\u00b5g/L)') + ylab('Delayed Response') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  scale_y_continuous(limits = c(-30,35)) +
  theme_bw() +
  theme(legend.position = 'none')
delta_srp

no3 <- ggplot(stormresp) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_point(aes(y = delay_chl_slope, x = post_no23_ugl, color = as.factor(year), shape = pond), size = 4) +
  xlab('Post Storm Nitrate (\u00b5g/L)') + ylab('Delayed Response') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  scale_y_continuous(limits = c(-30,35)) +
  theme_bw() +
  theme(legend.position = 'none')
no3

stormresp <- stormresp %>%
  mutate(mixing_cat = case_when(post_tempdiff > 1 & pre_tempdiff > 1 ~ "Remained stratified",
                                post_tempdiff < 1 & pre_tempdiff > 1 ~ "Strat pre, mix post",
                                post_tempdiff > 1 & pre_tempdiff < 1 ~ "Mix pre, strat post",
                                post_tempdiff < 1 & pre_tempdiff < 1 ~ "Remained mixed"),
         mixing_cat = factor(mixing_cat, levels = c("Mix pre, strat post", 
                                                    "Remained mixed", 
                                                    "Remained stratified", 
                                                    "Strat pre, mix post")))

strat <- ggplot(data = subset(stormresp, !is.na(mixing_cat))) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed', color = '#747474') +
  geom_boxplot(aes(x = mixing_cat, y = delay_chl_slope), size = 0.75) +
  geom_point(aes(x = mixing_cat, y = delay_chl_slope, color = as.factor(year), shape = pond), alpha = 0.7, size = 3) +
  ylab('Rate of Change in Chlorophyll (\u03bcg/L/day) \n during the Delayed Period') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77', '2023' = '#d95f02','2024' = '#7570b3')) +
  scale_shape_discrete(name = 'Pond') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = 'bottom') 
strat
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Stratification Patterns.png', width = 6, height = 8, units = 'in')

ggarrange(doc, srp, strat, ncol = 1, nrow = 3, labels = 'AUTO', heights = c(1,1,1.35), label.x = 0.1)
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Delayed Response Chemistry Relationships.pdf', width = 4, height = 10.5,  units = 'in')

#Table 1 Calculations#####
expvars_sum <- stormresp %>% 
  select(rise_chl_percch, pre_chl_ugl, precip_mm, risetime_hr, cumprecip, dayssinceprevstorm,daylight_hr, avgwatertemp_C) %>%
  summarize(across(1:8, list(mean = mean, sd = sd)))

test <- stormresp %>% select(stormdoy, year, pond, bounce_m)

#Figure 1 - Conceptual Figure Data Examples######
ggplot(exo_night_storms) +
  geom_line(aes(x = timesincestormstart, y = chl_ugl - pre_chl_ugl)) +
  facet_wrap(~stormnum) +
  theme_bw()

#Using Storm 27
stormstart <- stormresp$risestarttime[26]
riseend <- as.POSIXct('2024-09-23')
riseend_doy <- 267
level27 <- level_all %>% filter(pond == 'Strickers' & datetime >= stormstart & datetime <= as.POSIXct(stormstart) + days(10))
exo27 <- exo_night_storms %>% filter(stormnum == '27' & timesincestormstart <= 10) %>%
  mutate(datetime = as.POSIXct(paste(doy, year), format = '%j %Y'))
rise27 <- exo_night_storms %>% filter(stormnum == '27' & doy <= riseoverdoy) %>%
  mutate(datetime = as.POSIXct(paste(doy, year), format = '%j %Y'))
delay27 <- exo_night_storms %>% filter(stormnum == '27' & doy >= riseoverdoy & timesincestormstart <= 10) %>%
  mutate(datetime = as.POSIXct(paste(doy, year), format = '%j %Y'))

plot_breaks <- seq(from = as.POSIXct('2024-09-21'), length.out = 11, by = 'days')
plot_labs <- seq(from = 0, length.out = 11, by = 1)


exo27_plot <- ggplot(exo27) +
  geom_line(aes(x = datetime, y = chl_ugl), color = '#74C676', linewidth = 2) +
  ylab('Chlorophyll (\u00b5g/L)') + xlab('Days Since Storm Start') +
  scale_x_datetime(limits = c(as.POSIXct('2024-09-21'), as.POSIXct('2024-10-01')), breaks = plot_breaks, labels = plot_labs) +
  theme_cowplot() +
  theme(panel.border = element_rect(color = 'black')) 

level27_plot <- ggplot(level27) +
  geom_vline(xintercept = riseend, linetype = 'dashed', color = 'cornflowerblue', linewidth = 1) +
  geom_line(aes(x = datetime, y = maxdepth_m), color = 'cornflowerblue', linewidth = 2) +
  scale_y_continuous(position = 'right') +
  scale_x_datetime(limits = c(as.POSIXct('2024-09-21'), as.POSIXct('2024-10-01'))) +
  ylab('Water Depth (m)') +
  theme_cowplot() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
  
conc_aligned <- align_plots(level27_plot, exo27_plot, align = 'hv', axis = 'tblr')
st27_plot <- ggdraw(conc_aligned[[1]]) + draw_plot(conc_aligned[[2]])
st27_plot


exo27_plot_mods <- ggplot(exo27) +
  geom_vline(xintercept = riseend, linetype = 'dashed', color = 'darkgrey', linewidth = 1) +
  geom_line(aes(x = datetime, y = chl_ugl), color = 'darkgrey', linewidth = 2) +
  geom_smooth(data = rise27, aes(x = datetime, y = chl_ugl), method = 'lm', se = F, color = '#fc8d62', linewidth = 3) +
  geom_smooth(data = delay27, aes(x = datetime, y = chl_ugl), method = 'lm', se = F, color = '#66c2a5', linewidth = 3) +
  geom_smooth(data = exo27, aes(x = datetime, y = chl_ugl), method = 'lm', se = F, color = '#8da0cb', linewidth = 3) +
  ylab('Chlorophyll (\u00b5g/L)') + xlab('Days Since Storm Start') +
  scale_x_datetime(limits = c(as.POSIXct('2024-09-21'), as.POSIXct('2024-10-01')), breaks = plot_breaks, labels = plot_labs) +
  theme_cowplot() +
  theme(panel.border = element_rect(color = 'black')) 
exo27_plot_mods

plot_grid(fill,fill, st27_plot, exo27_plot_mods, align = 'v', axis = 'tb', rel_widths = c(1.25,1), rel_heights = c(1,2))

ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Conceptual Fig.pdf', width = 10.75, height = 5.625, units = 'in')
3.75+(0.5*3.75)

#Summary Stats of Responses######
stormresp_sum <- stormresp %>%
  select(rise_chl_percch, post10_chl_percch, delay_chl_slope) %>%
  summarize(across(rise_chl_percch:delay_chl_slope, c(med = median,sd = sd), na.rm = T))

            