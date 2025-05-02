#Storm Identification
#Author: Jess Briggs
#Date created: 2025-01-02

library(tidyverse)
library(lubridate)
library(zoo)
library(moments)
library(nortest)

#Read in Data#####
level <- read.csv('./Input Data Files/Corrected Max Water Levels Strickers and Tiedemans 2022 to 2024.csv') %>%
  #Add easy to interpret dayfrac for linear model outputs
  mutate(datetime = as.POSIXct(datetime),
         dayfrac = doy + (hour/24) + (minute/(24*60)),
         raindoy = ifelse(hour <= 7, doy, doy+1)) %>%
  filter(!(pond == 'Strickers' & year == '2022' & doy >= 182)) %>%
  filter(!(pond == 'Tiedemans' & year == '2023' & doy <= 165))

precip <- read.csv('./Input Data Files/CoCoRaHS Data Clean.csv') %>%
  filter(is.na(multiday)) %>%
  #summarize into useful averages for Strickers and Tied
  filter(station %in% c('WI-DA-46','WI-DA-60')) %>% # Only closest stations
  select(date, precip_mm) %>%
  group_by(date) %>%
  summarize(precip_mm = mean(precip_mm, na.rm = T)) %>%
  mutate(date = as.POSIXct(date),
         doy = yday(date),
         year = year(date))

risinglimbs_comb <- read.csv('./Input Data Files/Days with Recorded Precip CoCoRaHS Rising Limbs 2022 to 2024 with combined storms.csv') %>%
  mutate(date = as.POSIXct(date),
         risestarttime = as.POSIXct(risestarttime),
         riseendtime = as.POSIXct(riseendtime))

#Read in Tiedemans pump data
pump <- read.csv('./Input Data Files/Tiedemans Pump Operation Log.csv') %>%
  mutate(pumpstart = as.POSIXct(pumpstart),
         pumpend = as.POSIXct(pumpend))

#Make level dataframe without Tiedeman's pump periods included
level_notied <- level %>% filter(pond != 'Tiedemans')
level_tied_nopump <- level %>% filter(pond == 'Tiedemans')
for(i in 1:nrow(pump)){
  level_tied_nopump <- level_tied_nopump %>% 
    filter(!(datetime >= pump$pumpstart[i] & datetime <= pump$pumpend[i]))
}
level_nopump <- rbind(level_notied, level_tied_nopump)

#Calculate bounce for each storm#####

#Strickers
rise_s_comb <- risinglimbs_comb %>% filter(is.na(exclude) & pond == 'Strickers')

s_levelrise_comb <- level %>%
  filter(pond == 'Strickers') %>%
  filter(datetime %in% unique(rise_s_comb$risestarttime) | datetime %in% unique(rise_s_comb$riseendtime)) %>%
  select(datetime, maxdepth_m)

rise_s_comb <- left_join(rise_s_comb, s_levelrise_comb, by = join_by(risestarttime == datetime)) %>%
  rename(startdepth = maxdepth_m)

rise_s_comb <- left_join(rise_s_comb, s_levelrise_comb, by = join_by(riseendtime == datetime)) %>%
  rename(enddepth = maxdepth_m) %>%
  mutate(bounce_m = enddepth - startdepth,
         percchange = (enddepth-startdepth)/startdepth)

rise_s_comb <- rise_s_comb %>%
  mutate(bounce_m = ifelse(is.na(bounce_m), 0, bounce_m))

#Tiedemans
rise_t_comb <- risinglimbs_comb %>% filter(is.na(exclude) & pond == 'Tiedemans')

t_levelrise_comb <- level %>%
  filter(pond == 'Tiedemans') %>%
  filter(datetime %in% unique(rise_t_comb$risestarttime) | datetime %in% unique(rise_t_comb$riseendtime)) %>%
  select(datetime, maxdepth_m)

rise_t_comb <- left_join(rise_t_comb, t_levelrise_comb, by = join_by(risestarttime == datetime)) %>%
  rename(startdepth = maxdepth_m)

rise_t_comb <- left_join(rise_t_comb, t_levelrise_comb, by = join_by(riseendtime == datetime)) %>%
  rename(enddepth = maxdepth_m) %>%
  mutate(bounce_m = enddepth - startdepth,
         percchange = (enddepth-startdepth)/startdepth)

rise_t_comb <- rise_t_comb %>%
  mutate(bounce_m = ifelse(is.na(bounce_m), 0, bounce_m))

#Quantify daily increases######

#Add extra day assignments into level - easiest way to loop is by doy, so want to have columns that correspond to each period im considering a day
#As reminder, raindoy is 8a to 8a

level_nopump <- level_nopump %>% mutate(doy_4am = ifelse(hour <= 3, doy, doy+1),
                                        doy_6am = ifelse(hour <= 5, doy, doy+1),
                                        doy_10am = ifelse(hour <= 9, doy, doy+1),
                                        doy_12pm = ifelse(hour <= 11, doy, doy+1),
                                        doy_6pm = ifelse(hour >= 18, doy+1, doy),
                                        doy_12am = doy)

#Need to get rid of rain days that we know changed water level
risinglimbs_varyraindoy <- risinglimbs_comb %>% 
  mutate(start_hour = hour(risestarttime),
         startdoy_12am = yday(risestarttime),
         end_hour = hour(riseendtime),
         enddoy_12am = yday(riseendtime),
         startdoy_4am = ifelse(start_hour <= 3, startdoy_12am, startdoy_12am+1),
         startdoy_6am = ifelse(start_hour <= 5, startdoy_12am, startdoy_12am+1),
         startdoy_raindoy = ifelse(start_hour <= 7, startdoy_12am, startdoy_12am+1),
         startdoy_10am = ifelse(start_hour <= 9, startdoy_12am, startdoy_12am+1),
         startdoy_12pm = ifelse(start_hour <= 11, startdoy_12am, startdoy_12am+1),
         startdoy_6pm = ifelse(start_hour >= 18, startdoy_12am+1, startdoy_12am),
         enddoy_4am = ifelse(end_hour <= 3,enddoy_12am,enddoy_12am+1),
         enddoy_6am = ifelse(end_hour <= 5,enddoy_12am,enddoy_12am+1),
         enddoy_raindoy = ifelse(end_hour <= 7,enddoy_12am,enddoy_12am+1),
         enddoy_10am = ifelse(end_hour <= 9,enddoy_12am,enddoy_12am+1),
         enddoy_12pm = ifelse(end_hour <= 11,enddoy_12am,enddoy_12am+1),
         enddoy_6pm = ifelse(end_hour >= 18,enddoy_12am+1,enddoy_12am)) %>%
  filter(is.na(norise),is.na(exclude))

doys <- unique(level_nopump$doy)
years <- unique(level_nopump$year)
ponds <- unique(level_nopump$pond)
dayperiods <- c('doy_12am','raindoy','doy_4am','doy_6am','doy_10am','doy_12pm','doy_6pm')


for(i in 1:length(ponds)){
  for(j in 1:length(years)){
    for(k in 1:length(doys)){
      for(l in 1:length(dayperiods)){
        #Isolate rain days
        risinglimbs_sub <- risinglimbs_varyraindoy %>% 
          filter(year == years[j] & pond == ponds[i]) %>%
          select(ends_with(dayperiods[l])) %>%
          select(starts_with('start') | starts_with('end'))
        
        startraindoys <- unique(risinglimbs_sub[,1])
        endraindoys <- unique(risinglimbs_sub[,2])
        
        sub <- level_nopump %>% 
          rename(period = dayperiods[l]) %>%
          filter(pond == ponds[i] & year == years[j] & period == doys[k]) %>%
          filter(!(period %in% startraindoys) & !(period %in% endraindoys)) #Remove rain days from consideration
        
        if(nrow(sub) > 0){
          submin <- sub %>% slice_min(maxdepth_m)
          submax <- sub %>% slice_max(maxdepth_m)
          
          sub_postmin <- sub %>% filter(datetime > submin$datetime[1])
          
          dailyincr_sub <- data.frame(pond = NA,
                                      year = NA,
                                      doy = NA,
                                      day_period = NA,
                                      minwl_m = NA,
                                      minwl_time = NA,
                                      maxwl_m = NA,
                                      maxwl_time = NA,
                                      maxwl_post_m = NA,
                                      maxwl_post_time = NA,
                                      range = NA,
                                      incr = NA)
          
          dailyincr_sub$pond <- ponds[i]  
          dailyincr_sub$year <- years[j]
          dailyincr_sub$doy <- doys[k]
          dailyincr_sub$day_period <- dayperiods[l]
          dailyincr_sub$minwl_m <- submin$maxdepth_m[1]
          dailyincr_sub$minwl_time <- submin$datetime[1]
          dailyincr_sub$maxwl_m <- submax$maxdepth_m[1]
          dailyincr_sub$maxwl_time <- submax$datetime[1]
          dailyincr_sub$range <- dailyincr_sub$maxwl_m - dailyincr_sub$minwl_m
          
          if(nrow(sub_postmin)>0){
            submax_post <- sub_postmin %>% slice_max(maxdepth_m)
            
            dailyincr_sub$maxwl_post_m <- submax_post$maxdepth_m[1]
            dailyincr_sub$maxwl_post_time <- submax_post$datetime[1]
            dailyincr_sub$incr <- dailyincr_sub$maxwl_post_m - dailyincr_sub$minwl_m
          }
          
          if(nrow(sub_postmin) == 0){
            dailyincr_sub$maxwl_post_m <- NA
            dailyincr_sub$maxwl_post_time <- NA
            dailyincr_sub$incr <- 0
          }
          
          if(i==1 & j==1 & k==1 & l==1){dailyincr <- dailyincr_sub}
          if(i>1 | j>1 | k>1 | l>1){dailyincr <- rbind(dailyincr, dailyincr_sub)}
        }
      }
    }
  }
}

#Extract percentiles of water level increase#####
#Summary Table
dayperiod_95perc <- dailyincr %>%
  group_by(pond, day_period) %>%
  summarize(fullrange = quantile(range, c(0.95), na.rm = T),
            incr_95 = quantile(incr, c(0.95), na.rm = T),
            incr_975 = quantile(incr, c(0.975), na.rm = T),
            incr_98 = quantile(incr, c(0.98), na.rm = T),
            incr_99 = quantile(incr, c(0.99), na.rm = T))

dayperiod_95perc_s <- dayperiod_95perc %>% filter(pond == 'Strickers')
dayperiod_95perc_t <- dayperiod_95perc %>% filter(pond == 'Tiedemans')

n_s_incr <- dailyincr %>% filter(day_period == 'raindoy' & pond == 'Strickers')
n_t_incr <- dailyincr %>% filter(day_period == 'raindoy' & pond == 'Tiedemans')

dayperiod_labs <- c(doy_10am = '10:00',doy_12am = '00:00',doy_12pm = '12:00',doy_4am = '04:00',doy_6am = '06:00',doy_6pm = '18:00',raindoy = '08:00')

#Plots to compare periods and increase vs range
ggplot(dailyincr) +
  geom_density(aes(x = incr, color = day_period)) +
  facet_wrap(~pond, ncol =1) +
  theme_bw()

ggplot(dailyincr) +
  geom_density(aes(x = range, color = day_period)) +
  facet_wrap(~pond, ncol =1) +
  theme_bw()

ggplot(dailyincr) +
  geom_density(aes(x= range), linewidth = 2) +
  geom_density(aes(x = range, color = pond), linewidth = 1) +
  theme_bw()

#Supplemental Figure 1
dailyincr %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_density(aes(x = range),linewidth = 1) +
  geom_density(aes(x = incr), color = 'blue',linewidth = 1) +
  geom_vline(data = dayperiod_95perc_s, aes(xintercept = fullrange), linewidth = 1, linetype = 'dashed') +
  geom_vline(data = dayperiod_95perc_s, aes(xintercept = incr_99), linewidth = 1, color = 'blue', linetype = 'dashed') +
  facet_wrap(~ day_period, labeller = as_labeller(dayperiod_labs)) +
  ylab('Density') + xlab('Change in Water Level (m)') + ggtitle('Stricker\'s Pond') +
  theme_bw()
ggsave('./Figures/Supplemental Fig 1 - Strickers Distributions.png', width = 6.5, height = 5, units = 'in', dpi = 300)

#Supplemental Figure 2
dailyincr %>% filter(pond == 'Tiedemans') %>% 
  ggplot() +
  geom_density(aes(x = range),linewidth = 1) +
  geom_density(aes(x = incr), color = 'blue',linewidth = 1) +
  geom_vline(data = dayperiod_95perc_t, aes(xintercept = fullrange), linewidth = 1, linetype = 'dashed') +
  geom_vline(data = dayperiod_95perc_t, aes(xintercept = incr_99), linewidth = 1, color = 'blue', linetype = 'dashed') +
  facet_wrap(~ day_period, labeller = as_labeller(dayperiod_labs)) +
  ylab('Density') + xlab('Change in Water Level (m)') + ggtitle('Tiedeman\'s Pond') +
  theme_bw()

ggsave('./Figures/Supplemental Fig 2 - Tiedemans Distributions.png', width = 6.5, height = 5, units = 'in', dpi = 300)

#Conclusions: Using daily increase rather than range because it makes more sesne conceptually, upping the percentile sthreshold to still get a decent recovery limb, so will use this moving forward. Slight changes to the timing of the day ranges changes cutoffs minimally, so using the raindoy period to match.

#Extract final thresholds from df
s_storm_threshold <- dayperiod_95perc_s$incr_99[dayperiod_95perc_s$day_period == 'raindoy']
t_storm_threshold <- dayperiod_95perc_t$incr_99[dayperiod_95perc_t$day_period == 'raindoy']


#Final selected distributions plot - Supplemental Figure 3
dailyincr %>% filter(day_period == 'raindoy') %>%
  ggplot() +
  geom_density(aes(x = incr, color = pond), linewidth = 2) +
  geom_vline(xintercept = s_storm_threshold, linewidth = 2, linetype = 'dashed') +
  geom_vline(xintercept = t_storm_threshold, color = 'blue', linewidth = 2, linetype = 'dashed') +
  scale_color_manual(name = 'Pond',
                     breaks = c('Strickers','Tiedemans'),
                     values = c('Strickers'= 'black','Tiedemans' = 'blue')) +
  ylab('Density') + xlab('Daily Increase in Water Level (m)') +
  theme_bw()
ggsave('./Figures/Supplemental Fig 3 - Final Distributions.png', width = 6.5, height = 5, units = 'in', dpi = 300)  

#Identify storms that meet waterlevel change/precip requirement######
s_95_storms <- rise_s_comb %>% filter(bounce_m >= s_storm_threshold)
t_95_storms <- rise_t_comb %>% filter(bounce_m >= t_storm_threshold)


storms95 <- rbind(s_95_storms, t_95_storms)
write.csv(storms95, './Output and Intermediate Files/Storms above Thresholds.csv', row.names = F)


