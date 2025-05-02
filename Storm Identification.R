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

#Will need sub dataframes for each pond year pair
s22_level <- level %>% 
  filter(pond == 'Strickers' & year == '2022' & !is.na(maxdepth_m)) 

s23_level <- level %>% 
  filter(pond == 'Strickers' & year == '2023'& !is.na(maxdepth_m))

s24_level <- level %>% 
  filter(pond == 'Strickers' & year == '2024'& !is.na(maxdepth_m)) 

t23_level <- level %>% 
  filter(pond == 'Tiedemans' & year == '2023'& !is.na(maxdepth_m)) 

t24_level <- level %>% 
  filter(pond == 'Tiedemans' & year == '2024'& !is.na(maxdepth_m)) 

#Make level dataframe without Tiedeman's pump periods included
level_notied <- level %>% filter(pond != 'Tiedemans')
level_tied_nopump <- level %>% filter(pond == 'Tiedemans')
for(i in 1:nrow(pump)){
  level_tied_nopump <- level_tied_nopump %>% 
    filter(!(datetime >= pump$pumpstart[i] & datetime <= pump$pumpend[i]))
}
level_nopump <- rbind(level_notied, level_tied_nopump)

#Precip vs Water Level Change Relationship - run to get bounce for each storm#####

rise22_comb <- risinglimbs_comb %>% filter(year == '2022' & is.na(exclude))

levelrise22_comb <- level %>% 
  filter(pond == 'Strickers') %>%
  filter(datetime %in% unique(rise22_comb$risestarttime) | datetime %in% unique(rise22_comb$riseendtime)) %>%
  select(datetime, maxdepth_m)

rise22_comb <- left_join(rise22_comb, levelrise22_comb, by = join_by(risestarttime == datetime)) %>%
  rename(startdepth = maxdepth_m)

rise22_comb <- left_join(rise22_comb, levelrise22_comb, by = join_by(riseendtime == datetime)) %>%
  rename(enddepth = maxdepth_m) %>%
  mutate(bounce_m = enddepth - startdepth,
         percchange = (enddepth-startdepth)/startdepth)

rise22_comb <- rise22_comb %>%
  mutate(bounce_m = ifelse(is.na(bounce_m), 0, bounce_m))

#rise22_nozeros <- rise22 %>% filter(bounce_m >0)

ggplot() +
  #geom_smooth(data = rise22_nozeros, aes(x = precip_mm, y = bounce_m), method = 'lm', color = 'red') +
  geom_smooth(data = rise22_comb, aes(x = precip_mm, y = bounce_m), method = 'lm', color = 'red') +
  geom_point(data = rise22_comb, aes(x = precip_mm, y = bounce_m), color = 'red', size = 2) +
  #geom_smooth(data = rise22_sep, aes(x = precip_mm, y = bounce_m), method = 'lm') +
  #geom_point(data = rise22_sep, aes(x = precip_mm, y = bounce_m)) +
  theme_bw()
#These two lines completely overlap whether you exclude zeros or include them AND for both combined and separated storms, which is a good sign I can use a simple linear model to ID rain threshold vs using a lienar model with a breakpoint


#Strickers 2023 and 2024

rise_s234_comb <- risinglimbs_comb %>% filter(pond == 'Strickers' & year %in% c('2023','2024') & is.na(exclude))

levelrise_s234_comb <- level %>% 
  filter(pond == 'Strickers') %>%
  filter(datetime %in% unique(rise_s234_comb$risestarttime) | datetime %in% unique(rise_s234_comb$riseendtime)) %>%
  select(datetime, maxdepth_m)

rise_s234_comb <- left_join(rise_s234_comb, levelrise_s234_comb, by = join_by(risestarttime == datetime)) %>%
  rename(startdepth = maxdepth_m)

rise_s234_comb <- left_join(rise_s234_comb, levelrise_s234_comb, by = join_by(riseendtime == datetime)) %>%
  rename(enddepth = maxdepth_m) %>%
  mutate(bounce_m = enddepth - startdepth,
         percchange = (enddepth-startdepth)/startdepth)

rise_s234_comb <- rise_s234_comb %>%
  mutate(bounce_m = ifelse(is.na(bounce_m), 0, bounce_m))

rise_s_comb <- rbind(rise22_comb, rise_s234_comb)

ggplot() +
  geom_smooth(data = rise_s234_comb, aes(x = precip_mm, y = bounce_m), method = 'lm', color = 'black') +
  geom_point(data = rise_s234_comb, aes(x = precip_mm, y = bounce_m), size = 2) +
  geom_smooth(data = rise22_comb, aes(x = precip_mm, y = bounce_m), method = 'lm', color = 'red') +
  geom_point(data = rise22_comb, aes(x = precip_mm, y = bounce_m), color = 'red', size = 2) +
  #geom_smooth(data = rise_s_comb, aes(x = precip_mm, y = bounce_m), method = 'lm', color = 'blue') +
  theme_bw()

#Tiedemans 2023 & 2024
rise_t_comb <- risinglimbs_comb %>% filter(pond == 'Tiedemans' & is.na(exclude))

levelrise_t_comb <- level %>% 
  filter(pond == 'Tiedemans') %>%
  filter(datetime %in% unique(rise_t_comb$risestarttime) | datetime %in% unique(rise_t_comb$riseendtime)) %>%
  select(datetime, maxdepth_m)

rise_t_comb <- left_join(rise_t_comb, levelrise_t_comb, by = join_by(risestarttime == datetime)) %>%
  rename(startdepth = maxdepth_m)

rise_t_comb <- left_join(rise_t_comb, levelrise_t_comb, by = join_by(riseendtime == datetime)) %>%
  rename(enddepth = maxdepth_m) %>%
  mutate(bounce_m = enddepth - startdepth,
         percchange = (enddepth-startdepth)/startdepth)

rise_t_comb <- rise_t_comb %>%
  mutate(bounce_m = ifelse(is.na(bounce_m), 0, bounce_m))

ggplot() +
  geom_smooth(data = rise_t_comb, aes(x = precip_mm, y = bounce_m), method = 'lm', color = 'black') +
  geom_point(data = rise_t_comb, aes(x = precip_mm, y = bounce_m), size = 2) +
  geom_smooth(data = rise_s234_comb, aes(x = precip_mm, y = bounce_m), method = 'lm', color = 'blue') +
  geom_point(data = rise_s234_comb, aes(x = precip_mm, y = bounce_m), color = 'blue', size = 2) +
  #geom_hline(yintercept = s234_95perc, color = 'blue') +
  #geom_hline(yintercept = t_nopump_95perc, color = 'black') +
  theme_bw()

#Compare how much the ponds bounce for the same storms/rain events
rise_wide <- rbind(rise_s234_comb, rise_t_comb) %>%
  select(pond, raindoy, year, precip_mm, bounce_m) %>%
  pivot_wider(names_from = pond, values_from = bounce_m)

ggplot(rise_wide) +
  geom_abline() +
  geom_point(aes(x = Strickers, y = Tiedemans)) +
  theme_bw()
#Seems pretty equal when <0.05 m of bounce, but then Stricker's rises more when larger storms

#WL Change in overnight period and daily range calcs & visualization#####
#Started with chunk from 9p to 5a to capture full darkness hours, but realized this was not fully capturing the full increases being observed. Many increases began earlier in the day, and there were many instances on increases during the day and decreaes at night which was unexpected

#Overnight decreases
level22 <- level %>% filter(year == '2022')
levelsub22 <- level22 %>% filter(hour < 5 | hour >= 21)
levelsub <- level %>% filter(hour < 5 | hour >= 21) %>% filter(pond == 'Strickers')

slevel <- level %>% filter(pond == 'Strickers')

slevelsub23 <- s23_level %>% filter(hour < 5 | hour >= 21)
slevelsub24 <- s24_level %>% filter(hour < 5 | hour >= 21)

ggplot() +
  #Water Temp
  geom_point(data = level22, aes(x = datetime, y = watertemp_C), color = 'blue') +
  geom_point(data = level22, aes(x = datetime, y = maxdepth_m*30)) +
  geom_point(data = level22, aes(x = datetime, y = waterpres_psi), color = 'green') +
  geom_point(data = levelsub22, aes(x = datetime, y = maxdepth_m*30), color = 'red') +
  #Increases at night
  #scale_x_datetime(limits = c(as.POSIXct('2022-08-25'),as.POSIXct('2022-09-15'))) +
  scale_x_datetime(limits = c(as.POSIXct('2022-08-01'),as.POSIXct('2022-08-15'))) +
  #Decreases at night
  #scale_x_datetime(limits = c(as.POSIXct('2022-05-25'),as.POSIXct('2022-06-15'))) +
  #scale_y_continuous(limits = c(0.3,1.1)) +
  theme_bw()

ggplot() +
  geom_point(data = s23_level, aes(x = datetime, y = watertemp_C/30), color = 'blue') +
  geom_point(data = s23_level, aes(x = datetime, y = maxdepth_m)) +
  geom_point(data = slevelsub23, aes(x = datetime, y = maxdepth_m), color = 'red') +
  #scale_x_datetime(limits = c(as.POSIXct('2023-06-15'),as.POSIXct('2023-07-01'))) +
  scale_x_datetime(limits = c(as.POSIXct('2023-08-15'),as.POSIXct('2023-08-30'))) +
  #scale_y_continuous(limits = c(1,1.5)) +
  theme_bw()

ggplot() +
  geom_point(data = s24_level, aes(x = datetime, y = maxdepth_m)) +
  geom_point(data = slevelsub24, aes(x = datetime, y = maxdepth_m), color = 'red') +
  #scale_x_datetime(limits = c(as.POSIXct('2022-08-25'),as.POSIXct('2022-09-30'))) +
  #scale_y_continuous(limits = c(1,1.5)) +
  theme_bw()

ggplot() +
  geom_point(data = slevel, aes(x = dayfrac, y = maxdepth_m)) +
  geom_point(data = levelsub, aes(x = dayfrac, y = maxdepth_m), color = 'red') +
  facet_wrap(facets = 'year', ncol = 1) +
  #scale_y_continuous(limits = c(0.9,0.95)) +
  theme_bw()

ggplot() +
  geom_point(data = slevel, aes(x = datetime, y = maxdepth_m)) +
  geom_point(data = levelsub, aes(x = datetime, y = maxdepth_m), color = 'red') +
  scale_x_datetime(limits = c(as.POSIXct('2024-04-01'),as.POSIXct('2024-06-01'))) +
  #scale_y_continuous(limits = c(1,1.5)) +
  theme_bw()

#what about a daily range
dailyrange <- level %>%
  group_by(pond, year, raindoy) %>%
  summarize(minwl = min(maxdepth_m, na.rm = T),
            minwl_time = datetime[which.min(maxdepth_m)],
            maxwl = max(maxdepth_m, na.rm = T),
            maxwl_time = datetime[which.max(maxdepth_m)]) %>%
  mutate(wlrange = maxwl - minwl,
         timediff = maxwl_time - minwl_time)

dailyrange12 <- level %>%
  group_by(pond, year, doy) %>%
  summarize(minwl = min(maxdepth_m, na.rm = T),
            minwl_time = datetime[which.min(maxdepth_m)],
            maxwl = max(maxdepth_m, na.rm = T),
            maxwl_time = datetime[which.max(maxdepth_m)]) %>%
  mutate(wlrange = maxwl - minwl,
         timediff = maxwl_time - minwl_time)

#daily ranges with pump days excluded
dailyrange_nopump <- level_nopump %>%
  group_by(raindoy, year, pond) %>%
  summarize(minwl = min(maxdepth_m, na.rm = T),
            maxwl = max(maxdepth_m, na.rm = T)) %>%
  mutate(wlrange = maxwl - minwl)

dailymaxtimes <- level %>%
  group_by(raindoy, year, pond) %>%
  filter(maxdepth_m == max(maxdepth_m, na.rm = T) | maxdepth_m == min(maxdepth_m, na.rm =T)) %>%
  filter(pond == 'Strickers')

dailymaxtimes12 <- level %>%
  group_by(doy, year, pond) %>%
  filter(maxdepth_m == max(maxdepth_m, na.rm = T) | maxdepth_m == min(maxdepth_m, na.rm =T)) %>%
  filter(pond == 'Strickers')

#get 8am lines
eight <- level %>% filter(pond == 'Strickers', hour == '8', minute == '0')
twelve <- level %>% filter(pond == 'Strickers', hour == '0', minute == '0')

ggplot() +
  geom_point(data = slevel, aes(x = dayfrac, y = maxdepth_m)) +
  geom_point(data = dailymaxtimes, aes(x = dayfrac, y = maxdepth_m), color = 'red') +
  facet_wrap(facets = 'year', ncol = 1) +
  #scale_y_continuous(limits = c(0.9,0.95)) +
  theme_bw()

ggplot(dailyrange_nopump) +
  geom_point(aes(x = raindoy, y = wlrange, color = pond)) +
  facet_wrap(~year, ncol =1) +
  theme_bw()

ggplot() +
  geom_vline(data = eight, aes(xintercept = datetime), color= 'grey') +
  geom_point(data = slevel, aes(x = datetime, y = maxdepth_m)) +
  geom_point(data = dailymaxtimes, aes(x = datetime, y = maxdepth_m), color = 'red') +
  scale_x_datetime(limits = c(as.POSIXct('2023-08-01'),as.POSIXct('2023-09-01'))) +
  scale_y_continuous(limits = c(0.7,1.3)) +
  theme_bw()

#This looks like it is capturing it better, but still concerned about effect of large decreases

#What is the distribution of when it is capturing increases vs decreases
ggplot(dailyrange) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = raindoy, y= timediff, color = as.factor(year), shape = pond), size = 2) +
  theme_bw()
#No clear trend of when capturing increasing vs decreasing period

#Does it depend on water level?
ggplot(dailyrange) +
  geom_point(aes(x= maxwl, y = timediff, color = as.factor(year))) +
  facet_wrap(~pond, ncol =1)+
  theme_bw()
 #No clear pattern

dailyrange_s <- dailyrange %>% filter(pond == 'Strickers')
dailyrange_t <- dailyrange %>% filter(pond == 'Tiedemans')

#Get a pdf of each day and what periods its capturing using current daily range method
pdf('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Daily Level 8m to 8am.pdf', width = 6, height = 4)

for(i in 1:nrow(dailyrange)){
  sub <- level %>% filter(pond == dailyrange$pond[i] & raindoy == dailyrange$raindoy[i] & year == dailyrange$year[i])
  sub_points <- dailyrange[i,]
  
  levelplot <- ggplot() +
    geom_point(data = sub, aes(x = datetime, y = maxdepth_m)) +
    geom_point(data = sub_points, aes(x = minwl_time, y = minwl), color = 'red') +
    geom_point(data = sub_points, aes(x = maxwl_time, y = maxwl), color = 'red') +
    ggtitle(paste(dailyrange$pond[i],dailyrange$year[i])) +
    theme_bw()
  print(levelplot)
}
dev.off()


#Diel WL cycle quantification by night time increases Strickers 2022######
#Assign CoCoRaHS rain day to main level df
level <- level %>%
  mutate(raindoy = ifelse(hour <= 7, doy, doy+1))

#Loop to get slope of overnight increase in all days
level22 <- level %>% filter(year == '2022')
doys22 <- unique(level22$raindoy)

night22 <- data.frame(doy = numeric(),
                      year = numeric(),
                      pond = character(),
                      slope = numeric(),
                      wlchange_slope = numeric(),
                      wlchange_raw = numeric())

for(i in 1:length(doys22)){
  subset <- level22 %>% 
    filter(raindoy == doys22[i]) %>%
    filter(hour < 5 | hour >= 21)
  
  if(nrow(subset > 0)){
  wlstart <- subset$maxdepth_m[1]
  wlend <- subset$maxdepth_m[nrow(subset)]
  
  model <- summary(lm(maxdepth_m ~ dayfrac, data = subset))
  
  nightsub22 <- data.frame(doy = NA,
                           year = NA,
                           pond = NA,
                           slope = NA,
                           wlchange_slope = NA,
                           wlchange_raw = NA)
  
  nightsub22$doy = doys22[i]
  nightsub22$year = '2022'
  nightsub22$pond = 'Strickers'
  nightsub22$slope = model$coefficients[2,1]
  nightsub22$wlchange_slope = model$coefficients[2,1] * (1/3)
  nightsub22$wlchange_raw = wlend - wlstart
  }
  
  if(nrow(subset) == 0){
    nightsub22$doy = doys22[i]
    nightsub22$year = '2022'
    nightsub22$pond = 'Strickers'
    nightsub22$slope = NA
    nightsub22$wlchange_slope = NA
    nightsub22$wlchange_raw = NA
  }
  
  
  if(i == 1) {night22 = nightsub22}
  if(i > 1) {night22 = rbind(night22, nightsub22)}
}

#Compare slope derived vs raw calculation differences
ggplot(night22) +
  geom_abline(linetype = 'dashed') +
  geom_point(aes(x = wlchange_slope, y = wlchange_raw)) +
  theme_bw()
#Great, they are really similar! that is a big win actually, only gets pretty off at the very high end

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

#Extract 95th percentiles
#Summary Table?
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

dailyincr %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_density(aes(x = range),linewidth = 1) +
  geom_density(aes(x = incr), color = 'blue',linewidth = 1) +
  geom_vline(data = dayperiod_95perc_s, aes(xintercept = fullrange), linewidth = 1, linetype = 'dashed') +
  geom_vline(data = dayperiod_95perc_s, aes(xintercept = incr_99), linewidth = 1, color = 'blue', linetype = 'dashed') +
  facet_wrap(~ day_period, labeller = as_labeller(dayperiod_labs)) +
  ylab('Density') + xlab('Change in Water Level (m)') + ggtitle('Stricker\'s Pond') +
  theme_bw()
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Strickers Distributions.png', width = 6.5, height = 5, units = 'in', dpi = 300)

dailyincr %>% filter(pond == 'Tiedemans') %>% 
  ggplot() +
  geom_density(aes(x = range),linewidth = 1) +
  geom_density(aes(x = incr), color = 'blue',linewidth = 1) +
  geom_vline(data = dayperiod_95perc_t, aes(xintercept = fullrange), linewidth = 1, linetype = 'dashed') +
  geom_vline(data = dayperiod_95perc_t, aes(xintercept = incr_99), linewidth = 1, color = 'blue', linetype = 'dashed') +
  facet_wrap(~ day_period, labeller = as_labeller(dayperiod_labs)) +
  ylab('Density') + xlab('Change in Water Level (m)') + ggtitle('Tiedeman\'s Pond') +
  theme_bw()

ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Tiedemans Distributions.png', width = 6.5, height = 5, units = 'in', dpi = 300)

#Conclusions: Using daily increase rather than range because it makes more sesne conceptually, upping the percentile sthreshold to still get a decent recovery limb, so will use this moving forward. Slight changes to the timing of the day ranges changes cutoffs minimally, so using the raindoy period to match.

#Extract final thresholds from df
s_storm_threshold <- dayperiod_95perc_s$incr_99[dayperiod_95perc_s$day_period == 'raindoy']
t_storm_threshold <- dayperiod_95perc_t$incr_99[dayperiod_95perc_t$day_period == 'raindoy']


#Final selected distributions plot
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
ggsave('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Final Distributions.png', width = 6.5, height = 5, units = 'in', dpi = 300)  

#Distribution of Diel water level change by night time increases Strickers 2022######
ggplot(night22) +
  geom_density(aes(x = wlchange_raw)) +
  geom_vline(aes(xintercept = median(wlchange_raw, na.rm = T)), color = 'blue', size = 2) +
  geom_vline(aes(xintercept = quantile(wlchange_raw, c(0.95), na.rm = T)), linetype = 'dashed') +
  theme_bw()

#get numeric 95th percentile
quantile(night22$wlchange_raw, c(0.95), na.rm = T)


#What if we remove days with rain from the distribution?
raindoys22 <- unique(rise22$raindoy)
night22_norain <- night22 %>% filter(!(doy %in% raindoys22))

ggplot(night22_norain) +
  geom_density(aes(x = wlchange_raw)) +
  geom_vline(aes(xintercept = mean(wlchange_raw, na.rm = T)), color = 'blue', size = 2) +
  geom_vline(aes(xintercept = quantile(wlchange_raw, c(0.95), na.rm = T)), linetype = 'dashed') +
  theme_bw()

#get numeric 95th percentile
quantile(night22_norain$wlchange_raw, c(0.95), na.rm = T)
#Values don't change much, thats great

#Other percentiles
quantile(night22$wlchange_raw, c(0.75,0.9,0.95,0.975), na.rm = T)
quantile(night22_norain$wlchange_raw, c(0.75,0.9,0.95,0.975), na.rm = T)
#All of these are pretty similar, so will use all days for completeness


#Distribution of diel water level change by daily range######
s22dailyrange <- dailyrange %>% filter(pond == 'Strickers' & year == '2022')
s23dailyrange <- dailyrange %>% filter(pond == 'Strickers' & year == '2023')
t23dailyrange <- dailyrange %>% filter(pond == 'Tiedemans' & year == '2023')
s24dailyrange <- dailyrange %>% filter(pond == 'Strickers' & year == '2024')
t24dailyrange <- dailyrange %>% filter(pond == 'Tiedemans' & year == '2024')
s234dailyrange <- dailyrange %>% filter(pond == 'Strickers' & year %in% c('2023','2024'))
tdailyrange <- dailyrange %>% filter(pond == 'Tiedemans')
sdailyrange <- dailyrange %>% filter(pond == 'Strickers')
all234dailyrange <- dailyrange %>% filter(year %in% c('2023','2024'))
tdailyrange_nopump <- dailyrange_nopump %>% filter(pond == 'Tiedemans')

s234dailyrange_nofall <- s234dailyrange %>% filter(timediff >= -72000)

ggplot(tdailyrange_nopump) +
  geom_density(aes(x = wlrange)) +
  geom_vline(aes(xintercept = mean(wlrange, na.rm = T)), color = 'blue', linewidth = 2) +
  geom_vline(aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed') +
  ggtitle('')+
  theme_bw()

ggplot(dailyrange) +
  geom_point(aes(x = raindoy, y = wlrange)) +
  facet_grid(year~pond) +
  theme_bw()

#get numeric 95th percentile
s234_95perc <- quantile(s234dailyrange$wlrange, c(0.95), na.rm = T)
t_95perc <- quantile(tdailyrange$wlrange, c(0.95), na.rm = T)
t_nopump_95perc <- quantile(tdailyrange_nopump$wlrange, c(0.95), na.rm = T)


#What if we remove days with rain from the distribution?
raindoys22 <- unique(rise22_comb$raindoy)
s22dailyrange_norain <- s22dailyrange %>% filter(!(raindoy %in% raindoys22))
#This method worked for 2022 bc there was only one pond and year, but wont work to combine multiple years

rise23 <- risinglimbs_comb %>% filter(year == '2023')
rise23_multi <- rise23 %>% filter(!is.na(multiday)) %>%
  mutate(raindoy = raindoy + 1)
rise24 <- risinglimbs_comb %>% filter(year == '2024')
rise24_multi <- rise24 %>% filter(!is.na(multiday)) %>%
  mutate(raindoy = raindoy + 1)

raindoys23 <- unique(rise23$raindoy)
raindoys23_multi <- unique(rise23_multi$raindoy)
raindoys23 <- append(raindoys23,raindoys23_multi)
raindoys24 <- unique(rise24$raindoy)
raindoys24_multi <- unique(rise24_multi$raindoy)
raindoys24 <- append(raindoys24,raindoys24_multi)

s234dailyrange_norain <- s234dailyrange %>% 
  filter(!(raindoy %in% raindoys23 & year == '2023')) %>%
  filter(!(raindoy %in% raindoys24 & year == '2024'))

tdailyrange_norain <- tdailyrange %>% 
  filter(!(raindoy %in% raindoys23 & year == '2023')) %>%
  filter(!(raindoy %in% raindoys24 & year == '2024'))

tdailyrange_nopump_norain <- tdailyrange_nopump %>% 
  filter(!(raindoy %in% raindoys23 & year == '2023')) %>%
  filter(!(raindoy %in% raindoys24 & year == '2024'))

#Extract 95th percentile thresholds
s234_95_norain <- quantile(s234dailyrange_norain$wlrange, c(0.95), na.rm = T)
t_95_norain_nopump <- quantile(tdailyrange_nopump_norain$wlrange, c(0.95), na.rm = T)

#Plots
ggplot(s22dailyrange_norain) +
  geom_density(aes(x = wlrange)) +
  geom_vline(aes(xintercept = mean(wlrange, na.rm = T)), size = 2) +
  geom_vline(aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed') +
  geom_density(data = s22dailyrange, aes(x = wlrange), color = 'blue') +
  geom_vline(data = s22dailyrange, aes(xintercept = mean(wlrange, na.rm = T)), color = 'blue', size = 2) +
  geom_vline(data = s22dailyrange, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'blue') +
  theme_bw()

ggplot() +
  geom_density(data = s234dailyrange, aes(x = wlrange)) +
  geom_vline(data = s234dailyrange, aes(xintercept = mean(wlrange, na.rm = T)), size = 2) +
  geom_vline(data = s234dailyrange, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed') +
  geom_density(data = s234dailyrange_norain, aes(x = wlrange), color = 'blue') +
  geom_vline(data = s234dailyrange_norain, aes(xintercept = mean(wlrange, na.rm = T)), color = 'blue', size = 2) +
  geom_vline(data = s234dailyrange_norain, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'blue') +
  theme_bw()


ggplot() +
  geom_density(data = s234dailyrange, aes(x = wlrange)) +
  geom_vline(data = s234dailyrange, aes(xintercept = mean(wlrange, na.rm = T)), size = 2) +
  geom_vline(data = s234dailyrange, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed') +
  geom_density(data = tdailyrange_nopump, aes(x = wlrange), color = 'red') +
  geom_vline(data = tdailyrange_nopump, aes(xintercept = mean(wlrange, na.rm = T)), color = 'red', size = 1) +
  geom_vline(data = tdailyrange_nopump, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'red') +
  theme_bw()

ggplot() +
  geom_density(data = tdailyrange, aes(x = wlrange)) +
  geom_vline(data = tdailyrange, aes(xintercept = mean(wlrange, na.rm = T)), size = 1) +
  geom_vline(data = tdailyrange, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed') +
  geom_density(data = tdailyrange_norain, aes(x = wlrange), color = 'blue') +
  geom_vline(data = tdailyrange_norain, aes(xintercept = mean(wlrange, na.rm = T)), color = 'blue', size = 1) +
  geom_vline(data = tdailyrange_norain, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'blue') +
  geom_density(data = tdailyrange_nopump, aes(x = wlrange), color = 'red') +
  geom_vline(data = tdailyrange_nopump, aes(xintercept = mean(wlrange, na.rm = T)), color = 'red', size = 1) +
  geom_vline(data = tdailyrange_nopump, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'red') +
  geom_density(data = tdailyrange_nopump_norain, aes(x = wlrange), color = 'green') +
  geom_vline(data = tdailyrange_nopump_norain, aes(xintercept = mean(wlrange, na.rm = T)), color = 'green', size = 1) +
  geom_vline(data = tdailyrange_nopump_norain, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'green') +
  theme_bw()

ggplot() +
  geom_density(data = tdailyrange, aes(x = wlrange)) +
  geom_vline(data = tdailyrange, aes(xintercept = median(wlrange, na.rm = T)), size = 1) +
  geom_vline(data = tdailyrange, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed') +
  geom_density(data = tdailyrange_norain, aes(x = wlrange), color = 'blue') +
  geom_vline(data = tdailyrange_norain, aes(xintercept = median(wlrange, na.rm = T)), color = 'blue', size = 1) +
  geom_vline(data = tdailyrange_norain, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'blue') +
  geom_density(data = tdailyrange_nopump, aes(x = wlrange), color = 'red') +
  geom_vline(data = tdailyrange_nopump, aes(xintercept = median(wlrange, na.rm = T)), color = 'red', size = 1) +
  geom_vline(data = tdailyrange_nopump, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'red') +
  geom_density(data = tdailyrange_nopump_norain, aes(x = wlrange), color = 'green') +
  geom_vline(data = tdailyrange_nopump_norain, aes(xintercept = median(wlrange, na.rm = T)), color = 'green', size = 1) +
  geom_vline(data = tdailyrange_nopump_norain, aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed', color = 'green') +
  theme_bw()

#What if we only use days where the minimum depth is <1m (when the pond disconnects from the outlet)
ggplot(s22_basedoys) +
  geom_density(aes(x = wlrange)) +
  geom_vline(aes(xintercept = mean(wlrange, na.rm = T)), color = 'blue', size = 2) +
  geom_vline(aes(xintercept = quantile(wlrange, c(0.90), na.rm = T)), linetype = 'dashed') +
  theme_bw()

#get numeric 95th percentile
quantile(s22_basedoys$wlrange, c(0.95), na.rm = T)
#Number is not much different, but doesn't seem to be doing any better of a job of capturing true dynamics. 

#What if we only use days where the minimum depth is <1m AND no rain
s22_basedoys_norain <- s22_basedoys %>% filter(!(raindoy %in% raindoys22))

ggplot(s22_basedoys_norain) +
  geom_density(aes(x = wlrange)) +
  geom_vline(aes(xintercept = mean(wlrange, na.rm = T)), color = 'blue', size = 2) +
  geom_vline(aes(xintercept = quantile(wlrange, c(0.95), na.rm = T)), linetype = 'dashed') +
  theme_bw()

#get numeric 95th percentile
quantile(s22_basedoys$wlrange, c(0.95), na.rm = T)
#Number is not much different, but doesn't seem to be doing any better of a job of capturing true dynamics. 


#Identify days with normal WL distributions = not affected by precip Strickers 2022#####
ponds <- unique(level$pond)
years <- unique(level$year)
doys <- sort(unique(level$raindoy))

dailyskew <- data.frame(pond = as.character(),
                        year = as.character(),
                        doy = as.numeric(),
                        skewness = as.numeric(),
                        ad_pval = as.numeric())
pdf('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Daily Skew Histograms and Raw Data Plots.pdf', width = 6, height = 4)

for(i in 1:length(ponds)){
  for(j in 1:length(years)){
    for(k in 1:length(doys)){
      daysub <- level %>% filter(pond == ponds[i] & year == years[j] & raindoy == doys[k])
      
      dailyskew_sub <- data.frame(pond = NA,
                                  year = NA,
                                  doy = NA,
                                  skewness = NA,
                                  ad_pval = NA)
      
      dailyskew_sub$pond <- ponds[i]
      dailyskew_sub$year <- years[j]
      dailyskew_sub$doy <- doys[k]
      
      if(nrow(daysub)>0){
      
      dailyskew_sub$skewness <- skewness(daysub$maxdepth_m)
      dailyskew_sub$ad_pval <- ad.test(daysub$maxdepth_m)$p.value
      
      histplot <- ggplot(daysub) +
        geom_histogram(aes(x = maxdepth_m)) +
        ggtitle(paste(ponds[i],years[j],doys[k])) +
        theme_bw()
      print(histplot)
      rawplot <- ggplot(daysub) +
        geom_point(aes(x = dayfrac, y = maxdepth_m)) +
        ggtitle(paste(ponds[i],years[j],doys[k])) +
        theme_bw()
      print(rawplot)
      }
      
      if(i == 1 & j == 1 & k == 1){dailyskew <- dailyskew_sub}
      if(i > 1 | j>1 | k>1){dailyskew <- rbind(dailyskew,dailyskew_sub)}
    }
  }
}
dev.off()
dailyskew$ad_pval <- format(dailyskew$ad_pval, scientific = F, digits = 6)

write.csv(dailyskew, 'C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Intermediate Data Products/Daily water level distribution skewness and normality test results.csv', row.names = F)

ggplot(s24_level) +
  geom_point(aes(x = dayfrac, y = maxdepth_m)) +
  #scale_y_continuous(limits = c(0.9,1.1)) +
  #scale_x_continuous(limits = c(180,230)) +
  theme_bw()

#ID precip cutoffs by applying quantiles to WL change precip relationship - retired#####
#build little dataframe to store preds
s22_cutoffs <- data.frame(percentile = c(0.75,0.9,0.95,0.975), bounce = c(NA,NA,NA,NA), precip = c(NA,NA,NA,NA))
s22_cutoffs$bounce[1] <- quantile(night22$wlchange_raw, c(0.75), na.rm = T)
s22_cutoffs$bounce[2] <- quantile(night22$wlchange_raw, c(0.90), na.rm = T)
s22_cutoffs$bounce[3] <- quantile(night22$wlchange_raw, c(0.95), na.rm = T)
s22_cutoffs$bounce[4] <- quantile(night22$wlchange_raw, c(0.975), na.rm = T)

s22_wlprecip_mod <- summary(lm(bounce_m ~ precip_mm, data = rise22))
s22_wlprecip_m <- s22_wlprecip_mod$coefficients[2,1]
s22_wlprecip_b <- s22_wlprecip_mod$coefficients[1,1]

s22_cutoffs$precip <- (s22_cutoffs$bounce - s22_wlprecip_b)/s22_wlprecip_m

ggplot(rise22_comb) +
  geom_vline(data = s22_cutoffs, aes(xintercept = precip, color = percentile)) +
  geom_point(aes(x = precip_mm, y = bounce_m)) +
  theme_bw()

#Now for 2023 & 2024
#Strickers
s234_cutoffs <- data.frame(percentile = c(0.75,0.9,0.95,0.975), bounce = c(NA,NA,NA,NA), precip = c(NA,NA,NA,NA))
s234_cutoffs$bounce[1] <- quantile(s234dailyrange$wlrange, c(0.75), na.rm = T)
s234_cutoffs$bounce[2] <- quantile(s234dailyrange$wlrange, c(0.90), na.rm = T)
s234_cutoffs$bounce[3] <- quantile(s234dailyrange$wlrange, c(0.95), na.rm = T)
s234_cutoffs$bounce[4] <- quantile(s234dailyrange$wlrange, c(0.975), na.rm = T)

s234_wlprecip_mod <- summary(lm(bounce_m ~ precip_mm, data = rise_s234_comb))
s234_wlprecip_m <- s234_wlprecip_mod$coefficients[2,1]
s234_wlprecip_b <- s234_wlprecip_mod$coefficients[1,1]

s234_cutoffs$precip <- (s234_cutoffs$bounce - s234_wlprecip_b)/s234_wlprecip_m
s234_cutoffs$pond <- 'Strickers'

#Tiedemans
t_cutoffs <- data.frame(percentile = c(0.75,0.9,0.95,0.975), bounce = c(NA,NA,NA,NA), precip = c(NA,NA,NA,NA))
t_cutoffs$bounce[1] <- quantile(tdailyrange_nopump$wlrange, c(0.75), na.rm = T)
t_cutoffs$bounce[2] <- quantile(tdailyrange_nopump$wlrange, c(0.90), na.rm = T)
t_cutoffs$bounce[3] <- quantile(tdailyrange_nopump$wlrange, c(0.95), na.rm = T)
t_cutoffs$bounce[4] <- quantile(tdailyrange_nopump$wlrange, c(0.975), na.rm = T)

t_wlprecip_mod <- summary(lm(bounce_m ~ precip_mm, data = rise_t_comb))
t_wlprecip_m <- t_wlprecip_mod$coefficients[2,1]
t_wlprecip_b <- t_wlprecip_mod$coefficients[1,1]

t_cutoffs$precip <- (t_cutoffs$bounce - t_wlprecip_b)/t_wlprecip_m
t_cutoffs$pond <- 'Tiedemans'

cutoffs <- rbind(s234_cutoffs, t_cutoffs)

#ID Precip threshold from S2023/24 to be applied to Strickers 2022 - retired#######
s234_precipwl_mod <- summary(lm(bounce_m ~ precip_mm, data = rise_s234_comb))
s234_mod_m <- s234_precipwl_mod$coefficients[2,1]
s234_mod_b <- s234_precipwl_mod$coefficients[1,1]

s22_precip_threshold <- (s234_95_norain - s234_mod_b)/s234_mod_m

#Increases only
s234_precipwl_mod <- summary(lm(bounce_m ~ precip_mm, data = rise_s234_comb))
s234_mod_m <- s234_precipwl_mod$coefficients[2,1]
s234_mod_b <- s234_precipwl_mod$coefficients[1,1]

s22_precip_threshold_incr <- (s234_incr_95perc - s234_mod_b)/s234_mod_m

#Identify storms that meet waterlevel change/precip requirement######
s_95_storms <- rise_s_comb %>% filter(bounce_m >= s_storm_threshold)
t_95_storms <- rise_t_comb %>% filter(bounce_m >= t_storm_threshold)


storms95 <- rbind(s_95_storms, t_95_storms)
write.csv(storms95, 'C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Intermediate Data Products/Storms above Thresholds.csv', row.names = F)


#Plot Zone: Water Level Time series to identify rising limbs#####
level23 <- level %>% filter(year == '2023')

level %>% #filter(pond == 'Tiedemans') %>%
  ggplot() +
  geom_point(aes(x = datetime, y = maxdepth_m, color = pond)) +
  scale_x_datetime(limits = c(as.POSIXct('2024-07-02'),as.POSIXct('2024-07-02 12:00'))) +
  scale_y_continuous(limits = c(0.975,1.1)) +
  theme_bw()

ggplot(nws) +
  geom_point(aes(x = datetime, y = cumprecip_mm)) +
  scale_x_datetime(limits = c(as.POSIXct('2024-07-01'),as.POSIXct('2024-07-04'))) +
  scale_y_continuous(limits = c(400,500)) +
  theme_bw()

#Example 2022 level data
level %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_point(aes(x = datetime, y = depth_m)) +
  #geom_point(aes(x = datetime, y = airtemp_C)) +
  #geom_point(aes(x= datetime, y = (airpres_psi*0.703)), color = 'blue') +
  scale_x_datetime(limits = c(as.POSIXct('2022-05-10'),as.POSIXct('2022-07-01'))) +
  #scale_y_continuous(limits = c(0.6,1.2)) +
  theme_bw()

level %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_point(aes(x = dayfrac, y = depth_m, color = as.factor(year))) +
  geom_point(aes(x = dayfrac, y =watertemp_C, color = as.factor(year))) +
  #geom_point(aes(x = datetime, y = airtemp_C)) +
  geom_point(aes(x= dayfrac, y = (airpres_psi*0.703),  color = as.factor(year))) +
  #scale_x_datetime(limits = c(as.POSIXct('2022-05-01'),as.POSIXct('2022-10-08'))) +
  #scale_y_continuous(limits = c(0.6,1.2)) +
  theme_bw()

level %>% filter(pond == 'Strickers' & year == '2023') %>%
  ggplot() +
  #geom_point(aes(x = dayfrac, y = (airpres_psi-14)*15), color = 'red') +
  #geom_point(aes(x = dayfrac, y = (waterpres_psi-14)*15)) +
  geom_point(aes(x = dayfrac, y = (waterpres_psi-airpres_psi)*15)) +
  geom_point(aes(x = dayfrac, y =watertemp_C), color = 'blue') +
  #scale_x_datetime(limits = c(as.POSIXct('2022-05-01'),as.POSIXct('2022-10-08'))) +
  #scale_y_continuous(limits = c(0.2,0.3)) +
  #scale_x_continuous(limits = c(180,185)) +
  theme_bw()

level %>% filter(pond == 'Strickers' & year == '2022') %>%
  ggplot() +
  #geom_point(aes(x = dayfrac, y = (airpres_psi-14)*15), color = 'red') +
  #geom_point(aes(x = dayfrac, y = (waterpres_psi-14)*15)) +
  geom_point(aes(x = dayfrac, y = (waterpres_psi-airpres_psi)*15)) +
  geom_point(aes(x = dayfrac, y =watertemp_C), color = 'blue') +
  #scale_x_datetime(limits = c(as.POSIXct('2022-05-01'),as.POSIXct('2022-10-08'))) +
  #scale_y_continuous(limits = c(0.2,0.3)) +
  #scale_x_continuous(limits = c(180,185)) +
  theme_bw()

#Example 2023 level data
level %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_point(aes(x = datetime, y = maxdepth_m)) +
  scale_x_datetime(limits = c(as.POSIXct('2023-07-04'),as.POSIXct('2023-07-23'))) +
  #scale_y_continuous(limits = c(0.6,1.2)) +
  theme_bw()

#Example 2024 level data
level %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_point(aes(x = datetime, y = maxdepth_m)) +
  scale_x_datetime(limits = c(as.POSIXct('2024-07-04'),as.POSIXct('2024-07-23'))) +
  #scale_y_continuous(limits = c(0.6,1.2)) +
  theme_bw()

#Example day increasing data
level %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_point(aes(x = datetime, y = maxdepth_m)) +
  scale_x_datetime(limits = c(as.POSIXct('2024-08-20'),as.POSIXct('2024-08-30'))) +
  scale_y_continuous(limits = c(0.8,1.1)) +
  theme_bw()


peaks <- risinglimbs_comb %>% filter(!is.na(risestarttime))

pdf('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Water Level Peaks Investigation.pdf', width = 6, height = 4)

for(i in 1:nrow(peaks)){
  sub <- level %>% 
    filter(pond == peaks$pond[i] & datetime >= peaks$risestarttime[i] & datetime <= peaks$riseendtime[i])
  
  peakplot <- ggplot(sub) +
    geom_point(aes(x = datetime, y = maxdepth_m)) +
    theme_bw()
  print(peakplot)
}
dev.off()

level %>% filter(year == '2023' & pond == "Tiedemans") %>%
  ggplot() +
  geom_point(aes(x = datetime, y = watertemp_C), color = 'red') +
  geom_point(aes(x = datetime, y = maxdepth_m * 20)) +
  scale_x_datetime(limits = c(as.POSIXct('2023-05-01'),as.POSIXct('2023-07-10'))) +
  theme_bw()

level %>% filter(year == '2022') %>%
  ggplot() +
  geom_point(aes(x = datetime, y = watertemp_C), color = 'red') +
  geom_point(aes(x = datetime, y = maxdepth_m*30)) +
  scale_x_datetime(limits = c(as.POSIXct('2022-05-20'),as.POSIXct('2022-07-15'))) +
  scale_y_continuous(limits = c(15,35)) +
  theme_bw()

level %>% filter(datetime <= as.POSIXct('2022-07-01')) %>%
  ggplot() +
  geom_point(aes(x = waterpres_psi, y = watertemp_C)) +
  theme_bw()
level22_oksub <- level %>% filter(datetime <= as.POSIXct('2022-07-01'))

level %>% filter(year == '2022' & datetime >= as.POSIXct('2022-07-01')) %>%
  ggplot() +
  geom_point(aes(x = waterpres_psi, y = watertemp_C)) +
  theme_bw()
level22_badsub <- level %>% filter(year == '2022' & datetime >= as.POSIXct('2022-07-01'))


ggplot() +
  geom_point(data = level22_oksub, aes(y = maxdepth_m, x = watertemp_C), color = 'blue') +
  geom_point(data = level22_badsub, aes(y = maxdepth_m, x = watertemp_C)) +

  theme_bw()

level %>% filter(pond == 'Strickers') %>%
  ggplot() +
  geom_point(aes(x = datetime, y = maxdepth_m)) +
  scale_x_datetime(limits = c(as.POSIXct('2023-09-24 23:00'),as.POSIXct('2023-09-28'))) +
  scale_y_continuous(limits = c(0.9,1.1)) +
  theme_bw()

#Find storm timing: NWS Hourly Weather Data######
nws <- read.csv('./Meteo Data/NWS Middleton Airport Hourly Rainfall.csv') %>%
  rename(datetime = DATE, raininfo = AA1) %>%
  select(datetime, raininfo) %>%
  mutate(datetime = as.POSIXct(datetime, format = '%Y-%m-%dT%H:%M:%S') - hours(5)) %>%
  #Download is in weird format, this makes it readable
  separate_wider_delim(raininfo, delim = ',', names = c('period','rainfall_mm','condition','qualitycode'), too_few = 'align_start') %>%
  #Important note about this data: rainfall amounts are summed hourly, so the rainfall amount recorded is not necessarily for that 20 minute period but rather what has fallen so far that hour. So every 3 rows gets summed. To account for this, adding a minute column and only keeping observations at 55 minutes - the last of the hour that will have the summed value for the previous hour. This is what makes it truly hourly data.
  mutate(doy = yday(datetime),
         hour = hour(datetime), 
         year = year(datetime),
         minute = minute(datetime),
         rainfall_mm = ifelse(is.na(rainfall_mm), 0, as.numeric(rainfall_mm)/10)) %>% #rainfall data has a 10x scaling factor
  filter(minute == '55') %>%
  group_by(year) %>%
  mutate(cumprecip_mm = cumsum(rainfall_mm))

nwsrain <- nws %>% filter(rainfall_mm > 0)

ggplot(nws) +
  geom_point(aes(x = datetime, y = rainfall_mm)) +
  scale_x_datetime(limits = c(as.POSIXct('2022-08-22'), as.POSIXct('2022-09-01'))) +
  theme_bw()



#Saved for Later: Calculate Rolling Slopes#####

#Create the function to be applied to each subset of data - this will give us the slope of a basic linear model from whatever data is fed into it
rollingslope <- function(input){
  slope <- coef(lm(maxdepth_m ~ dayfrac, data = as.data.frame(input)))[2]
  return(slope)
}

#Now apply said function to the desired dataframe
#2022 sensors set at 15 minute intervals, so width = 5 for 1 hour span
s22_level$rollingslope <- rollapplyr(data = s22_level, width = 5, FUN = rollingslope, by.column = F, fill = NA)

#2023 sensors set at 10 minute intervals, so width = 7 for 1 hour span
s23_level$rollingslope <- rollapplyr(data = s23_level, width = 7, FUN = rollingslope, by.column = F, fill = NA)
t23_level$rollingslope <- rollapplyr(data = t23_level, width = 7, FUN = rollingslope, by.column = F, fill = NA)

#2024 sensors set at 5 minute intervals, so width = 13 for 1 hour span
s24_level$rollingslope <- rollapplyr(data = s24_level, width = 13, FUN = rollingslope, by.column = F, fill = NA)
t24_level$rollingslope <- rollapplyr(data = t24_level, width = 13, FUN = rollingslope, by.column = F, fill = NA)


#Saved for Later: Identify timesteps with slopes 2x > those observed in prior 24 hours#####

#First - get rid of slopes calculated from points with data gaps
s22_level <- s22_level %>% 
  mutate(slope_timespan = (dayfrac - lag(dayfrac, n = 4))*24) %>%
  filter(slope_timespan < 1.01) #Using 1.01 instead of 1 because dayfrac rounding has residual decimal point issues

s23_level <- s23_level %>% 
  mutate(slope_timespan = (dayfrac - lag(dayfrac, n = 6))*24) %>%
  filter(slope_timespan < 1.01) #Using 1.01 instead of 1 because dayfrac rounding has residual decimal point issues

t23_level <- t23_level %>% 
  mutate(slope_timespan = (dayfrac - lag(dayfrac, n = 6))*24) %>%
  filter(slope_timespan < 1.01) #Using 1.01 instead of 1 because dayfrac rounding has residual decimal point issues

s24_level <- s24_level %>% 
  mutate(slope_timespan = (dayfrac - lag(dayfrac, n = 12))*24) %>%
  filter(slope_timespan < 1.01) #Using 1.01 instead of 1 because dayfrac rounding has residual decimal point issues

t24_level <- t24_level %>% 
  mutate(slope_timespan = (dayfrac - lag(dayfrac, n = 12))*24) %>%
  filter(slope_timespan < 1.01) #Using 1.01 instead of 1 because dayfrac rounding has residual decimal point issues

#Combine into one large dataframe
levelslopes <- rbind(s22_level, s23_level, t23_level, s24_level, t24_level)

#Loop that will go through each row in levelslopes, ID all timepoints from the last 24 hours in the correct pond and find the maximum value, and then upend that value to a new column in levelslopes

i = 102
for(i in 1:nrow(levelslopes)){
  #Identify the timepoint of interest
  reftime <- levelslopes$dayfrac[i]
  #Identify the year of interest
  refyear <- levelslopes$year[i]
  #Identify the pond of interest
  refpond <- levelslopes$pond[i]
  
  #Filter to only capture timepoints in that pond and within 1 day of timepoint
  subset24hr <- levelslopes %>% filter(dayfrac < reftime &
                                       dayfrac >= (reftime - 1) &
                                       pond == refpond &
                                       year == refyear) 
  
  ##LEFT OFF HERE: 
  #Want to make sure there are at least 12 hours worth of data points to be considered, but the # of observations that is is different for each year...
  if(nrow(subset24hr) < 48){
    levelslopes$maxslope24hr[i] = NA
  }
  
  if(nrow(subset24hr) >= 48){
    maxslope <- max(subset24hr$rollingslope, na.rm = T)
    levelslopes$maxslope24hr[i] <- maxslope
  }
  }




ggplot(level) +
  geom_point(aes(x = dayfrac, y  = maxdepth_m, color = pond)) +
  facet_wrap(facets = 'year', nrow = 3) +
  scale_x_continuous(limits = c(200,225)) +
  theme_bw()


#Troubleshooting 2022 Water Level Data #####

#Saved for later thoughts: check 2023 and 2024 data to see if any of the depths of the tchain corresponded well with the water level logger. Try applying one to the 2022 logger and calculating depth using that. I dont think it will fully solve the problem since the signal is also very strong in the water presure data directly, but could help?