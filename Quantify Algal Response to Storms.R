#Quantify Algal Response to Storms
#Author: Jess Briggs
#Date created: 2024-02-06

library(tidyverse)
library(slider)
#library(zoo)

setwd("C:/Users/Jessica Briggs/Box/CAREER Grant/Data")

#Read in Data#####
exo <- read.csv('./Sensors/EXO/EXO Full dataset.csv') %>% 
  mutate(datetime = as.POSIXct(datetime),
         year = year(datetime),
         doy = yday(datetime),
         dayfrac = yday(datetime) + (hour(datetime)/24) + (minute(datetime)/(60*24))) %>%
  #Using all 2 minute data points breaks the models below. Down sample in 2023 and 2024 to every 10 minutes
  filter(year(datetime) == '2022' | (year(datetime) != '2022' & minute(datetime) %in% c(00,10,20,30,40,50)))

#Hourly EXO
#exo_hour <- exo %>%
 # mutate(hour = hour(datetime)) %>%
  #group_by(pond, year, doy, hour) %>%
  #summarize(chl_rfu = mean(chl_rfu, na.rm = T),
  #          pc_rfu = mean(pc_rfu, na.rm = T),
   #         chl_ugl = mean(chl_ugl, na.rm = T),
    #        pc_ugl = mean(pc_ugl, na.rm = T)) %>%
  #mutate(dayfrac = doy + hour/24,
   #      datetime = as.POSIXct(paste(year,doy,hour), format = '%Y %j %H'))

#Daily EXO
#exo_day <- exo %>%
 # group_by(pond, year, doy) %>%
  #summarize(chl_rfu = mean(chl_rfu, na.rm = T),
   #         pc_rfu = mean(pc_rfu, na.rm = T),
    #        chl_ugl = mean(chl_ugl, na.rm = T),
     #       pc_ugl = mean(pc_ugl, na.rm = T))

#Storms from daily range
storms <- read.csv('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Intermediate Data Products/Storms above Thresholds.csv') %>%
  mutate(risestarttime = as.POSIXct(risestarttime),
         riseendtime = as.POSIXct(riseendtime),
         stormdoy = yday(risestarttime)) %>%
  group_by(pond, year) %>%
  mutate(nextstormstart = lead(risestarttime),
         nextstormstart = if_else(is.na(nextstormstart),risestarttime+days(30),nextstormstart),
         risetime = riseendtime - risestarttime) %>%
  ungroup()

#Storms from daily increases
#storms <- read.csv('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Intermediate Data Products/Storms above Thresholds increases only.csv') %>%
 # mutate(risestarttime = as.POSIXct(risestarttime),
  #       riseendtime = as.POSIXct(riseendtime),
  #      stormdoy = yday(risestarttime)) %>%
  #group_by(pond, year) %>%
 # mutate(nextstormstart = lead(risestarttime),
  #       nextstormstart = if_else(is.na(nextstormstart),risestarttime+days(30),nextstormstart),
  #       risetime = riseendtime - risestarttime) %>%
  #ungroup()


#Only nightly chlorophyll data - removing diel signal
exo_night <- exo %>% filter(hour(datetime) < 5) %>%
  group_by(pond, year, doy) %>%
  summarize(across(chl_rfu:pc_ugl, mean)) %>%
  ungroup()

#Visualize how nightly averages are different from daily averages
ggplot(exo_day) +
  geom_line(aes(x = doy, y = chl_ugl), color = 'green') +
  geom_line(data = exo_night, aes(x = doy, y = chl_ugl), color = 'darkgreen') +
  geom_line(aes(x = doy, y = pc_ugl), color = 'cornflowerblue') +
  geom_line(data = exo_night, aes(x = doy, y = pc_ugl), color = 'darkblue') +
  facet_grid(pond ~ year) +
  theme_bw()

cyanofluor <- read.csv('./Laboratory Data/Chem Masters/Ponds Chemistry Jan 2025.csv') %>%
  filter(pond %in% c('S','T') & site == '0') %>%
  mutate(pond = ifelse(pond == 'S', 'Strickers','Tiedemans'))
  

#Check cyanofluor and EXO following same patterns#####
pigments_check <- left_join(exo_night, cyanofluor, by = join_by(pond, year, doy))


ggplot(pigments_check) +
  geom_point(aes(x = chl_rfu, y = chl_cyanofluor_avg, color = pond)) +
  theme_bw()

ggplot(pigments_check) +
  geom_point(aes(x = pc_rfu, y = pc_cyanofluor_avg, color = pond)) +
  theme_bw()

#Calculate Pre-Storm Pigment Conditions - run to exclude storms without correct data ranges, butdont use these values#####
#setup dataframe with new columns to fill
storms <- storms %>%
  mutate(stormnum = as.numeric(NA),
         instant_time = as.POSIXct(NA),
         instant_chl_ugl = as.numeric(NA),
         instant_chl_rfu = as.numeric(NA),
         instant_pc_ugl = as.numeric(NA),
         instant_pc_rfu = as.numeric(NA),
         hour_chl_ugl = as.numeric(NA),
         hour_chl_rfu = as.numeric(NA),
         hour_pc_ugl = as.numeric(NA),
         hour_pc_rfu = as.numeric(NA),
         day_chl_ugl = as.numeric(NA),
         day_chl_rfu = as.numeric(NA),
         day_pc_ugl = as.numeric(NA),
         day_pc_rfu = as.numeric(NA))


for(i in 1:nrow(storms)){
  #Instantaneous pre-storm value: pre-storm chlorophyll = the chlorophyll at the first timestep of the storm's rising limb
  exo_sub <- exo %>% 
    filter(pond == storms$pond[i]) %>%
    filter(datetime >= storms$risestarttime[i] & datetime <= storms$nextstormstart[i] & datetime <= (storms$risestarttime[i] + days(30))) %>%
  arrange(datetime) %>%
    slice_head(n=1) %>%
    select(datetime, chl_rfu, chl_ugl, pc_rfu, pc_ugl)
  
  storms$instant_time[i] <- exo_sub$datetime[1]
  storms$instant_chl_rfu[i] <- exo_sub$chl_rfu[1]
  storms$instant_chl_ugl[i] <- exo_sub$chl_ugl[1]
  storms$instant_pc_rfu[i] <- exo_sub$pc_rfu[1]
  storms$instant_pc_ugl[i] <- exo_sub$pc_ugl[1]
  
  #Hourly pre-storm value: pre-storm pigment = the average pigment in the hour before the rising limb begins
  exo_sub_hour <- exo %>%
    filter(pond == storms$pond[i]) %>%
    filter(datetime >= (storms$risestarttime[i] - hours(1)) & datetime <= storms$risestarttime[i])
  storms$hour_chl_rfu[i] <- mean(exo_sub_hour$chl_rfu, na.rm = T)
  storms$hour_chl_ugl[i] <- mean(exo_sub_hour$chl_ugl, na.rm = T)
  storms$hour_pc_rfu[i] <- mean(exo_sub_hour$pc_rfu, na.rm = T)
  storms$hour_pc_ugl[i] <- mean(exo_sub_hour$pc_ugl, na.rm = T)
  
  #Daily pre-storm value: pre-storm pigment = average of previous 24 hours
  exo_sub_day <- exo %>%
    filter(pond == storms$pond[i]) %>%
    filter(datetime >= (storms$risestarttime[i] - days(1)) & datetime <= storms$risestarttime[i])
  
  storms$day_chl_rfu[i] <- mean(exo_sub_day$chl_rfu, na.rm = T)
  storms$day_chl_ugl[i] <- mean(exo_sub_day$chl_ugl, na.rm = T)
  storms$day_pc_rfu[i] <- mean(exo_sub_day$pc_rfu, na.rm = T)
  storms$day_pc_ugl[i] <- mean(exo_sub_day$pc_ugl, na.rm = T)
  
  storms$stormnum[i] <- i
}

#If instant time is not within an hour of the storm's rise time, this storm gets excluded for not having a true pre to compare to
storms$exclude <- ifelse(is.na(storms$exclude),ifelse(storms$instant_time - storms$risestarttime > (60*60) | is.na(storms$instant_time),'X', NA),storms$exclude)

storms <- storms %>% filter(is.na(exclude))

#Compare different pre values
ggplot(storms) +
  geom_abline() +
  geom_point(aes(x = instant_chl_ugl, y = hour_chl_ugl)) +
  theme_bw()

ggplot(storms) +
  geom_abline() +
  geom_point(aes(x = instant_chl_ugl, y = day_chl_ugl)) +
  theme_bw()

ggplot(storms) +
  geom_abline() +
  geom_point(aes(x = hour_chl_ugl, y = day_chl_ugl)) +
  theme_bw()
#Actually aren't as different as I had thought, so thats good. 


#Get nightly averages with storm info in df #####

for(i in 1:nrow(storms)){
  
  if(hour(storms$risestarttime[i]) < 5 & hour(storms$nextstormstart[i]) < 5){
    sub <- exo_night %>% 
      filter(pond == storms$pond[i] & 
             year == storms$year[i] &
               doy >= storms$stormdoy[i] - 1 &
               doy <= yday(storms$nextstormstart[i]) - 1 &
               doy <= storms$stormdoy[i] + 21) %>%
      mutate(stormnum = storms$stormnum[i],
             t0 = storms$stormdoy[i] - 1,
             timesincestormstart = doy - t0,
             risestarttime = storms$risestarttime[i],
             riseendtime = storms$riseendtime[i],
             risetime = storms$risetime[i],
             riseoverdoy = yday(riseendtime) + 1)
  }
  
  if(hour(storms$risestarttime[i]) >= 5 & hour(storms$nextstormstart[i]) < 5){
    sub <- exo_night %>% 
      filter(pond == storms$pond[i] & 
               year == storms$year[i] &
               doy >= storms$stormdoy[i] &
               doy <= yday(storms$nextstormstart[i]) - 1 &
               doy <= storms$stormdoy[i] + 21) %>%
      mutate(stormnum = storms$stormnum[i],
             t0 = storms$stormdoy[i],
             timesincestormstart = doy - t0,
             risestarttime = storms$risestarttime[i],
             riseendtime = storms$riseendtime[i],
             risetime = storms$risetime[i],
             riseoverdoy = yday(riseendtime) + 1)
  }
  
  if(hour(storms$risestarttime[i]) < 5 & hour(storms$nextstormstart[i]) >= 5){
    sub <- exo_night %>% 
      filter(pond == storms$pond[i] & 
               year == storms$year[i] &
               doy >= storms$stormdoy[i] - 1 &
               doy <= yday(storms$nextstormstart[i]) &
               doy <= storms$stormdoy[i] + 21) %>%
      mutate(stormnum = storms$stormnum[i],
             t0 = storms$stormdoy[i] - 1,
             timesincestormstart = doy - t0,
             risestarttime = storms$risestarttime[i],
             riseendtime = storms$riseendtime[i],
             risetime = storms$risetime[i],
             riseoverdoy = yday(riseendtime) + 1)
  }
  
  if(hour(storms$risestarttime[i]) >= 5 & hour(storms$nextstormstart[i]) >= 5){
    sub <- exo_night %>% 
      filter(pond == storms$pond[i] & 
               year == storms$year[i] &
               doy >= storms$stormdoy[i] &
               doy <= yday(storms$nextstormstart[i]) &
               doy <= storms$stormdoy[i] + 21) %>%
      mutate(stormnum = storms$stormnum[i],
             t0 = storms$stormdoy[i],
             timesincestormstart = doy - t0,
             risestarttime = storms$risestarttime[i],
             riseendtime = storms$riseendtime[i],
             risetime = storms$risetime[i],
             riseoverdoy = yday(riseendtime) + 1)
  }
  
  if(i == 1) {exo_night_storms <- sub}
  if(i > 1) {exo_night_storms <- rbind(exo_night_storms,sub)}
}

#Add in pre data
exo_night_pre <- exo_night_storms %>% filter(timesincestormstart == 0) %>%
  select(stormnum, chl_rfu:pc_ugl) %>%
  rename(pre_chl_rfu = chl_rfu,
         pre_chl_ugl = chl_ugl,
         pre_pc_rfu = pc_rfu,
         pre_pc_ugl = pc_ugl)

exo_night_storms <- left_join(exo_night_storms, exo_night_pre)
#Export for use in figures script
write.csv(exo_night_storms, 'C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Intermediate Data Products/EXO Nightly Averages with Storm Numbers.csv')


#Use rolling regressions to quantify algal dynamics during and after storm - all data######
#Make list of the model lengths to use
model_lengths = c(24,36,48,60,72,96,120,144,168)

pdf('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Chlorophyll Response Raw Data and Slopes of varying window lengths.pdf', width = 6, height = 4)

#Loop through all identified storms
for(i in 1:nrow(storms)){
  for(j in 1:length(model_lengths)){
  #Make subset of data that only contains data from that storm period
  exo_sub <- exo %>% 
    filter(pond == storms$pond[i]) %>%
    filter(datetime >= storms$risestarttime[i] & datetime <= storms$nextstormstart[i] & datetime <= (storms$risestarttime[i] + days(30))) %>%
    mutate(model_m = NA,
           model_b = NA,
           stormstart = storms$risestarttime[i],
           stormstart_dayfrac = yday(stormstart) + (hour(stormstart)/24) + (minute(stormstart)/(24*60)),
           stormnum = storms$stormnum[i],
           timesincestormstart = dayfrac - stormstart_dayfrac,
           datagap = datetime - lag(datetime)) 
  
  #Next 3 lines get rid of any data gaps greater than 1.5 days in length
  gaps <- exo_sub %>% filter(datagap >= (60*36)) %>%
    arrange(datetime)
  if(nrow(gaps) > 0){exo_sub <- exo_sub %>% filter(datetime < gaps$datetime[1])}
 
  if(nrow(exo_sub) > 0){
  #Approach to assign slope to first timepoint in each chunk
    #models <- slide_index(exo_sub, exo_sub$datetime, ~lm(chl_ugl~ dayfrac, data = .x), .after = hours(model_lengths[j]), .complete = T)
    #Approach to assign slope to middle timepoint in each chunk
    models <- slide_index(exo_sub, exo_sub$datetime, ~lm(chl_ugl~ dayfrac, data = .x), .after = hours(model_lengths[j]/2), .before = hours(model_lengths[j]/2), .complete = T)

    for(k in 1:nrow(exo_sub)){
      if(!is.null(models[[k]])){
        exo_sub$model_m[k] <- models[[k]]$coefficients[2]
        exo_sub$model_b[k] <- models[[k]]$coefficients[1]
        exo_sub$model_length <- model_lengths[j]
       }
      if(is.null(models[[k]])){
      exo_sub$model_m[k] <- NA
       exo_sub$model_b[k] <- NA
       exo_sub$model_length <- model_lengths[j]
       }
    }

  if(!is.null(models[[1]])){
  chlplot <- ggplot(exo_sub) +
    geom_vline(xintercept = (yday(storms$riseendtime[i]) +(hour(storms$riseendtime[i])/24) + (minute(storms$riseendtime[i])/(24*60)))) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_point(aes(x = dayfrac, y = chl_ugl)) +
    geom_point(aes(x = dayfrac, y = model_m),color = 'blue') +
    ggtitle(paste(storms$pond[i],' Storm Number:',i,' Window Length:',model_lengths[j],'hours')) +
    theme_bw()
  print(chlplot)
  }

  if(i == 1 & j == 1){exo_slopes <- exo_sub}
  if(i>1|j>1){exo_slopes <- rbind(exo_slopes, exo_sub)}
  }}
  message(sprintf("Completed %s of %s",i,nrow(storms)))
}
dev.off()

#Tried using hourly data - basically the same slopes, patterns definitely did not change

#Check complete argument of slide funtion:
#Based on running with complete = T and complete = F, it seems like this function only looks at the time passed, not the number of timesteps. This means it successfully cuts off the incomplete chunks at the end, but not those in the middle. For data gaps in the middle of a response period, only missing values where there can't be one stored bc there isnt a row. 

#Check to see what the slide function is actually doing with incomplete periods in the middle of the time period - what is it using to calculate? Test with 48 hour time period
#Storm #3 has known gap
gap1 <- exo %>% filter(datetime >= as.POSIXct('2022-07-17') & datetime <= as.POSIXct('2022-07-19') & pond == 'Strickers')
gap2 <- exo %>% filter(datetime >= as.POSIXct('2022-07-17 12:00') & datetime <= as.POSIXct('2022-07-19 12:00') & pond == 'Strickers')
gap3 <- exo %>% filter(datetime >= as.POSIXct('2022-07-18') & datetime <= as.POSIXct('2022-07-20') & pond == 'Strickers')
gap4 <- exo %>% filter(datetime >= as.POSIXct('2022-07-18 8:00') & datetime <= as.POSIXct('2022-07-20 8:00') & pond == 'Strickers')

lm(chl_ugl ~ dayfrac, data = gap1)
#Checked with values reported in exo_slopes for all chunks - is working as expected with the index, only missing periods bc no place to store their values. Should consider making some guidelines for inclusion of partial windows - does the gap need to be less than a certain % of the window for a point to be included?

#Quantify change in chlorophyll during rising limb - all data######

#Make dataframe with only rising limbs - this will be useful for visualizations
for(i in 1:nrow(storms)){
  risesub <- exo %>% 
    filter(pond == storms$pond[i] & datetime >= storms$risestarttime[i] & datetime <= storms$riseendtime[i]) %>%
    mutate(stormnum = storms$stormnum[i],
           stormstart_dayfrac = yday(storms$risestarttime[i]) + hour(storms$risestarttime[i])/24 + minute(storms$risestarttime[i])/(24*60),
           timesincestorm = dayfrac - stormstart_dayfrac,
           instant_chl_ugl = storms$instant_chl_ugl[i],
           hour_chl_ugl = storms$hour_chl_ugl[i])
  
  if(i == 1){exo_rise <- risesub}
  if(i > 1){exo_rise <- rbind(exo_rise, risesub)}
}

ggplot(exo_rise) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = timesincestorm, y = (chl_ugl-instant_chl_ugl)/instant_chl_ugl, color = pond)) +
  facet_wrap(~stormnum) +
  theme_bw()



#Make dataframe of change in chlorophyll observed during rising limb - using both a pure end - initial approach and linear modeled slope * risetime approaches
rise_responses <- data.frame(stormnum = numeric(),
                             pond = character(),
                             stormdoy = numeric(),
                             year = numeric(),
                             precip_mm = numeric(),
                             pre_instant_chl_ugl = numeric(),
                             instant_chl_ugl = numeric(),
                             instant_pc_ugl = numeric(),
                             instant_chl_rfu = numeric(),
                             instant_pc_rfu = numeric(),
                             pre_hour_chl_ugl = numeric(),
                             hour_chl_ugl = numeric(),
                             hour_pc_ugl = numeric(),
                             hour_chl_rfu = numeric(),
                             hour_pc_rfu = numeric(),
                             mod_chl_ugl = numeric(),
                             mod_pc_ugl = numeric(),
                             mod_chl_rfu = numeric(),
                             mod_pc_rfu = numeric())

for(i in 1:nrow(storms)){
  risesub <- exo_rise %>% filter(stormnum == storms$stormnum[i])
  
  rise_responses_sub <- data.frame(stormnum = NA,
                               pond = NA,
                               stormdoy = NA,
                               year = NA,
                               precip_mm = NA,
                               pre_instant_chl_ugl = NA,
                               instant_chl_ugl = NA,
                               instant_pc_ugl = NA,
                               instant_chl_rfu = NA,
                               instant_pc_rfu = NA,
                               pre_hour_chl_ugl = NA,
                               hour_chl_ugl = NA,
                               hour_pc_ugl = NA,
                               hour_chl_rfu = NA,
                               hour_pc_rfu = NA,
                               mod_chl_ugl = NA,
                               mod_pc_ugl = NA,
                               mod_chl_rfu = NA,
                               mod_pc_rfu = NA)
  
  #Fill in instant values and storm info to response sub
  rise_responses_sub$stormnum <- storms$stormnum[i]
  rise_responses_sub$pond <- storms$pond[i]
  rise_responses_sub$stormdoy <- storms$stormdoy[i]
  rise_responses_sub$year <- storms$year[i]
  rise_responses_sub$precip_mm <- storms$precip_mm[i]
  rise_responses_sub$pre_instant_chl_ugl <- storms$instant_chl_ugl[i]
  rise_responses_sub$instant_chl_rfu <- risesub$chl_rfu[nrow(risesub)] - storms$instant_chl_rfu[i]
  rise_responses_sub$instant_chl_ugl <- risesub$chl_ugl[nrow(risesub)] - storms$instant_chl_ugl[i]
  rise_responses_sub$instant_pc_rfu <- risesub$pc_rfu[nrow(risesub)] - storms$instant_pc_rfu[i]
  rise_responses_sub$instant_pc_ugl <- risesub$pc_ugl[nrow(risesub)] - storms$instant_pc_ugl[i]
  rise_responses_sub$pre_hour_chl_ugl <- storms$hour_chl_ugl[i]
  rise_responses_sub$hour_chl_rfu <- risesub$chl_rfu[nrow(risesub)] - storms$hour_chl_rfu[i]
  rise_responses_sub$hour_chl_ugl <- risesub$chl_ugl[nrow(risesub)] - storms$hour_chl_ugl[i]
  rise_responses_sub$hour_pc_rfu <- risesub$pc_rfu[nrow(risesub)] - storms$hour_pc_rfu[i]
  rise_responses_sub$hour_pc_ugl <- risesub$pc_ugl[nrow(risesub)] - storms$hour_pc_ugl[i]
  
  #Linear models of pigment responses during rising limbs
  chl_ugl_mod <- summary(lm(chl_ugl ~ timesincestorm, data = risesub))
  chl_rfu_mod <- summary(lm(chl_rfu ~ timesincestorm, data = risesub))
  pc_ugl_mod <- summary(lm(pc_ugl ~ timesincestorm, data = risesub))
  pc_rfu_mod <- summary(lm(pc_rfu ~ timesincestorm, data = risesub))
  
  #Fill in modeled change in chlorophyll for each pigment type - done by multiplying the slope of the rising limb by the length of the rising limb
  rise_responses_sub$mod_chl_ugl <- chl_ugl_mod$coefficients[2,1] * (as.numeric(storms$risetime[i]/24))
  rise_responses_sub$mod_chl_rfu <- chl_rfu_mod$coefficients[2,1] * (as.numeric(storms$risetime[i]/24))
  rise_responses_sub$mod_pc_ugl <- pc_ugl_mod$coefficients[2,1] * (as.numeric(storms$risetime[i]/24))
  rise_responses_sub$mod_pc_rfu <- pc_rfu_mod$coefficients[2,1] * (as.numeric(storms$risetime[i]/24))
  
  if(i == 1){rise_responses <- rise_responses_sub}
  if(i > 1){rise_responses <- rbind(rise_responses, rise_responses_sub)}
}

#How different are these approaches
ggplot(rise_responses) +
  geom_abline() +
  geom_point(aes(x = instant_pc_ugl, y = mod_pc_ugl, color = pond)) +
  theme_bw()

ggplot(rise_responses) +
  geom_abline() +
  geom_point(aes(x = instant_chl_ugl, y = hour_chl_ugl, color = pond)) +
  theme_bw()

#Plotting rising limbs over whole chlorophyll response - does the chl generally keep going down after the rising limb or level out
ggplot() +
  geom_point(data = stormresp_mod72, aes(x = timesincestormstart, y = chl_ugl)) +
  geom_point(data = exo_rise, aes(x = timesincestorm, y = chl_ugl), color = 'blue') +
  facet_wrap(~as.factor(stormnum)) +
  theme_bw()
#Actually pretty good for most of them, though hard to tell for sure with noise

ggplot(exo_rise) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = timesincestorm, y = chl_ugl-hour_chl_ugl)) +
  facet_wrap(~stormnum) +
  theme_bw()

#Are bigger storms flushing more? Gut check on ability to capture a flush
ggplot(rise_responses) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = precip_mm, y = hour_chl_ugl/pre_hour_chl_ugl * 100, color = pond), size = 4) +
  ylab('Percent Change in Chl during storm hydrograph rising limb (ug/L)') +
  theme_bw()

#Identify maximum rates of increase for each storm from slopes - all data#####
slope_responses <- data.frame(stormnum = as.numeric(),
                              stormdoy = as.numeric(),
                              year = as.numeric(),
                              pond = as.character(),
                              maxslope = as.numeric(),
                              maxslope_time = as.numeric())

for(i in 1:nrow(storms)){
  sub <- exo_slopes %>% filter(stormnum == storms$stormnum[i] & model_length == '72')
  
  maxslope <- slice_max(sub, order_by = model_m)
  
  slope_responses_sub <- data.frame(stormnum = NA,
                                stormdoy = NA,
                                year = NA,
                                pond = NA,
                                maxslope = NA,
                                maxslope_time = NA)
  slope_responses_sub$stormnum <- storms$stormnum[i]
  slope_responses_sub$stormdoy <- storms$stormdoy[i]
  slope_responses_sub$year <- storms$year[i]
  slope_responses_sub$pond <- storms$pond[i]
  
  if(nrow(maxslope) == 1){
  slope_responses_sub$maxslope <- maxslope$model_m
  slope_responses_sub$maxslope_time <- maxslope$timesincestormstart
  }
  
  if(nrow(maxslope) > 1){
    slope_responses_sub$maxslope <- NA
    slope_responses_sub$maxslope_time <- NA
  }
  
  if(i == 1) {slope_responses <- slope_responses_sub}
  if(i > 1) {slope_responses <- rbind(slope_responses, slope_responses_sub)}
}


#Any patterns with storm size? No
test <- left_join(storms, slope_responses)

ggplot(test) +
  geom_point(aes(x = precip_mm, y = maxslope, color = pond)) +
  theme_bw()






#Slopes post rising limb whole period - all data#####
post_rising_slopes <- data.frame(stormnum = as.numeric(),
                              stormdoy = as.numeric(),
                              year = as.numeric(),
                              pond = as.character(),
                              slope = as.numeric())

for(i in 1:nrow(storms)){
  sub <- exo_slopes %>% filter(stormnum == storms$stormnum[i] & model_length == '72') %>%
    filter(datetime >= storms$riseendtime[i] & datetime <= storms$nextstormstart[i] & datetime <= (storms$riseendtime[i] + days(10)))
  
  model <- summary(lm(chl_ugl ~ dayfrac, data = sub))
  
  
  post_rising_slopes_sub <- data.frame(stormnum = NA,
                                   stormdoy = NA,
                                   year = NA,
                                   pond = NA,
                                   slope = NA)
  
  post_rising_slopes_sub$stormnum <- storms$stormnum[i]
  post_rising_slopes_sub$stormdoy <- storms$stormdoy[i]
  post_rising_slopes_sub$year <- storms$year[i]
  post_rising_slopes_sub$pond <- storms$pond[i]
  post_rising_slopes_sub$slope <- model$coefficients[2,1]
  
  if(i == 1){post_rising_slopes <- post_rising_slopes_sub}
  if(i > 1){post_rising_slopes <- rbind(post_rising_slopes, post_rising_slopes_sub)}
}

ggplot(dailyavgs) +
  geom_hline(yintercept = 0) +
  geom_point(data = stormresp_mod72, aes(x = timesincestormstart, y = chl_ugl)) +
  geom_line(aes(x = dayssincestorm, y = chl_ugl, color = pond), linewidth = 2) +
  geom_smooth(data = stormresp_mod72, aes(x = timesincestormstart, y = chl_ugl), method = 'lm', color = 'blue') +
  facet_wrap(~as.factor(stormnum)) +
  scale_x_continuous(limits = c(0,10)) +
  ylab('Change in chlorophyll (ug/L)') +
  theme_bw()
#Matches what I expected - just a slope of the whole time period is not an accurate description of dynamics. Does not


#Extending window slopes nightly summarized data#####

pdf('C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Figures/Pigment Response Nightly Avgs Extending Window Slopes.pdf', width = 6, height = 4)

for(i in 1:nrow(storms)){
  for(j in 1:21){
  sub <- exo_night_storms %>% 
    filter(stormnum == storms$stormnum[i] & timesincestormstart <= j)
  
  if(nrow(sub) == j+1){ #Don't want a bunch of repeat numbers when the length of window isn't changing, so only repeat when we actually add a new point
  chl_model <- summary(lm(chl_ugl ~ doy, data = sub))
  pc_model <- summary(lm(pc_ugl ~ doy, data = sub))
  
  slopes_night_sub <- data.frame(stormnum = NA,
                                       stormdoy = NA,
                                       year = NA,
                                       pond = NA,
                                       window_length = NA,
                                       chl_slope = NA,
                                       pc_slope = NA,
                                       pre_chl_ugl = NA,
                                       pre_pc_ugl = NA)
  
  slopes_night_sub$stormnum <- storms$stormnum[i]
  slopes_night_sub$stormdoy <- storms$stormdoy[i]
  slopes_night_sub$year <- storms$year[i]
  slopes_night_sub$pond <- storms$pond[i]
  slopes_night_sub$window_length <- j
  slopes_night_sub$chl_slope <- chl_model$coefficients[2,1]
  slopes_night_sub$pc_slope <- pc_model$coefficients[2,1]
  slopes_night_sub$pre_chl_ugl <- exo_night_pre$pre_chl_ugl[i]
  slopes_night_sub$pre_pc_ugl <- exo_night_pre$pre_pc_ugl[i]
  
  chl_plot <- ggplot(sub) +
    geom_point(aes(x = doy, y = chl_ugl), size = 4) +
    geom_smooth(aes(x = doy, y = chl_ugl), method = 'lm') +
    ggtitle(paste('Chl Storm Num:',i,storms$pond[i],'Window Length:',j)) +
    theme_bw()
  
  pc_plot <- ggplot(sub) +
    geom_point(aes(x = doy, y = pc_ugl), size =4) +
    geom_smooth(aes(x = doy, y = pc_ugl), method = 'lm') +
    ggtitle(paste('PC Storm Num:',i,storms$pond[i],'Window Length:',j)) +
    theme_bw()
  
  print(chl_plot)
  print(pc_plot)
  
  if(i == 1 & j == 1){slopes_night <- slopes_night_sub}
  if(i > 1 | j > 1){slopes_night <- rbind(slopes_night, slopes_night_sub)}
  }}}
dev.off()

ggplot(slopes_night) +
  geom_line(aes(x = window_length, y = chl_slope, color = as.factor(stormnum))) +
  theme_bw()

#Calculate median responses at each timestep #######
exo_night_storms <- exo_night_storms %>%
  mutate(deltachl_ugl = chl_ugl - pre_chl_ugl,
         deltapc_ugl = pc_ugl - pre_pc_ugl,
         percch_chl = deltachl_ugl/pre_chl_ugl *100,
         percch_pc = deltapc_ugl/pre_pc_ugl *100)

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

slopes_night_sum <- slopes_night %>%
  group_by(pond, window_length) %>%
  summarize(avg_chl_slope = median(chl_slope, na.rm = T),
            sd_chl_slope = sd(chl_slope, na.rm = T),
            avg_pc_slope = median(pc_slope, na.rm=T),
            sd_pc_slope = sd(pc_slope, na.rm =T),
            obsnum = n())

#Figure: All responses over time#####

ggplot(exo_night_storms) +
  geom_ribbon(data = exo_night_storms_sum, aes(x = timesincestormstart, ymin = (avg_deltachl - sd_deltachl), ymax = (avg_deltachl + sd_deltachl)), alpha = 0.2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = timesincestormstart, y = chl_ugl - pre_chl_ugl, color = as.factor(year), group = stormnum), linewidth = 1) +
  geom_line(data = exo_night_storms_sum, aes(x=timesincestormstart, y = avg_deltachl), linewidth = 2) +
  facet_wrap(~pond) +
  scale_x_continuous(limits = c(0,10)) +
  ylab('Change in Chlorophyll (ug/L)') + xlab('Days Since Storm Start') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  theme_bw() +
  theme(legend.position = 'bottom')

ggplot(exo_night_storms) +
  geom_ribbon(data = exo_night_storms_sum, aes(x = timesincestormstart, ymin = (avg_percch_chl - sd_percch_chl), ymax = (avg_percch_chl + sd_percch_chl)), alpha = 0.2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = timesincestormstart, y = percch_chl, color = as.factor(year), group = stormnum), linewidth = 1) +
  geom_line(data = exo_night_storms_sum, aes(x=timesincestormstart, y = avg_percch_chl), linewidth = 2) +
  facet_wrap(~pond) +
  scale_x_continuous(limits = c(0,10)) +
  ylab('Percent Change in Chlorophyll') + xlab('Days Since Storm Start') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  theme_bw() +
  theme(legend.position = 'bottom')



ggplot(exo_night_storms) +
  geom_ribbon(data = exo_night_storms_sum, aes(x = timesincestormstart, ymin = (avg_percch_pc - sd_percch_pc), ymax = (avg_percch_pc + sd_percch_pc)), alpha = 0.2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = timesincestormstart, y = percch_pc, color = as.factor(year), group = stormnum), linewidth = 1) +
  geom_line(data = exo_night_storms_sum, aes(x=timesincestormstart, y = avg_percch_pc), linewidth = 2) +
  facet_wrap(~pond) +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(-100, 250)) +
  ylab('Percent Change in Phycocyanin') + xlab('Days Since Storm Start') +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  theme_bw() +
  theme(legend.position = 'bottom')

ggplot(slopes_night) +
  geom_ribbon(data = slopes_night_sum, aes(x = window_length, ymin = (avg_chl_slope - sd_chl_slope), ymax = (avg_chl_slope + sd_chl_slope)), alpha = 0.2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = window_length, y = chl_slope, color = as.factor(year), group = stormnum), linewidth = 1) +
  geom_line(data =slopes_night_sum, aes(x = window_length, y = avg_chl_slope), linewidth = 2) +
  facet_wrap(~pond) +
  ylab('Chlorophyll Slope (ug L-1 day -1)') + xlab('Days post storm included in slope calculation') +
  scale_x_continuous(limits = c(1,10)) +
  scale_color_manual(name = 'Year',
                     breaks = c('2022','2023','2024'),
                     values = c('2022' = '#1b9e77','2023' = '#d95f02','2024' = '#7570b3'))+
  theme_bw() +
  theme(legend.positon = 'bottom')

#Figures: Rise and Delay Slope Windows Raw Data Plotted
#Rise
exo_night_storms %>% filter(doy <= riseoverdoy +1) %>%
  ggplot() +
  geom_hline(yintercept = 1) +
  geom_line(aes(x = timesincestormstart, y = deltachl_ugl, group = stormnum, color = as.factor(year)), linewidth = 1) +
  facet_wrap(~pond) +
  theme_bw()

#Delay
exo_night_endrise <- exo_night_storms %>% 
  filter(doy == riseoverdoy) %>%
  select(stormnum, chl_ugl, pc_ugl) %>%
  rename(endrise_chl_ugl = chl_ugl,
         endrise_pc_ugl = pc_ugl)

delayresp_plotinput <- left_join(exo_night_storms, exo_night_endrise) %>%
  mutate(dayssinceriseend = doy - riseoverdoy,
         delay_deltachl = (chl_ugl - endrise_chl_ugl) )%>%
  filter(timesincestormstart <= 10 & dayssinceriseend >= 0)

ggplot(delayresp_plotinput) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(aes(x = dayssinceriseend, y = delay_deltachl, group = stormnum, color = as.factor(year)), linewidth =1) +
  facet_wrap(~pond) +
  theme_bw()

#Rolling window slopes nightly summarized data######
for(i in 1:nrow(storms)){
    #Make subset of data that only contains data from that storm period
    exo_sub <- exo_night_storms %>% 
      filter(stormnum == storms$stormnum[i]) %>%
      mutate(model_m = NA,
             model_b = NA)

      #Approach to assign slope to first timepoint in each chunk
      #models <- slide_index(exo_sub, exo_sub$datetime, ~lm(chl_ugl~ dayfrac, data = .x), .after = hours(model_lengths[j]), .complete = T)
      #Approach to assign slope to middle timepoint in each chunk
      models <- slide_index(exo_sub, exo_sub$doy, ~lm(chl_ugl~ doy, data = .x), .after = 1, .before = 1, .complete = T)
      
      for(k in 1:nrow(exo_sub)){
        if(!is.null(models[[k]])){
          exo_sub$model_m[k] <- models[[k]]$coefficients[2]
          exo_sub$model_b[k] <- models[[k]]$coefficients[1]
        }
        if(is.null(models[[k]])){
          exo_sub$model_m[k] <- NA
          exo_sub$model_b[k] <- NA
        }
      }
      
      if(i == 1){night_slopes_rolling <- exo_sub}
      if(i>1){night_slopes_rolling <- rbind(night_slopes_rolling, exo_sub)}
      
  message(sprintf("Completed %s of %s",i,nrow(storms)))
}

ggplot(night_slopes_rolling) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = timesincestormstart, y = (chl_ugl - pre_chl_ugl)), color = 'lightgreen') +
  geom_line(aes(x = timesincestormstart, y = model_m), size = 1) +
  facet_wrap(~stormnum) +
  theme_bw()



#Rising limb slopes only nightly summarized data######
exo_night_rise <- exo_night_storms %>%
  filter(doy <= riseoverdoy)

ggplot(exo_night_rise) +
  geom_hline(yintercept = 0) +
  geom_line(data = exo_night_storms, aes(x = timesincestormstart, y = (chl_ugl - pre_chl_ugl)/pre_chl_ugl), linewidth = 1) +
  geom_line(aes(x = timesincestormstart, y = (chl_ugl - pre_chl_ugl)/pre_chl_ugl), linewidth = 1, color = 'red') +
  facet_wrap(~stormnum) +
  scale_x_continuous(limits = c(0,5)) +
  theme_bw()

for(i in 1:nrow(storms)){
  sub <- exo_night_rise %>% filter(stormnum == storms$stormnum[i])
  
  chl_model <- summary(lm(chl_ugl ~ doy, data = sub))
  pc_model <- summary(lm(pc_ugl ~ doy, data = sub))
  
  slopes_night_rise_sub <- data.frame(stormnum = NA,
                                       stormdoy = NA,
                                       year = NA,
                                       pond = NA,
                                       chl_slope = NA,
                                      pc_slope = NA,
                                      pointsinslope = NA)
  
  slopes_night_rise_sub$stormnum <- storms$stormnum[i]
  slopes_night_rise_sub$stormdoy <- storms$stormdoy[i]
  slopes_night_rise_sub$year <- storms$year[i]
  slopes_night_rise_sub$pond <- storms$pond[i]
  slopes_night_rise_sub$chl_slope <- chl_model$coefficients[2,1]
  slopes_night_rise_sub$pc_slope <- pc_model$coefficients[2,1]
  slopes_night_rise_sub$pointsinslope <- nrow(sub)
  
  if(i == 1){slopes_night_rise <- slopes_night_rise_sub}
  if(i > 1){slopes_night_rise <- rbind(slopes_night_rise, slopes_night_rise_sub)}
}


#Compare rising limb length to static 2 and 3 day slopes
slopes_night_23 <- slopes_night %>%
  filter(window_length %in% c('2','3')) %>%
  select(stormnum, window_length, chl_slope) %>%
  pivot_wider(names_from = window_length, values_from = chl_slope) %>%
  rename(slope_2day = 2,
         slope_3day = 3)

slopes_night_rise <- left_join(slopes_night_rise, slopes_night_23) %>%
  mutate(risevs2day = (slope_2day - slope)/slope,
         day2vs3day = (slope_2day - slope_3day)/slope_3day)

ggplot(slopes_night_rise) +
  geom_abline() +
  geom_point(aes(x = slope_2day, y = slope)) +
  theme_bw()
#Most points are quite similar whether using the slope for the rising limb length, 2day slope, or 3day slope



#Delayed response slopes - nightly summarized data#####

for(i in 1:nrow(storms)){
  sub <- exo_night_storms %>% 
    filter(stormnum == storms$stormnum[i] & doy >= riseoverdoy & timesincestormstart <= 10)
  #Only want to capture period after rising limb 

  slopes_night_delayresp_sub <- data.frame(stormnum = NA,
                                      stormdoy = NA,
                                      year = NA,
                                      pond = NA,
                                      chl_slope = NA,
                                      pc_slope = NA,
                                      pointsinslope = NA)
  
  slopes_night_delayresp_sub$stormnum <- storms$stormnum[i]
  slopes_night_delayresp_sub$stormdoy <- storms$stormdoy[i]
  slopes_night_delayresp_sub$year <- storms$year[i]
  slopes_night_delayresp_sub$pond <- storms$pond[i]
  
  if(nrow(sub) > 1){
    chl_model <- summary(lm(chl_ugl ~ doy, data = sub))
    pc_model <- summary(lm(pc_ugl ~ doy, data = sub))
    slopes_night_delayresp_sub$chl_slope <- chl_model$coefficients[2,1]
    slopes_night_delayresp_sub$pc_slope <- pc_model$coefficients[2,1]
    slopes_night_delayresp_sub$pointsinslope <- nrow(sub)}
  
  if(nrow(sub) <= 1){
    slopes_night_delayresp_sub$chl_slope <- NA
    slopes_night_delayresp_sub$pc_slope <- NA
    slopes_night_delayresp_sub$pointsinslope <- nrow(sub)}
  
  if(i == 1){slopes_night_delayresp <- slopes_night_delayresp_sub}
  if(i > 1){slopes_night_delayresp <- rbind(slopes_night_delayresp, slopes_night_delayresp_sub)}
}






#Create data frame with all relevant slopes together######
#Value I Want --- dataframe its in: 
#Varying rising limb slope and days included --- slopes_night_rise
#static rising limb slope 2 and 3 day --- slopes_night (window lengths 2 and 3)
#full response period days 10-14 --- slopes_night (window lengths 10-14)
#delay response period 3-10 day and days included --- slopes_night_delayresp

#Go down this list order to make things easier
#Varying rising limb
slopes_night_rise <- slopes_night_rise %>%
  rename(rise_chl_slope = chl_slope,
         rise_pc_slope = pc_slope,
         rise_points = pointsinslope)

#Extending window pullouts - static rising limbs and full response period
slopes_windowsub <- slopes_night %>%
  select(stormnum, window_length, chl_slope, pc_slope) %>%
  filter(window_length %in% c('2','3','10','11','12','13','14')) %>%
  pivot_wider(names_from = window_length, values_from = c(chl_slope, pc_slope), names_glue = "post{window_length}_{.value}")

#Delay resp
slopes_night_delayresp <- slopes_night_delayresp %>%
  rename(delay_chl_slope = chl_slope,
         delay_pc_slope = pc_slope,
         delay_points = pointsinslope)

#Get pre-storm values sorted
#True pre values saved in exo_night_pre
#Get delayed response pre values
exo_night_endrise <- exo_night_storms %>% 
  filter(doy == riseoverdoy) %>%
  select(stormnum, chl_ugl, pc_ugl) %>%
  rename(endrise_chl_ugl = chl_ugl,
         endrise_pc_ugl = pc_ugl)

#Combine all slope and pre dfs
relslopes <- left_join(slopes_night_rise, slopes_windowsub)
relslopes <- left_join(relslopes, slopes_night_delayresp)
relslopes <- left_join(relslopes, exo_night_pre) %>% select(-pre_chl_rfu, -pre_pc_rfu)
relslopes <- left_join(relslopes, exo_night_endrise)


#Calculate desired percent changes and export#####
relslopes <- relslopes %>%
  mutate(rise_chl_percch = ((rise_chl_slope * (rise_points-1))/pre_chl_ugl)*100,
         rise_pc_percch = ((rise_pc_slope * (rise_points-1))/pre_pc_ugl)*100,
         post10_chl_percch = ((post10_chl_slope * 10)/pre_chl_ugl)*100,
         post10_pc_percch = ((post10_pc_slope * 10)/pre_pc_ugl)*100)

write.csv(relslopes, "C:/Users/Jessica Briggs/Box/Dissertation/Sondes & Ponds/Intermediate Data products/Storm Quantitative Responses.csv", row.names = F)

#Are delay slopes related to how long is included in the period?#####
ggplot(relslopes) +
  geom_point(aes(x = delay_points, y = delay_chl_slope)) +
  theme_bw()


#How similar are chl and pc responses?#####
ggplot(relslopes) +
  geom_abline(linewdith = 1) +
  geom_point(aes(x = post10_chl_percch, y = post10_pc_percch, color = as.factor(year), shape = pond), size = 4) +
  theme_bw()

ggplot(relslopes) +
  geom_abline(linewdith = 1) +
  geom_point(aes(x = rise_chl_percch, y = rise_pc_percch, color = as.factor(year), shape = pond), size = 4) +
  theme_bw()

ggplot(relslopes) +
  geom_point(aes(x = delay_chl_slope, y = delay_pc_slope, color = as.factor(year), shape = pond), size = 4) +
  theme_bw()

#Rising limb responses very similar, 10 day is pretty good, delay is a bit messier but still afollows the same general pattern. Don't think it makes much sense to analyze both - redundant


#Nightly summarized data visualization - percent change and raw change######

#Percent change all storms together
ggplot(exo_night_storms) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = timesincestormstart, y = (chl_ugl - pre_chl_ugl)/pre_chl_ugl, color = as.factor(stormnum)))+
  scale_x_continuous(limits = c(0,4)) +
  scale_y_continuous(limits = c(-1,1)) +
  theme_bw()

#Percent change all storms faceted
ggplot(exo_night_storms) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = timesincestormstart, y = (chl_ugl - pre_chl_ugl)/pre_chl_ugl), linewidth = 1)+
  facet_wrap(~stormnum) +
  theme_bw()

#raw change all storms together
ggplot(exo_night_storms) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = timesincestormstart, y = (chl_ugl - pre_chl_ugl), color = as.factor(stormnum)))+
  theme_bw()

#Raw change all storms faceted
ggplot(exo_night_storms) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3) +
  geom_vline(xintercept = 10) +
  geom_line(aes(x = timesincestormstart, y = (chl_ugl - pre_chl_ugl)), linewidth = 1)+
  facet_wrap(~stormnum) +
  theme_bw()

#Slopes over time all storms faceted
ggplot(slopes_night) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = window_length, y = chl_slope)) +
  facet_wrap(~stormnum) +
  theme_bw()
 
#Example Slopes Plot
exo_night_storms %>% filter(stormnum == 46) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3) +
  geom_vline(xintercept = 10) +
  geom_line(aes(x = timesincestormstart, y= chl_ugl - pre_chl_ugl), linewidth = 2) +
  scale_x_continuous(limits = c(0,14)) +
  theme_bw()


#When in storm periods are most reaching their min and max?
mins <- exo_night_storms %>%
  group_by(stormnum) %>%
  slice_min(chl_ugl)
maxes <- exo_night_storms %>%
  filter(timesincestormstart != '0') %>%
  group_by(stormnum) %>%
  slice_max(chl_ugl)

ggplot(mins) +
  geom_histogram(aes(x = timesincestormstart)) +
  theme_bw()

ggplot(maxes) +
  geom_histogram(aes(x = timesincestormstart)) +
  theme_bw()

#Basic visualizations of all storms#####
precond <- storms %>% select(instant_chl_ugl:stormnum)

stormresp_mod72 <- exo_slopes %>% filter(model_length == '72') %>%
  left_join(precond, by = join_by(stormnum)) %>%
  mutate(percchange = (chl_ugl-instant_chl_ugl)/instant_chl_ugl)

stormresp_mod96 <- exo_slopes %>% filter(model_length == '96') %>%
  left_join(precond, by = join_by(stormnum)) %>%
  mutate(percchange = (chl_ugl-instant_chl_ugl)/instant_chl_ugl)

  
#Chl Slopes over time post storm - separated by pond and year
ggplot(stormresp_mod72) +
  geom_point(aes(x = timesincestormstart, y = model_m, color = stormstart_dayfrac)) +
  facet_grid(year(datetime)~pond) +
  theme_bw()

#Chl Slopes over time post storm - separated by storm
ggplot(stormresp_mod96) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = timesincestormstart, y = model_m, color = pond)) +
  facet_wrap(~as.factor(stormnum)) +
  ylab('Chlorophyll Rolling Window Slope with 4 day period (ugl/day)') +
  theme_bw()

bigplot <- exo_slopes %>% filter(!(model_length %in% c('24','36','48'))) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = timesincestormstart, y = model_m, color = as.factor(model_length))) +
  facet_wrap(~as.factor(stormnum)) +
  scale_y_continuous(limits = c(-150,150)) +
  theme_bw()
bigplot

#Slopes and data on one plot?
ggplot(stormresp_mod96) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = timesincestormstart, y = (chl_ugl - hour_chl_ugl))) +
  geom_point(aes(x = timesincestormstart, y = model_m, color = pond)) +
  facet_wrap(~as.factor(stormnum)) +
  theme_bw()

ggplot(stormresp_mod72) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = timesincestormstart, y = percchange*50)) +
  geom_point(aes(x = timesincestormstart, y = model_m, color = pond)) +
  facet_wrap(~as.factor(stormnum)) +
  theme_bw()

#Percent Change in Chl over time post storm - by pond and year
ggplot(stormresp_mod72) +
  geom_point(aes(x = timesincestormstart, y = (chl_ugl-hour_chl_ugl)/hour_chl_ugl, color = stormstart_dayfrac)) +
  facet_grid(year(datetime)~pond) +
  ylab('Percent Change Chlorophyll') +
  #scale_y_continuous(limits = c(-150,150)) +
  theme_bw()

#Percent change in chl post storm - by stormnum
ggplot(stormresp_mod72) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = timesincestormstart, y = (chl_ugl-instant_chl_ugl)/instant_chl_ugl, color = pond)) +
  facet_wrap(~stormnum) +
  theme_bw() +
  theme(legend.position = 'none')

#Plot daily average chlorophyll after each storm
#starting with this chunk of stormIDed points
dailyavgs <- stormresp_mod72 %>%
  mutate(dayssincestorm = ceiling(timesincestormstart)) %>%
  group_by(pond, stormnum, year, dayssincestorm) %>%
  summarize(chl_ugl = mean(chl_ugl, na.rm = T),
            chl_rfu = mean(chl_rfu, na.rm = T),
            pc_ugl = mean(pc_ugl, na.rm = T),
            pc_rfu = mean(pc_rfu, na.rm = T),
            instant_chl_ugl = mean(instant_chl_ugl, na.rm = T),
            instant_chl_rfu = mean(instant_chl_rfu, na.rm = T),
            instant_pc_ugl = mean(instant_pc_ugl, na.rm = T),
            instant_pc_rfu = mean(instant_pc_rfu, na.rm = T))

#Percetn change in chl with daily avg data
ggplot(dailyavgs) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = dayssincestorm, y = (chl_ugl - instant_chl_ugl)/instant_chl_ugl, color = as.factor(stormnum)), linewidth = 1) +
  facet_grid(year~pond) +
  scale_x_continuous(limits = c(0,20)) +
  theme_bw()

#Daily data over 15 minute data
ggplot(dailyavgs) +
  geom_hline(yintercept = 0) +
  geom_point(data = stormresp_mod72, aes(x = timesincestormstart, y = (chl_ugl - hour_chl_ugl))) +
  geom_line(aes(x = dayssincestorm, y = (chl_ugl - instant_chl_ugl), color = pond), linewidth = 2) +
  facet_wrap(~as.factor(stormnum)) +
  scale_x_continuous(limits = c(0,10)) +
  ylab('Change in chlorophyll (ug/L)') +
  theme_bw()


#Chl vs PC responses
ggplot(exo_night) +
  geom_point(aes(x = chl_rfu, y = pc_rfu)) +
  theme_bw()

exo_night %>% filter(year == '2023') %>%
  ggplot() +
  geom_point(aes(x = doy, y = chl_ugl, color = pond), size = 1) +
  theme_bw()
