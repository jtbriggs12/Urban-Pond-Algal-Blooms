#Quantify Algal Response to Storms
#Author: Jess Briggs
#Date created: 2024-02-06

library(tidyverse)
library(slider)
#library(zoo)

#Read in Data#####
exo <- read.csv('./Input Data Files/EXO Full dataset.csv') %>% 
  mutate(datetime = as.POSIXct(datetime),
         year = year(datetime),
         doy = yday(datetime),
         dayfrac = yday(datetime) + (hour(datetime)/24) + (minute(datetime)/(60*24))) %>%
  #Using all 2 minute data points breaks the models below. Down sample in 2023 and 2024 to every 10 minutes
  filter(year(datetime) == '2022' | (year(datetime) != '2022' & minute(datetime) %in% c(00,10,20,30,40,50)))

#Identified Storms
storms <- read.csv('./Output and Intermediate Files/Storms above Thresholds.csv') %>%
  mutate(risestarttime = as.POSIXct(risestarttime),
         riseendtime = as.POSIXct(riseendtime),
         stormdoy = yday(risestarttime),
         stormnum = row_number()) %>%
  group_by(pond, year) %>%
  mutate(nextstormstart = lead(risestarttime),
         nextstormstart = if_else(is.na(nextstormstart),risestarttime+days(30),nextstormstart),
         risetime = riseendtime - risestarttime) %>%
  ungroup()

#Summarize to nightly averages
exo_night <- exo %>% filter(hour(datetime) < 5) %>%
  group_by(pond, year, doy) %>%
  summarize(across(chl_rfu:temp_C, mean)) %>%
  ungroup() %>%
  select(pond, year, doy,chl_rfu, chl_ugl, pc_rfu, pc_ugl, do_sat, do_mgl, pH, temp_C)
write.csv(exo_night, './Output and Intermediate Files/EXO Nightly Averages.csv', row.names = F)

#Filter to relevant time periods and add storm info to df#####

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
write.csv(exo_night_storms, './Output and Intermediate Files/EXO Nightly Averages with Storm Info.csv', row.names = F)

#Filter to only storms with pre data
storms <- left_join(storms, exo_night_pre) %>%
  filter(!is.na(pre_chl_ugl)) #Cannot assess effects if no pre data

#Extending window slopes - nightly summarized data#####

pdf('./Figures/Pigment Response Nightly Avgs Extending Window Slopes.pdf', width = 6, height = 4)

for(i in 1:nrow(storms)){
  for(j in 1:10){
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

#Rising limb slopes only - nightly summarized data######
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

write.csv(relslopes, "./Output and Intermediate Files/Storm Quantitative Responses.csv", row.names = F)

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