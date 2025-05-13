#Gathering Storm Quantitative Response and Explanatory Data
#Author: Jess Briggs
#Date Created: 2024-09-25

library(tidyverse)
  
#In this script I will build the explanatory variable dataframe with hypothesized factors that would control how each pond responds to each individual storm event
#Variables of interest: 
#Storm Characteristics - Rainfall amounts, days since most recent storm, # of storms that had already occurred that year in liquid rain --> amount of liquid rainfall so far that year
#Pond environment - water temp and daylight periods
#Pond Chem Changes - Dissolved nutrient, DOC, Abs @440nm pre and post storms
#Pond Pre-Storm Conditions - Nutrients, Sonde PC/Chl/Cond
#Stratification Disturbance 

#Start with bringing in storm hydrograph data#####
storms <- read.csv("./Output and Intermediate Files/Storms Above Thresholds.csv") %>%
  select(pond, raindoy, year, precip_mm, risestarttime, riseendtime, startdepth, bounce_m) %>%
  mutate(risestarttime = as.POSIXct(risestarttime),
         riseendtime = as.POSIXct(riseendtime),
         risetime_hr = as.numeric(riseendtime - risestarttime),
         stormdoy = yday(risestarttime)) %>%
  group_by(pond, year) %>%
  arrange(stormdoy) %>%
  mutate(nextstormstart = lead(risestarttime),
         nextstormstart = if_else(is.na(nextstormstart),risestarttime+days(10),nextstormstart),
         endstorm_doy = yday(nextstormstart)) %>%
  ungroup()

 #This will be the base to which we add all of the other variables

#Add in calculated quantitative pigment responses######
calcresp <- read.csv("./Output and Intermediate Files/Storm Quantitative Responses.csv")

join1 <- left_join(calcresp,storms, by = join_by(stormdoy, year, pond))


#Storm traits#####
#Read in CoCoRaHS rainfall data
rain <- read.csv('./Input Data Files/CoCoRaHS Data Clean.csv') %>%
  #Filter to the stations closest to the ponds (equal distance so we use both) and remove multiday observations
  filter(station %in% c('WI-DA-46','WI-DA-60') & is.na(multiday)) %>%
  #Average rainfall together if both stations reported
  group_by(date) %>%
  summarize(precip_mm = mean(precip_mm, na.rm = T),
            newsnow_mm = mean(newsnow_mm, na.rm = T)) %>%
  mutate(date = as.POSIXct(date),
         doy = yday(date),
         year = year(date))

#Calculate how much liquid rain has fallen so far that open water season 
#to do this, we will make an assumption that days with snowfall had no liquid rain and will exclude these precip amounts in our sum - doing this because most stations do not report SWE to subtract the snow out of the precip total
liquidrain <- rain %>%
  #Get rid of days with measurable snowfall
  filter(!(newsnow_mm > 0) | is.na(newsnow_mm)) %>%
  group_by(year) %>%
  mutate(precip_mm = ifelse(is.nan(precip_mm),0,precip_mm),
         cumprecip = cumsum(precip_mm)) %>%
  ungroup() %>%
  select(year, doy, cumprecip)

#Add cumulative rain to main dataframe
join2 <- left_join(join1, liquidrain, by = join_by(stormdoy == doy, year)) %>%
  #Add column of time since last storm event - group by prevents confusion with different years and ponds
  group_by(pond,year) %>%
  arrange(stormdoy) %>%
  #Also add days since previous storm
  mutate(dayssinceprevstorm = stormdoy - lag(stormdoy)) %>%
  #For the first storm we have data for each year, there wouldn't be a previous storm to index this to in the dataframe. Add in that data here if we have it:
  #2022 145 - Prior storm on 112 based on CoCoRas data  = 34
  #2023 186 - Prior storm on April16 DOY 106 - time to prior = 80
  #2024 92 - Prior Storm on DOY 86 = 6
  ungroup() %>%
  arrange(stormnum) %>%
  mutate(dayssinceprevstorm = ifelse(!is.na(dayssinceprevstorm), dayssinceprevstorm, 
                                     ifelse(stormdoy == '92', "6", 
                                            ifelse(stormdoy == '185', '57',
                                                   ifelse(stormdoy == '209', '16',
                                                          ifelse(stormdoy == '145', '34', NA))))))

#Amount of rain occurring in the previous 10 days
prestorm_rain <- data.frame(stormnum = as.character(),
                            rain_10daysprev = as.numeric())

for(i in 1:nrow(join2)){
  sub <- rain %>% filter(year == join2$year[i] & doy >= join2$raindoy[i]-10 & doy < join2$raindoy[i])
  
  prestorm_rain_sub <- data.frame(stormnum = NA,
                              rain_10daysprev = NA)
  
  prestorm_rain_sub$stormnum <- join2$stormnum[i]
  prestorm_rain_sub$rain_10daysprev <- sum(sub$precip_mm, na.rm = T)
  
  if(i == 1){prestorm_rain <- prestorm_rain_sub}
  if(i > 1){prestorm_rain <- rbind(prestorm_rain, prestorm_rain_sub)}
}

join2half <- left_join(join2, prestorm_rain)
#Success - all storm based metrics now added

#Stratification Conditions######
#Read in temperature chain data
tchain <- read.csv('./Input Data Files/TChain Full Dataset.csv') %>%
  filter(pond %in% c('Strickers','Tiedemans')) %>%
  mutate(datetime = as.POSIXct(datetime),
         doy = yday(datetime),
         year = year(datetime))

unique(tchain$depth) #Check what the out of water ones were tagged as to remove them
tchain <- tchain %>% 
  #Remove depths that were out of water
  filter(!(depth %in% c('At','Atmosphere')) & temp_C < 50)

#Tiedeman's tchain has static positioning on the dock rather than floating at the surface, so need to look at water depth to assign true depths to the temp pendants. 
#Different years have different deployment schemes (facepalm) so each will need a different adjustment
#2024: 1.5 m depth on tchain = bottom of pond
#2023: 1.25 m depth on tchain = bottom of pond
#2022: 1 m depth on tchain = bottom of pond

#Read in raw depth data to know hwo deep Tied was for corrections
depth <- read.csv('./Input Data Files/Water Depth Manual Calculations Full Dataset.csv') %>%
  mutate(datetime = as.POSIXct(datetime)) %>%
  filter(pond %in% c('Strickers','Tiedemans'))


#2024 Corrections:
#Subset depths to only 2024
tieddepth24 <- depth %>% 
  filter(pond == 'Tiedemans' & year(datetime) == '2024') %>%
  select(datetime, pond, depth_m) %>%
  rename(maxdepth_m = depth_m)

#Subset tchain to only 2024
tiedtemp24 <- tchain %>%
  filter(pond == 'Tiedemans' & year == '2024') %>%
  rename(tchain_depth = depth) %>%
  mutate(tchain_depth = as.numeric(tchain_depth))

#Create adjustment table: adjustfac = the amount that should be subtracted from the pond's max depth to get the depth at which that sensor is deployed
adjust24 <- data.frame(tchain_depth = c(0, 0.25, 0.5, 0.75,1, 1.25, 1.5),
                   adjustfac = c(1.5,1.25,1,0.75,0.5,0.25,0))

#Combine the dataframes
tied24 <- left_join(tiedtemp24, tieddepth24)
tied24 <- left_join(tied24, adjust24) %>%
  #Calculate the actual depth of the sensor and assign sensors out of the water a position of 'Atm'
  mutate(sensor_depth = maxdepth_m - adjustfac,
         sensor_depth = ifelse(sensor_depth <= 0.02, 'Atm', sensor_depth))

#2023 Corrections
#Subset depths to only 2023
tieddepth23 <- depth %>% 
  filter(pond == 'Tiedemans' & year(datetime) == '2023') %>%
  select(datetime, pond, depth_m) %>%
  rename(maxdepth_m = depth_m)

#Subset tchain to only 2023
tiedtemp23 <- tchain %>%
  filter(pond == 'Tiedemans' & year == '2023') %>%
  rename(tchain_depth = depth) %>%
  mutate(tchain_depth = as.numeric(tchain_depth))

#Create adjustment table: adjustfac = the amount that should be subtracted from the pond's max depth to get the depth at which that sensor is deployed
adjust23 <- data.frame(tchain_depth = c(0, 0.25, 0.5, 0.75, 1, 1.25),
                       adjustfac = c(1.25,1,0.75,0.5,0.25,0))

#Combine the dataframes
tied23 <- left_join(tiedtemp23, tieddepth23)
tied23 <- left_join(tied23, adjust23) %>%
  #Calculate the actual depth of the sensor and assign sensors out of the water a position of 'Atm'
  mutate(sensor_depth = maxdepth_m - adjustfac,
         sensor_depth = ifelse(sensor_depth <= 0.02, 'Atm', sensor_depth))

#Combine the years of interest for Tied and reformat to match Strickers
tiedtemps <- rbind(tied23, tied24) %>%
  filter(!(sensor_depth == 'Atm')) %>%
  select(datetime, temp_C, light_lux, pond, sensor_depth, doy, year, tchain_depth) %>%
  rename(depth = sensor_depth,
         sensorid = tchain_depth)
  
#Get Strickers temps to merge with updated Tied positions
stricktemps <- tchain %>% filter(pond == "Strickers") %>%
  mutate(sensorid = depth)

#Combine Strickers and Tiedemans back together
temp <- rbind(stricktemps, tiedtemps)  %>%
  mutate(depth = as.numeric(depth))


#Get chunks of data that correspond with the storm time periods - going to do this by building a data frame of time frames to be interested in based on the hydrograph data
#Want to look at the hour before and hour after flushing portions of hydrographs - making the assumption that if the strat got disturbed, it would probably occur before the water level started going bakc down
stratchunks <- join1 %>% 
  mutate(starttime_pre = risestarttime - (60*60), #60s come from needing to add in seconds rather than hours
         endtime_post = riseendtime + (60*60))

#Then filter the data frames to only those identified period one hour pre and post storm rise
ponds <- unique(storms$pond)
  
for(i in 1:length(ponds)){
  pondtempsub <- temp %>% filter(pond == ponds[i])
  pondstorms <- stratchunks %>% filter(pond == ponds[i])
  
  for(j in 1:nrow(pondstorms)){
    pre_time_start <- pondstorms$starttime_pre[j]
    pre_time_end <- pondstorms$starttime[j]
    post_time_start <- pondstorms$endtime[j]
    post_time_end <- pondstorms$endtime_post[j]
    stormnum <- pondstorms$stormnum[j]
    
    prestormsub <- pondtempsub %>% filter(datetime >= pre_time_start & datetime <= pre_time_end) %>%
      mutate(period = 'pre',
             stormnum = stormnum)
    poststormsub <- pondtempsub %>% filter(datetime >= post_time_start & datetime <= post_time_end) %>%
      mutate(period = 'post',
             stormnum = stormnum)
    
    if(i == 1 & j == 1){stormtempsub <- rbind(prestormsub, poststormsub)}
    if(i > 1 | j > 1){stormtempsub <- rbind(stormtempsub, prestormsub, poststormsub)}
  }
}

tempdiffs <- stormtempsub %>%
  #Group Together to Get average temps are each depth during this time period
  group_by(stormnum, pond, period, sensorid) %>%
  summarize(mntemp_C = mean(temp_C, na.rm = T),
            mndepth_m = mean(depth, na.rm = T)) %>%
  #keep only the top and bottom sensors from each time point
  filter(mndepth_m == max(mndepth_m) |mndepth_m == min(mndepth_m)) %>%
  #assign top or bottom 
  mutate(position = ifelse(mndepth_m == max(mndepth_m), 'bottom','top')) %>%
  select(-sensorid, -mndepth_m) %>%
  #Move into columns to make calcualting stratified or not easier
  pivot_wider(names_from = c(position, period), values_from = mntemp_C) %>%
  #Calculate top vs bottom temp differences and change in difference pre vs post storm rise
  mutate(pre_tempdiff = top_pre - bottom_pre,
         post_tempdiff = top_post - bottom_post,
         delta_tempdiff = post_tempdiff - pre_tempdiff) %>%
  ungroup() %>%
  select(stormnum, pond, pre_tempdiff, post_tempdiff, delta_tempdiff)

#Join with main sheet
join3 <- left_join(join2half, tempdiffs)

#Daylight Length Data#####
#Read in individual years' data, get into useable format, and combine years
day22 <- read.csv("./Input Data Files/Middleton Daylight 2022.csv") %>%
  pivot_longer(Jan.:Dec., names_to = "month", values_to = 'daylength') %>%
  filter(daylength != '') %>%
  separate_wider_delim(daylength, delim = ':', names = c('hours','minutes')) %>%
  mutate(daylength_hr = as.numeric(hours) + as.numeric(minutes)/60,
         month = substr(month, 1, 3)) %>%
  mutate(year = '2022',
         date = as.POSIXct(paste(month,Day,year), format = "%b %d %Y"),
         doy = yday(date)) %>%
  select(doy, year, daylength_hr) %>%
  arrange(doy)



day23 <- read.csv("./Input Data Files/Middleton Daylight 2023.csv") %>%
  pivot_longer(Jan.:Dec., names_to = "month", values_to = 'daylength') %>%
  filter(daylength != '') %>%
  separate_wider_delim(daylength, delim = ':', names = c('hours','minutes')) %>%
  mutate(daylength_hr = as.numeric(hours) + as.numeric(minutes)/60,
         month = substr(month, 1, 3)) %>%
  mutate(year = '2023',
         date = as.POSIXct(paste(month,Day,year), format = "%b %d %Y"),
         doy = yday(date)) %>%
  select(doy, year, daylength_hr) %>%
  arrange(doy)

day24 <- read.csv("./Input Data Files/Middleton Daylight 2024.csv") %>%
  pivot_longer(Jan.:Dec., names_to = "month", values_to = 'daylength') %>%
  filter(daylength != '') %>%
  separate_wider_delim(daylength, delim = ':', names = c('hours','minutes')) %>%
  mutate(daylength_hr = as.numeric(hours) + as.numeric(minutes)/60,
         month = substr(month, 1, 3)) %>%
  mutate(year = '2024',
         date = as.POSIXct(paste(month,Day,year), format = "%b %d %Y"),
         doy = yday(date)) %>%
  select(doy, year, daylength_hr) %>%
  arrange(doy)

daylight <- rbind(day22, day23, day24) %>%
  mutate(year = as.numeric(year))

#Get average daylight hours during response period
stormresp_daylight <- data.frame(stormnum = as.character(),
                                 daylight_hr = as.numeric())


for(i in 1:nrow(join3)){
  sub <- daylight %>%
    filter(year == join3$year[i] & doy >= join3$stormdoy[i] & doy <= join3$endstorm_doy[i] & doy <= join3$stormdoy[i] +10) 
  
  stormresp_daylight_sub <- data.frame(stormnum = NA,
                                       daylight_hr = NA)
  
  stormresp_daylight_sub$stormnum <- join3$stormnum[i]
  stormresp_daylight_sub$daylight_hr <- mean(sub$daylength_hr, na.rm = T)
  
  if(i == 1){stormresp_daylight <- stormresp_daylight_sub}
  if(i > 1){stormresp_daylight <- rbind(stormresp_daylight, stormresp_daylight_sub)}
}

join4 <- left_join(join3, stormresp_daylight)


#Water Temperature from EXOs######
#Get storm response period average EXO temp
exo <- read.csv('./Input Data Files/EXO Full dataset.csv') %>%
  mutate(datetime = as.POSIXct(datetime))

stormresp_watertemp <- data.frame(stormnum = as.character(),
                                  avgwatertemp_C = as.numeric())

for(i in 1:nrow(join4)){
  sub <- exo %>%
    filter(pond == join4$pond[i] & datetime >= join4$risestarttime[i] & datetime <= join4$nextstormstart[i] & datetime <= join4$risestarttime[i] + days(10))
  
  stormresp_watertemp_sub <- data.frame(stormnum = NA,
                                        avgwatertemp_C = NA)
  
  stormresp_watertemp_sub$stormnum <- join4$stormnum[i]
  stormresp_watertemp_sub$avgwatertemp_C <- mean(sub$temp_C, na.rm = T)
  
  if(i == 1){stormresp_watertemp <- stormresp_watertemp_sub}
  if(i > 1){stormresp_watertemp <- rbind(stormresp_watertemp, stormresp_watertemp_sub)}
}

join5 <- left_join(join4, stormresp_watertemp)

#Chemistry Changes#####

#Read in Lab Data
chem <- read.csv('./Input Data Files/Ponds Chemistry Jan 2025.csv') %>%
  filter(pond %in% c('S','T') & !(sampletype %in% c('BW','BP')) & site %in% c('0','9')) %>%
  select(year, doy, pond, site, sampletype, doc_mgl, tn_ugl, tp_ugl, no23_ugl, srp_ugl, nh4_ugl, chloride_mgl, x440) %>%
  mutate(pond = ifelse(pond == 'S', 'Strickers','Tiedemans'))

stormchem <- data.frame()

#Create loop to ID possible pre-post storm samples
for(i in 1:nrow(join1)){
  doy_loop <- join1$stormdoy[i]
  year_loop <- join1$year[i]
  eventid_loop <- join1$stormnum[i]
  pond_loop <- join1$pond[i]
  
  #window around storm want to be looking for samples from
  scan_days <- c(doy_loop-3,doy_loop-2,doy_loop-1,doy_loop,doy_loop+1,doy_loop+2,doy_loop+3)
  
  chemchunk <- chem %>% filter(year == year_loop & doy %in% scan_days & pond == pond_loop) %>%
    mutate(stormnum = eventid_loop,
           timetostorm = doy - doy_loop)
  
  if(i == 1){stormchem <- chemchunk}
  if(i > 1){stormchem <- rbind(stormchem, chemchunk)}
}

#Write to separate file and sort through in there - if there is a programable way to do this, its not coming to my brain right now. 
write.csv(stormchem, "./Output and Intermediate Files/Possible Pre or Post Storm Chemistry Samples.csv", row.names = F)

#Read back in those deemed true storm samples that had a pair
stormchem <- read.csv("./Input Data Files/Confirmed Pre or Post Storm Chemistry Samples.csv") %>%
  mutate(g440 = (2.303*x440)/(0.01)) %>%
  select(-site, -doy, -sampletype, -year, -x440, -timetostorm) %>% 
  pivot_wider(names_from = prepost, values_from = c(doc_mgl, tn_ugl, tp_ugl, no23_ugl, srp_ugl, nh4_ugl, chloride_mgl, g440)) %>%
  mutate(delta_doc = doc_mgl_post - doc_mgl_pre,
         delta_tn = tn_ugl_post - tn_ugl_pre,
         delta_tp = tp_ugl_post - tp_ugl_pre,
         delta_no23 = no23_ugl_post - no23_ugl_pre,
         delta_srp = srp_ugl_post - srp_ugl_pre,
         delta_nh4 = nh4_ugl_post - nh4_ugl_pre,
         delta_cl = chloride_mgl_post - chloride_mgl_pre,
         delta_g440 = g440_post - g440_pre) %>%
  select(stormnum, pond, delta_doc, delta_tn, delta_tp, delta_no23, delta_srp, delta_nh4, delta_cl, delta_g440)

#Join with larger dataset
join6 <- left_join(join5, stormchem) 

#Read back in post storm samples
poststormchem <- read.csv("./Input Data Files/Confirmed Post Storm Chemistry Samples.csv") %>%
  mutate(g440 = (2.303*x440)/(0.01)) %>%
  select(-site, -doy, -sampletype, -year, -timetostorm, -x440) %>%
  rename_with(~ paste0("post_", .x)) %>%
  rename(pond = post_pond, stormnum = post_stormnum)

join7 <- left_join(join6, poststormchem)

#Write out the whole dataframe to csv######
write.csv(join7, "./Output and Intermediate Files/Quantitative Pigment Responses and Potential Explanatory Variables.csv", row.names = F)

