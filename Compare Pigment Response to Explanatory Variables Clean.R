#Compare Pigment Response to Explanatory Variables
#Author: Jess Briggs
#Date created: 2025-03-17

library(tidyverse)
library(corrplot) #colinearity matrix plots
library(MuMIn) #dredge function, Can calculate mixed models R2 

#Read in data########

stormresp <- read.csv('./Output and Intermediate Files/Quantitative Pigment Responses and potential Explanatory Variables.csv') %>%
  filter(!is.na(stormnum))

#Finalize model inputs (colinearity check) and standardize######
corrinputs <- stormresp %>%
  select(precip_mm, startdepth:risetime_hr, pre_chl_ugl, pre_pc_ugl, rise_chl_percch, rise_pc_percch, cumprecip, dayssinceprevstorm, daylight_hr, avgwatertemp_C)
corr_matrix <- cor(corrinputs, use = 'pairwise.complete.obs')

corrplot(corr_matrix)

#bounce is highly correlated with precip amount, prestorm depth highly correlated with time since storm
#Can remove bounce and prestorm depth from model inputs

#Standardize Model Input Variables
modsel_inputs <- stormresp %>% 
  select(pond, stormnum, year, stormdoy, post10_chl_percch, post10_pc_percch, post10_chl_slope, post10_pc_slope, delay_chl_slope, delay_pc_slope, rise_chl_percch, rise_pc_percch, rise_chl_slope, rise_pc_slope, precip_mm, risetime_hr, pre_chl_ugl, pre_pc_ugl, cumprecip, dayssinceprevstorm, daylight_hr, avgwatertemp_C) %>%
  mutate(year = as.factor(year),
         std_rise_chl_percch = rise_chl_percch,
         std_rise_pc_percch = rise_pc_percch)

#Apply scale function to all model inputs - standardizes data (centers and divides by stdev)
modsel_inputs[,15:24] <- scale(modsel_inputs[,15:24])

#Full Response Period Model selection of best explanatory variables#####
fullresp_modinputs <- modsel_inputs %>% filter(!is.na(post10_chl_percch))


#Chlorophyll
fullresp_fullmod <- lm(post10_chl_slope ~ (std_rise_chl_percch + precip_mm + risetime_hr + pre_chl_ugl + cumprecip + dayssinceprevstorm + daylight_hr)^2, data = fullresp_modinputs, na.action = 'na.fail')
fullresp_fullmodelselect <- dredge(fullresp_fullmod, trace=2, m.lim = c(1,6))

#All individual models have adjusted R2 less than 0
fullresp_prechl_mod <- lm(post10_chl_percch ~ pre_chl_ugl, data = fullresp_modinputs, na.action = 'na.fail')
fullresp_risetime_mod <- lm(post10_chl_percch ~ risetime_hr, data = fullresp_modinputs, na.action = 'na.fail')
fullresp_flush_mod <- lm(post10_chl_percch ~ std_rise_chl_percch, data = fullresp_modinputs, na.action = 'na.fail')


#Phycocyanin
pc_fullresp_fullmod <- lm(post10_pc_slope ~ (std_rise_pc_percch + precip_mm + risetime_hr + pre_pc_ugl + cumprecip + dayssinceprevstorm + daylight_hr)^2, data = fullresp_modinputs, na.action = 'na.fail')
pc_fullresp_fullmodelselect <- dredge(pc_fullresp_fullmod, trace=2, m.lim = c(1,6))

#Examine best model
pc_full_bestmod <- lm(post10_pc_slope ~ pre_pc_ugl + risetime_hr  + std_rise_pc_percch + pre_pc_ugl:risetime_hr + pre_pc_ugl:std_rise_pc_percch, data = fullresp_modinputs, na.action = 'na.fail')
summary(pc_full_bestmod)

#and interaction terms
stddevs <- c(-2,-1,0,1,2)
slopes_risetimevary <- pc_full_bestmod$coefficients[2] + pc_full_bestmod$coefficients[5]*stddevs
slopes_risetimevary
#When risetimes are faster and material moves into and out of pond faster, pre storm pc conditions matter more
slopes_risepcchvary <- pc_full_bestmod$coefficients[2] + pc_full_bestmod$coefficients[6]*stddevs
slopes_risepcchvary
#when the perc change in pc is higher, pre_chl matters more - aka when there is  less of a flush then pre-storm pc  more important

#Delay Response Period Model selection of best explanatory variables#####

delayresp_modinputs <- modsel_inputs %>% filter(!is.na(delay_chl_slope))


#Chlorophyll
delayresp_fullmod <- lm(delay_chl_slope ~ (std_rise_chl_percch + precip_mm + risetime_hr + pre_chl_ugl + cumprecip + dayssinceprevstorm + daylight_hr)^2, data = delayresp_modinputs, na.action = 'na.fail')
delayresp_fullmodelselect <- dredge(delayresp_fullmod, trace=2, m.lim = c(1,6))

#Examine models with delta AICc < 2
delayresp_bestmod <- lm(delay_chl_slope ~ std_rise_chl_percch + dayssinceprevstorm + std_rise_chl_percch:dayssinceprevstorm, data = delayresp_modinputs, na.action = 'na.fail')
summary(delayresp_bestmod)

delayresp_mod2 <- lm(delay_chl_slope ~ cumprecip*dayssinceprevstorm + std_rise_chl_percch + dayssinceprevstorm: std_rise_chl_percch, data = delayresp_modinputs, na.action = 'na.fail')
summary(delayresp_mod2)

delayresp_mod3 <- lm(delay_chl_slope ~ daylight_hr*dayssinceprevstorm + std_rise_chl_percch + dayssinceprevstorm:daylight_hr, data = delayresp_modinputs, na.action = 'na.fail')
summary(delayresp_mod3)

delayresp_mod4 <- lm(delay_chl_slope ~ precip_mm + dayssinceprevstorm + std_rise_chl_percch + dayssinceprevstorm: std_rise_chl_percch, data = delayresp_modinputs, na.action = 'na.fail')
summary(delayresp_mod4)

delayresp_flushrespmod <- lm(delay_chl_slope ~ std_rise_chl_percch, data = delayresp_modinputs, na.action = 'na.fail')

#Understanding the interaction term here
at.dayssinceprevstorm <- c(-1,0,1,2)
slopes <- delayresp_bestmod$coefficients[2] + delayresp_bestmod$coefficients[4]*at.dayssinceprevstorm
slopes
#What this means: as the length of time between storms increases, the extent of the flush matters more


#Phycocyanin
pc_delayresp_fullmod <- lm(delay_pc_slope ~ (std_rise_pc_percch + precip_mm + risetime_hr + pre_pc_ugl + cumprecip + dayssinceprevstorm + daylight_hr)^2, data = delayresp_modinputs, na.action = 'na.fail')
pc_delayresp_fullmodelselect <- dredge(pc_delayresp_fullmod, trace=2, m.lim = c(1,6))

#Examine best model - very similar to full resp period
pc_delay_bestmod <- lm(delay_pc_slope ~ pre_pc_ugl + risetime_hr  + std_rise_pc_percch + pre_pc_ugl:risetime_hr + pre_pc_ugl:std_rise_pc_percch + risetime_hr:std_rise_pc_percch, data = delayresp_modinputs, na.action = 'na.fail')
summary(pc_delay_bestmod)

#Rise Response Period Model selection of best explanatory variables#####
riseresp_modinputs <- modsel_inputs %>% filter(!is.na(rise_chl_percch))


#Chlorophyll
riseresp_fullmod <- lm(rise_chl_percch ~ (precip_mm + cumprecip + dayssinceprevstorm + daylight_hr)^2, data = riseresp_modinputs, na.action = 'na.fail')
riseresp_fullmodelselect <- dredge(riseresp_fullmod, trace=2, m.lim = c(1,6))

#Examine models with delta AICc <2
riseresp_bestmod1 <- lm(rise_chl_percch ~ precip_mm + dayssinceprevstorm * cumprecip, data = riseresp_modinputs, na.action = 'na.fail')
riseresp_bestmod2 <- lm(rise_chl_percch ~ precip_mm + dayssinceprevstorm + cumprecip, data = riseresp_modinputs, na.action = 'na.fail')
riseresp_bestmod3 <- lm(rise_chl_percch ~ dayssinceprevstorm * cumprecip, data = riseresp_modinputs, na.action = 'na.fail')
summary(riseresp_bestmod1)
summary(riseresp_bestmod2)
summary(riseresp_bestmod3)

#Phycocyanin
pc_riseresp_fullmod <- lm(rise_pc_percch ~ (precip_mm + cumprecip + dayssinceprevstorm + daylight_hr)^2, data = riseresp_modinputs, na.action = 'na.fail')
pc_riseresp_fullmodelselect <- dredge(pc_riseresp_fullmod, trace=2, m.lim = c(1,6))

#Examine models with delta AICc <2
pc_riseresp_mod1 <- lm(rise_pc_percch ~ cumprecip + dayssinceprevstorm + precip_mm, data = riseresp_modinputs, na.action = 'na.fail')
pc_riseresp_mod2 <- lm(rise_pc_percch ~ cumprecip  + precip_mm, data = riseresp_modinputs, na.action = 'na.fail')
pc_riseresp_mod3 <- lm(rise_pc_percch ~ cumprecip + daylight_hr + precip_mm, data = riseresp_modinputs, na.action = 'na.fail')
pc_riseresp_mod4 <- lm(rise_pc_percch ~ cumprecip, data = riseresp_modinputs, na.action = 'na.fail')

summary(pc_riseresp_mod1)
summary(pc_riseresp_mod2)
summary(pc_riseresp_mod3)
summary(pc_riseresp_mod4)

#Differences between ponds#####
#Get nicer dataframe to compare
pondsub <- stormresp %>%
  select(pond, stormdoy, year, rise_points, rise_chl_percch, post10_chl_percch, delay_chl_slope)

tiedsub <- pondsub %>% filter(pond == 'Tiedemans') %>%
  rename(t_rise_points = rise_points,
         t_rise_chl_percch = rise_chl_percch,
         t_post10_chl_percch = post10_chl_percch, 
         t_delay_chl_slope = delay_chl_slope) %>%
  select(-pond)
stricksub <- pondsub %>% filter(pond == 'Strickers') %>%
  rename(s_rise_points = rise_points,
         s_rise_chl_percch = rise_chl_percch,
         s_post10_chl_percch = post10_chl_percch, 
         s_delay_chl_slope = delay_chl_slope) %>%
  select(-pond)

pondsub2 <- full_join(stricksub, tiedsub) %>%
  filter(!is.na(t_rise_points))

#Normality test
shapiro.test(pondsub2$s_rise_chl_percch - pondsub2$t_rise_chl_percch)
shapiro.test(pondsub2$s_post10_chl_percch - pondsub2$t_post10_chl_percch)
shapiro.test(pondsub2$s_delay_chl_slope - pondsub2$t_delay_chl_slope) #Not normal - these ones need to be assessed with nonparametric tests

#T-tests with subbed data
#rise
t.test(pondsub2$s_rise_chl_percch, pondsub2$t_rise_chl_percch, paired = F, alternative = 'two.sided') #Not different
t.test(pondsub2$s_post10_chl_percch, pondsub2$t_post10_chl_percch, paired = F, alternative = 'two.sided') #Not diff
wilcox.test(pondsub2$s_delay_chl_slope, pondsub2$t_delay_chl_slope, paired = F, alternative = 'two.sided') #Not diff

#Ttests with all data
t.test(tiedsub$t_rise_chl_percch, stricksub$s_rise_chl_percch, paired = F, alternative = 'two.sided')
t.test(tiedsub$t_post10_chl_percch, stricksub$s_post10_chl_percch, paired = F, alternative = 'two.sided')
t.test(tiedsub$t_delay_chl_slope, stricksub$s_delay_chl_slope, paired = F, alternative = 'two.sided')
