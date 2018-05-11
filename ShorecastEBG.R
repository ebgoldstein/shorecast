#Use LSTM to make shoreline change predictions
#local file structure is "Case1" and "Case 2" folders are within a folder entitled "Data"
#
#EBG 5/2018
#MIT license
#
#Some helpful info to keep handy:
#https://keras.rstudio.com
#https://tensorflow.rstudio.com/keras/

library(keras)
library(tidyverse)
library(lubridate)

#CASE 1

#load the wave data
waves <- read_delim("Data/Case1/Waves/Wave hindcast_corrected.txt", delim = " ", local = locale(encoding = "latin1"))

#load the tide data
tides <- read_delim("Data/Case1/Tides/tide past.txt", delim = " ", local = locale(encoding = "latin1"))

#load shorelines
shorelineData <- read_delim("Data/Case1/Shorelines/Camera/Averaged shoreline data.txt", delim = " ", local = locale(encoding = "latin1"))

#load SL
####
#drop mins and sec wave data 
waves <- waves %>%
  select(-one_of(c("Min","Sec")))

#drop mins and sec tide data 
tides <- tides %>%
  rename(tide = 'tide[m]') %>%
  select(-one_of(c("Min","Sec")))

#drop mins and sec from shoreline data and rename shoreline position column.
shoreline <- shorelineData %>%
  rename(Average_Shoreline = 'Average[m]') %>%
  select(-one_of(c("Min","Sec")))

####
#for each day find the max + min tide
daily_tides <- tides %>% 
  group_by(Year,Month,Day) %>% 
  summarize(maxT=max(tide),minT=min(tide))

#find mean shoreline for each day
daily_shoreline <- shoreline %>% 
  group_by(Year,Month,Day) %>% 
  summarize(AverageShoreline=mean(Average_Shoreline))

#find max,min,mean waves,period, direction for each day (Hs,Tp, Dir)
#dir is not correct yet because of 360 to 0...
daily_waves <- waves %>%
  group_by(Year,Month,Day) %>%
  summarize(maxHs = max(`Hs[m]`),
            minHs = min(`Hs[m]`),
            meanHs = mean(`Hs[m]`),
            maxTs = max(`Tp[s]`),
            minTs = min(`Tp[s]`),
            meanTs = mean(`Tp[s]`),
            maxD = max(`Dir[°]`),
            minD = min(`Dir[°]`),
            meanD = mean(`Dir[°]`)
            )

########

#join shoreline (x) to tide (y). 
daily_shore_and_tide <- left_join(daily_shoreline,daily_tides,by = c("Year","Month","Day"))

#join waves
Shore_Tide_Wave <- left_join(daily_shore_and_tide,daily_waves,by = c("Year","Month","Day"))

#put a timestamp
Shore_Tide_Wave_time <- Shore_Tide_Wave %>%
  add_column(time = ymd(str_c(Shore_Tide_Wave$Year,'-',Shore_Tide_Wave$Month,'-',Shore_Tide_Wave$Day)))

####
#plots
ggplot(data= Shore_Tide_Wave_time) +
  geom_line(aes(time,AverageShoreline))

# ggplot(data= Shore_Tide_Wave_time) +
#   geom_line(aes(time,maxT)) +
#   geom_line(aes(time,minT))
# 
# ggplot(data= Shore_Tide_Wave_time) +
#   geom_line(aes(time,maxHs)) +
#   geom_line(aes(time,minHs))


####
#keras info will go here
