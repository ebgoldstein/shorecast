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
  select(-one_of(c("Min","Sec")))

#drop mins and sec from shoreline data and rename shoreline position column.
shoreline <- shorelineData %>%
  rename('Average_Shoreline[m]' = 'Average[m]') %>%
  select(-one_of(c("Min","Sec")))

####

#join shoreline (x) to tide (y). 
Shore_and_Tide <- left_join(shoreline,tides,by = c("Year","Month","Day","Hour[NZST]"))

#simplest case is to join waves to this 
#(even though the waves at the moment may not be represenative of teh day (not min, not mean, not max,etc..)
Shore_Tide_Wave <- left_join(Shore_and_Tide,waves,by = c("Year","Month","Day","Hour[NZST]"))


#take the mean shoreline of each day

#take the max waves for that day


#join
#recover plots
