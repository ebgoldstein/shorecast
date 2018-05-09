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

#CASE 1

#load the wave data
waves <- read_delim("Data/Case1/Waves/Wave hindcast_corrected.txt", delim = " ", local = locale(encoding = "latin1"))

#load the tide data
tides <- read_delim("Data/Case1/Tides/tide past.txt", delim = " ", local = locale(encoding = "latin1"))

#load shorelines
shoreline <- read_delim("Data/Case1/Shorelines/Camera/Averaged shoreline data.txt", delim = " ", local = locale(encoding = "latin1"))

#load SL

#join
#recover plots
