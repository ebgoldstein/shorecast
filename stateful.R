#Use LSTM to make shoreline change predictions
#local file structure is "Case1" and "Case 2" folders are within a folder entitled "Data"
#
#EBG 5/2018
#MIT license
#

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
#keras info
#Some helpful info to keep handy:
#https://keras.rstudio.com
#https://tensorflow.rstudio.com/keras/
#following: https://tensorflow.rstudio.com/blog/time-series-forecasting-with-recurrent-neural-networks.html

#remove the 3 date columns 
data <- data.matrix(Shore_Tide_Wave_time[,-1:-3])
#remove time column
data <- data.matrix(data[,-16])

#subtract using the training data, take the mean 
#and SD of all variables and normalize 
training_data_length <- 3000
testing_data_length <- nrow(data)-training_data_length
train_data <- data[1:training_data_length,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)


# [samples, timesteps, features]; (training_data_length, 1 ,1) 

forcing_training_data <- array(data = data[1:training_data_length,2:12], dim = c(training_data_length, 1, 1))
forcing_testing_data <- array(data = data[(training_data_length+1):nrow(data),2:12], dim = c(testing_data_length, 1, 1))

#  [samples, timesteps]  ;  (testing_data_length, 1 ,1) 
shoreline_training_data <- array(data = data[1:training_data_length,1], dim = c(training_data_length, 1))
shoreline_testing_data <- array(data = data[(training_data_length+1):nrow(data),1], dim = c(testing_data_length, 1))



lookback <- 50 # # of timesteps to refer to
step <- 1 #How many step between each sample  (1 day)
delay <- 1 #timesteps in the future for the target (1 day)
batch_size <- 3000 #samples per batch

model <- keras_model_sequential() %>% 
  layer_lstm(units = 32, 
             input_shape      = c(step, 1), 
             batch_size       = batch_size,
             return_sequences = FALSE,
             stateful = TRUE) %>% 
  layer_dense(units = 1)


model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% 
  fit(
    x = forcing_training_data,
    y = shoreline_training_data,
    steps_per_epoch = 1000,
    epochs = 20,
  )

plot(history)

shorecast <- model %>%
  predict(forcing_testing_data, 
  batch_size = testing_data_length) 
