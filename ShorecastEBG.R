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
#keras info
#Some helpful info to keep handy:
#https://keras.rstudio.com
#https://tensorflow.rstudio.com/keras/
#following: https://tensorflow.rstudio.com/blog/time-series-forecasting-with-recurrent-neural-networks.html

#remove the 3 date columns 
data <- data.matrix(Shore_Tide_Wave_time[,-1:-3])
#remove time column
data <- data.matrix(data[,-16])

#subtract mean of TS and divide by SD. 
training_data_length <- 2000
train_data <- data[1:training_data_length,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 400, step = 1) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,1]
    }            
    
    list(samples, targets)
  }
}

lookback <- 50
step <- 1
delay <- 1
batch_size <- 400

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = training_data_length,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = training_data_length+1,
  max_index = training_data_length+1000,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = training_data_length+1000+1,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- ((training_data_length+1000) - (training_data_length+1) - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - (training_data_length+1000+1) - lookback) / batch_size

#dropout model:

#model <- keras_model_sequential() %>% 
#  layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.2,
#            input_shape = list(NULL, dim(data)[[-1]])) %>% 
#  layer_dense(units = 1)

model <- keras_model_sequential() %>% 
  layer_lstm(units = 32, input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)


model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)
