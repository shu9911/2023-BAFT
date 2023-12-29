library(fabletools) 
library(fable) 
library(tsibble) 
library(feasts) 
library(slider)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(readr)  
library(scales)
library(ggplot2)
library(zoo)
library(ggfortify)
library(stats)
library(fpp3)
library(reshape2)
library(scales)

# load data and check gaps
hsinchu <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Hsinchu.csv") |>
  select(c("trnOpDate", "totalPass")) 
hsinchu$trnOpDate <- ymd(hsinchu$trnOpDate)
hsinchu <- hsinchu |>
  as_tsibble(index = trnOpDate)
has_gaps(hsinchu, .full = TRUE)

zhunan <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Zhunan.csv") |>
  select(c("trnOpDate", "totalPass"))
zhunan$trnOpDate <- ymd(zhunan$trnOpDate)
zhunan <- zhunan |>
  as_tsibble(index = trnOpDate)
has_gaps(zhunan, .full = TRUE)


zhudong <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Zhudong.csv") |>
  select(c("trnOpDate", "totalPass"))
zhudong$trnOpDate <- ymd(zhudong$trnOpDate)
zhudong <- zhudong |>
  as_tsibble(index = trnOpDate)
zhudong |> has_gaps(.full = TRUE)
zhudong |> scan_gaps() #gap: 2007-08-18

neiwan <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Neiwan.csv") |>
  select(c("trnOpDate", "totalPass")) 
neiwan$trnOpDate <- ymd(neiwan$trnOpDate)
neiwan <- neiwan |>
  as_tsibble(index = trnOpDate)
neiwan |> has_gaps(.full = TRUE)
neiwan |> scan_gaps()  #gap: 2005-07-18, 2007-07-18, 2015-08-18

# combine data
station.all <- hsinchu |> 
  left_join(zhunan, by = "trnOpDate") |>
  left_join(zhudong, by = "trnOpDate") |>
  left_join(neiwan, by = "trnOpDate") |>
  filter_index("2017-11-01"~.) |>
  rename(time = trnOpDate, Hsinchu = totalPass.x, Zhunan = totalPass.y, 
         Zhudong = totalPass.x.x, Neiwan = totalPass.y.y)
str(station.all)
summary(station.all)
station.all |> has_gaps(.full = TRUE)

# ------------------------------------------------------------------------------
# decomposition
Hsinchu.comp <- station.all |>
  model(stl1 = STL(Hsinchu)) |>
  components() |>
  autoplot()

Zhunan.comp <- station.all |>
  model(stl1 = STL(Zhunan)) |>
  components() |>
  autoplot()

Zhudong.comp <- station.all |>
  model(stl1 = STL(Zhudong)) |>
  components() |>
  autoplot()

Neiwan.comp <- station.all |>
  model(stl1 = STL(Neiwan)) |>
  components() |>
  autoplot()

grid.arrange(Hsinchu.comp, Zhunan.comp, Zhudong.comp, Neiwan.comp,
             nrow = 2, ncol = 2)

# ------------------------------------------------------------------------------
# convert to long format
station.long <- station.all |>
  melt(id.vars = "time", 
       variable.name = "train_id",
       measure.vars = c("Hsinchu", "Zhunan", "Zhudong", "Neiwan"),
       value.name = "total_pass") |>
  as_tsibble(index = time, key = train_id) |>
  filter_index("2017-11-01"~.) |>
  arrange(time)
str(station.long)

# plot 4 series
series.plot <- station.long |>
  autoplot(.vars = total_pass, color = "black") +
  facet_wrap(~ train_id, scales = "free_y") +
  labs(title = "Daily Passengers of 4 Stations", x = "Date", y = "Total Passengers") +
  theme(legend.position = "none") 
series.plot

# ------------------------------------------------------------------------------
# add dummy
# covid = 1, start = 2020/1/1, end = 2022/12/31
station.long <- station.long |>
  mutate(covid = ifelse(time > ymd("2019-12-31") & time < ymd("2023-01-01"), 1, 0))

station.long$covid <- station.long$covid |>
  factor(levels = c(0, 1))
str(station.long)
head(station.long)

# ------------------------------------------------------------------------------
# data partition
station.train <- station.long |> filter_index(~ "2022-10-31")
station.valid <- station.long |> filter_index("2022-11-01" ~.)

# ------------------------------------------------------------------------------
# modeling with roll-forward(one-day-ahead) validation
fits <- station.train |>
  group_by(train_id) |>
  model(
    naive = NAIVE(total_pass),
    snaive = SNAIVE(total_pass),
    ets = ETS(total_pass),
    reg = TSLM(total_pass ~ trend() + season() + covid),
    arima = ARIMA(total_pass ~ covid) 
  )
fits.arima <- fits |> select(train_id, arima)
augmented.train <- augment(fits)

fits.roll <- refit(fits, station.long) |>
  augment() |>
  filter(time > "2022-10-31") 

# performance measures
train.accuracy <- accuracy(fits)
train.accuracy

valid.roll.accuracy <- fits.roll |>
  as_tibble() |> # tsibble objects maintain each date separately
  group_by(train_id, .model) |>
  summarise(
    MAE = mean(abs(.resid), na.rm = TRUE),
    RMSE = sqrt(mean((.resid)^2, na.rm = TRUE)),
    MAPE = mean(abs((.resid) / total_pass), na.rm = TRUE) * 100
  )
valid.roll.accuracy
combined.all.roll <- bind_rows(augmented.train, fits.roll)

# performance charts
# plots in groups of stations
Hsinchu.models <- combined.all.roll |>
  filter(train_id == "Hsinchu") |>
  group_by(.model)

Hsinchu.models.plot <- Hsinchu.models |>
  autoplot(.vars = total_pass) +
  scale_color_manual(values = c("total_pass" = "black")) +
  autolayer(Hsinchu.models, .fitted, color = "blue", alpha = 0.7) +
  facet_wrap(~ .model, scales = "free_y") +
  labs(x = "Date", y = "Total Daily Passengers", title = "Hsinchu Models Plot") +
  theme(legend.position = "none")
Hsinchu.models.plot

Hsinchu.error.boxplot <- Hsinchu.models |>
  ggplot(aes(x = .model, y = .resid)) + 
  geom_boxplot() +
  labs(x = "Model", y = "Residuals", title = "Hsinchu Error BoxPlot")
Hsinchu.error.boxplot

Zhunan.models <- combined.all.roll |>
  filter(train_id == "Zhunan") |>
  group_by(.model)

Zhunan.models.plot <- Zhunan.models |>
  autoplot(.vars = total_pass) +
  scale_color_manual(values = c("total_pass" = "black")) +
  autolayer(Zhunan.models, .fitted, color = "blue", alpha = 0.7) +
  facet_wrap(~ .model, scales = "free_y") +
  labs(x = "Date", y = "Total Daily Passengers", title = "Zhunan Models Plot") +
  theme(legend.position = "none")
Zhunan.models.plot

Zhunan.error.boxplot <- Zhunan.models |>
  ggplot(aes(x = .model, y = .resid)) + 
  geom_boxplot() +
  labs(x = "Model", y = "Residuals", title = "Zhunan Error BoxPlot")
Zhunan.error.boxplot

Zhudong.models <- combined.all.roll |>
  filter(train_id == "Zhudong") |>
  group_by(.model)

Zhudong.models.plot <- Zhudong.models |>
  autoplot(.vars = total_pass) +
  scale_color_manual(values = c("total_pass" = "black")) +
  autolayer(Zhudong.models, .fitted, color = "blue", alpha = 0.7) +
  facet_wrap(~ .model, scales = "free_y") +
  labs(x = "Date", y = "Total Daily Passengers", title = "Zhudong Models Plot") +
  theme(legend.position = "none")
Zhudong.models.plot

Zhudong.error.boxplot <- Zhudong.models |>
  ggplot(aes(x = .model, y = .resid)) + 
  geom_boxplot() +
  labs(x = "Model", y = "Residuals", title = "Zhudong Error BoxPlot")
Zhudong.error.boxplot

Neiwan.models <- combined.all.roll |>
  filter(train_id == "Neiwan") |>
  group_by(.model)

Neiwan.models.plot <- Neiwan.models |>
  autoplot(.vars = total_pass) +
  scale_color_manual(values = c("total_pass" = "black")) +
  autolayer(Neiwan.models, .fitted, color = "blue", alpha = 0.7) +
  facet_wrap(~ .model, scales = "free_y") +
  labs(x = "Date", y = "Total Daily Passengers", title = "Neiwan Models Plot") +
  theme(legend.position = "none")
Neiwan.models.plot

Neiwan.error.boxplot <- Neiwan.models |>
  ggplot(aes(x = .model, y = .resid)) + 
  geom_boxplot() +
  labs(x = "Model", y = "Residuals", title = "Neiwan Error BoxPlot")
Neiwan.error.boxplot

# ------------------------------------------------------------------------------
# select best model
bestrmse.roll <- valid.roll.accuracy |>
  group_by(train_id) |>
  filter(RMSE == min(RMSE)) |>
  select(train_id, .model)
bestrmse.roll

best.models.valid.roll.accuracy <- valid.roll.accuracy |>
  semi_join(bestrmse.roll, by = c("train_id", ".model"))

best.models.roll.results <- combined.all.roll |>
  semi_join(bestrmse.roll, by = c("train_id", ".model"))

# performance charts of best models
best.models.plot <-
  best.models.roll.results |> 
  autoplot(.vars = total_pass) +
  #scale_color_manual(values = c("total_pass" = "black")) +
  autolayer(best.models.roll.results, .fitted, color = "blue", alpha = 0.7) +
  facet_wrap(~ train_id, scales = "free_y") +
  labs(x = "Date", y = "Total Daily Passengers", title = "The Best Models Plot") +
  scale_fill_identity(name = "Total Pass", guide = "legend",labels = c('m1')) +
  scale_color_manual(name = "Total Pass",
                     breaks = c("Actual", "Forecast"),
                     values = c("Actual" = "black", "Forecast" = "blue")) +
  theme(legend.position = "bottom")
best.models.plot

# -------------------------------------------------------------------------------
# combine training and validation period
fits.all <- station.long |>
  group_by(train_id) |>
  model(arima = ARIMA(total_pass ~ covid))
augmented.train.all <- augment(fits.all)

fits.all.roll <- refit(fits.all, station.long) 
fits.all.roll.augmented <- fits.all.roll |>
  augment() |>
  group_by(train_id)

all.roll.accuracy <- fits.all.roll.augmented |>
  as_tibble() |> 
  group_by(train_id, .model) |>
  summarise(
    MAE = mean(abs(.resid), na.rm = TRUE),
    RMSE = sqrt(mean((.resid)^2, na.rm = TRUE)),
    MAPE = mean(abs((.resid) / total_pass), na.rm = TRUE) * 100
  )
all.roll.accuracy
str(fits.all.roll.augmented)

# -------------------------------------------------------------------------------
# forecast future values
station.future <- new_data(station.long, 30) |>
  mutate(covid = 0)
station.future$covid <- station.future$covid |>
  factor(levels = c(0, 1))
str(station.future)

fc.Nov <- fits.all.roll |>
  forecast(new_data = station.future) |>
  relocate(time, .before = train_id) |>
  arrange(time)

# plot the future values
fc.plot <- fc.Nov |> 
  autoplot(.vars = .mean) +
  labs(x = "Date", y = "Total Daily Passengers", 
       title = "Daily Passenger Forecast for Nov 2023")
fc.plot

# -------------------------------------------------------------------------------
# load data in Nov 2023
hsinchu.new <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Hsinchu_new.csv") |>
  select(c("trnOpDate", "totalPass")) 
hsinchu.new$trnOpDate <- ymd(hsinchu.new$trnOpDate)

zhunan.new <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Zhunan_new.csv") |>
  select(c("trnOpDate", "totalPass")) 
zhunan.new$trnOpDate <- ymd(zhunan.new$trnOpDate)

zhudong.new <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Zhudong_new.csv") |>
  select(c("trnOpDate", "totalPass")) 
zhudong.new$trnOpDate <- ymd(zhudong.new$trnOpDate)

neiwan.new <- read.csv("/Users/shutingchen/Desktop/ISS/112/BAFT/project/Data/Neiwan_new.csv") |>
  select(c("trnOpDate", "totalPass")) 
neiwan.new$trnOpDate <- ymd(neiwan.new$trnOpDate)

# combine data
station.new <- hsinchu.new |> 
  left_join(zhunan.new, by = "trnOpDate") |>
  left_join(zhudong.new, by = "trnOpDate") |>
  left_join(neiwan.new, by = "trnOpDate") |>
  rename(time = trnOpDate, Hsinchu = totalPass.x, Zhunan = totalPass.y, 
         Zhudong = totalPass.x.x, Neiwan = totalPass.y.y) |>
  as_tsibble(index = time)
station.new |> has_gaps(.full = TRUE)

# convert to long format
station.new.long <- station.new |>
  melt(id.vars = "time", 
       variable.name = "train_id",
       measure.vars = c("Hsinchu", "Zhunan", "Zhudong", "Neiwan"),
       value.name = "total_pass") |>
  as_tsibble(index = time, key = train_id) |>
  arrange(time)
# -------------------------------------------------------------------------------
# compare actual and forecast values
# forecast errors
compare.Nov <- fc.Nov |>
  select(time, train_id, .mean) |>
  left_join(station.new.long, by = c("time", "train_id")) |>
  rename(forecast = .mean, actual = total_pass) |>
  relocate(actual, .before = forecast) |>
  mutate(fc_error = actual - forecast) |>
  mutate(relative_fcerror = percent(fc_error/actual)) |>
  group_by(train_id)
str(compare.Nov)

r_error <- compare.Nov |>
  select(time, train_id, relative_fcerror) |>
  group_by(train_id) |>
  mutate(abs_numeric = abs(as.numeric(sub("%", "", relative_fcerror, fixed = TRUE))/100), 
         numeric = as.numeric(sub("%", "", relative_fcerror, fixed = TRUE))/100)

ave_error <- r_error |>
  as_tibble() |>
  group_by(train_id) |>
  summarise(average = mean(numeric)) |>
  mutate(ave_percent = percent(average))

min_error <- r_error |>
  group_by(train_id) |>
  filter(abs_numeric == min(abs_numeric)) |>
  select(train_id, numeric, relative_fcerror)

max_error <- r_error |>
  group_by(train_id) |>
  filter(abs_numeric == max(abs_numeric)) |>
  select(train_id, numeric, relative_fcerror)

combined.error <- ave_error |>
  left_join(min_error, by = "train_id") |>
  left_join(max_error, by = "train_id") |>
  select(train_id, ave_percent, relative_fcerror.x, time.x, relative_fcerror.y, time.y) |>
  rename(ave_rerror = ave_percent, min_rerror = relative_fcerror.x, min_time = time.x,
         max_rerror = relative_fcerror.y, max_time = time.y)
combined.error 

# performance measure
fc.Nov.accuracy <- compare.Nov |>
  as_tibble() |> 
  group_by(train_id) |>
  summarise(
    MAE = mean(abs(fc_error), na.rm = TRUE),
    RMSE = sqrt(mean((fc_error)^2, na.rm = TRUE)),
    MAPE = mean(abs((fc_error) / actual), na.rm = TRUE) * 100
  )
fc.Nov.accuracy

# performance chart
compare.Nov.plot <- compare.Nov |>
  autoplot(actual) +
  scale_color_manual(values = c("actual" = "black")) +
  autolayer(compare, forecast, color = "blue", alpha = 0.7) +
  facet_wrap(~ train_id, scales = "free_y") +
  labs(x = "Date", y = "Total Daily Passengers", 
       title = "Forecast vs. Actual Daily Passenger in Nov 2023") +
  theme(legend.position = "none")

compare.Nov.plot

# -------------------------------------------------------------------------------
# create the % information
# calculate the average of weekdays of 4 series in Oct 2023
oct.all <- station.long |>
  filter_index("2023-10-01"~.) |>
  select(-covid) |>
  mutate(weekday = lubridate::wday(time, label = TRUE))

oct.all$weekday <- oct.all$weekday |>
  factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
str(oct.all)

oct.average <- oct.all |>
  as_tibble() |> 
  group_by(train_id, weekday) |>
  summarise(average = mean(total_pass))
oct.average

# calculate the percentage of the differences between Oct and Nov
Nov.combined <- compare.Nov |>
  mutate(weekday = lubridate::wday(time, label = TRUE)) |>
  select(-c("actual", "fc_error"))
Nov.combined$weekday <- Nov.combined$weekday |>
  factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

Nov.combined <- Nov.combined |>
  left_join(oct.average, by = c("train_id", "weekday")) |>
  rename(last_month_avg = average) |>
  mutate(percentage_diff = percent(forecast/last_month_avg - 1)) |>
  select(-c(relative_fcerror))
Nov.combined

# --------------------------------------------------------------------------------
# example plot
# eg. 2023-11-06 Mon ~ 11-12 Sun
example <- Nov.combined |>
  filter_index("2023-11-06" ~ "2023-11-12")

ggplot(example, aes(x = weekday)) +
  geom_bar(aes(y = last_month_avg), stat = "identity", position = "identity", fill = "grey", color = "grey4") +
  geom_bar(aes(y = forecast), stat = "identity", position = "identity", 
           fill = "pink", color = "pink3", alpha = 0.5) + 
  facet_wrap(~ train_id, scales = "free_y") +
  labs(x = "Weekdays", y = "Total Daily Passengers", 
       title = "Forecast Daily Passengers between Nov 6 to 12 2023") +
  theme_bw()
