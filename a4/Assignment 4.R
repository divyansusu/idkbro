library(fpp2)
library(forecast)
library(tidyverse)
library(lubridate)


entire <- read.csv("data/Msia Tourist Arrival2.csv") #entire dataset
tourist_data <- read.csv("data/Msia Tourist Arrival.csv") #tourist arrivals

entire_ts <- entire |>
  select(-Date) |>
  ts(frequency = 12, start = c(1989,1), end = c(2019,12))

entire_ts

tourist <- tourist_data |>
  select(-Date) |>
  ts(frequency = 12, start = c(1989,1), end = c(2020,1))

tourist



#Phase 1

autoplot(entire_ts, facets = T)

ts_scatterplot <- entire_ts |>
  as.data.frame() |>
  ggplot(aes(x = Ecxhange.Rate, y = Tourist.Arrival)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ts_scatterplot

(fit <- tslm(formula = Tourist.Arrival ~ Ecxhange.Rate, lambda = 0, data = entire_ts))

checkresiduals(fit)

fc <- forecast(fit, newdata = data.frame(Ecxhange.Rate = c(3.5 , 4.9)))
fc


#Phase 2
autoplot(tourist)
ggtsdisplay(tourist)


class(tourist_data$Date)

#converting from character to date for filteration

tourist_data$Date <- dmy(paste0("01-", tourist_data$Date))

tourist2_data <- tourist_data |>
  filter(Date >= as.Date("2010-01-01") & Date <= as.Date("2020-01-31"))

tourist2 <- tourist2_data |>
  select(-Date) |>
  ts(frequency = 12, start = c(2010,1), end = c(2020,1))

autoplot(tourist2)
ggtsdisplay(tourist2)



#training and testing

train <- window(tourist2, end = c(2017,12))
test <- window(tourist2, start = c(2018,1))

length(train)
length(test)

h <- length(test)

autoplot(train)
ggtsdisplay(train)
lambda <- BoxCox.lambda(train)

#piecewise linear trend model
t <- time(train)
knot <- c(2014,1)
t1 <- ts(pmax(0, t - knot), start = c(2010,1))
piecewise_fit <- tslm(train ~ t + t1 + season)
piecewise_fit

checkresiduals(piecewise_fit)

fitted(piecewise_fit)
autoplot(fitted(piecewise_fit))

autoplot(train) + ylab("train values") +
  autolayer(fitted(piecewise_fit), series = "Fitted")

#FORECASTS
#piecewise forecast
t_new <- max(t) + seq(h)
t1_new <- pmax(0, t_new - knot)
newdata <- cbind(t = t_new, t1 = t1_new) |> as.data.frame()
newdata
pw_forecast <- forecast(piecewise_fit, newdata = newdata)
pw_forecast

autoplot(pw_forecast)

#auto arima and auto ets forecasts
ARIMA <- auto.arima(train) |> forecast(h = h)
ETS <- ets(train) |> forecast.ets(h = h)
ensemble <- (ARIMA[["mean"]] + ETS[["mean"]])/2

autoplot(train) +
  autolayer(ensemble, series = "ensemble")


#bootstrap
set.seed(123)

is.univariate <- function(x) {
  is.ts(x) && NCOL(x) == 1
}
is.univariate(train)

bootseries <- bld.mbb.bootstrap(ts(train), 10) |> as.data.frame() |>
  ts(start = c(2010,1), end = c(2017,12), frequency = 12)

autoplot(train) +
  autolayer(bootseries, colour = T) +
  autolayer(train, colour = F) +
  ylab("bootstrapped series") +
  guides(colour = 'none')

set.seed(123)
bagged.ets <- train |> 
  baggedModel(bootstrapped_series = bootseries, fn = ets) |> forecast(h = h)

autoplot(train) +
  autolayer(bagged.ets, series = "bagged", PI = F)


autoplot(tourist2) +
  autolayer(ensemble, series = "ensemble", PI = F) +
  autolayer(pw_forecast, series = "piecewise", PI = F) +
  autolayer(bagged.ets, series = "bagged ets", PI = F)


accuracy(pw_forecast, test)
accuracy(ensemble, test)
accuracy(bagged.ets, test)


