# Class Six Notes

# Time Series! 
library(fable)
library(feasts)
library(tsibble)
library(timetk)
library(dplyr)
library(tidyr)
library(quantmod)
library(lubridate)

spy <- getSymbols('SPY', auto.assign=FALSE)
spy |> head()

spy |>  class()
spy <- spy |> as.data.frame() |> tibble::rownames_to_column(var='Date') |> 
    as.tibble() |> 
    mutate(Date=ymd(Date)) |> 
    select(
        Date,
        Close=SPY.Close,
        Volume=SPY.Volume,
        Adjusted=SPY.Adjusted
    ) |> 
    as_tsibble(index=Date)
spy
spy |> autoplot(Close)

spy <- spy |> mutate(Return=log(Close/lag(Close)))
spy |> autoplot(Return)
#Stationarity: mean that your data is roughly flat over time. 
#Time series are supposed to be like this. 
#Taking the log allows us to better satisfy this requirement. 
#Garch: Generalized AutoRegressive Conditional Heteroskedasticity

spy |> 
    plot_time_series(.date_var=Date, .value=Return)

spy |> 
    filter(!is.na(Return)) |> 
    plot_anomaly_diagnostics(.date_var=Date, .value=Return)

spy |> filter_index('2016-01-01' ~ .)

etf <- spy |> filter_index('2016' ~ .)
etf |> autoplot(Return)

etf <-  etf |> 
    fill_gaps() |> 
    fill(Return, .direction = 'down') |> 
    fill(Close, .direction = 'down') |> 
    fill(Adjusted, Volume, .direction = 'down')
etf |> autoplot(Return)
etf |> ACF(Return) |> autoplot()

etf_mean <- etf |> 
    model(
        Mean=MEAN(Return)
    )
etf_mean
etf_mean |> forecast(h=7)    

etf_mean |>
    forecast(h=7) |> 
    autoplot() +
    autolayer(etf |> filter_index('2022-10'),Return)


etf_simple <- etf |> 
    model(
        Mean=MEAN(Return),
        Naive=NAIVE(Return)
    )
etf_simple    
etf_simple |> forecast(h=7)
etf_simple |> forecast(h=7) |> 
    autoplot(level=NULL) +
    autolayer(etf |> filter_index('2022-10' ~ .), Return)

train <- etf |> filter_index(. ~ '2022-09-30')
test <- etf |> filter_index('2022-10' ~ .)
train
test

etf_simple_2 <- train |> 
    model(
        Mean=MEAN(Return),
        Naive=NAIVE(Return)
    )
etf_simple_2 |> forecast(h=7)
simple_fore <- etf_simple_2 |> forecast(test)
accuracy(simple_fore, test)

simple_fore |> 
    autoplot(level=NULL) +
    autolayer(train |> filter_index('2022-09' ~ .), Return) +
    autolayer(test, Return, color='green')


# ETS ####

etf_ets <- train |> 
    model(
        AAA=ETS(Return ~ error('A') +trend('A') + season('A') )
    )
etf_ets
etf_ets |> report()

etf_ets <- train |> 
    model(
        AAA=ETS(Return ~ error('A') +trend('A') + season('A') ),
        AAN=ETS(Return ~ error('A') +trend('A') + season('N') ),
        ANN=ETS(Return ~ error('A') +trend('N') + season('N') )
    )
etf_ets |> forecast(test)

etf_ets |> forecast(test) |> 
    autoplot(level=NULL) +
    autolayer(train |> filter_index('2022-09' ~ .) ,Return) +
    autolayer(test, Return, color='grey')

accuracy(etf_ets |> forecast(test), test)

#ARIMA ####
#Auto Regressive Integrated Moving Average! 
train |> autoplot(Return)
train |> mutate(diffed=Return - lag(Return)) |> 
    autoplot(diffed)

#Three parameters:
#p:
#d: diffing (can be one or two level or none)
#q:
#P, D, Q = seasonal versions 

arima1 <- train |> 
    model(
        arima_101=ARIMA(Return ~ pdq(1,0,1) + PDQ(0,0,0))
        )
arima1 |> report(arima1)
arima1 |> 
    forecast(test) |> 
    autoplot(level=NULL) +
    autolayer(train |> filter_index('2022-09' ~ .), Return) +
    autolayer(test, Return, color='yellow')

arima2 <- train |> 
    model(
        arima=ARIMA(Return ~ pdq(0:4, 0:2, 0:4) + PDQ(0,0,0))
    )
arima2 |> report()
#prophet = another framework for time series. 
all_mods <- train |> 
    model(
        Mean=MEAN(Return),
        Naive=NAIVE(Return),
        ets_auto=ETS(Return),
        arima_auto = ARIMA(Return)
    )
