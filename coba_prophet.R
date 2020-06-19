library(prophet)

library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(tidyverse)
library(TSrepr)
library(smooth)
library(forecTheta)
library(thief)
library(svrpath)
library(e1071)
library(nnfor)
library(neuralnet)
library(RSNNS)
library(BBmisc)
library(gsheet)
library(dplyr)
library(NMOF)
library(TSstudio)
library(fpp2)
library(bestNormalize)

url<-"https://docs.google.com/spreadsheets/d/1pYpYd04zw6iUz32mGkGNz_1_-jorwM-QWGxXSKiOzpo/edit?usp=sharing"
#gsheet2tbl(url)
a <- gsheet2text(url, format='csv')
b <- read.csv(text=a, stringsAsFactors=FALSE)
c<-b %>% filter(Kota == "Jakarta")

Dataset_Surabaya <- c
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         data1=Dataset_Surabaya[["K50000"]])

data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

data_outflow.ts<-ts(data_outflow[,3],frequency = 12)

dataset_outflow <- ts(data_outflow[,3],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts<-window(dataset_outflow,end=c(2017,12))
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2019,6))

myts_prophet<-ts_to_prophet(myts)

#add_country_holidays(model.prophet,country_name = "ID")
model.prophet<-prophet(n.changepoints = 10000,changepoint.prior.scale = 0.9)
model.prophet<-add_country_holidays(model.prophet, 'Indonesia')
model.prophet<-fit.prophet(model.prophet,df=myts_prophet)

future <- make_future_dataframe(model.prophet, periods = 100)
fcst <- predict(model.prophet, future)

plot(model.prophet, fcst)
prophet_plot_components(model.prophet, fcst)

prophet_cv<-cross_validation(prophet_fit,horizon = 1, units="days")


predict_uncertainty(model.prophet, df=myts_prophet)
future <- make_future_dataframe(model.prophet, periods = 12)
forecast <- predict(prophet_fit, future)

evaluate


prophet_plot_components(model.prophet, forecast)

