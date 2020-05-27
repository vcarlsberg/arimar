rm(list=ls())
source("modelling_function.R")

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
library(tsDyn)
library(vars)
library(DMwR)
require(quantmod)

url<-"https://docs.google.com/spreadsheets/d/1pYpYd04zw6iUz32mGkGNz_1_-jorwM-QWGxXSKiOzpo/edit?usp=sharing"
#gsheet2tbl(url)
a <- gsheet2text(url, format='csv')
b <- read.csv(text=a, stringsAsFactors=FALSE)
c<-b %>% filter(Kota == "Surabaya")


Dataset_Surabaya <- c
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K5000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

data_outflow.ts<-ts(data_outflow[["y"]])

dataset_outflow <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
myts<-window(dataset_outflow,end=c(2017,12))
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2018,12))

#test
adf.test.result<-tseries::adf.test(na.omit(myts))
print(adf.test.result)
kpss.test.result<-tseries::kpss.test(na.omit(myts))
print(kpss.test.result)

components.ts = decompose(myts)
plot(components.ts)

set.seed(72)
if(adf.test.result$p.value<=0.05)
{
  print("Data Stasioner")
  arima<-function_arima(myts)
  forecast.arima<-forecast::forecast(arima,h=12)
  forecast::accuracy(arima)
  
  
  residual<-myts-arima$fitted
  nnetar<-function_nnetar(residual)
  forecast::accuracy(nnetar)
  forecast.nnetar<-forecast::forecast(nnetar,h=12)
  
  sum_linear_nonlinear<-forecast.arima$mean+forecast.nnetar$mean
  sum_linear_nonlinear
  myts_2018
  
  mlp<-function_mlp(residual,forecast_horizon = 12)
  forecast.mlp<-forecast::forecast(mlp,h=12)
  sqrt(mlp$MSE)
  
  sum_linear_nonlinear<-forecast.arima$mean+forecast.mlp$mean
  sum_linear_nonlinear
  
  rmse(myts_2018,sum_linear_nonlinear)
  #fh<-12
  #svr<-function_svr(residual,forecast_horizon = fh)
  
  #nd <- (length(myts)+1):(length(myts)+fh)
  #forecast.svr<-predict(svr,newdata = data.frame(x=nd),probability = TRUE)
  #plot(svr$fitted)
  
} else {
  print("emweh")
  number.diff<-1
  myts.diff<-diff(myts,differences = number.diff)
  
  arima<-function_arima(myts.diff)
  forecast.arima<-forecast::forecast(arima,h=12)
  
  
  nnetar<-function_nnetar(forecast.arima$residuals)  
  forecast.nnetar<-forecast::forecast(nnetar,h=12)
  #elman<-function_elman(myts.diff)
  
  sum_linear_nonlinear<-forecast.arima$mean+forecast.nnetar$mean
  
  pred.result<-myts[length(myts)]+cumsum(sum_linear_nonlinear)
  pred.result<-ts(pred.result,start=c(2018,1),frequency=12)
  
  rmse(pred.result,myts_2018)
}

