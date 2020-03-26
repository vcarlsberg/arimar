#SVR - ARIMA - NN Parallel SA

library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(tidyverse)
library(TSrepr)
library(svrpath)
library(e1071)
library(NMOF)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K10000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

daftar.mape.mae.smape<-data.frame(fh=NULL,mape=NULL,mae=NULL,smape=NULL)
#daftar.mae<-data.frame(fh=NULL,mae=NULL)
#daftar.smape<-data.frame(fh=NULL,smape=NULL)
#daftar.mape<-rbind(daftar.mape,data.frame(fh=21,mape=12))

myts <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow[["y"]],start=c(2018, 1), end=c(2018, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

lambda <- BoxCox.lambda(myts,lower = 0)

testFun <- function(x)
{
  svm_model.tuning <- svm(x=c(1:length(myts)),y=data_outflow$y[1:length(myts)],
                          kernel="radial",gamma=2^x[1],cost = 2^x[2])
  
  mape(myts,svm_model.tuning$fitted)
}

for(x in c(1:12))
{
  print(x)
  
  forecast_horizon<-x
  
  #arima
  arima.model <- auto.arima(myts,trace=FALSE,seasonal = TRUE,
                            start.p=1,start.q=1,lambda = lambda)
  fitted.arima<-arima.model[["fitted"]]
  forecast.arima<-forecast(arima.model,h=forecast_horizon)
  
  #svr grid search
  levels <- list(a = -50:50, b = -10:10)
  res <- gridSearch(testFun, levels)
  
  svm_model <- svm(x=c(1:length(myts)),y=data_outflow$y[1:length(myts)],
                   kernel="radial",
                   gamma=2^res$minlevels[1],
                   cost = 2^res$minlevels[2])
  fitted.svm<-ts(svm_model$fitted)
  nd <- (length(myts)+1):(length(myts)+forecast_horizon)
  forecast.svm<-predict(svm_model,newdata = data.frame(x=nd))
  
  #nnetar
  set.seed(34)
  nnetar.model<-nnetar(myts,size = 30,lambda=lambda)
  #CVar(myts,k=10,h=12,nnetar(myts,lambda=lambda))
  forecast::accuracy(nnetar.model)
  fitted.nnetar<-nnetar.model[["fitted"]]
  forecast.nnetar<-forecast(nnetar.model,h=forecast_horizon)
  
  yhat<-1/3*forecast.svm+1/3*forecast.arima[["mean"]]+1/3*forecast.nnetar[["mean"]]
  
  daftar.mape.mae.smape<-rbind(daftar.mape.mae.smape,
                               data.frame(fh=forecast_horizon,
                                          smape=smape(myts_2018[1:forecast_horizon],yhat),
                                          mae=mae(myts_2018[1:forecast_horizon],yhat),
                                          mape=mape(myts_2018[1:forecast_horizon],yhat),
                                          rmse=rmse(myts_2018[1:forecast_horizon],yhat)
                               )
  )
}
Metrics::mase()
