#SARIMA - SVR Series

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
library(NMOF)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K10000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

#daftar.mape.mae.smape<-data.frame(fh=NULL,mape=NULL,mae=NULL,smape=NULL)
#daftar.mae<-data.frame(fh=NULL,mae=NULL)
#daftar.smape<-data.frame(fh=NULL,smape=NULL)
#daftar.mape<-rbind(daftar.mape,data.frame(fh=21,mape=12))

myts <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow[["y"]],start=c(2018, 1), end=c(2018, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

lambda <- BoxCox.lambda(myts,lower = 0)



for(x in c(1:12))
{
  print(x)
  
  forecast_horizon<-x
  
  #arima
  arima.model <- auto.arima(myts,trace=FALSE,start.p=1,start.q=1,
                            ic="aic",lambda = lambda,seasonal = TRUE)
  
  forecast.arima<-forecast(arima.model,forecast_horizon)
  
  #svr
  testFun <- function(x)
  {
    svm.model.tuning <- svm(x=c(1:length(myts)),y=arima.model$residuals[1:length(myts)],
                            kernel="radial",gamma=2^x[1],cost = 2^x[2])
    
    mae(arima.model$residuals,svm.model.tuning$fitted)
  }
  
  if(x==1)
  {
    levels <- list(a = -50:50, b = -10:10)
    res <- gridSearch(testFun, levels)
    
    svm.model <- svm(x=c(1:length(myts)),y=arima.model$residuals[1:length(myts)],
                     kernel="radial",
                     gamma=2^res$minlevels[1],
                     cost = 2^res$minlevels[2])
    #fitted.svm<-ts(svm.model$fitted)
  }
  
  nd <- (length(myts)+1):(length(myts)+forecast_horizon)
  forecast.svm<-predict(svm.model,newdata = data.frame(x=nd))
  
  #forecast.arima<-forecast(arima.model,12)
  #forecast::accuracy(arima.model)
  #fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
  #fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
  #fitted.and.forecast.arima
  #plot(fitted.and.forecast.arima)
  
  #nnetar.model<-nnetar(arima.model$residuals,size = 30)
  #forecast::accuracy(nnetar.model)
  #fitted.nnetar<-nnetar.model[["fitted"]]
  
  
  #forecast.nnetar<-forecast(nnetar.model,h=forecast_horizon)
  
  yhat<-forecast.svm+forecast.arima[["mean"]]
  
  daftar.mape.mae.smape<-rbind(daftar.mape.mae.smape,
                               data.frame(fh=forecast_horizon,
                                          model="SARIMA-SVR_Series",
                                          smape=TSrepr::smape(myts_2018[1:forecast_horizon],yhat),
                                          mae=TSrepr::mae(myts_2018[1:forecast_horizon],yhat),
                                          mape=TSrepr::mape(myts_2018[1:forecast_horizon],yhat),
                                          rmse=TSrepr::rmse(myts_2018[1:forecast_horizon],yhat)
                               )
  )
  
  
}

#rmse(myts_2018,yhat[289:300])
#shapiro.test(myts_2018-yhat[289:300])
#mean(na.omit(svm.model$residuals))
#qqnorm(na.omit(svm.model$residuals))
#qqline(na.omit(svm.model$residuals))
#plot(density(na.omit(svm.model$residuals)))
