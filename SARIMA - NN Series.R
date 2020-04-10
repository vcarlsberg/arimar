#SARIMA - NN Series
rm(list=ls())
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

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K100000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

daftar.mape.mae.smape<-data.frame(fh=NULL,mape=NULL,mae=NULL,smape=NULL,maape=NULL)
#daftar.mae<-data.frame(fh=NULL,mae=NULL)
#daftar.smape<-data.frame(fh=NULL,smape=NULL)
#daftar.mape<-rbind(daftar.mape,data.frame(fh=21,mape=12))

#data_outflow.ts<-ts(data_outflow[["y"]])

dataset_outflow <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), 
                      end=c(2019, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts<-window(dataset_outflow,start=c(2012,6),end=c(2017,12),frequency=12)
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2018,12),frequency=12)
#myts[288]
#xmyts<-myts(start)
#myts<-ts(myts[(288-24):288],start=c(2014,1),end=c(2017,12),frequency = 12)
#myts<-window(myts,start=c(2015,1),end=c(2018,12))

components.ts = decompose(myts)
plot(components.ts)

print(tseries::adf.test(na.omit(myts)))
#ndiffs(myts)
#nsdiffs(myts)
#myts.diff<-diff(myts,differences = 1)
#auto.arima(myts.diff)
#auto.arima(myts)
#auto.arima(myts,stationary = TRUE)

#plot(myts.diff)
#acf(myts.diff)
#pacf(myts.diff)
#Box.test(myts.diff)




#adf(myts.diff)
#myts<-myts.diff

lambda <- BoxCox.lambda(myts,lower = 0)

for(x in c(1:12))
{
  print(x)
  
  forecast_horizon<-x
  
  #arima
  #arima.model <- auto.arima(myts,trace=FALSE,seasonal = FALSE,
  #                          start.p=1,start.q=1,lambda = lambda)
  #fitted.arima<-arima.model[["fitted"]]
  #forecast.arima<-forecast(arima.model,h=forecast_horizon)
  
  #lambda <- BoxCox.lambda(myts,lower=0)
  arima.model <- auto.arima(myts,trace=FALSE,
                            ic="aic",seasonal = TRUE)
  #forecast::splinef(myts,h=12,method="gcv")
  #auto.msarima(myts)
  
  #arima.diff<-Arima(myts,order=c(0,0,0),seasonal=c(0,1,0),include.drift = FALSE)
  #forecast.arima.diff<-forecast(arima.diff,12)
  
  #forecast.arima<-forecast(arima.model,12)
  #forecast::accuracy(arima.model)
  #fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
  #fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
  #fitted.and.forecast.arima
  #plot(fitted.and.forecast.arima)
  
  residual.data1<-myts-arima.model$fitted
  
  set.seed(72)
  #mlp.model<-nnfor::mlp(residual.data1,hd=c(150,300))
  #forecast::accuracy(mlp.model)
  
  #set.seed(72)
  nnetar.model<-nnetar(residual.data1,size = 50,scale.inputs = TRUE,lambda = lambda)
  #forecast::accuracy(nnetar.model)
  #fitted.nnetar<-nnetar.model[["fitted"]]
  
  #residual.data2<-residual.data1-nnetar.model$fitted
  #summary(residual.data2)
  
  
  forecast.arima<-forecast(arima.model,forecast_horizon)
  forecast.nnetar<-forecast(nnetar.model,h=forecast_horizon)
  
  yhat<-forecast.nnetar$mean[1:forecast_horizon]+forecast.arima$mean[1:forecast_horizon]
  myts_2018
  
  daftar.mape.mae.smape<-rbind(daftar.mape.mae.smape,
                               data.frame(fh=forecast_horizon,
                                          model="SARIMA-NN_Series",
                                          smape=TSrepr::smape(myts_2018[1:forecast_horizon],yhat),
                                          mae=TSrepr::mae(myts_2018[1:forecast_horizon],yhat),
                                          mape=TSrepr::mape(myts_2018[1:forecast_horizon],yhat),
                                          rmse=TSrepr::rmse(myts_2018[1:forecast_horizon],yhat),
                                          maape=TSrepr::maape(myts_2018[1:forecast_horizon],yhat)
                               )
  )
  
  print(daftar.mape.mae.smape)
}

#rmse(myts_2018,yhat[289:300])
#shapiro.test(myts_2018-yhat[289:300])
#mean(na.omit(nnetar.model$residuals))
#qqnorm(na.omit(nnetar.model$residuals))
#qqline(na.omit(nnetar.model$residuals))
#plot(density(na.omit(nnetar.model$residuals)))
