#ARIMA - NN Parallel Simple Averaging

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

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K10000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

daftar.mape.mae.smape<-data.frame(fh=NULL,mape=NULL,mae=NULL,smape=NULL,maape=NULL)
#daftar.mae<-data.frame(fh=NULL,mae=NULL)
#daftar.smape<-data.frame(fh=NULL,smape=NULL)
#daftar.mape<-rbind(daftar.mape,data.frame(fh=21,mape=12))

data_outflow.ts<-ts(data_outflow[["y"]])

dataset_outflow <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts<-window(dataset_outflow,start=c(2015,1),end=c(2018,12))
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2018,12))
#myts[288]
#xmyts<-myts(start)
#myts<-ts(myts[(288-24):288],start=c(2014,1),end=c(2017,12),frequency = 12)
#myts<-window(myts,start=c(2015,1),end=c(2018,12))
tseries::adf.test(na.omit(myts))

components.ts = decompose(myts)
plot(components.ts)

lambda <- BoxCox.lambda(myts,lower = 0)

for(x in c(1:12))
{
  print(x)
  
  forecast_horizon<-x
  
  arima.model <- auto.arima(myts,trace=FALSE,seasonal = FALSE,
                            start.p=1,start.q=1,lambda = lambda)
  fitted.arima<-arima.model[["fitted"]]
  forecast.arima<-forecast(arima.model,h=forecast_horizon)
  
  nnetar.model<-nnetar(myts,size = 30,lambda=lambda)
  #forecast::accuracy(nnetar.model)
  fitted.nnetar<-nnetar.model[["fitted"]]
  forecast.nnetar<-forecast(nnetar.model,h=forecast_horizon)
  
  yhat<-0.5*forecast.nnetar[["mean"]]+0.5*forecast.arima[["mean"]]
  
  daftar.mape.mae.smape<-rbind(daftar.mape.mae.smape,
                               data.frame(fh=forecast_horizon,
                                          smape=smape(myts_2018[1:forecast_horizon],yhat),
                                          mae=mae(myts_2018[1:forecast_horizon],yhat),
                                          mape=mape(myts_2018[1:forecast_horizon],yhat),
                                          rmse=rmse(myts_2018[1:forecast_horizon],yhat),
                                          maape=maape(myts_2018[1:forecast_horizon],yhat)
                               )
  )
}
#daftar.mape[1,3]<-21
#mape(myts_2018,yhat)
#mae(myts_2018,yhat)
#smape(myts_2018,yhat)
