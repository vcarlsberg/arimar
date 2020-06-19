#SARIMAX - NN Parallel Simple Averaging

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

EIDULFITR<-c(
  0,0,1,0,0,0,0,0,0,0,0,0, #1994
  0,0,1,0,0,0,0,0,0,0,0,0, #1995
  0,1,0,0,0,0,0,0,0,0,0,0, #1996
  0,1,0,0,0,0,0,0,0,0,0,0, #1997
  1,0,0,0,0,0,0,0,0,0,0,0, #1998
  1,0,0,0,0,0,0,0,0,0,0,0, #1999
  1,0,0,0,0,0,0,0,0,0,0,0, #2000
  0,0,0,0,0,0,0,0,0,0,0,1, #2001
  0,0,0,0,0,0,0,0,0,0,0,1, #2002
  0,0,0,0,0,0,0,0,0,0,1,0, #2003
  0,0,0,0,0,0,0,0,0,0,1,0, #2004
  0,0,0,0,0,0,0,0,0,0,1,0, #2005
  0,0,0,0,0,0,0,0,0,1,0,0, #2006
  0,0,0,0,0,0,0,0,0,1,0,0, #2007
  0,0,0,0,0,0,0,0,0,1,0,0, #2008
  0,0,0,0,0,0,0,0,1,0,0,0, #2009
  0,0,0,0,0,0,0,0,1,0,0,0, #2010
  0,0,0,0,0,0,0,1,0,0,0,0, #2011
  0,0,0,0,0,0,0,1,0,0,0,0, #2012
  0,0,0,0,0,0,0,1,0,0,0,0, #2013
  0,0,0,0,0,0,1,0,0,0,0,0, #2014
  0,0,0,0,0,0,1,0,0,0,0,0, #2015
  0,0,0,0,0,0,1,0,0,0,0,0, #2016
  0,0,0,0,0,1,0,0,0,0,0,0, #2017
  0,0,0,0,0,1,0,0,0,0,0,0, #2018
  0,0,0,0,0,1 #2019
)

dataset_EIDULFITR<-ts(EIDULFITR,start = c(1994,1), end=c(2019,6),frequency = 12)

for(bill in c("K100000","K50000","K20000","K10000","K5000","K2000"))
{
  
  
  
  Dataset_Surabaya <- c
  data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                           bulan=Dataset_Surabaya[["Bulan"]],
                           data1=Dataset_Surabaya[bill]
  )
  data_outflow$bulan<-match(data_outflow$bulan,month.abb)
  data_outflow<-na.omit(data_outflow)
  head<-head(data_outflow)
  tail<-tail(data_outflow)
  
  data_outflow.ts<-ts(data_outflow[,3],frequency = 12)
  
  dataset_outflow <- ts(data_outflow[,3],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
  #myts <- ts(data_outflow_10000, frequency=12)
  myts<-window(dataset_outflow,end=c(2017,12))
  myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2019,6))
  
  ts_decompose(myts)
  
  #head(myts)
  #diff.myts<-diff(myts)
  #head(diff.myts)
  #undiff.myts<-diffinv(diff.myts)
  #head(undiff.myts)
  
  #54096+cumsum(diff.myts)
  #myts
  
  set.seed(72)
  kpss_result<-kpss.test(myts)
  adf_result<-adf.test(myts)
  
  
  lambda.value <- 1
  
  myts_transformed<-BoxCox(myts,lambda.value)
  shapiro.test(myts_transformed)
  
  arima.model <- auto.arima(myts_transformed,trace=FALSE,seasonal = TRUE,
                            start.p=1,start.q=1,xreg = window(dataset_EIDULFITR,
                                                              start=start(myts_transformed),
                                                              end=end(myts_transformed))
                            
  )
  
  nnetar.model<-nnetar(myts_transformed,size = 30,xreg = window(dataset_EIDULFITR,
                                                                start=start(myts_transformed),
                                                                end=end(myts_transformed))) 
  
  for(x in c(1:18))
  {
    forecast_horizon<-x
    

    forecast.arima<-forecast(arima.model,
                             h=forecast_horizon,
                             xreg = subset(dataset_EIDULFITR,
                                           start=289,end=(289+forecast_horizon-1)))
    
    
    forecast.nnetar<-forecast(nnetar.model,h=forecast_horizon,
                              xreg = subset(dataset_EIDULFITR,
                                            start=289,end=(289+forecast_horizon-1)))
    
    yhat_forecast<-0.5*forecast.nnetar[["mean"]]+0.5*forecast.arima[["mean"]]
    yhat_fitted<-0.5*arima.model$fitted+0.5*nnetar.model$fitted
    
    yhat_fitted_backtransform<-InvBoxCox(yhat_fitted,lambda = lambda.value)
    yhat_forecast_backtransform<-InvBoxCox(yhat_forecast,lambda = lambda.value)
    
    yhat_fitted_backtransform_no_na<-na.omit(yhat_fitted_backtransform)
    start_point<-start(yhat_fitted_backtransform_no_na)
    
    #residual
    residual_value<-window(myts,start = start_point)-na.omit(yhat_fitted_backtransform)
      
    #checkresiduals(residual_value,2)
    box_test_result<-Box.test(residual_value)
    

    
    in_sample_mape<-mape(window(myts,start = start_point),
                         na.omit(yhat_fitted_backtransform) )
    out_sample_mape<-mape(subset(myts_2018,start = 1,end = forecast_horizon),
                          subset(yhat_forecast_backtransform,start = 1,end = forecast_horizon))
    
    print(paste(bill,"fh =",forecast_horizon,", in sample mape =",in_sample_mape,
                ", out sample mape =",out_sample_mape,"box test result =",box_test_result$p.value))
  }
  
}
