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

url<-"https://docs.google.com/spreadsheets/d/1pYpYd04zw6iUz32mGkGNz_1_-jorwM-QWGxXSKiOzpo/edit?usp=sharing"
#gsheet2tbl(url)
a <- gsheet2text(url, format='csv')
b <- read.csv(text=a, stringsAsFactors=FALSE)
c<-b %>% filter(Kota == "Jakarta")




Dataset_Surabaya <- c
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         data=Dataset_Surabaya[["K20000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

#daftar.mape.mae.smape<-data.frame(fh=NULL,mape=NULL,mae=NULL,smape=NULL,maape=NULL)
#daftar.mae<-data.frame(fh=NULL,mae=NULL)
#daftar.smape<-data.frame(fh=NULL,smape=NULL)
#daftar.mape<-rbind(daftar.mape,data.frame(fh=21,mape=12))

data_outflow.ts<-ts(data_outflow[["data"]],frequency = 12)

dataset_outflow <- ts(data_outflow[["data"]],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts<-window(dataset_outflow,end=c(2017,12))
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2018,12))

NATAL<-c(rep(c(0,0,0,0,0,0,0,0,0,0,0,1),25),0,0,0,0,0,0)
NATAL<-as.data.frame(NATAL)

Y2K<-c(rep(0,71),1,rep(0,234))
Y2K<-as.data.frame(Y2K)

KRISMON<-c(rep(0,48),rep(1,12),rep(0,246))
KRISMON<-as.data.frame(KRISMON)

data_outflow_exo<-cbind(data_outflow,NATAL,Y2K,KRISMON)

lm.model<-lm(data~NATAL+tahun+Y2K+KRISMON,data=data_outflow_exo)
summary(lm.model)

tslm.model<-tslm(myts~season)
summary(tslm.model)

#y <- ts(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), frequency=12)
#fit <- tslm(y ~ trend + season)
#summary(fit)
#plot(y)
ts_plot(data_outflow.ts,line.mode = "lines+markers")
