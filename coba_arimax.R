#coba arimax

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
library(TSA)
library(lubridate)
library(zoo)



url<-"https://docs.google.com/spreadsheets/d/1pYpYd04zw6iUz32mGkGNz_1_-jorwM-QWGxXSKiOzpo/edit?usp=sharing"
#gsheet2tbl(url)
a <- gsheet2text(url, format='csv')
b <- read.csv(text=a, stringsAsFactors=FALSE)
c<-b %>% filter(Kota == "Surabaya")




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

NATAL<-c(rep(c(0,0,0,0,0,0,0,0,0,0,0,1),24))
#NATAL<-as.data.frame(NATAL)

Y2K<-c(rep(0,71),1,rep(0,216))
#Y2K<-as.data.frame(Y2K)

JANEFFECT<-c(rep(c(1,0,0,0,0,0,0,0,0,0,0,0),24))

EFFECT98<-c(rep(0,48),rep(1,12),rep(0,228))

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
  0,0,0,0,0,1,0,0,0,0,0,0 #2017
)

CNY<-c(
  0,1,0,0,0,0,0,0,0,0,0,0, #1994
  1,0,0,0,0,0,0,0,0,0,0,0, #1995
  0,1,0,0,0,0,0,0,0,0,0,0, #1996
  0,1,0,0,0,0,0,0,0,0,0,0, #1997
  1,0,0,0,0,0,0,0,0,0,0,0, #1998
  0,1,0,0,0,0,0,0,0,0,0,0, #1999
  0,1,0,0,0,0,0,0,0,0,0,0, #2000
  1,0,0,0,0,0,0,0,0,0,0,0, #2001
  0,1,0,0,0,0,0,0,0,0,0,0, #2002
  0,1,0,0,0,0,0,0,0,0,0,0, #2003
  1,0,0,0,0,0,0,0,0,0,0,0, #2004
  0,1,0,0,0,0,0,0,0,0,0,0, #2005
  1,0,0,0,0,0,0,0,0,0,0,0, #2006
  0,1,0,0,0,0,0,0,0,0,0,0, #2007
  0,1,0,0,0,0,0,0,0,0,0,0, #2008
  1,0,0,0,0,0,0,0,0,0,0,0, #2009
  0,1,0,0,0,0,0,0,0,0,0,0, #2010
  0,1,0,0,0,0,0,0,0,0,0,0, #2011
  1,0,0,0,0,0,0,0,0,0,0,0, #2012
  0,1,0,0,0,0,0,0,0,0,0,0, #2013
  1,0,0,0,0,0,0,0,0,0,0,0, #2014
  0,1,0,0,0,0,0,0,0,0,0,0, #2015
  0,1,0,0,0,0,0,0,0,0,0,0, #2016
  1,0,0,0,0,0,0,0,0,0,0,0  #2017
)

PILDPR<-c(
  0,0,0,0,0,0,0,0,0,0,0,0, #1994
  0,0,0,0,0,0,0,0,0,0,0,0, #1995
  0,0,0,0,0,0,0,0,0,0,0,0, #1996
  0,0,0,0,1,0,0,0,0,0,0,0, #1997
  0,0,0,0,0,0,0,0,0,0,0,0, #1998
  0,0,0,0,0,1,0,0,0,0,0,0, #1999
  0,0,0,0,0,0,0,0,0,0,0,0, #2000
  0,0,0,0,0,0,0,0,0,0,0,0, #2001
  0,0,0,0,0,0,0,0,0,0,0,0, #2002
  0,0,0,0,0,0,0,0,0,0,0,0, #2003
  0,0,0,1,0,0,0,0,0,0,0,0, #2004
  0,0,0,0,0,0,0,0,0,0,0,0, #2005
  0,0,0,0,0,0,0,0,0,0,0,0, #2006
  0,0,0,0,0,0,0,0,0,0,0,0, #2007
  0,0,0,0,0,0,0,0,0,0,0,0, #2008
  0,0,0,1,0,0,0,0,0,0,0,0, #2009
  0,0,0,0,0,0,0,0,0,0,0,0, #2010
  0,0,0,0,0,0,0,0,0,0,0,0, #2011
  0,0,0,0,0,0,0,0,0,0,0,0, #2012
  0,0,0,0,0,0,0,0,0,0,0,0, #2013
  0,0,0,1,0,0,0,0,0,0,0,0, #2014
  0,0,0,0,0,0,0,0,0,0,0,0, #2015
  0,0,0,0,0,0,0,0,0,0,0,0, #2016
  0,0,0,0,0,0,0,0,0,0,0,0  #2017
)

YEAR<-c(
  rep(1994,12),
  rep(1995,12),
  rep(1996,12),
  rep(1997,12),
  rep(1998,12),
  rep(1999,12),
  rep(2000,12),
  rep(2001,12),
  rep(2002,12),
  rep(2003,12),
  rep(2004,12),
  rep(2005,12),
  rep(2006,12),
  rep(2007,12),
  rep(2008,12),
  rep(2009,12),
  rep(2010,12),
  rep(2011,12),
  rep(2012,12),
  rep(2013,12),
  rep(2014,12),
  rep(2015,12),
  rep(2016,12),
  rep(2017,12)
  
  
)

fff<-matrix(data=c(YEAR,NATAL,Y2K,EIDULFITR,JANEFFECT,EFFECT98,CNY,PILDPR),ncol=8)
colnames(fff)<-c("YEAR","NATAL","Y2K","EIDULFITR","JANEFFECT","EFFECT98","CNY","PILDPR")

#arima(myts,order=c(1,1,1),xreg=fff)

model.arima1<-auto.arima(myts)
model.arima2<-auto.arima(myts,xreg = fff)
dm.test(residuals(model.arima1),residuals(model.arima2),h=10)
summary(model.arima)

fff[,2:3]

ts_plot(myts,line.mode = "lines+markers")


myts_vector<-as.vector(myts)

View(myts_vector)
ggg<-matrix(data=c(myts_vector,YEAR,NATAL,Y2K,EIDULFITR,JANEFFECT,EFFECT98,CNY,PILDPR),ncol=9)
colnames(ggg)<-c("UANG","YEAR","NATAL","Y2K","EIDULFITR","JANEFFECT","EFFECT98","CNY","PILDPR")


ggg.ts<-ts(ggg,frequency = 12,start=1994)
lm.model<-lm(formula=UANG~poly(YEAR, 20)+EIDULFITR,data=ggg.ts)
summary(lm.model)



