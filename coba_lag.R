library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(tidyverse)
library(tseries)
library(RSNNS)
library(quantmod)
library(caret)


Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K100000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

myts <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow[["y"]][289:300],start=c(2018, 1), end=c(2018, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

myts.lag.1<-Lag(myts,k=1)
head(myts)
head(myts.lag.1)

datax<-data.frame(
  myts=data_outflow$y,
  myts.lag.1=Lag(data_outflow$y,k=1)
)

trainControl <- trainControl(method="repeatedcv", number=10, repeats=10)
svm.price.model <- train(myts ~ Lag.1,data=datax, method="svmLinear", 
                        trControl=trainControl)


set.seed(1)
neural.price.model <- nnet(myts ~ Lag.1,data=datax,
                           maxit=100000,  size = 30, trace=TRUE)
mape(myts,neural.price.model$fitted.values)
neural.price.model$fitted.values

mlp.model<-mlp(x=myts,y=myts.lag.1,size = c(10,30,50))
#stats::ar.ols(myts)
