library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(prophet)


Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow_10000<-data.frame(y=Dataset_Surabaya[["K10000"]])
myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow_10000[["y"]][289:300],start=c(2018, 1), end=c(2018, 12), frequency=12)

model.prophet<-prophet(myts)
