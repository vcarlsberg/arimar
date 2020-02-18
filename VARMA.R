library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(e1071)
library(Metrics)
library(MTS)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(K50=Dataset_Surabaya[["K50000"]],
                         K20=Dataset_Surabaya[["K20000"]],
                         K10=Dataset_Surabaya[["K10000"]],
                         K5=Dataset_Surabaya[["K5000"]],
                         K2=Dataset_Surabaya[["K2000"]])
myts <- ts(data_outflow,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
#myts_2018<-ts(data_outflow_10000[["y"]][289:300],start=c(2018, 1), end=c(2018, 12), frequency=12)

Eccm(myts,maxp=12,maxq=12)
model_varma<-VARMA(myts, p = 8, q = 5)
