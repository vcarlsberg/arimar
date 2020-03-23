library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(e1071)
library(Metrics)
library(MTS)
library(vars)
library(nonlinearAnalysis)
library(tsfeatures)
library(keras)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(K50K=Dataset_Surabaya[["K50000"]],
                         K20K=Dataset_Surabaya[["K20000"]],
                         K10K=Dataset_Surabaya[["K10000"]],
                         K5K=Dataset_Surabaya[["K5000"]],
                         K1K=Dataset_Surabaya[["K1000"]],
                         K5=Dataset_Surabaya[["K500"]])
myts <- ts(data_outflow,start=c(1994, 1), end=c(2017, 12), frequency=12)

train<-fread(myts[,1])
