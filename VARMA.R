library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(e1071)
library(Metrics)
library(MTS)
library(vars)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(K50K=Dataset_Surabaya[["K50000"]],
                         K20K=Dataset_Surabaya[["K20000"]],
                         K10K=Dataset_Surabaya[["K10000"]],
                         K5K=Dataset_Surabaya[["K5000"]],
                         K1K=Dataset_Surabaya[["K1000"]],
                         K5=Dataset_Surabaya[["K500"]])
myts <- ts(data_outflow,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
#myts_2018<-ts(data_outflow_10000[["y"]][289:300],start=c(2018, 1), end=c(2018, 12), frequency=12)

Eccm(myts,maxp=12,maxq=12)
model_varma<-VARMA(myts, p = 6, q =0)

VARselect(myts, lag.max = 48)
model_var<-VAR(myts,p=9)
summary(model_var)
stabil<-stability(model_var)
predict_var<-predict(model_var,n.ahead=12)

model_var_2<-VARselect(myts,lag.max=48)
summary(model_var_2)
model_var_2$criteria
