library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(tidyverse)
library(TSrepr)
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

myts <- ts(data_outflow[["y"]],start=c(2013, 1), end=c(2017, 12), frequency=12)
index.x<-ts(c(1:60),start=c(2013, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow[["y"]],start=c(2018, 1), end=c(2018, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

#svrpath(x=c(1:242),y=data_outflow$y)
index.x<-c(1:300)

svm_model <- svm(x=c(1:288),y=data_outflow$y[1:288],
                 kernel="radial",gamma=2^12)
fitted.svm<-ts(svm_model$fitted)
forecast(svm_model,h=12)
nd <- 289:300
svm_model$fitted
forecast.svm<-predict(svm_model,newdata = data.frame(x=nd))
myts_2018
smape(myts_2018,hasil.prediksi.svm)

smape(myts,)
#summary(svm_model)

plot(data_outflow$y[1:288], col="red", type="o")
points(svm_model$fitted,col="black",pch="*")
#points(data_outflow$y[1:288], col="red", pch="*")
#lines(data_outflow$y[1:288], col="red",lty=2)

smape(svm_model$fitted,data_outflow$y[1:288])
mape(svm_model$fitted,data_outflow$y[1:288])

#nnet
set.seed(34)
nnetar.model<-nnetar(data_outflow$y[1:288],size = 30,lambda=0)
points(nnetar.model$fitted,col="green",pch="*")
#CVar(myts,k=10,h=12,nnetar(myts,lambda=lambda))
forecast::accuracy(nnetar.model)
fitted.nnetar<-nnetar.model[["fitted"]]
forecast.nnetar<-forecast(nnetar.model,h=forecast_horizon)

yhat<-0.5*svm_model$fitted+0.5*nnetar.model$fitted
yhat_outsample<-0.5*hasil.prediksi.svm+0.5*forecast.nnetar$mean
smape(myts_2018,yhat_outsample)
plot(data_outflow$y[1:288], col="red", type="o")
points(yhat,col="black",pch="*")

smape(na.omit(yhat),data_outflow$y[15:288])

hasil_tune_svm<-tune.svm(data_outflow$y[1:288]~c(1:288),
                         kernel="radial",
                         gamma = 2^seq(0, 10, by = 0.5)
)

summary(hasil_tune_svm)
plot(hasil_tune_svm)

obj <- tune.svm(svm, data_outflow$y[1:288]~c(1:288), 
                kernel="radial",
                gamma = 2^(-1:1)
                )
summary(obj)
plot(obj)

obj <- tune(svm, data_outflow$y[1:300]~c(1:300), kernel="polynomial",
            ranges = list(gamma = 2^(-2:2), cost = 2^(-2:2))
            )
summary(obj)


obj <- tune(svm, data_outflow$y[1:288]~c(1:288), kernel="radial",
            ranges = list(gamma = 2^(-2:2), cost = 2^(-2:2))
            )

hasil_tune_svm<-tune.svm(data_outflow$y[1:300]~c(1:300),
                         kernel="polynomial",
                         cost=10^(-2:2),
                         degree=c(2,3),
                         gamma = c(0.1,1,10),
                         coef0=c(0.1,1,10)
                         )
  
summary(hasil_tune_svm)
plot(hasil_tune_svm)

tuneResult1 <- tune(svm(), x=index.x,y=myts,
                    ranges = list(epsilon = seq(0,1,0.1), 
                                  cost = 2^(seq(0.5,8,.5)),
                                  tunecontrol = tune.control(sampling = "fix")
                                  )
)
