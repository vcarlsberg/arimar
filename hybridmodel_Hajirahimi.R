library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow_10000<-data.frame(y=Dataset_Surabaya[["K10000"]])
myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)

components.ts = decompose(myts)
plot(components.ts)

#arima
arima.model <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1,ic="aic")
forecast.arima<-forecast(arima.model,h=12)
fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.arima
plot(fitted.and.forecast.arima)

#nnetar
set.seed(34)
nnetar.model<-nnetar(fitted.and.forecast.arima,lambda=0)
forecast.nnetar<-forecast(nnetar.model,h=12)
fitted.and.forecast.nnetar<-ts(c(forecast.nnetar[["fitted"]],forecast.nnetar[["mean"]]))
fitted.and.forecast.nnetar<-ts(fitted.and.forecast.nnetar,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.nnetar
plot(fitted.and.forecast.nnetar)

yhat<-fitted.and.forecast.arima+fitted.and.forecast.nnetar
yhat
plot(yhat)

myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2018, 12), frequency=12)

plot(myts, col="green", type="o")
points(fitted.and.forecast.nnetar, col="red", pch="*")
lines(fitted.and.forecast.nnetar, col="red",lty=2)
points(fitted.and.forecast.arima, col="blue", pch="*")
lines(fitted.and.forecast.arima, col="blue",lty=2)
points(yhat, col="black", pch="*")
lines(yhat, col="black",lty=2)



