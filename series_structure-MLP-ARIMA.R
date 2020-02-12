#series structure - MLP - ARIMA

library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(e1071)
library(Metrics)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow_10000<-data.frame(y=Dataset_Surabaya[["K10000"]])
myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow_10000[["y"]][289:300],start=c(2018, 1), end=c(2018, 12), frequency=12)


components.ts = decompose(myts)
plot(components.ts)

#mlp
lambda <- BoxCox.lambda(myts,lower=0)
nnetar.model<-nnetar(residual,lambda="auto",scale.inputs = TRUE,size=30,repeats=10)
forecast::accuracy(nnetar.model)
forecast.nnetar<-forecast(nnetar.model,h=12)
fitted.and.forecast.nnetar<-ts(c(forecast.nnetar[["fitted"]],forecast.nnetar[["mean"]]))
fitted.and.forecast.nnetar<-ts(fitted.and.forecast.nnetar,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.nnetar
plot(fitted.and.forecast.nnetar)

#nnetar
set.seed(72)
residual<-myts-na.omit(fitted.and.forecast.nnetar)
lambda <- BoxCox.lambda(residual,lower=0)
arima.model <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1,ic="aic",lambda = lambda,seasonal = TRUE)
forecast.arima<-forecast(arima.model,12)
forecast::accuracy(arima.model)
fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.arima
plot(fitted.and.forecast.arima)


yhat<-fitted.and.forecast.arima+fitted.and.forecast.nnetar
yhat
plot(yhat)

#myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2018, 12), frequency=12)

plot(myts, col="green", type="o",axes=FALSE)
axis(side=1, at=seq(1994, 2019, by=1))
points(fitted.and.forecast.nnetar, col="red", pch="*")
#lines(fitted.and.forecast.nnetar, col="red",lty=2)
points(fitted.and.forecast.arima, col="blue", pch="*")
#lines(fitted.and.forecast.arima, col="blue",lty=2)
points(yhat, col="black", pch="*")
#lines(yhat, col="black",lty=2)

rmse(myts_2018,yhat[289:300])

#var.omit<-data.frame(y=yhat)
#omit<-omit["y"]
#myts_maju<-(data.frame(y=myts[25:300]))
#rmse(myts[25:300],yhat[25:300])
