library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(e1071)
library(Metrics)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow_10000<-data.frame(y=Dataset_Surabaya[["K5000"]])
myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)

components.ts = decompose(myts)
plot(components.ts)

#arima
lambda <- BoxCox.lambda(myts,lower=0)
arima.model <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1,ic="aic",lambda = lambda,seasonal = TRUE)
forecast.arima<-forecast(arima.model,12)
accuracy(arima.model)
fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.arima
plot(fitted.and.forecast.arima)

#svm
set.seed(72)
residual<-data.frame(x=1:288,y=arima.model[["x"]]-arima.model[["fitted"]])
lambda <- BoxCox.lambda(residual,lower=0)
svmodel <- svm(y ~ x,data=residual, type="eps-regression",kernel="linear")
nd <- 1:300
svm.residual <- predict(svmodel, newdata=data.frame(x=nd))
#accuracy(svmodel)
#forecast(svmodel,12)

yhat<-fitted.and.forecast.arima+svm.residual
yhat

myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2018, 12), frequency=12)

plot(myts, col="green", type="o",axes = FALSE)
axis(side=1, at=seq(1994, 2020, by=1))
points(yhat, col="red", pch="*")

rmse(myts,yhat)
