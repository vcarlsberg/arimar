library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(tidyverse)
library(tsrepr)


Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(y=Dataset_Surabaya[["L1000"]])
myts <- ts(data_outflow,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow[["y"]][289:300],start=c(2018, 1), end=c(2018, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

lambda <- BoxCox.lambda(myts,lower=0)

forecast_horizon<-12


#arima
arima.model <- auto.arima(myts,trace=TRUE,seasonal = TRUE,
                          start.p=1,start.q=1,lambda = lambda)

#ao<-arimaorder(arima.model)
#far2 <- function(x, h){forecast(Arima(myts, order=c(3,1,1),seasonal = c(1,0,0)), h=h)}
#e <- tsCV(myts, far2, h=12)
#cr<-checkresiduals(arima.model, lag=36)

#mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
#data.frame(h = 1:12, MSE = mse) %>%
#  ggplot(aes(x = h, y = MSE)) + geom_point()

#tscv<-tsCV(myts,forecastfunction=Arima(myts,order=c(3,1,1),seasonal=c(1,0,0)),drift=TRUE,h=12)
fitted.arima<-arima.model[["fitted"]]
forecast.arima<-forecast(arima.model,h=forecast_horizon)
#forecast::accuracy(arima.model)
#fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
#fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
#fitted.and.forecast.arima
#plot(fitted.and.forecast.arima)

#nnetar
#set.seed(34)
nnetar.model<-nnetar(myts,lambda=lambda)
#CVar(myts,k=10,h=12,nnetar(myts,lambda=lambda))
#forecast::accuracy(nnetar.model)
fitted.nnetar<-nnetar.model[["fitted"]]
forecast.nnetar<-forecast(nnetar.model,h=forecast_horizon)
#fitted.and.forecast.nnetar<-ts(c(forecast.nnetar[["fitted"]],forecast.nnetar[["mean"]]))
#fitted.and.forecast.nnetar<-ts(fitted.and.forecast.nnetar,start=c(1994, 1), end=c(2018, 12), frequency=12)
#fitted.and.forecast.nnetar
#plot(fitted.and.forecast.nnetar)

#calculating weight
#rmse(myts,na.omit(fitted.arima)+na.omit(fitted.nnetar))
#mape(myts,na.omit(fitted.arima)+na.omit(fitted.nnetar))

#rmse(myts[25:288],fitted.arima[25:288]+fitted.nnetar[25:288])

weight_kecil<-function(w1,w2) 
{
  library(Metrics)
  sse(myts,w1*na.omit(fitted.arima)+w2*na.omit(fitted.nnetar))
  #rmse()
}

set.seed(72)
GA <- ga(type = "real-valued", nBits = 1000,
         fitness = function(w) -weight_kecil(w[1],w[2]),
         lower =c(-100,-100), upper = c(100,100),
         maxiter=1000,parallel=TRUE,monitor=FALSE)
summary(GA)
#rmse(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#bias(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#sse(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#mae(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#mape(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))

yhat<-GA@solution[1]*forecast.arima[["mean"]]+
  GA@solution[2]*forecast.nnetar[["mean"]]
yhat
myts_2018
rmse(myts_2018,yhat)
mape(myts_2018,yhat)
smape(myts_2018,yhat)
mase(myts_2018,yhat)
MAAPE(myts_2018,yhat)

actual<-c(0,1)
predicted<-c(10,8)
mape(actual,predicted)
mase(actual,predicted)
MAAPE(actual,predicted)


smape(myts_2018,yhat)

residuals<-myts_2018-yhat
residuals
mean(residuals)

#cor.test(residuals,yhat)
acf(residuals)
pacf(residuals)



scatter(myts_2018-yhat[289:300])
qqnorm(myts_2018-yhat[289:300])
qqline(myts_2018-yhat[289:300])

hist(myts_2018-yhat[289:300])
shapiro.test(myts_2018-yhat[289:300])


#pake weight 0.5 --> simple averaging (SA)
yhat_weight05<-0.5*forecast.arima[["mean"]]+
  0.5*forecast.nnetar[["mean"]]
rmse(myts_2018,yhat_weight05)

#pake linear regression
lm<-lm(myts~fitted.arima+fitted.nnetar)
summary(lm)
rmse(myts_2018,-0.016*forecast.arima[["mean"]]+
       1.021*forecast.nnetar[["mean"]])
