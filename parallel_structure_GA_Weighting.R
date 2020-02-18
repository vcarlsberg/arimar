library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)


Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow_10000<-data.frame(y=Dataset_Surabaya[["K10000"]])
myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow_10000[["y"]][289:300],start=c(2018, 1), end=c(2018, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

lambda <- BoxCox.lambda(myts,lower=0)


#arima
arima.model <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1,ic="aic",lambda = lambda)
fitted.arima<-arima.model[["fitted"]]
forecast.arima<-forecast(arima.model,h=12)
#fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
#fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
#fitted.and.forecast.arima
#plot(fitted.and.forecast.arima)

#nnetar
#set.seed(34)
nnetar.model<-nnetar(myts,lambda=lambda)
fitted.nnetar<-nnetar.model[["fitted"]]
forecast.nnetar<-forecast(nnetar.model,h=12)
#fitted.and.forecast.nnetar<-ts(c(forecast.nnetar[["fitted"]],forecast.nnetar[["mean"]]))
#fitted.and.forecast.nnetar<-ts(fitted.and.forecast.nnetar,start=c(1994, 1), end=c(2018, 12), frequency=12)
#fitted.and.forecast.nnetar
#plot(fitted.and.forecast.nnetar)

#calculating weight
rmse(myts,na.omit(fitted.arima)+na.omit(fitted.nnetar))

#rmse(myts[25:288],fitted.arima[25:288]+fitted.nnetar[25:288])

weight_kecil<-function(w1,w2) 
{
  library(Metrics)
  rmse(myts,w1*na.omit(fitted.arima)+w2*na.omit(fitted.nnetar))
}

GA <- ga(type = "real-valued", nBits = 1000,
         fitness = function(w) -weight_kecil(w[1],w[2]),
         lower =c(-100,-100), upper = c(100,100),
         maxiter=500,parallel=TRUE)
summary(GA)
rmse(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))

yhat<-GA@solution[1]*forecast.arima[["mean"]]+
  GA@solution[2]*forecast.nnetar[["mean"]]
yhat
rmse(myts_2018,yhat)



#pake weight 0.5 --> simple averaging (SA)
yhat_weight05<-0.5*forecast.arima[["mean"]]+
  0.5*forecast.nnetar[["mean"]]
rmse(myts_2018,yhat_weight05)

#pake linear regression
lm<-lm(myts~fitted.arima+fitted.nnetar)
summary(lm)
rmse(myts_2018,-0.016*forecast.arima[["mean"]]+
       1.021*forecast.nnetar[["mean"]])
