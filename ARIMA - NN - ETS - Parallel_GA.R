#ARIMA - NN - ETS

library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(tidyverse)
library(TSrepr)
library(smooth)
library(forecTheta)
library(thief)
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

myts <- ts(data_outflow[["y"]],start=c(1994, 1), end=c(2017, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts_2018<-ts(data_outflow[["y"]],start=c(2018, 1), end=c(2018, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

lambda <- BoxCox.lambda(myts,lower=0)

forecast_horizon<-6


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
forecast::accuracy(arima.model)
#fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
#fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
#fitted.and.forecast.arima
#plot(fitted.and.forecast.arima)

#nnetar
set.seed(34)
nnetar.model<-nnetar(myts,size = 30,lambda=lambda)
#CVar(myts,k=10,h=12,nnetar(myts,lambda=lambda))
forecast::accuracy(nnetar.model)
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

#ets
ets.model<-ets(myts,opt.crit = "mae")
forecast::accuracy(ets.model)
fitted.ets<-ets.model[["fitted"]]
forecast.ets<-forecast(ets.model,h=forecast_horizon)

#rwf
rwf.model<-rwf(myts,drift = TRUE)
forecast::accuracy(rwf.model)
#fitted.rwf<-na.omit(lead(rwf.model[["fitted"]],n=1,default=NA))
fitted.rwf<-rwf.model[["fitted"]]
forecast.rwf<-forecast(rwf.model,h=forecast_horizon)
mape(myts,fitted.rwf)

#snaive
snaive.model<-snaive(myts,biasadj = TRUE)
forecast::accuracy(snaive.model)
fitted.snaive<-lead(na.omit(snaive.model[["mean"]]),n=0)
forecast.snaive<-forecast(snaive.model,h=forecast_horizon)

#ces
ces.model<-auto.ces(myts,h = 12)
summary(ces.model)
fitted.ces<-ces.model$fitted

#sma
sma.model<-smooth::sma(myts)
fitted.sma<-sma.model$fitted
forecast.sma<-forecast(sma.model,h=forecast_horizon)
#plot(sma.model$fitted)
forecast::accuracy(sma.model)


#dotm
dotm.model<-dotm(myts,h = 12)
summary(dotm.model)
fitted.dotm<-dotm.model$fitted
forecTheta::errorMetric(myts,fitted.dotm,type = "APE")

#svr
#x.index.svm<-ts(c(1:288),start=c(1994, 1), end=c(2017, 12), frequency=12)
svm.model <- svm(x=c(1:length(myts)),y=myts,
                 kernel="radial",gamma=2^12)
fitted.svm<-ts(svm_model$fitted,start=c(1994, 1), end=c(2017, 12), frequency=12)

nd <- length(myts)+1:length(myts)+forecast_horizon
forecast.svm<-predict(svm_model,newdata = data.frame(x=nd))
forecast.svm<-ts(forecast.svm,start=c(2018,1),end=c(2018,12),frequency = 12)
plot(fitted.svm)

#thief
thief.model<-thief(myts)

mstl(myts)%>%autoplot()


plot(myts,type="o")
points(fitted.ets,col="red",type = "o")
points(fitted.nnetar,col="blue",type="o")
points(fitted.arima,col="green",type="o")
points(fitted.rwf,col="magenta",type="o")
points(fitted.snaive,col="cyan",type="o")
points(fitted.ces,col="orange",type="o")
points(fitted.dotm,col="brown",type="o")
points(fitted.sma,col="gray",type="o")

forecast.arima<-forecast(arima.model,h=12)
forecast.nnetar<-forecast(nnetar.model,h=12)


#yhat<-0.5*forecast.arima$mean+0.5*forecast.nnetar$mean
#yhat_2018<-ts(yhat,start = c(2018,1), end = c(2018,12),frequency=12)

#smape(yhat,myts_2018)
#mape(yhat,myts_2018)

#dshw(myts,lambda = lambda,period1=12,period2=12)

#tbats(myts,seasonal.periods = TRUE)
forecast::accuracy(tbats(myts,seasonal.periods = FALSE))

#splinef.model<-splinef(myts,lambda=lambda)
#forecast::accuracy(splinef.model)

#thetaf.model<-thetaf(myts)
#forecast::accuracy(thetaf.model)

#checkresiduals(ets.model)

weight_kecil<-function(w1,w2,w3,w4) 
{
  library(Metrics)
  sse(myts,w1*na.omit(fitted.nnetar)+
        w2*na.omit(fitted.svm)+
        w3*na.omit(fitted.arima)+
        w4*na.omit(fitted.sma)
  )
  #rmse()
}

set.seed(72)
GA <- ga(type = "real-valued",pmutation=0.5,
         fitness = function(w) -weight_kecil(w[1],w[2],w[3],w[4]),
         lower =c(-1,-1,-1,-1), upper = c(1,1,1,1),
         maxiter=1000,parallel=TRUE,monitor=TRUE,seed=72)
summary(GA)
#rmse(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#bias(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#sse(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#mae(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))
#mape(myts,GA@solution[1]*na.omit(fitted.arima)+GA@solution[2]*na.omit(fitted.nnetar))



yhat<-GA@solution[1]*forecast.nnetar[["mean"]]+
  GA@solution[2]*forecast.svm+
  GA@solution[3]*forecast.arima[["mean"]]+
  GA@solution[4]*forecast.sma[["mean"]]
yhat<-0.25*forecast.nnetar[["mean"]]+
  0.25*forecast.svm+
  0.25*forecast.arima[["mean"]]+
  0.25*forecast.sma[["mean"]]
yhat
myts_2018
rmse(myts_2018[1:6],yhat)
mape(myts_2018[1:6],yhat)
smape(myts_2018[1:6],yhat)
mase(myts_2018,yhat)
MAAPE(myts_2018,yhat)

mean(forecast.arima$mean[2],forecast.nnetar$mean[2],forecast.ets$mean[2],trim=20)


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
