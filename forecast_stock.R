library(rtsdata)


quote<-ds.getSymbol.yahoo("WSBP.JK", from = "2019-01-01", to = Sys.Date())

ts_quote<-ts(quote$WSBP.JK.Open,start=1,frequency =5 )
components.ts = decompose(ts_quote)
plot(components.ts)

quote<-ds.getSymbol.yahoo("REAL.JK", from = "2019-01-01", to = Sys.Date())

lambda <- BoxCox.lambda(na.omit(quote$REAL.JK.Close),lower=0)
arima.model <- auto.arima(na.omit(quote$REAL.JK.Close),
                          trace=TRUE,start.p=1,start.q=1,ic="aic",
                          lambda = lambda,seasonal = TRUE)
forecast.arima<-forecast(arima.model,5)
accuracy(arima.model)
fitted.and.forecast.arima<-ts(c(forecast.arima[["fitted"]],forecast.arima[["mean"]]))
#fitted.and.forecast.arima<-ts(fitted.and.forecast.arima,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.arima
plot(fitted.and.forecast.arima)

set.seed(72)
residual<-arima.model[["x"]]-arima.model[["fitted"]]
lambda <- BoxCox.lambda(residual,lower=0)
nnetar.model<-nnetar(residual,lambda=lambda,scale.inputs = TRUE,size=30,repeats=10)
accuracy(nnetar.model)
forecast.nnetar<-forecast(nnetar.model,h=5)
fitted.and.forecast.nnetar<-ts(c(forecast.nnetar[["fitted"]],forecast.nnetar[["mean"]]))
#fitted.and.forecast.nnetar<-ts(fitted.and.forecast.nnetar,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.nnetar
plot(fitted.and.forecast.nnetar)

yhat<-fitted.and.forecast.arima+fitted.and.forecast.nnetar
yhat
plot(yhat)

set.seed(34)
nnetar.model<-nnetar(fitted.and.forecast.arima,lambda=0)
forecast.nnetar<-forecast(nnetar.model,h=12)
fitted.and.forecast.nnetar<-ts(c(forecast.nnetar[["fitted"]],forecast.nnetar[["mean"]]))
#fitted.and.forecast.nnetar<-ts(fitted.and.forecast.nnetar,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.nnetar
plot(fitted.and.forecast.nnetar)





nnetar.model<-nnetar(fitted.and.forecast.arima,lambda=0)
forecast.nnetar<-forecast(nnetar.model,h=12)
fitted.and.forecast.nnetar<-ts(c(forecast.nnetar[["fitted"]],forecast.nnetar[["mean"]]))
fitted.and.forecast.nnetar<-ts(fitted.and.forecast.nnetar,start=c(1994, 1), end=c(2018, 12), frequency=12)
fitted.and.forecast.nnetar
plot(fitted.and.forecast.nnetar)

hm<-hybridModel(quote$WSBP.JK.Open, models = "an")
forecast(hm,20)
plot(forecast(hm,20))
accuracy(hm)

arima_auto <- auto.arima(quote$WSBP.JK.Open,trace=TRUE,start.p=1,start.q=1)
accuracy(arima_auto)
forecast(arima_auto, 20)
plot(forecast(arima_auto, 20))

nnet_ar<-nnetar(quote$WSBP.JK.Open)
accuracy(nnet_ar)
forecast(nnet_ar, 20)
plot(forecast(nnet_ar, 20))
