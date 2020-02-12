library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)

Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow_10000<-data.frame(y=Dataset_Surabaya[["K10000"]])
myts <- ts(data_outflow_10000,start=c(1994, 1), end=c(2018, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)

components.ts = decompose(myts)
plot(components.ts)

arima1<-arima(myts, order = c(1,0,0))
accuracy(arima1)
forecast(arima1, 5)
plot(forecast(arima1, 5))

arima_auto <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1,ic="aic")
accuracy(arima_auto)
forecast(arima_auto, 6)
predict(arima_auto,n.ahead=6)
plot(forecast(arima_auto, 6))

nnet_ar<-nnetar(myts)
accuracy(nnet_ar)
forecast(nnet_ar, 5)

plot(forecast(nnet_ar, 5))

#ini pake hybrid model series 
#fitted.arima_auto<-fitted.values(arima_auto)
#residual.arima_auto<-residuals(arima_auto)
#nnetar.arima_auto<-nnetar(fitted.arima_auto)
#fitted.nnetar<-fitted.values(nnetar.arima_auto)
#fitted.value_total<-fitted.arima_auto+fitted.nnetar

arima_auto <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1,ic="aic",lambda = 0)
forecast.arima_auto<-forecast(arima_auto,6)
xxts_tot<-ts(c(forecast.arima_auto[["fitted"]],forecast.arima_auto[["mean"]]))
forecast.nnetar.from.arima_auto<-nnetar(xxts_tot,lambda=0)
xxts_tot2<-ts(c(forecast.nnetar.from.arima_auto[["fitted"]]))
xxts_finish<-xxts_tot+xxts_tot2
xxts_ts <- ts(xxts_finish,start=c(1994, 1), end=c(2019, 6), frequency=12)
plot(xxts_ts)

arima_auto <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1,ic="aic",lambda=0)
forecast.arima_auto2<-forecast(arima_auto,6)
xxts_tot<-ts(c(forecast.arima_auto2[["fitted"]],forecast.arima_auto2[["mean"]]))
xxts_tot
nnetar.residual.from.arima_auto2<-nnetar(forecast.arima_auto2[["residuals"]],lambda=0)
forecast.residual.from.nnetar.arima_auto2<-forecast(nnetar.residual.from.arima_auto2,6)
xxts_tot2<-ts(c(forecast.residual.from.nnetar.arima_auto2[["fitted"]],forecast.residual.from.nnetar.arima_auto2[["mean"]]))
xxts_finish<-xxts_tot+xxts_tot2
xxts_ts <- ts(xxts_finish,start=c(1994, 1), end=c(2019, 6), frequency=12)
plot(xxts_ts)

hm<-hybridModel(myts, models = "an")
forecast(hm,5)
plot(forecast(hm,5))
accuracy(hm)

plot(arima_auto[["residuals"]])
plot(arima_auto[["x"]])
plot(arima_auto[["fitted"]])

nnetar.residual.arima<-nnetar(arima_auto[["fitted"]])
forecast(arima_auto,10)
forecast(nnetar.residual.arima,10)
plot(nnetar.residual.arima[["fitted"]])

nnet(nnetar.residual.arima[["fitted"]])

