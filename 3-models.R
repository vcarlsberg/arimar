#ARIMA - NN Series
rm(list=ls())

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
library(nnfor)
library(neuralnet)
library(RSNNS)
library(BBmisc)




Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K50000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

daftar.mape.mae.smape<-data.frame(fh=NULL,mape=NULL,mae=NULL,smape=NULL,maape=NULL)
#daftar.mae<-data.frame(fh=NULL,mae=NULL)
#daftar.smape<-data.frame(fh=NULL,smape=NULL)
#daftar.mape<-rbind(daftar.mape,data.frame(fh=21,mape=12))

data_outflow.ts<-ts(data_outflow[["y"]])

dataset_outflow <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts<-window(dataset_outflow,start=c(2014,1),end=c(2017,12))
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2018,12))
#myts[288]
#xmyts<-myts(start)
#myts<-ts(myts[(288-24):288],start=c(2014,1),end=c(2017,12),frequency = 12)
#myts<-window(myts,start=c(2015,1),end=c(2018,12))
print(tseries::adf.test(na.omit(myts)))

components.ts = decompose(myts)
plot(components.ts)

lambda <- BoxCox.lambda(myts)

#normalize(c(1,3,4,4.5,5))

#arima
arima.model<-auto.arima(myts,ic="aic",trace = FALSE,seasonal = TRUE)
forecast::accuracy(arima.model)
forecast::checkresiduals(arima.model)
forecast::forecast(arima.model)
plot(myts,col="red",type="o")
lines(arima.model$fitted,col="blue",pch="*")
summary(myts-arima.model$fitted)

#nnetar
nnetar.model<-nnetar(myts,size = 60,scale.inputs = TRUE,lambda = lambda)
forecast::accuracy(nnetar.model)
forecast::checkresiduals(nnetar.model)
forecast::forecast(nnetar.model)

#mlp
set.seed(72)
model.mlp<-nnfor::mlp(myts,hd=c(150,300),comb = "mean",sel.lag = TRUE,
                      hd.auto.type = "valid",difforder = 0, outplot = TRUE)
lines(model.mlp$fitted,col="green",pch="*")
sqrt(model.mlp$MSE)
rmse(window(myts,start=c(2015,1)),model.mlp$fitted)
forecast::forecast(model.mlp,h=12)

#mlp_type_2 --> x nya pake 1:xx (bukan autoregression)
index_x<-1:length(myts)
index_y<-(myts[1:length(myts)])
df.xy<-data.frame(x=index_x,y=index_y)
model.mlp.type1<-nnet(x=index_x,y=myts[1:length(myts)],size=c(10,30,50),maxit=1000)
model.mlp.type1$fitted.values

model.mlp.type2<-neuralnet(y~(x),data=df.xy,stepmax = 500,rep = 3,startweights = NULL)
model.mlp.type2$response

model.mlp.type3<-mlp(x=df.xy$x,
                     y=normalizeData(df.xy$y,type="0_1"),
                     size=c(50,100,150,300),maxit=1000,learnFuncParams = 0.1
                     )
plot(ts(model.mlp.type3$fitted.values,start=c(2014,1),end=c(2017,12),frequency = 12))
plot(myts)

summary(model.mlp.type3)

norm_y<-normalizeData(df.xy$y,type="0_1")
getNormParameters(norm_y)

fungsi_elman<-function(w1,w2) 
{
  library(Metrics)
  elman.model<-elman(x=df.xy$x,y=normalizeData(df.xy$y,type="0_1"),
                     size=c(round(w1),round(w2)),
                     learnFuncParams = 0.1,
                     maxit=1000,linOut = FALSE)
  
  fitted.elman<-denormalizeData(elman.model$fitted.values,
                                getNormParameters(normalizeData(df.xy$y,type="0_1")))
  
  rmse(myts,fitted.elman)
  
  #sse(myts,w1*na.omit(fitted.nnetar)+
  #      w2*na.omit(fitted.svm)+
  #      w3*na.omit(fitted.arima)+
  #      w4*na.omit(fitted.sma)
  #)
  #rmse()
}

set.seed(72)
GA <- ga(type = "real-valued",pmutation=0.5,
         fitness = function(w) -fungsi_elman(w[1],w[2]),
         lower =c(1,1), upper = c(50,50),
         maxiter=100,parallel=TRUE,monitor=TRUE,seed=72)
summary(GA)

elman.model<-elman(x=df.xy$x,y=normalizeData(df.xy$y,type="0_1"),size=c(50,100,150,300),
                   learnFuncParams = 0.1,
                   maxit=1000,linOut = FALSE)
fitted.elman<-denormalizeData(elman.model$fitted.values,
                getNormParameters(normalizeData(df.xy$y,type="0_1")))
plot(fitted.elman)

plot(myts)
plot(denormalizeData(elman.model$fitted.values,
                getNormParameters(normalizeData(df.xy$y,type="0_1"))),type="o")

rbf.model<-rbf(x=df.xy$x,y=normalizeData(df.xy$y,type="0_1"),size = c(50,200))
plot(denormalizeData(rbf.model$fitted.values,
                     getNormParameters(normalizeData(df.xy$y,type="0_1"))),type="o")
plot(myts)

data("snnsData")
laser <- snnsData$laser_1000.pat

#nnetar
model.nnetar<-forecast::nnetar(myts,size=30,lambda=lambda)
forecast::accuracy(model.nnetar)

#elm
model.elm<-elm(myts,hd=1000,m=12,comb="mean")
sqrt(model.elm$MSE)
plot(model.elm$fitted)
plot(myts)

#svr
testFun <- function(x)
{
  svm.model.tuning <- svm(x=c(1:length(myts)),y=myts[1:length(myts)],
                          kernel="radial",gamma=2^x[1],cost = 2^x[2])
  
  rmse(myts,svm.model.tuning$fitted)
}

levels <- list(a = -20:20, b = -10:10)
res <- gridSearch(testFun, levels)

svm.model <- svm(x=c(1:length(myts)),y=myts[1:length(myts)],
                 kernel="radial",
                 gamma=2^res$minlevels[1],
                 cost =2^res$minlevels[2],type = "nu-regression" )

acf(svm.model$residuals)
svm.model$gamma
svm.model$cost

#exponential smoothing / Holt Winters
hw.model<-HoltWinters(myts,beta = FALSE,gamma=FALSE)
hw.model2<-hw(myts)
forecast::accuracy(hw.model2)

holt.model<-holt(myts,h=12)
forecast::accuracy(holt.model)

ses.model<-ses(myts,h=12)
forecast::accuracy(ses.model)

ets.model<-ets(myts,model="ZZZ",ic="aic")
forecast::accuracy(ets.model)
acf(ets.model$residuals, lag.max=20)

#calculate in-sample error
yhat_arima<-window(arima.model$fitted,start=c(2015,1))
yhat_nnetar<-window(model.nnetar$fitted,start=c(2015,1))
yhat_svr<-ts(svm.model$fitted,start=c(2014,1),end=c(2017,12),frequency = 12)
yhat_svr<-window(yhat_svr,start=c(2015,1))

yhat_insample<-(1/3)*yhat_arima+
  (1/3)*yhat_nnetar+
  (1/3)*yhat_svr
plot(yhat_insample)
rmse(window(myts,start=c(2015,1)),yhat_insample)

#calculate weighting with GA
weight_kecil<-function(w1,w2,w3) 
{
  library(Metrics)
  rmse(myts,w1*yhat_arima+w2*yhat_nnetar+w3*yhat_svr)
  #rmse()
}

set.seed(72)
GA <- ga(type = "real-valued",pmutation=0.5,
         fitness = function(w) -weight_kecil(w[1],w[2],w[3]),
         lower =c(-1,-1,-1), upper = c(1,1,1),
         maxiter=1000,parallel=TRUE,monitor=TRUE,seed=72)
summary(GA)

yhat_ga<-GA@solution[1]*yhat_arima+
  GA@solution[2]*yhat_nnetar+
  GA@solution[3]*yhat_svr

rmse(myts[13:48],yhat_ga)
