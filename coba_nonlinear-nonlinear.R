#Script experiment for nonlinear-nonlinear modelling

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
library(gsheet)
library(dplyr)
library(NMOF)
library(TSstudio)
library(fpp2)
library(bestNormalize)

url<-"https://docs.google.com/spreadsheets/d/1pYpYd04zw6iUz32mGkGNz_1_-jorwM-QWGxXSKiOzpo/edit?usp=sharing"
#gsheet2tbl(url)
a <- gsheet2text(url, format='csv')
b <- read.csv(text=a, stringsAsFactors=FALSE)
c<-b %>% filter(Kota == "Jakarta")

Dataset_Surabaya <- c
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         data1=Dataset_Surabaya[["K50000"]]
)
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

data_outflow.ts<-ts(data_outflow[,3],frequency = 12)

dataset_outflow <- ts(data_outflow[,3],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts<-window(dataset_outflow,end=c(2017,12))
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2019,6))

lambda.value <- 1
myts_transformed<-BoxCox(myts,lambda = lambda.value)

set.seed(72)

#Modelling MLP
model.mlp<-nnfor::mlp(myts_transformed,hd = c(3,5,4),difforder = 0)
fitted.mlp<-model.mlp$fitted
#End of Modelling MLP

#Modelling SVR
levels <- list(a = -50:50, b = -10:10)
res <- gridSearch(testFun, levels)

svm.model <- svm(x=c(1:length(myts_transformed)),
                 y=myts_transformed,
                 kernel="radial",
                 gamma=2^res$minlevels[1],
                 cost = 2^res$minlevels[2])
fitted.svm<-ts(svm.model$fitted,start=c(head[1,1], head[1,2]),frequency = 12)
#End of Modelling SVR

#detect length
if(length(fitted.svm)>=length(fitted.mlp))
{
  length_data<-length(fitted.mlp)
}else{
  length_data<-length(fitted.svm)
}
#end of detect length

#start calculate v
v_svm.model<-(fitted.svm-mean(fitted.svm))/(sd(fitted.svm)^2)
v_mlp.model<-(fitted.mlp-mean(fitted.mlp))/(sd(fitted.mlp)^2)

#end calculate

#Start Weighting
GA <- ga(type = "real-valued", nBits = 1000,
         fitness = function(w) -weight_kecil(w[1],w[2],w[3],w[4]),
         lower =c(-10000000,-1,-1,-1), upper = c(10000000,1,1,1),
         maxiter=1000,parallel=TRUE,monitor=TRUE)
#End of weighting

#FUNCTIONS

testFun <- function(x)
{
  svm.model.tuning <- svm(x=c(1:length(myts_transformed)),
                          y=myts_transformed,
                          kernel="radial",gamma=2^x[1],cost = 2^x[2])
  
  mae(myts_transformed,svm.model.tuning$fitted)
}

weight_kecil<-function(w1,w2,w3,w4) 
{
  library(Metrics)
  sse(myts_transformed,w1+
        w2*(fitted.mlp[1:length_data])+
        w3*(fitted.svm[1:length_data])+
        w4*v_svm.model[1:length_data]*v_mlp.model[1:length_data]
      )
  #rmse()
}

