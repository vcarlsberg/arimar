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
months <- 1:288

components.ts = decompose(myts)
plot(components.ts)

df.myts<-data.frame(x=1:288,y=as.numeric(myts))

svmodel <- svm(y ~ x,data=df.myts, type="eps-regression",kernel="radial",cost=10000, gamma=10)
rmse(df.myts$y,svmodel$fitted)

plot(df.myts$y, col="green", type="o")
points(svmodel$fitted, col="red", pch="*")
