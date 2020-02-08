library(e1071)
library(Metrics)
#https://stackoverflow.com/questions/49081801/time-series-forecasting-using-support-vector-machine-svm-in-r

data(AirPassengers) 
monthly_data <- unclass(AirPassengers)
months <- 1:144
DF <- data.frame(months,monthly_data)
colnames(DF)<-c("x","y")

# train an svm model, consider further tuning parameters for lower MSE
svmodel <- svm(y ~ x,data=DF, type="eps-regression",kernel="radial",cost=10000, gamma=10)
#specify timesteps for forecast, eg for all series + 12 months ahead
nd <- 1:156
#compute forecast for all the 156 months 
prognoza <- predict(svmodel, newdata=data.frame(x=nd))
actual<-data.frame(y=DF$y)
predicted<-data.frame(y=svmodel[["fitted"]])
rmse(actual$y,predicted$y)
