library(forecast)
library(forecastHybrid)
data_outflow_5000<-data.frame(y=c(16124,19768,19339,
                                  14345,14138,11061,
                                  13066,16467,20864,
                                  8858,7390,7560,
                                  
                                  6488,15359,6701,
                                  9082,4638,8352,
                                  7760,7575,7535,
                                  12050,13257,13580,
                                  
                                  10931,10558,7824,
                                  7574,9982,10606, 
                                  10927,10954,11480, 
                                  10967,13434,12881,
                                  
                                  19290,9190,9295, 
                                  11101,10983,13737, 
                                  12246,12544,11972, 
                                  14370,27582,30735, 
                                  
                                  136470,24154,23482, 
                                  10311,38409,68227, 
                                  17580,10169,15827, 
                                  14535,13835,16903,
                                  
                                  14389,11467,11773, 
                                  10054,7228,8408, 
                                  9363,10491,11164, 
                                  11855,12062,27823,
                                  
                                  12359,11162,13188, 
                                  10894,14767,16711, 
                                  15773,16950,14382, 
                                  17494,16315,22634 
                                  ))






myts <- ts(data_outflow_5000, start=c(1994, 1), end=c(2000, 12), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

arima1<-arima(myts, order = c(1,0,0))
accuracy(arima1)
forecast(arima1, 5)
plot(forecast(arima1, 5))

arima_auto <- auto.arima(myts,trace=TRUE,start.p=1,start.q=1)
accuracy(arima_auto)
forecast(arima_auto, 5)
plot(forecast(arima_auto, 5))

nnet_ar<-nnetar(myts)
accuracy(nnet_ar)
forecast(nnet_ar, 5)

hm<-hybridModel(myts, models = "an")
plot(forecast(hm,5))
accuracy(hm)

plot(arima1[["residuals"]])
