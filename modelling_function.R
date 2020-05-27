

function_arima<-function(time_series_data)
{
  lambda <- BoxCox.lambda(time_series_data)
  arima.model<-auto.arima(time_series_data,ic="aic",test="kpss",
                          trace = FALSE,lambda = "auto",seasonal = TRUE,
                          max.p = 12,max.q = 12,max.d = 12)
}

function_nnetar<-function(time_series_data)
{
  lambda <- BoxCox.lambda(time_series_data,lower = 0)
  nnetar.model<-nnetar(time_series_data,size = 30,lambda=lambda)
}

function_mlp<-function(time_series_data,forecast_horizon)
{
  model.mlp<-nnfor::mlp(myts,m=forecast_horizon,hd=c(12,32,20),
                        comb = "median",sel.lag = TRUE,
                        difforder = 0, outplot = TRUE)
}

testFun <- function(x)
{
  svm.model.tuning <- svm(x=c(1:length(myts)),y=myts[1:length(myts)],
                          kernel="radial",gamma=2^x[1],cost = 2^x[2])
  
  rmse(myts,svm.model.tuning$fitted)
}

function_svr<-function(time_series_data,forecast_horizon)
{
  levels <- list(a = -25:25, b = -25:25)
  res <<- gridSearch(testFun, levels)
  
  svr.model <- svm(x=c(1:length(time_series_data)),y=myts[1:length(time_series_data)],
                   kernel="radial",
                   gamma=2^res$minlevels[1],
                   cost =2^res$minlevels[2] )
  
  #nd <<- (length(time_series_data)+1):(length(time_series_data)+forecast_horizon)
  #forecast.svr<-predict(svr.model,newdata = data.frame(x=nd),probability = TRUE)
  
}

function_elman<-function(time_series_data)
{
  slog<-myts[1:length(myts)]
  
  input.df<-data.frame(y=slog)
  
  var.select<-VARselect(slog)
  optimum.aic<-var.select[["selection"]][["AIC(n)"]]
  optimum.aic<-12
  
  for (a in c(1:optimum.aic)) {
    input.df<-cbind(input.df,Lag(slog,k=a))
  }
  
  input.df<-input.df[-(1:optimum.aic),]
  
  fh<-12
  
  for(iter in c(1:fh))
  {
    input.df.scaled<-scale(input.df)
    input.df.unscale<-unscale(input.df.scaled,norm.data = input.df.scaled)
    
    store.df<-input.df.scaled
    inputs<-input.df.scaled[,2:(optimum.aic+1)]
    outputs<-input.df.scaled[,1]
    
    if(iter==1)
    {
      fit<-elman(x=inputs,
                 y=outputs,
                 size=c(3,5,2),
                 maxit=500)
      plotIterativeError(fit)
      
      #print(Metrics::rmse(unscale(outputs,norm.data=input.df.scaled),
      #                    unscale(fit$fitted.values,norm.data=input.df.scaled)))
    }
    
    
    
    
    
    #ts.inputs<-ts(inputs)
    
    #rownames(input.df) <- NULL
    #fff<-data.frame(2,3,2,2,5,6,7,8)
    test1<-input.df.scaled[dim(input.df.scaled)[1],1:optimum.aic]
    #test2<-data.frame(NA,test1[,1:(optimum.aic)])
    #test1[,1:(optimum.aic)]
    label<-paste("Lag.",1:optimum.aic,sep="")
    transpose.test<-t(test1)
    colnames(transpose.test)<-label
    
    
    V1<-predict(fit,transpose.test)
    
    #unscale(transpose.test,norm.data = input.df.scaled)
    #unscale(V1,norm.data = input.df.scaled)
    combined<-cbind(unscale(V1,norm.data = input.df.scaled),
                    unscale(transpose.test,norm.data = input.df.scaled))
    colnames(combined)[1]<-"y"
    
    input.df<-rbind(input.df,combined)
  }
  
  rownames(input.df) <- NULL
  predicted.value<-input.df[(dim(input.df)[1]-fh):(dim(input.df)[1])*1,1]
  print(predicted.value)
}

function_elm<-function(time_series_data)
{
  elm.model<-elm(time_series_data,hd=c(20))
}
