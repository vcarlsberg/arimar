library(tsibble)

coba_slider<-(slider(myts,.size = 21))

length(coba_slider)

training_set<-NULL
for(a in 1:length(coba_slider))
{
  xxxx<-t(as.data.frame(coba_slider[[a]]))
  rownames(xxxx)<-NULL
  training_set<-rbind(training_set,xxxx)
}

# Load library
library(h2o)

# start h2o cluster
invisible(h2o.init())

h2o_input<-as.h2o(training_set)

aml <- h2o.automl(x = 1:20, y = 21,
                  training_frame = h2o_input,
                  seed = 72,sort_metric = "RMSE"
)

model_path <- h2o.saveModel(object=aml, path = getwd())
print(model_path)

lb<-aml@leaderboard
h2o.saveModel(aml@leader, path = "./bestmodel")

h2o.loadModel("E:\\My Documents\\coba.nn\\elman_ar\\bestmodel\\DeepLearning_grid__1_AutoML_20200609_125400_model_1")

predictions<-h2o.predict(aml, train_h)


lb <- h2o.get_leaderboard(aml)
head(lb)

modelfile <- h2o.saveModel(DeepLearning_grid__1_AutoML_20200609_125400_model_1,path = getwd())

saved_model <- h2o.loadModel(modelfile)


predictions<-h2o.predict(aml, train_h[2:20])
mape(input.df[,1],predictions)
rmse(input.df[,1],predictions)

myts

dl.model = h2o.deeplearning(x = 2:20, y = 1,
                            training_frame = train_h,
                            seed=72
)
dl.model@model

gbm.model<-h2o.gbm(x = 2:20, y = 1,
                   training_frame = train_h,
                   seed=72)

predictions <- h2o.predict(dl.model, train_h)

mape(input.df[,1],predictions)


h2o.shutdown()

tsCV()
