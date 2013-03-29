
library(caret)
  rm(list=ls())

# Parallize
  library(doParallel)
  registerDoParallel(cores=4)
  getDoParWorkers()

# data
  load("dataY2.rda")
  data0 = dataY2[, c(2:15,18:118,121:128)]
  ncol(data0)
  sum(names(data0) == make.names(names(data0), unique = TRUE, allow_ = FALSE))
  rm(dataY2)

# outcome
  data0$someDays = factor(
    ifelse(data0$DaysInHospital==0 & !is.na(data0$DaysInHospital), 0,
           ifelse(!is.na(data0$DaysInHospital), 1, NA))
  )
#   data0$someDays = 
#     ifelse(data0$DaysInHospital==0 & !is.na(data0$DaysInHospital), 0,
#            ifelse(!is.na(data0$DaysInHospital), 1, NA))
  table(data0$someDays)

# partition # 80% train
  training.partition0 = createDataPartition(data0[,"someDays"],
                                          p=.7, list = FALSE) 
  table(data0[training.partition0, "someDays"])

  X = data0[training.partition0, 2:(ncol(data0)-1)]
  y = data0[training.partition0, "someDays"]

  X.test = data0[-training.partition0, 2:(ncol(data0)-1)]
  y.test = data0[-training.partition0, "someDays"]

# nnet
  
  nnet.grid = expand.grid(
    .decay=c(0, 0.01, 0.03, 0.1, 0.3, 1), 
    .size=c(1, 3, 10, 30, 100))

  neural.grid = expand.grid(.layer1=c(10), .layer2=c(0), .layer3=0)
  
  start.time = Sys.time() ; start.time
  model0 = train(
                someDays ~ .,
                data = data0[training.partition0[], c(2:124) ], 
                method = "nnet",
                preProcess = c("ica"),
                tuneGrid = nnet.grid ,
                trControl = trainControl(
                  method = "cv",
                  number = 3,
                  repeats = 1,
                  predictionBounds = c(0,1),
                  allowParallel = TRUE
                  ),
                threshold = 0.01,
                stepmax = 1e+06,
                rep = 1, 
                err.fct = "ce", 
                linear.output = FALSE ,
                lifesign = 'full',
                lifesign.step = 2000)
  stop.time = Sys.time() 

# time
  stop.time
  t = difftime(stop.time, start.time, units="mins")
  total.time = round(as.numeric(t), digits=1)
  paste("total training time was ", total.time, " minutes")

# summary
  model0
  model0.predict <- predict(model0, X.test )
  pred0=ifelse(model0.predict>=0.5,1,0)
  pred0r = round(model0.predict)
  table(pred0, pred0r)
  confusionMatrix(pred0 , y.test, positive="1")
  plot(model0)

# save
  save(model0, file="model0.rda")    

print(net <- neuralnet(formula = someDays ~ AgeAtFirstClaim. + AgeAtFirstClaim.0.9 + 
                         AgeAtFirstClaim.10.19 + AgeAtFirstClaim.20.29 + AgeAtFirstClaim.30.39 + 
                         AgeAtFirstClaim.40.49 + AgeAtFirstClaim.50.59 + AgeAtFirstClaim.60.69 + 
                         AgeAtFirstClaim.70.79 + AgeAtFirstClaim.80. + Sex. + Sex.F + Sex.M + 
                         ProcedureGroup. + ProcedureGroup.ANES , 
                       data = data0[training.partition0, c(2:16, 124)], 
                       hidden=10,
                       rep=1, 
                       stepmax = 10000, 
                       err.fct="ce", 
                       linear.output=FALSE,
                       lifesign = 'minimal',
                       lifesign.step = 10))



