
library(caret)
rm(list=ls())

# data
  load("dataY2.rda")
  data1 = dataY2[, c(2:15,18:118,121:128)]
  names(data1)
  # Check if names ok
  sum(names(data1) == make.names(names(data1), unique = TRUE, allow_ = FALSE))
  rm(dataY2)

# outcome
  # Factor Days3 into  days >0
  # limit to days >0
  someDays = which(data1$DaysInHospital > 0)
  data1 = data1[someDays,]
  # factor
  data1 = cbind(someDays = factor(data1$DaysInHospital),
                data1[,2:ncol(data1)])
  # no factor
  data1 = cbind(someDays = data1$DaysInHospital,
                                 data1[, 2:ncol(data1)])

  table(data1$someDays)

  X = data1[, 2:(ncol(data1))]
  names(X)

#   y = data1[training.partition1, "someDays"]
# 
#   X.test = data1[-training.partition1, 2:(ncol(data1)-1)]
#   y.test = data1[-training.partition1, "someDays"]

# preprocess
  preProc1 <- preProcess( X, method = c("range") )
  preProcData1 = predict(preProc1,  X)

# formula
  #   library(Formula)
  f = formula( ~ .*.)
  dv1 = dummyVars(f, preProcData1[])
  df1 = predict(dv1, newdata = preProcData1[])
  colnames(df1) = make.names(colnames(df1), unique = TRUE, allow_ = FALSE)
  colnames(df1)
  #   df1 = as.data.frame(df1)


# pca
  nzv = nearZeroVar(df1)
  X1 = df1[,-nzv]

# see effect of pca
#   pca1 <- preProcess( df1[,-nzv], method = c("pca") , thresh=0.99)
#   pcaData1 = predict(pca1,  df1[,-nzv], )
#   dim(pcaData1)
#   colnames(pcaData1)

# data sets
  y = data1$someDays
  X1y = cbind(y, X1)

# training index
  training.partition1 = createDataPartition(X1y[,"y"],
                                          p=.8, list = FALSE) 
  table(X1y[training.partition1, "y"])
  
  X1.test = X1y[-training.partition1, ]
  y.test =  X1y[-training.partition1,"y"]

# randomize order
#   rand <- sample(nrow(X1y))
#   X1y = X1y[rand,]

# nnet

  neural.grid = expand.grid(.layer1=c(3, 10, 30, 100), 
                            .layer2=c(0, 3, 10), .layer3=0)
  nnet.grid = expand.grid(
    .decay= c(0.001, 0.01, 1, 30), 
    .size= c(3, 10, 30)
    )

# Parallize
  library(doParallel)
  registerDoParallel(cores=4)
  getDoParWorkers()

  start.time = Sys.time() ; start.time
  model1 = train(
    y ~ . ,
    # data = data1[tx1raining.partition1[], c(2:124) ], 
    data = X1y,
#     na.action = "na.omit",
#     preProcess = c("pca"),
    tuneGrid = nnet.grid ,  
    trControl=trainControl(
      preProcOptions = list(thresh=0.99),
      method = "cv",
      number=50,
      repeats = 25, 
      predictionBounds = c(1,15),
      allowParallel = TRUE,
      verboseIter = TRUE
    ),
    rep = 1,  
    method = "nnet",
    # ==== neural net options ====
      # algorithm = "rprop+",
      # threshold = 0.01,
      # stepmax = 1e+06,
      # err.fct="ce", 
      # linear.ouput = FALSE, 
      # lifesign = 'full',
      # lifesign.step = 2000
    # nnet options   ===== 
      entropy = TRUE,
      linout = FALSE,
      # softmax = FALSE,
      maxit = 1e+05,
      trace = TRUE    
 )
  stop.time = Sys.time() 

# time
  stop.time
  t = difftime(stop.time, start.time, units="mins")
  total.time = round(as.numeric(t), digits=1)
  paste("total training time was ", total.time, " minutes")

# summary
  model1
  plot(model1)
  model1.predict <- predict(model1, newdata=X1.test )
  table(model1.predict)
  confusionMatrix(model1.predict , y.test, positive="1")
  

# save
save(model1, file="model1.rda")  