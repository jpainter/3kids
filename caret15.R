library(caret)
rm(list=ls())

# Parallize
  library(doParallel)
  registerDoParallel(cores=4)
  getDoParWorkers()

# data
  load("dataY2.rda")
  data15 = dataY2[, c(2:15,18:118,121:128)]
  ncol(data15)
  sum(names(data15) == make.names(names(data15), unique = TRUE, allow_ = FALSE))
  rm(dataY2)

# outcome
# Factor DaysInHospital
  data15$someDays = factor(data15$DaysInHospital, levels=0:15,
                           labels=paste("d", 0:15, sep="")
                          )
  table(data15$someDays)

# partition # 80% train
  training.partition15 = createDataPartition(data15[,"someDays"],
                                          p=.8, list = FALSE) 
  table(data15[training.partition15, "someDays"])

  X = data15[training.partition15, 2:(ncol(data15)-1)]
  y = data15[training.partition15, "someDays"]
  
  X.test = data15[-training.partition15, 2:(ncol(data15)-1)]
  y.test = data15[-training.partition15, "someDays"]

# nnet

#   neural.grid = expand.grid(.layer1=c(10, 20, 40, 60), .layer2=c(0, 3, 10), .layer3=0)
  nnet.grid = expand.grid(
    .decay=c(0, 0.01, .1, 1, 10), 
    .size=c(3, 10, 30)
    )

  start.time = Sys.time() ; start.time
  model15 = train(
    someDays ~ .,
    data = data15[training.partition15[], c(2:124) ], 
    #     data = data15[, c(2:124) ],
    #     na.action = "na.omit",
    preProcess = c("BoxCox"),
    tuneGrid = nnet.grid ,  
    trControl=trainControl(
      preProcOptions = list(
        thresh=0.99),
      method = "cv",
      number=50,
      repeats = 1, 
      predictionBounds = c(0,15),
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
    # linout = FALSE,
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
  model15
  plot(model15)
  model15.predict <- predict(model15, X.test )
  table(model15.predict)
  confusionMatrix(model15.predict , y.test, positive="1")


# save
save(model15, file="model15.rda")  