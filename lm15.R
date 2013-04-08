
rm(list=ls())



# data
  load("dataY2.rda")
  data15 = dataY2[, c(2:15,18:118,121:128)]
  ncol(data15)
  sum(names(data15) == make.names(names(data15), unique = TRUE, allow_ = FALSE))
  rm(dataY2)

# outcome
# Factor DaysInHospital
  data15$someDays = factor(data15$DaysInHospital, levels=0:15,
                           labels=paste("d", 0:15, sep=""),
                           ordered = TRUE
  )
  table(data15$someDays)

# partition # 80% train
  library(caret)
  training.partition15 = createDataPartition(data15[,"someDays"],
                                             p=.8, list = FALSE) 
  table(data15[training.partition15, "someDays"])
  
  X = data15[training.partition15, 2:(ncol(data15)-1)]
  y = data15[training.partition15, "someDays"]
  
  X.test = data15[-training.partition15, 2:(ncol(data15)-1)]
  y.test = data15[-training.partition15, "someDays"]
  table(y.test)

# ==== LM ====
  start.time = Sys.time() ; start.time

# Parallize
  library(doParallel)
  registerDoParallel(cores=4)
  getDoParWorkers()

  lm15 = glm(someDays ~ . , 
             data=data15[sample(training.partition15[], 10000), 2:ncol(data15)], 
             family="binomial")

    stop.time = Sys.time() ; stop.time
    t = difftime(stop.time, start.time, units="mins")
    total.time = round(as.numeric(t), digits=1)
  paste("total training time was ", total.time, " minutes")

  lm15
  plot(lm15)
  lm15.predict <- round(predict(lm15, newdata = X.test, type = "response" ))
  table(lm15.predict)
  confusionMatrix(lm15.predict , y.test, positive="1")

# caret alternatives
  library(caret)
  start.time = Sys.time() ; start.time
  model15 = train(
    someDays ~ .,
    data = data15[sample(training.partition15[], 1000), c(2:124) ], 
    preProcess = c("BoxCox"),
    model = "multinom"
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

