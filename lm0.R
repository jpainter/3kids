

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

# ==== LM ====
    start.time = Sys.time() ; start.time
  lm0 = glm(someDays ~ .*. , data=data0[training.partition0, 2:ncol(data0)], family="binomial")
    stop.time = Sys.time() ; stop.time
    t = difftime(stop.time, start.time, units="mins")
    total.time = round(as.numeric(t), digits=1)
  paste("total training time was ", total.time, " minutes")
  
  lm0
  summary(lm0)
  plot(lm0)
  lm0.predict <- predict(lm0, newdata = X.test, type = "response" )
  table(round(lm0.predict))
  confusionMatrix( round(lm0.predict) , y.test, positive="1")