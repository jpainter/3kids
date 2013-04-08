library(caret)
rm(list=ls())

# data
  load("dataY2.rda")
  data15 = dataY2[, c(2:15,18:118,121:128)]
  ncol(data15)
  sum(names(data15) == make.names(names(data15), unique = TRUE, allow_ = FALSE))
  rm(dataY2)

# Factor DaysInHospital
  data15$someDays = factor(data15$DaysInHospital,
                           labels=paste("d", 0:15, sep=""),
                           ordered = TRUE
  )
  table(data15$someDays)

  y = data15$someDays
  X = data15[, 2:(ncol(data15)-1)] # remove DaysInHospital and someDays
  rm(data15)

# pca
  nzv15 = nearZeroVar(X)
  if (length(nzv15)>0){Xnzv = X[,-nzv15]} else {Xnzv=X}
  colnames(Xnzv)

# preprocess
  prePCA15 <- preProcess(Xnzv, method = c("center","scale"))#  c("center","scale","pca")
  PCA = predict(prePCA15,  Xnzv)
  preRange15 <- preProcess(PCA, method = c("range"))
  Range = predict(preRange15,  PCA)
  summary(Range)

# formula
  f = formula( ~ .*.)
  dv = dummyVars(f, Range)
  dvf = predict(dv, newdata = Range)
  colnames(dvf) = make.names(colnames(dvf), unique = TRUE, allow_ = FALSE)
  colnames(dvf)

# preprocess, again!
  nzv215 = nearZeroVar(dvf)
  if (length(nzv215)>0){Xnzv2 = dvf[,-nzv215]} else {Xnzv2=dvf}
  colnames(Xnzv2)
  prePCA215 <- preProcess(Xnzv2, method = c("center","scale")) # c("center","scale","pca")
  PCA2 = predict(prePCA215,  Xnzv2)
  preRange215 <- preProcess(PCA2, method = c("range"))
  Range2 = predict(preRange215,  PCA2)
  summary(Range2)

  process15 = list(nzv15, prePCA15, preRange15, nzv215, prePCA215, preRange215)
  save(process15, file = "process15.rda")

# data sets
  Xy = as.data.frame(Range2) # as.data.frame(df)
  Xy$y = y
  str(Xy)
  save(Xy, file="Xy15.rda")

# ======  end preprocess

  rm(list=ls())
  load("Xy15.rda")

# training index
  library(caret)  
  training.partition = createDataPartition(Xy[,"y"],
                                           p=.8, list = FALSE) 
  
  train.data = Xy[training.partition,]
# Sample, if you will
#   train.data = train.data[sample(nrow(train.data), 1000),]
    index=NA
    for ( i in 0:15){
      sd = which(train.data$y==paste("d",i, sep=""))[1:8000]
      index = c(index[!is.na(index)], sd[!is.na(sd)])     
    }
    train.data = train.data[index,]

  table(train.data[, "y"])
  
  X.test = Xy[-training.partition, ]
  y.test =  Xy[-training.partition,"y"]
  table(y.test)

# start
start.time = Sys.time() ; start.time

# Parallize
#      library(doParallel)
#     registerDoParallel(cores=4)
# # #      #   registerDoSEQ()
#      getDoParWorkers()

model15 = train(
  y ~ .,
  data = train.data, 
  method = "avNNet",
  #                 preProcess = c("range"),
  trControl = trainControl(
    method = "cv",
    number = 2,
    repeats =0,
    #                   predictionBounds = c(0,1),
    allowParallel = TRUE
  ),
#   abstol = 0.001,
#   reltol = .00001,
  rep = 1, 
  # nnet options   ===== 
  tuneGrid = expand.grid(
    .decay= c(0.1, 10), 
    .size= c(32, 96), .bag = TRUE  #c(1, 3, 10, 30, 100) )
  ),
  #                 entropy = TRUE,
  linout = FALSE,
  # softmax = FALSE,
  maxit = 1e+04,
  MaxNWts = 100000,
  trace = TRUE,
  verboseIter = TRUE
)
# stop
stop.time = Sys.time() 

# time
  t = difftime(stop.time, start.time, units="mins")
  total.time = round(as.numeric(t), digits=1)
  paste("total training time was ", total.time, " minutes")

# summary
  model15
  table(predict(model15, type="raw"))
  model15.predict <- predict(model15, X.test, type="raw")
  summary(model15.predict )

# ======
  confusionMatrix(model15.predict , y.test, positive="d0")
  plot(model15)

# save
  save(model15, file="model15_nopca.rda")    
