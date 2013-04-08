
library(caret)
rm(list=ls())

# data
  load("dataY2.rda")
  data1 = dataY2[, c(2:15,18:118,121:128)]
  ncol(data1)
  sum(names(data1) == make.names(names(data1), unique = TRUE, allow_ = FALSE))
  rm(dataY2)

# Factor DaysInHospital
  data1 = data1[data1$DaysInHospital>0,]
  data1$someDays = factor(data1$DaysInHospital,
                           labels=paste("d", 1:15, sep=""),
                           ordered = TRUE
  )
  table(data1$someDays)

  y = data1$someDays
  X = data1[, 2:(ncol(data1)-1)] # remove DaysInHospital and someDays
  rm(data1)

# pca
  nzv1 = nearZeroVar(X)
  if (length(nzv1)>0){Xnzv = X[,-nzv1]} else {Xnzv=X}
  colnames(Xnzv)

# preprocess
  prePCA1 <- preProcess(Xnzv, method = c("center","scale","pca"))
  PCA = predict(prePCA1,  Xnzv)
  preRange1 <- preProcess(PCA, method = c("range"))
  Range = predict(preRange1,  PCA)
  summary(Range)

# formula
  f = formula( ~ .*.)
  dv = dummyVars(f, Range)
  dvf = predict(dv, newdata = Range)
  colnames(dvf) = make.names(colnames(dvf), unique = TRUE, allow_ = FALSE)
  colnames(dvf)

# preprocess, again!
  nzv21 = nearZeroVar(dvf)
  if (length(nzv21)>0){Xnzv2 = dvf[,-nzv21]} else {Xnzv2=dvf}
  colnames(Xnzv2)
  prePCA21 <- preProcess(Xnzv2, method = c("center","scale","pca"))
  PCA2 = predict(prePCA21,  Xnzv2)
  preRange21 <- preProcess(PCA2, method = c("range"))
  Range2 = predict(preRange21,  PCA2)
  summary(Range2)

  process1 = list(nzv1, prePCA1, preRange1, nzv21, prePCA21, preRange21)
  save(process1, file = "process1.rda")

# data sets
  Xy = as.data.frame(Range2) # as.data.frame(df)
  Xy$y = y
  str(Xy)
  save(Xy, file="Xy1.rda")

# ======  end preprocess

  rm(list=ls())
  load("Xy1.rda")
# training index
  training.partition = createDataPartition(Xy[,"y"],
                                         p=.8, list = FALSE) 
  table(Xy[, "y"])


  train.data = Xy[training.partition,]
# Sample, if you will
#   train.data = train.data[sample(nrow(train.data), 1000),]
    index=NA
    for ( i in 1:15){
      sd = which(train.data$y==paste("d",i, sep=""))[1:100]
      index = c(index[!is.na(index)], sd[!is.na(sd)])     
    }
    train.data = train.data[index,]

  table(train.data["y"])

  X.test = Xy[-training.partition, ]
  y.test =  Xy[-training.partition,"y"]
  table(y.test)

# Parallize
#   library(doParallel)
#   #   registerDoParallel(cores=4)
#   registerDoSEQ()
#   getDoParWorkers()

# start
start.time = Sys.time() ; start.time

model1 = train(
    y ~ .,
    data = train.data, 
    method = "nnet",
    #                 preProcess = c("range"),
    trControl = trainControl(
      method = "cv",
      number = 5,
      repeats = 2,
      #                   predictionBounds = c(0,1),
      allowParallel = TRUE
    ),
    threshold = 0.001,
    stepmax = 1e+04,
    rep = 1, 
    # nnet options   ===== 
    tuneGrid = expand.grid(
      .decay= c(.001) , #c(0, 0.01, 0.03, 0.1, 0.3, 1), 
      .size= c(60)  #c(1, 3, 10, 30, 100) )
    ),
    # entropy = TRUE,
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
  stop.time
  t = difftime(stop.time, start.time, units="mins")
  total.time = round(as.numeric(t), digits=1)
  paste("total training time was ", total.time, " minutes")

# summary
  model1
  table(predict(model1, type="raw"))
  model1.predict <- predict(model1, X.test, type="raw")
  summary(model1.predict )

# ======
  confusionMatrix(model1.predict , y.test)
  plot(model1)

# save
  save(model1, file="model1.rda")    


