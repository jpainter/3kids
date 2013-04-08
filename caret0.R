
library(caret)
  rm(list=ls())

# data
  load("dataY2.rda")
  data0 = dataY2[, c(2:15,18:118,121:128)]
  ncol(data0)
  sum(names(data0) == make.names(names(data0), unique = TRUE, allow_ = FALSE))
  rm(dataY2)

# Factor DaysInHospital
  data0$someDays = cut(data0$DaysInHospital, breaks=c(0,1,16),
                       labels=c("d0", "d1"),
                          ordered_result = TRUE, right=FALSE
                    )
  table(data0$someDays)

  y = data0$someDays
  X = data0[, 2:(ncol(data0)-1)] # remove DaysInHospital and someDays
  rm(data0)

# pca
  nzv0 = nearZeroVar(X)
  if (length(nzv0)>0){Xnzv = X[,-nzv0]} else {Xnzv=X}
  colnames(Xnzv)
    
# preprocess
  prePCA0 <- preProcess(Xnzv, method = c("center","scale","pca"))
  PCA = predict(prePCA0,  Xnzv)
  preRange0 <- preProcess(PCA, method = c("range"))
  Range = predict(preRange0,  PCA)
  summary(Range)

# formula
  f = formula( ~ .*.)
  dv = dummyVars(f, Range)
  dvf = predict(dv, newdata = Range)
  colnames(dvf) = make.names(colnames(dvf), unique = TRUE, allow_ = FALSE)
  colnames(dvf)

# preprocess, again!
  nzv20 = nearZeroVar(dvf)
  if (length(nzv20)>0){Xnzv2 = dvf[,-nzv20]} else {Xnzv2=dvf}
  colnames(Xnzv2)
  prePCA20 <- preProcess(Xnzv2, method = c("center","scale","pca"))
  PCA2 = predict(prePCA20,  Xnzv2)
  preRange20 <- preProcess(PCA2, method = c("range"))
  Range2 = predict(preRange20,  PCA2)
  summary(Range2)

  process0 = list(nzv0, prePCA0, preRange0, nzv20, prePCA20, preRange20)
  save(process0, file = "process0.rda")

# data sets
  Xy = as.data.frame(Range2) # as.data.frame(df)
  Xy$y = y
  str(Xy)
  save(Xy, file="Xy.rda")
  

# ======  end preprocess

  rm(list=ls())
  load("Xy.rda")
# training index
  training.partition = createDataPartition(Xy[,"y"],
                                          p=.8, list = FALSE) 

  train.data = Xy[training.partition,]
  # Sample, if you will
  #   train.data = train.data[sample(nrow(train.data), 1000),]
    index=NA
    for ( i in 0:15){
      sd = which(train.data$y==paste("d",i, sep=""))[1:3000]
      index = c(index[!is.na(index)], sd[!is.na(sd)])     
    }
    train.data = train.data[index,]

  table(train.data[, "y"])

  X.test = Xy[-training.partition, ]
  y.test =  Xy[-training.partition,"y"]
  table(y.test)
  
# Parallize
#   library(doParallel)
# #   registerDoParallel(cores=4)
#   registerDoSEQ()
#   getDoParWorkers()

# start
  start.time = Sys.time() ; start.time
  model0 = train(
                y ~ .,
                data = train.data, 
                method = "nnet",
#                 preProcess = c("range"),
                trControl = trainControl(
                  method = "cv",
                  number = 2,
                  repeats = 0,
#                   predictionBounds = c(0,1),
                  allowParallel = TRUE
                  ),
                threshold = 0.001,
                stepmax = 1e+04,
                rep = 1,     
                # nnet options   ===== 
               tuneGrid = expand.grid(
                 .decay= c(.01) , #c(0, 0.01, 0.03, 0.1, 0.3, 1), 
                 .size= c(33,66)  #c(1, 3, 10, 30, 100) )
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
  stop.time
  t = difftime(stop.time, start.time, units="mins")
  total.time = round(as.numeric(t), digits=1)
  paste("total training time was ", total.time, " minutes")

# summary
  model0
  table(predict(model0, type="raw"))
  model0.predict <- predict(model0, X.test, type="raw")
  summary(model0.predict )

# ======
  confusionMatrix(model0.predict , y.test, positive="d1")
  plot(model0)

# save
  save(model0, file="model0.rda")    



