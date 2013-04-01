
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
  nzv = nearZeroVar(X)
  if (length(nzv)>0){Xnzv = X[,-nzv2]} else {Xnzv=X}
  colnames(Xnzv)
    
# preprocess
  prePCA <- preProcess(Xnzv, method = c("center","scale","pca"))
  PCA = predict(prePCA,  Xnzv)
  preRange <- preProcess(PCA, method = c("range"))
  Range = predict(preRange,  PCA)
  summary(Range)

# formula
  f = formula( ~ .*.)
  dv = dummyVars(f, Range)
  dvf = predict(dv, newdata = Range)
  colnames(dvf) = make.names(colnames(dvf), unique = TRUE, allow_ = FALSE)
  colnames(dvf)

# preprocess, again!
  nzv2 = nearZeroVar(dvf)
  if (length(nzv2)>0){Xnzv2 = dvf[,-nzv2]} else {Xnzv2=dvf}
  colnames(Xnzv2)
  prePCA2 <- preProcess(Xnzv2, method = c("center","scale","pca"))
  PCA2 = predict(prePCA2,  Xnzv2)
  preRange2 <- preProcess(PCA2, method = c("range"))
  Range2 = predict(preRange2,  PCA2)
  summary(Range2)

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
  table(train.data[, "y"])

  X.test = Xy[-training.partition, ]
  y.test =  Xy[-training.partition,"y"]
  table(y.test)

# start
start.time = Sys.time() ; start.time
  
# Parallize
  library(doParallel)
#   registerDoParallel(cores=4)
  registerDoSEQ()
  getDoParWorkers()

  model0 = train(
                y ~ .,
                data = train.data, 
                method = "nnet",
#                 preProcess = c("range"),
                trControl = trainControl(
                  method = "cv",
                  number = 10,
                  repeats = 1,
#                   predictionBounds = c(0,1),
                  allowParallel = TRUE
                  ),
                threshold = 0.001,
                stepmax = 1e+04,
                rep = 1, 
                # ==== neural net options 
#                 tuneGrid = expand.grid(.layer1=1, .layer2=1, .layer3=1) ,
#                 algorithm = "rprop+",
#                 threshold = 0.001,
#                 stepmax = 1e+06,
# #                 err.fct="ce", 
#                 linear.ouput = TRUE, 
#                 lifesign = 'full',
#                 lifesign.step = 2000,
#                 hidden = c(10,0,0)
#                 
                # nnet options   ===== 
               tuneGrid = expand.grid(
                 .decay= c(.01, 1) , #c(0, 0.01, 0.03, 0.1, 0.3, 1), 
                 .size= c(10, 30)  #c(1, 3, 10, 30, 100) )
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
  model0.predict <- predict(model0, X.test, type="raw")
  summary(model0.predict )

# ======
  confusionMatrix(model0.predict , y.test, positive="d1")
  plot(model0)

# save
  save(model0, file="model0.rda")    

# =====
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



