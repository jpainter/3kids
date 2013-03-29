

library(caret)

#Parallize

  library(doParallel)
  cl <- makeCluster(4)
  registerDoParallel(cl)

# set training and cv/test data
training0 = predict(preProcValues0 , filteredData0[training.partition0, training.cols0] ) 
training0.target = data0[training.partition0, "someDays"] 
table(training0.target)

# CV(test)
test0 = predict(preProcValues0 , data0[-training.partition0, training.cols0])
test0.target = data0[-training.partition0, "someDays"]
table(test0.target)

#Build model
  # X <- train.set[,-1]
  X = training0
  # Y <- factor(train.set[,1],levels=c('N','Y'))
  Y = training0.target
  
  === LDA
  Y = factor(Y)
  lda <- train(X,Y,method='lda')
  print(lda)
  pred.lda <- predict(lda ,test0)
  confusionMatrix(pred.lda ,test0.target)
  
  === SVM
  svm <- train(X,Y, method='nnet')
  print(svm)
  pred.svm <- predict(svm,test0)
  confusionMatrix(pred.svm,test0.target)


