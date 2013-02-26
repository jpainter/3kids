# models 


  load("data.y2.rda")
  data.nn = data.y2

  library(caret)

  # create dummy variables for Age, Sex
  age.dummy = dummyVars( ~ AgeAtFirstClaim, data = data.nn)
  age.dummy.frame = predict(age.dummy, newdata = data.y2)
  head(age.dummy.frame)
  
  sex.dummy = dummyVars( ~ Sex, data = data.nn)
  sex.dummy.frame = predict(sex.dummy, newdata = data.y2)
  head(sex.dummy.frame)
  
  data.nn = cbind( data.nn, age.dummy.frame, sex.dummy.frame)
  
  # Impute
  library(imputation)
  #   data.nn.i = kNNImpute(data.nn[1:100,], 1)
  
  # Change na to 0 ... seems accurate thing to do
  data.nn[is.na(data.nn)] = 0
  
  # limit to days >0
  someDays = which(data.nn$DaysY3 > 0)
  data.nn = data.nn[someDays,]
  
  # partition 
  training.partition = createDataPartition(data.nn[,"DaysY3"],
                                           p=.8, list = FALSE) # 80% train
  #   sample
  n = length(training.partition)
  #   sample.training = sample( training.partition, 1000, replace=FALSE)  
  # all
  sample.training = sample( training.partition, n, replace=FALSE)  
  # bootstrap
  #   sample.training = sample( training.partition, n, replace=TRUE)  
  
  # select cols
  training.cols = c(6:7, 9:87) # skip (intercept), AgeAtFirstClaim, and Year
  
  training = data.nn[sample.training, training.cols]  
  training.target = data.nn[sample.training, "DaysY3"] 
  
  test = data.nn[-sample.training, training.cols]
  test.target = data.nn[-sample.training, "DaysY3"]
  
  X = as.matrix(training)
  y = as.matrix(training.target)
  X.test = as.matrix(test)[1:1000,]
  y.test = test.target[1:1000]

  ### 2nd power feature set
  X2 = cbind(X, X^2)
  X.test2 = cbind(X.test, X.test^2)

model.x2 =  nnNg(X= X2,
           y= y,
           layer.size.mulitplier = c(1, 3, 9),
           lambdas = c(.01, .04, .16, .64, 1.2, 2.4, 4.8, 10, 20),
           maxit = 100,
           X.test= X.test2 ,
           y.test= y.test,
           model = "X2")
    
    