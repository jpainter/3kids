# model Any Days Hospitalized 
library(caret)

## ==================== Data
  rm(list=ls())
  load("dataY2.rda")
  data0 = dataY2
  rm(dataY2)

# Factor Days3 into quantiles for days >0
  summary(data0[data0$DaysInHospital>0,"DaysInHospital"])
  # someDays = cut(data$DaysY3,
  #                   breaks=c(0,1,3,6, 16), 
  #                   include.lowest=TRUE, ordered_result=TRUE)

  data0$someDays = factor(
                    ifelse(data0$DaysInHospital==0 & !is.na(data0$DaysInHospital), 0,
                    ifelse(!is.na(data0$DaysInHospital), 1, NA))
                    )
  table(data0$someDays)

# partition # 80% train
  training.partition0 = createDataPartition(data0[,"DaysInHospital"],
                                           p=.8, list = FALSE) 
#   sample
  sample.training0 = sample( training.partition0, 10000, replace=FALSE)
  t.all = table( data0[, "DaysInHospital"])
  t.sample = table(data0[sample.training0, "DaysInHospital"])
  t.sample
  prop.table(t.sample)
  prop.table(t.all)

# all
  #   n = length(training.partition)
  #   sample.training = sample( training.partition, n, replace=FALSE)  
  # bootstrap
  #   sample.training = sample( training.partition, n, replace=TRUE)  

# select cols
  names(data0)
  training.cols0 = c(3:123) 

# scale
  preProcValues0 <- preProcess(data0[, training.cols0], method = c("center", "scale") )

  # remove columns with zero variance
  zero.var.cols0 = c("ProcedureGroup.SMCD", "ProcedureGroup.SO", 
                    "PrimaryConditionGroup.PNCRDZ", "PrimaryConditionGroup.RENAL1", 
                    "PrimaryConditionGroup.RENAL2", "PrimaryConditionGroup.SEPSIS")

  # update training columns
  nonZero.cols0 = setdiff(names(data0[,training.cols0]), zero.var.cols0)

  # set training and cv/test data
  training0 = predict(preProcValues0 , data0[sample.training0, training.cols0] ) 
  training0 = training0[, nonZero.cols0]
  training0.target = data0[sample.training0, "someDays"] 
  table(training0.target)

  test0 = predict(preProcValues0 , data0[-sample.training0, training.cols0])
  test0 = test0[, nonZero.cols0]
  test0.target = data0[-sample.training0, "someDays"]
  table(test0.target)

  X = as.matrix(training0)
  y = as.matrix(as.numeric(training0.target))
  X.test = as.matrix(test0[1:2000,])  # should try to randomize this with setseed/sample
  y.test = as.matrix(as.numeric(test0.target[1:2000]))

# clear memory space
  rm(data0, training0, training0.target, test0, test0.target)
  source("NgModel.R")

  hidden.layer.sizes = round(ncol(X)/2) # seq(1, ncol(X), by=10)
  lambdas = 1 # c(0, 0.1, 1, 10)
  maxits = c(300)

## ===================== model0 X1
### 1st power feature set

  for (h in hidden.layer.sizes ){
    for (l in lambdas){
      for (i in maxits){
      model0.x1 =  nnNg(X.train= X, y.train= y,
                      hidden.layer.size = h,
                       lambda = l,
                       maxit = i,
                       X.cv= X.test ,
                       y.cv= y.test,
                       model = "X1")
      
      # add results to table
      if ( !exists("models0")){ load("models0.rda")} 
      models0 = rbind(models0, model0.x1$results) 
      save(models0, file="models0.rda")
    }}}
  
  costVersusLambda( model = "X1", results = models0)
  accurayVersusLambda( model = "X1", results = models0)

## ===================== model X2
### 2nd power feature set

X2 = cbind(X, X^2)
X.test2 = cbind(X.test, X.test^2)

for (layer.m in layer.ms ){
  for (l in lambdas){
    for (i in maxits){
    model0.x2 =  nnNg(X.train= X2, y.train= y,
                     layer.size.mulitplier = layer.m,
                     lambda = l,
                     maxit = i,
                     X.cv= X.test2 ,
                     y.cv= y.test,
                     model = "X2")
    
    # add results to table
    if ( !exists("models0")){ models0 = model0.x2} else {
      models0 = rbind(models0, model0.x2) }
    save(models0, file="models0.rda")
  }}}

costVersusLambda( model0 = "X2",  factor = 1)
accurayVersusLambda( model0 = "X2",  factor = 1)

## ===================== model X3
### 3rd power feature set
X3 = cbind(X2, X^3)
X.test3 = cbind(X.test2, X.test^3)+
  
  for (layer.m in layer.ms ){
    for (l in lambdas){
      for (i in maxits){
    model0.x3 =  nnNg(X.train= X3, y.train= y,
                     layer.size.mulitplier = layer.m,
                     lambda = l,
                     maxit = i,
                     X.cv= X.test3 ,
                     y.cv= y.test,
                     model = "X3")
    
    # add results to table
    if ( !exists("models0")){ models0 = model0.x3} else {
      models0 = rbind(models0, model0.x3) }
    save(models0, file="models0.rda")
  }}}

costVersusLambda( model0 = "X3",  factor = 1)
accurayVersusLambda( model0 = "X3",  factor = 1)

#==== combine model results
models = rbind(model0.x2, model0.x3)