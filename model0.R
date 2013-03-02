# model Any Days Hospitalized 
library(caret)

## ==================== Data
  load("data.y2.rda")
  data = data.y2
  rm(data.y2)

# Factor Days3 into quantiles for days >0
  summary(data[data$DaysY3>0,"DaysY3"])
  # someDays = cut(data$DaysY3,
  #                   breaks=c(0,1,3,6, 16), include.lowest=TRUE, ordered_result=TRUE)
  someDays = cut(data$DaysY3,
                 breaks=c(0, 1, 16), include.lowest=TRUE)
  table(someDays)
  data$DaysY3 = as.numeric(someDays)  # convert from 1,2 to 0,1
  rm(someDays)

# create dummy variables for Age, Sex
  age.dummy = dummyVars( ~ AgeAtFirstClaim, data = data)
  age.dummy.frame = predict(age.dummy, newdata = data)
  head(age.dummy.frame)
  
  sex.dummy = dummyVars( ~ Sex, data = data)
  sex.dummy.frame = predict(sex.dummy, newdata = data)
  head(sex.dummy.frame)
#   sex.dummy.frame = sex.dummy.frame[, !(colnames(sex.dummy.frame) %in% "Sex.M")]

  data = cbind( data, age.dummy.frame, sex.dummy.frame)
  rm(age.dummy.frame, sex.dummy.frame)

# remove redundant or innapropriate columns
# skip MemberID, (Intercept), AgeAtFirstClaim, Sex, and Year
  skipCols = c("Sex", "AgeAtFirstClaim", "(Intercept)","MemberID", "Year")
  data= data[, !(names(data) %in% skipCols)]

# Impute
#   library(imputation)
#   data.nn.i = kNNImpute(data.nn[1:100,], 1)

# NAs..... Change na to 0 ... seems accurate thing to do
  data = data[!is.na(data$DaysY3),]  # remove rows with missing target
  data[is.na(data)] = 0

# partition 
  training.partition = createDataPartition(data[,"DaysY3"],
                                           p=.8, list = FALSE) # 80% train
#   sample
  sample.training = sample( training.partition, 2000, replace=FALSE)  
# all
#   n = length(training.partition)
#   sample.training = sample( training.partition, n, replace=FALSE)  
# bootstrap
#   sample.training = sample( training.partition, n, replace=TRUE)  

# select cols
  training.cols = c(2:82) 

# scale
  preProcValues <- preProcess(data[, training.cols], method = c("center", "scale") )
#   data <- predict(preProcValues, data)

# set training and cv/test data
  training = predict(preProcValues , data[sample.training, training.cols] ) 
  training.target = data[sample.training, "DaysY3"] 
  
  test = predict(preProcValues , data[-sample.training, training.cols])
  test.target = data[-sample.training, "DaysY3"]
  
  X = as.matrix(training)
  y = as.matrix(training.target)
  X.test = as.matrix(test[1:1000,])  # should try to randomize this with setseed/sample
  y.test = as.matrix(test.target[1:1000])

# clear memory space
  rm(data, training, training.target, test, test.target)
  source("NgModel.R")

layer.ms = 1 # c(1 , 3, 9 )
lambdas = 10 #  c(0, .01, .16, 1.2, 10, 100)

## ===================== model0 X1
### 1st power feature set

for (layer.m in layer.ms ){
  for (l in lambdas){
    model0.x1 =  nnNg(X.train= X, y.train= y,
                     layer.size.mulitplier = layer.m,
                     lambda = l,
                     maxit = 100,
                     X.cv= X.test ,
                     y.cv= y.test,
                     model = "X1")
    
    # add results to table
    if ( is.na(models0)[1]){models0 = model0.x1} else {
      models0 = rbind(models0, model0.x1$results) }
    save(models0, file="models0.rda")
  }}

costVersusLambda( model0 = "X1", factor = 1)
accurayVersusLambda( model0 = "X1", factor = 1)

## ===================== model X2
### 2nd power feature set

X2 = cbind(X, X^2)
X.test2 = cbind(X.test, X.test^2)

for (layer.m in layer.ms ){
  for (l in lambdas){
    model0.x2 =  nnNg(X.train= X2, y.train= y,
                     layer.size.mulitplier = layer.m,
                     lambda = l,
                     maxit = 50,
                     X.cv= X.test2 ,
                     y.cv= y.test,
                     model = "X2")
    
    # add results to table
    if ( is.na(models0)[1]){models0 = model0.x2} else {
      models0 = rbind(models0, model0.x2) }
    save(models0, file="models0.rda")
  }}

costVersusLambda( model0 = "X2",  factor = 1)
accurayVersusLambda( model0 = "X2",  factor = 1)

## ===================== model X3
### 3rd power feature set
X3 = cbind(X2, X^3)
X.test3 = cbind(X.test2, X.test^3)+
  
  for (layer.m in layer.ms ){
    for (l in lambdas){
    model0.x3 =  nnNg(X.train= X3, y.train= y,
                     layer.size.mulitplier = layer.m,
                     lambda = l,
                     maxit = 50,
                     X.cv= X.test3 ,
                     y.cv= y.test,
                     model0 = "X3")
    
    # add results to table
    if ( is.na(models0)[1]){models0 = model0.x3} else {
      models0 = rbind(models0, model0.x3) }
    save(models0, file="models.rda")
  }}

costVersusLambda( model0 = "X3",  factor = 1)
accurayVersusLambda( model0 = "X3",  factor = 1)

#==== combine model results
models = rbind(model0.x2, model0.x3)