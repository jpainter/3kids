# models 
library(caret)

## ==================== Data
  load("dataY2.rda")
  data1 = dataY2
  rm(dataY2)

# Factor Days3 into  days >0
# limit to days >0
  someDays = which(data1$DaysInHospital > 0)
  data1 = data1[someDays,]
  data1$someDays = factor(data1$DaysInHospital)
  table(data1$someDays)

# partition # 80% train
  training.partition = createDataPartition(data1[,"someDays"],
                                           p=.8, list = FALSE) 
#   sample
  sample.training1 = sample( training.partition, 10000, replace=FALSE)
  t.all = table( data1[, "someDays"])
  t.sample = table(data1[sample.training1, "someDays"])
  t.sample
  prop.table(t.sample)
  prop.table(t.all)

  # all
  #   n = length(training.partition)
  #   sample.training = sample( training.partition, n, replace=FALSE)  
  # bootstrap
  #   sample.training = sample( training.partition, n, replace=TRUE)  
  
# select cols
  names(data1)
  training.cols = c(3:90) 

# scale
  preProcValues1 <- preProcess(data1[, training.cols], method = c("center", "scale") )

# remove columns with zero variance
  zero.var.cols1 =  c("ProcedureGroup.", "ProcedureGroup.ANES", "ProcedureGroup.SAS", "ProcedureGroup.SCS", 
  "ProcedureGroup.SMCD", "ProcedureGroup.SMS", "ProcedureGroup.SNS", "ProcedureGroup.SO", 
  "ProcedureGroup.SRS", "ProcedureGroup.SUS", "PrimaryConditionGroup.CANCRA", "PrimaryConditionGroup.CANCRM", 
  "PrimaryConditionGroup.CATAST", "PrimaryConditionGroup.CHF", "PrimaryConditionGroup.FLaELEC", 
  "PrimaryConditionGroup.HIPFX", "PrimaryConditionGroup.METAB1", "PrimaryConditionGroup.PERINTL", 
  "PrimaryConditionGroup.PNCRDZ", "PrimaryConditionGroup.RENAL1", "PrimaryConditionGroup.RENAL2", 
  "PrimaryConditionGroup.SEPSIS", "CharlsonIndex.3-4", "CharlsonIndex.5+")
  
# set training and cv/test data
  training1 = predict(preProcValues1 , data1[sample.training1, training.cols] ) 
  training1.target = data[sample.training, "someDays"] 
  table(training1.target)
  # CV(test)
  test1 = predict(preProcValues , data[-sample.training1, training.cols])
  test1.target = data[-sample.training, "someDays"]
  table(test1.target)

# update training columns
  nonZero.cols1 = setdiff(names(data[,training.cols]), zero.var.cols1)
  training1 = training1[, nonZero.cols1]
  test1 = test1[, nonZero.cols1]

  X = as.matrix(training1)
  y = as.matrix(as.numeric(training1.target))
  X.test = as.matrix(test1[1:2000,])  # should try to randomize this with setseed/sample
  y.test = as.matrix(as.numeric(test1.target[1:2000]))
  table(y.test)

# clear memory space
  rm(data1, training1, training1.target, test1, test1.target)
  source("NgModel.R")

hidden.layer.sizes = ncol(X) # seq(1, ncol(X), by=10)
lambdas = 0 # c(0, 0.1, 1, 10)
maxits = c(300)

## ===================== model0 X1
### 1st power feature set

  for (h in hidden.layer.sizes ){
    for (l in lambdas){
      for (i in maxits){
        model1.x1 =  nnNg(X.train= X, y.train= y,
                          hidden.layer.size = h,
                          lambda = l,
                          maxit = i,
                          X.cv= X.test ,
                          y.cv= y.test,
                          model = "X1")
      # add results to table
        if ( !exists("models1")){ load("models1.rda")} 
        models1 = rbind(models1, model1.x1$results) 
        save(models1, file="models1.rda")
    }}}

  costVersusLambda(results = models, model = "X1", factor = 1)
  accurayVersusLambda(results = models, model = "X1", factor = 1)

  ## ===================== model X2
  ### 2nd power feature set
  X2 = cbind(X, X^2)
  X.test2 = cbind(X.test, X.test^2)
  
for (layer.m in layer.ms ){
  for (l in lambdas){
      model.x2 =  nnNg(X.train= X2, y.train= y,
                       layer.size.mulitplier = layer.m,
                       lambda = l,
                       maxit = 50,
                       X.cv= X.test2 ,
                       y.cv= y.test,
                       model = "X2")
  
      # add results to table
      if ( !exists("models")){ models = model.x2$results} else {
        models = rbind(models, model.x2$results) }
      save(models, file="models.rda")
    }}

costVersusLambda(results = models, model = "X1", factor = 1)
accurayVersusLambda(results = models, model = "X1", factor = 1)
   
  ## ===================== model X3
  ### 3rd power feature set
  X3 = cbind(X2, X^3)
  X.test3 = cbind(X.test2, X.test^3)

for (layer.m in layer.ms ){
  for (l in lambdas){
      model.x3 =  nnNg(X.train= X3, y.train= y,
                       layer.size.mulitplier = layer.m,
                       lambda = l,
                       maxit = 50,
                       X.cv= X.test3 ,
                       y.cv= y.test,
                       model = "X3")
      
      # add results to table
      if ( is.na(models)[1]){models = model.x3} else {
        models = rbind(models, model.x3) }
      save(models, file="models.rda")
    }}

  


  