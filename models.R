# models 
library(caret)

## ==================== Data
  load("dataY2.rda")
  data1 = dataY2
  rm(dataY2)

# # rename columns to remove weird characters and spaces
#   cn = colnames(data1) 
#   library(stringr)
#   colnames(data1) = str_replace_all(cn, "[^[:alnum:]]" , ".")
#   colnames(data1)

# Factor Days3 into  days >0
# limit to days >0
  someDays = which(data1$DaysInHospital > 0)
  data1 = data1[someDays,]
  data1$someDays = factor(data1$DaysInHospital)
  table(data1$someDays)

  # all
  #   n = length(training.partition)
  #   sample.training = sample( training.partition, n, replace=FALSE)  
  # bootstrap
  #   sample.training = sample( training.partition, n, replace=TRUE)  
  
# select cols
  data1.cols = names(data1)
  training.cols1 = setdiff(data1.cols, c("MemberID", "DaysInHospital", "someDays")) 
  filteredData1 = data1[, training.cols1]

# columns with zero variance
  zero.var.cols1 = nearZeroVar(filteredData1, uniqueCut = .02)
  paste(length(zero.var.cols1),"columns with near ero variance: ", 
        paste(names(filteredData1)[zero.var.cols1], collapse = ", "))
  training.cols1 = training.cols1[-zero.var.cols1]
  filteredData1 = data1[, training.cols1]
  dim(filteredData1)
  
# cols with high correlation
  descrCor <- cor(filteredData1)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.9)
  paste(length(highlyCorDescr), "columns with high correlation: ", 
        paste(names(filteredData1)[highlyCorDescr], collapse = ", "))
  training.cols1 = training.cols1[-highlyCorDescr]

# linear combinations
  linearCombos = findLinearCombos(filteredData1)
  paste(length(linearCombos$remove), "columns are linear combinations of each other: ", 
      paste(names(filteredData1)[linearCombos$remove], collapse = ", "))
  training.cols1 = training.cols1[-linearCombos$remove]
  
  paste(length(training.cols1), "training columns: ", 
      paste(training.cols1, collapse = ", "))

# reveiw data
#   library(tables)
#   vars = paste("Factor(", names(filteredData1), ") ", collapse=" + ", sep="")
#   table.formula = as.formula(paste( vars, " ~ 1"))
#   #   table.formula = as.formula("Factor(AgeAtFirstClaim.)  + Factor(AgeAtFirstClaim.0.9) ~ 1")
#   tabular(table.formula, data = filteredData1)

# scale
  filteredData1 = subset(data1, select=training.cols1)
  preProcValues1 <- preProcess(filteredData1, method = c("center", "scale") )

  # pca


  # partition # 80% train
  training.partition = createDataPartition(data1[,"someDays"],
                                           p=.8, list = FALSE) 
  #   sample
  sample.training1 = sample( training.partition, 5000, replace=FALSE)
  t.all = table( data1[, "someDays"])
  t.sample = table(data1[sample.training1, "someDays"])
  t.sample
  prop.table(t.sample)
  prop.table(t.all)

# set training and cv/test data
  training1 = predict(preProcValues1 , filteredData1[sample.training1, training.cols1] ) 
  training1.target = data1[sample.training1, "someDays"] 
  table(training1.target)

  # CV(test)
  test1 = predict(preProcValues1 , data1[-training.partition, training.cols1])
  test1.target = data1[-training.partition, "someDays"]
  table(test1.target)

  X = as.matrix(training1)
  y = as.matrix(as.numeric(training1.target))
  X.test = as.matrix(test1)  
  y.test = as.matrix(as.numeric(test1.target))
  table(y.test)

# clear memory space
#   rm(list= c("X","y","X.test", "y.test"))

  source("NgModel.R")

  hidden.layer.sizes = ncol(X) # seq(1, ncol(X), by=10)
  lambdas = 0 # c(0, 0.1, 1, 10)
  maxits = c(1, 5, 50)

## ===================== model0 X1
### 1st power feature set

X2 = cbind(X, X^2)
X2.test = cbind(X.test, X.test^2)
X3 = cbind(X2, X^3)
X3.test = cbind(X2.test, X.test^3)

  for (h in hidden.layer.sizes ){
    for (l in lambdas){
      for (i in maxits){
        model1.x1 =  nnNg(X.train= X, y.train= y,
                          hidden.layer.size = h,
                          lambda = l,
                          maxit = i,
                          X.cv= X3.test ,
                          y.cv= y.test,
                          model = "X3")
      # add results to table
        if ( !exists("models1")){ load("models1.rda")} 
        models1 = rbind(models1, model1.x1$results) 
        save(models1, file="models1.rda")
    }}}

  costVersusLambda(results = models, model = "X1", factor = 1)
  accurayVersusLambda(results = models, model = "X1", factor = 1)

## ===================== nnet
library(caret)
    Xy = as.data.frame(cbind(y, X))
    Xcol.num = length(colnames(Xy))
    colnames(Xy)[1] = "days"
    Xy$days = factor(Xy$days)
    Xvars = paste(colnames(Xy)[2:Xcol.num], collapse= " + ")
    nn.formula = as.formula( paste("days ~ ", Xvars, sep=""))

    start.time = Sys.time() ; start.time
      nn.grid <- expand.grid(.decay = c(1, 0.01), .size = c(5, 100))
      model1 = train(
                    form = nn.formula,
                    data = Xy,
                    method = "nnet",
                    tuneGrid = nn.grid, 
                    maxit = 10000,                    
                    trace=TRUE,
                    lifesign = 'small',
                    lifesign.test = 200
                    ) 
  
    stop.time = Sys.time() ; stop.time
    t = difftime(stop.time, start.time, units="mins")
    total.time = round(as.numeric(t), digits=1)
    paste("total training time was ", total.time)
    model1
    save(model1, file="model1.rda")    

    model1.predict <- predict(model1, newdata = X.test)
    table(model1.predict)
    model1.e <- sqrt( mean( (log(as.numeric(model1.predict)) - log(as.numeric(y.test)) )^2 ))



  