  library(caret)
  
## ==================== Data
  load("dataY2.rda")
  data0 = dataY2
  rm(dataY2)
  
  # # rename columns to remove weird characters and spaces
  #   cn = colnames(data1) 
  #   library(stringr)
  #   colnames(data1) = str_replace_all(cn, "[^[:alnum:]]" , ".")
  #   colnames(data1)
  
# Factor Days3 into quantiles for days >0
  summary(data0[data0$DaysInHospital>0,"DaysInHospital"])
#   data0$someDays = factor(
#     ifelse(data0$DaysInHospital==0 & !is.na(data0$DaysInHospital), 0,
#            ifelse(!is.na(data0$DaysInHospital), 1, NA))
#   )
  data0$someDays = 
    ifelse(data0$DaysInHospital==0 & !is.na(data0$DaysInHospital), 0,
           ifelse(!is.na(data0$DaysInHospital), 1, NA))
  
  table(data0$someDays)
  
  # all
  #   n = length(training.partition)
  #   sample.training = sample( training.partition, n, replace=FALSE)  
  # bootstrap
  #   sample.training = sample( training.partition, n, replace=TRUE)  
  
# select cols
  data0.cols = names(data0)
  training.cols0 = setdiff(data0.cols, c("MemberID", "DaysInHospital", "someDays")) 
  filteredData0 = data0[, training.cols0]
  
# columns with zero variance
  zero.var.cols0 = nearZeroVar(filteredData0, uniqueCut = .01)
  paste(length(zero.var.cols0),"columns with near zero variance: ", 
        paste(names(filteredData0)[zero.var.cols0], collapse = ", "))
  training.cols0 = training.cols0[-zero.var.cols0]
  filteredData0 = data0[, training.cols0]
  dim(filteredData0)
  
# cols with high correlation
  descrCor <- cor(filteredData0)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.95)
  paste(length(highlyCorDescr), "columns with high correlation: ", 
        paste(names(filteredData0)[highlyCorDescr], collapse = ", "))
  training.cols0 = training.cols0[-highlyCorDescr]
  
# linear combinations
  linearCombos = findLinearCombos(filteredData0)
  paste(length(linearCombos$remove), "columns are linear combinations of each other: ", 
        paste(names(filteredData0)[linearCombos$remove], collapse = ", "))
  training.cols0 = training.cols0[-linearCombos$remove]
  
# final training columns
  paste(length(training.cols0), "training columns: ", 
        paste(training.cols0, collapse = ", "))
  
  # reveiw data
  #   library(tables)
  #   vars = paste("Factor(", names(filteredData1), ") ", collapse=" + ", sep="")
  #   table.formula = as.formula(paste( vars, " ~ 1"))
  #   #   table.formula = as.formula("Factor(AgeAtFirstClaim.)  + Factor(AgeAtFirstClaim.0.9) ~ 1")
  #   tabular(table.formula, data = filteredData1)
  
  # scale
  filteredData0 = subset(data0, select=training.cols0)
  preProcValues0 <- preProcess(filteredData0, method = c("scale") )
  
  # pca
  
  
  # partition # 80% train
  training.partition0 = createDataPartition(data0[,"someDays"],
                                           p=.8, list = FALSE) 
  #   sample
#   sample.training0 = sample( training.partition, 5000, replace=FALSE)
#   t.all = table( data0[, "someDays"])
#   t.sample = table(data0[sample.training0, "someDays"])
#   t.sample
#   prop.table(t.sample)
#   prop.table(t.all)
  
  # set training and cv/test data
  training0 = predict(preProcValues0 , filteredData0[training.partition0, training.cols0] ) 
  
#   training0 = data0[training.partition0, training.cols0]
  training0.target = data0[training.partition0, "someDays"] 
  table(training0.target)
  
  # CV(test)
  test0 = predict(preProcValues0 , data0[-training.partition0, training.cols0])
  
  test0 = data0[-training.partition0, training.cols0]
  test0.target = data0[-training.partition0, "someDays"]
  table(test0.target)
  
  X0 = as.matrix(training0)
  y0 = as.matrix(as.numeric(training0.target))
  X0.test = as.matrix(test0)  
  y0.test = as.matrix(as.numeric(test0.target))
  table(y0.test)
  
  # clear memory space
#   rm(list= setdiff( ls(),  
#                     c("X","y","X.test", "y.test"))
 
  X0.2 = cbind(X0, X0^2)
  X0.2.test = cbind(X0.test, X0.test^2)
  X0.3 = cbind(X0.2, X0^3)
  X0.3.test = cbind(X0.2.test, X0.test^3)
  
  ## ===================== model0 X1 
  ### 1st power feature set
  
  source("NgModel.R")
  
  hidden.layer.sizes = ncol(X) # seq(1, ncol(X), by=10)
  lambdas = 0 # c(0, 0.1, 1, 10)
  maxits = c(1, 5, 50)
  
  for (h in hidden.layer.sizes ){
    for (l in lambdas){
      for (i in maxits){
        model1.x1 =  nnNg(X.train= X0, y.train= y0,
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
    Xy0 = as.data.frame(cbind(y0, X0))
    Xcol.num0 = length(colnames(Xy0))
    colnames(Xy0)[1] = "days"
    Xy0$days = factor(Xy0$days)
    Xvars0 = paste(colnames(Xy0)[2:15], collapse= " + ")
    nn.formula0 = as.formula( paste("days ~ ", Xvars0, sep=""))
#     nn.formula0 = as.formula( "days ~ .")
  
    start.time = Sys.time() ; start.time
#     nn.grid <- expand.grid(.decay = c(10, 1, 0.01), .size = c(Xcol.num0))
      nn.grid <- expand.grid(.decay = c(0, 1), .size = c(10,30,100))
        model0 = train(X, Y,
#           form = nn.formula0,
#           data = Xy0,
          method = "nnet",
          entropy = TRUE,
#           tuneGrid = nn.grid, 
          maxit = 10000, 
          repeats = 1,
          trace=TRUE,
          lifesign = 'small',
          lifesign.test = 200
        ) 
    
    stop.time = Sys.time() ; stop.time
    t = difftime(stop.time, start.time, units="mins")
    total.time = round(as.numeric(t), digits=1)
    paste("total training time was ", total.time, " minutes")
    model0
    model0.predict <- predict(model0, newdata = X0.test)
    confusionMatrix(model0.predict ,test0.target)
    save(model0, file="model0.rda")    
    
  model0.predict <- predict(model0, newdata = X0.test)
  table(model0.predict, y0.test)
  model0.e <- sqrt( mean( (log(as.numeric(model0.predict)) - log(as.numeric(y0.test)) )^2 ))
  