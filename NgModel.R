<<"Ng-Model">>=

  load("data.y2.rda")
  
  data.nn = data.y2

library(caret)

# for predictors, convert factors to numeric
#   factors = c("AgeAtFirstClaim", "Sex")
#   library(nnet) # class.ind()
#   for (var in factors){
#     data.nn[, var] = as.integer( data.nn[, var] )
#   }

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
  
  # sample validation size
#   cv.size = floor(.2 * data.nn)
#   data.rows = seq(1:nrow(data.nn))
#   cv = (data.rows == sample.training)
#   cv = sample( seq(1:nrow(data.nn)  ))

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

  # initialize results data set
  results = data.frame(factor = NA, Theta1 = NA, Theta2 = NA, pred = NA, 
                       time = NA, maxit = NA, lambda = NA, 
                       cost = NA, accuracy = NA,  
                       accuracy.cv = NA, cost.cv = NA, model = NA)
  results.iter = results
  
# for (lambda in c(.01,.02,.04,.08,.16,.32,.64,1.2,2.4,4.8,10, 20)){

  # iterate models
  nnNg <- function (X= X,
                    y= y,
                    layer.size.mulitplier = c(1,3,9), 
                    lambdas = c(.01, .04, .16, .64, 1.2, 2.4, 4.8, 10, 20), 
                    maxit = 100, 
                    X.test= X.test , 
                    y.test= y.test, 
                    model = "test") {
    
    source('~/3kids/alphaism.R')
    num.labels = length(levels(factor(y)))
    num.input = ncol(X)
  
    for (layer.m in layer.size.mulitplier ){
      for (lambda in lambdas){
        
        
        test_cla <- nnet.train.cla(X, y, hidden_layer_size = layer.m * num.input,
                               lambda = lambda, maxit = maxit);
    
        # populate results
        results.iter$model = model
        results.iter$factor = layer.m
        results.iter$lambda = lambda
        results.iter$time = test_cla[4]
        
        # training results
        results.iter$cost = test_cla[5]
        results.iter$accuracy = test_cla[6]
        
        # table training results
        print(table(test_cla$pred, y))
        
        
        # cross validation results
        
        # table cross validation results
        pred_cla <- nnet.predict.cla(test_cla$Theta1, test_cla$Theta2, X.test)
        print(table(pred_cla, y.test))
        
        accuracy.cv = sum(pred_cla == y.test)/length(pred_cla)*100
        cat("\nValidation Set Accuracy: ", accuracy.cv ,
            "%\n", sep = "");
        results.iter$accuracy.cv = accuracy.cv
        
        nn_params = c(c(test_cla$Theta1), c(test_cla$Theta2));
        cost.cv <- costFunction.cla(nn_params = nn_params ,
                                     input_layer_size = ncol(X.test),
                                     hidden_layer_size = layer.m * num.input,
                                     num_labels = length(levels(factor(y.test))),
                                     X = X.test, y = y.test, 
                                     lambda = lambda) 
        results.iter$cost.cv = cost.cv
        
        
        # graph predicted versus real, cross validation set
        p = plot(y.test, col = "green", pch = 15);
        p = p + points(pred_cla, col = "red", pch = 3);
        legend("topright", legend = c("Original", "Predicted"), 
                       pch = c(15, 3), col = c("green", "red"));
        p = p + title( paste( as.character(layer.m), as.character(lambda), sep = ", " ) )
        print(p)
        
        # add results to table
        if ( is.na(results)[1]){results = results.iter} else {
            results = rbind(results, results.iter) }
 
    }}
    return(results)
  }



# plot cost versus lambda for test and cv
  costs= results[ which(results$factor==3),]
  cost.train = unlist(costs$cost)   
  cost.cv = costs$cost.cv
  lambdas = costs$lambda
  plot(lambdas, cost.cv, col = "red", pch = 3);
  points(lambdas, cost.train, col = "green", pch = 15);
  legend("topright", legend = c("Training", "CV"), 
         pch = c(15, 3), col = c("green", "red"));
  title("Cost versus Lambda")

  # plot accuracy versus lambda for test and cv
  accuracy= results[ which(results$factor==3),]
  accuracy.train = unlist(accuracy$accuracy)   
  accuracy.cv = accuracy$accuracy.cv
  lambdas = accuracy$lambda
  plot(lambdas, accuracy.train, col = "green", pch = 15);
  points(lambdas, accuracy.cv, col = "red", pch = 3);
  legend("topright", legend = c("Training", "CV"), 
         pch = c(15, 3), col = c("green", "red"));
  title("Accuracy versus Lambda")
  


