
  nnNg <- function (X.train= X,
                    y.train= y,
                    layer.size.mulitplier= 1, 
                    lambda = 1, 
                    maxit = 1, 
                    X.cv= X.test , 
                    y.cv= y.test, 
                    model = "test") {
    
        results = data.frame(model = NA, factor = NA, hidden = NA, lambda = NA, maxit = NA, 
                         cost = NA, cost.cv = NA, e = NA, e.cv = NA,
                         accuracy = NA, accuracy.cv = NA, 
                         ppv.cv = NA, sens.cv = NA, F1.cv = NA,
                         time = NA
                         )  
    
   
        num.labels = length(levels(factor(y.train)))
        num.input = ncol(X.train)
        
        source('alphaism.R')
        test_cla <- nnet.train.cla(X.train, y.train, 
                                   hidden_layer_size = layer.size.mulitplier * num.input,
                                   lambda = lambda, 
                                   maxit = maxit
                                   );
    
        # populate results
        results$model = model
        results$factor = layer.size.mulitplier
        results$hidden = layer.size.mulitplier * num.input
        results$lambda = lambda
        results$maxit = maxit
        results$time = test_cla[4]
        results$cost = test_cla[5]
        results$accuracy = test_cla[6]
        results$e = (sum((log(test_cla$pred + 1) - log(y.train+1))^2)/length(y.train))^0.5
        
        # table training results
        print(table(test_cla$pred, y.train))
        
        # table cross validation results
        pred_cla <- nnet.predict.cla(test_cla$Theta1, test_cla$Theta2, X.cv)
        print(table(pred_cla, y.cv))
        
        accuracy.cv = sum(pred_cla == y.cv)/length(pred_cla)*100
        cat("\nValidation Set Accuracy, model=", model, 
            ", factor=", as.character(layer.size.mulitplier * num.input), 
            ", lambda=", as.character(lambda), 
            ": ", accuracy.cv ,
            "%\n", sep = "");
        results$accuracy.cv = accuracy.cv
        results$ppv.cv = sum(pred_cla == y.cv)/as.integer(sum(pred_cla))*100
        results$sens.cv = sum(pred_cla == y.cv)/as.integer(sum(y.cv))*100
        results$F1.cv = 2*results$ppv.cv*results$sens.cv/as.integer((results$ppv.cv+results$sens.cv))
        results$e.cv = (sum((log(results$ppv.cv + 1) - log(y.cv+1))^2)/length(y.cv))^0.5
        
        nn_params = c(c(test_cla$Theta1), c(test_cla$Theta2));
        cost.cv <- costFunction.cla(nn_params = nn_params ,
                                     input_layer_size = ncol(X.cv),
                                     hidden_layer_size = layer.m * num.input,
                                     num_labels = length(levels(factor(y.test))),
                                     X = X.cv, y = y.cv, 
                                     lambda = lambda) 
        results$cost.cv = cost.cv
            
        # graph predicted versus real, cross validation set
#         p = plot(y.test, col = "green", pch = 15);
#         p = p + points(pred_cla, col = "red", pch = 3);
#         legend("topright", legend = c("Original", "Predicted"), 
#                        pch = c(15, 3), col = c("green", "red"));
#         p = p + title( paste( as.character(layer.m), as.character(lambda), sep = ", " ) )
#         print(p)
    
    return(list(results=results, Theta1=test_cla$Theta1, Theta2=test_cla$Theta2))
  }

##  =========== Diagnosti plots
costVersusLambda <- function ( model = "X1",
                               results = models
    ) {
      # plot cost versus lambda for test and cv
      costs= results[ which(results$model %in% model),]
      cost.train = unlist(costs$cost)   
      cost.cv = costs$cost.cv
      lambdas = costs$lambda
      factor = factor(costs$factor)
      
      dev()
      library(ggplot2)
      g = ggplot( aes(x=lambdas), data=data.frame() )  +
        geom_point( aes(y = cost.train, pch = factor), size = 3, color = 'green' ) +
        geom_point( aes(y = cost.cv, pch = factor), size = 3, color = 'red' ) +
        ggtitle(paste("Cost versus Lambda,", model, ", ", as.character(factor)))
      print(g)
      return(costs) 
}
  
costVersusFactor <- function ( model = "X1",
                                 results = models
  ) {
    # plot cost versus lambda for test and cv
    costs= results[ which(results$model %in% model),]
    cost.train = unlist(costs$cost)   
    cost.cv = costs$cost.cv
    lambdas = factor(costs$lambda)
    factor = costs$factor
    
    dev()
    library(ggplot2)
    g = ggplot( aes(x=factor), data=data.frame() )  +
      geom_point( aes(y = cost.train, pch = lambda), size = 3, color = 'green' ) +
      geom_point( aes(y = cost.cv, pch = lambda), size = 3, color = 'red' ) +
      ggtitle(paste("Cost versus Lambda,", model, ", ", as.character(factor)))
    print(g)
    return(costs) 
  }
  

accurayVersusLambda <- function (model = "X2",
                                 results = models) {
  # plot accuracy versus lambda for test and cv
  accuracys= results[ results$model %in% model,]
  accuracy.train = unlist(accuracys$accuracy)   
  accuracy.cv = accuracys$accuracy.cv
  lambdas = accuracys$lambda
  factor = factor(accuracys$factor)
  
  library(ggplot2)
  g = ggplot( aes(x=lambdas), data=data.frame() )  +
    geom_point( aes(y = accuracy.train, pch = factor), size = 3, color = 'green' ) +
    geom_point( aes(y = accuracy.cv, pch = factor), size = 3, color = 'red' ) +
    ggtitle(paste("Accuracy versus Lambda,", model, ", ", as.character(factor)))
  print(g)
  
}



