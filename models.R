# models 




















nnNg <- function (X= X,
                  y= y,
                  layer.size.mulitplier = c(1,3,9), 
                  lambda = c(.01, .04, .16, .64, 1.2, 2.4, 4.8, 10, 20), 
                  maxit = 100, 
                  X.test= X.test , 
                  y.test= y.test, 
                  results= results, 
                  results.iter= results.iter,
                  model = "test")