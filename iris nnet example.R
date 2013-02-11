nnet example from http://mkseo.pe.kr/stats/?p=17

# install.packages(“nnet”) ----
library(nnet)
data(iris)
ir = rbind(iris3[,,1], iris3[,,2], iris3[,,3])

targets = class.ind(c(rep("s", 50), rep("c", 50), rep("v", 50)))
targets[1:5,]
           
set.seed(1234)           
samp = c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
           
ir1 = nnet(ir[samp,], targets[samp,], size=3, rang=0.1, decay=5e-4, maxit=10)
           
test.cl = function(true, pred) {
  true = max.col(true)
  cres = max.col(pred)
  table(true, cres)
}
           
predict(ir1, ir[1:10,])
test.cl(targets[-samp,], predict(ir1, ir[-samp,]))

err = function(true, pred) {
  true = max.col(true)
  cres = max.col(pred)
  sqe = (true-cres)^2
  sse = sqrt(sum(sqe))
  sse
}
err(targets[-samp,], predict(ir1, ir[-samp,]))

n = 10
err.test = data.frame(iter=rep(NA,n), err=rep(NA,n))
for (i in 1:n){
  iter = 3^i
  ir.i= nnet(ir[samp,], targets[samp,], size=4, rang=0.1, decay=5e-4, maxit=iter)
  error = err(targets[-samp,], predict(ir.i, ir[-samp,]))
  err.test[i,] = c(iter, error)
}
err.test
plot( err.test$iter,  err.test$err, xlab="iteration", ylab="error")
           

# caret ----
install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)

data(iris)
ir = rbind(iris3[,,1], iris3[,,2], iris3[,,3])

targets = class.ind(c(rep("s", 50), rep("c", 50), rep("v", 50)))
targets[1:5,]

ir.target = cbind(ir, targets)

set.seed(1234)           
samp = c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))

# ir1 = avNNet(ir[samp,], targets[samp,], size=3, linout = FALSE)

# borrowing example from caret manual (p. 133)
samp = createDataPartition(iris[,5], p=.8, list = FALSE)
mygrid <- expand.grid(.decay=c(1, 0.0001), .size=c(2,3))
train.mass = train(iris[samp,1:4], iris[samp,5], method="nnet", 
                   preProcess = c("center", "scale"), 
                   tuneLength = 3, tuneGrid=mygrid,
                   maxit=1000, trace=F)

# results
err = function(true, pred) {
  true = as.numeric(true)
  pred = as.numeric(pred)
  sqe = (true-pred)^2
  sse = sqrt(sum(sqe))
  sse
}
table(iris[-samp,5], predict(train.mass, iris[-samp,1:4]))
err(iris[-samp,5], predict(train.mass, iris[-samp, 1:4]))


n = 10
err.test = data.frame(iter=rep(NA,n), err=rep(NA,n))
for (i in 1:n){
  iter = 3^i
  ir.i= nnet(ir[samp,], targets[samp,], size=4, rang=0.1, decay=5e-4, maxit=iter)
  error = err(targets[-samp,], predict(ir.i, ir[-samp,]))
  err.test[i,] = c(iter, error)
}
err.test
plot( err.test$iter,  err.test$err, xlab="iteration", ylab="error")
