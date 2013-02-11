
load("mem.data")
str(mem.data)

mem.data.nnet = mem.data[,c(2,3,5,7,10,11,13,14,16,17)]
str(mem.data.nnet)

# change the factors AgeAtFirstClaim and Sex to numeric
mem.data.nnet[,c(1)] = as.numeric(mem.data.nnet[,c(1)])
mem.data.nnet[,c(2)] = as.numeric(mem.data.nnet[,c(2)])

str(mem.data.nnet)

# scale vars and then comapre results of before and after
m = scale(mem.data.nnet)
colnames(m) = names(mem.data.nnet)
rownames(m) = mem.data$MemeberID
summary(mem.data.nnet)
summary(m)

# training set
m.complete = m[which(complete.cases(m)),]
set.seed(1234)
trainIndicator = rbinom(nrow(m.complete), 1, 0.7)
table(trainIndicator)
m.t = m.complete[trainIndicator==1,]

# trial neuralnet
library(neuralnet)
set.seed(1234)
startweights = replicate(3, rnorm(10)) 
n = neuralnet(DaysY3~AgeAtFirstClaim+Sex+DaysY2+drugY1+drugY2+labY1+labY2+claimY1+claimY2, 
              m.t , hidden=c(9,9), startweights = startweights,
              threshold=0.01, 
              algorithm='backprop', learningrate=.01,
              err.fct='sse', act.fct='logistic', linear.output=FALSE,
              rep=1)
n
plot(n)
