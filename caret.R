
#--------------------------------------------------------------------
# read brfss file (huge 135 MB file)
#--------------------------------------------------------------------
y <- read.csv("http://www.hofroe.net/stat579/brfss%2009/brfss-2009-clean.csv")
indicator <- c("DIABETE2", "GENHLTH", "PERSDOC2", "SEX", "FLUSHOT3", "PNEUVAC3", 
               "X_RFHYPE5", "X_RFCHOL", "RACE2", "X_SMOKER3", "X_AGE_G", "X_BMI4CAT", 
               "X_INCOMG", "X_RFDRHV3", "X_RFDRHV3", "X_STATE");
target <- "DIABETE2";
diabetes <- y[, indicator];

#--------------------------------------------------------------------
# recode DIABETE2
#--------------------------------------------------------------------
x <- diabetes$DIABETE2;
x[x > 1]  <- 'N';
x[x != 'N']  <- 'Y';
diabetes$DIABETE2 <- x; 
rm(x);

#--------------------------------------------------------------------
# remove NA
#--------------------------------------------------------------------
x <- na.omit(diabetes);
diabetes <- x;
rm(x);

#--------------------------------------------------------------------
# reproducible research 
#--------------------------------------------------------------------
set.seed(1612);
nsamples <- 1000; 
sample.diabetes <- diabetes[sample(nrow(diabetes), nsamples), ]; 

#--------------------------------------------------------------------
# split the dataset into training and test
#--------------------------------------------------------------------
ratio <- 0.7;
train.samples <- ratio*nsamples;
train.rows <- c(sample(nrow(sample.diabetes), trunc(train.samples)));

train.set  <- sample.diabetes[train.rows, ];
test.set   <- sample.diabetes[-train.rows, ];

train.result <- train.set[ , which(names(train.set) == target)];
test.result  <- test.set[ , which(names(test.set) == target)];


library(caret)

#Parallize
library(doSMP)
w <- startWorkers()
registerDoSMP(w)

#Build model
X <- train.set[,-1]
Y <- factor(train.set[,1],levels=c('N','Y'))
model <- train(X,Y,method='lda')

#Evaluate model on test set
print(model)
predY <- predict(model,test.set[,-1])
confusionMatrix(predY,test.set[,1])
stopWorkers(w)