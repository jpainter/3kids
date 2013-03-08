
# tree

rm(list=ls())

library(tree)

load("dataY2.rda")
cols = colnames(dataY2)

library(stringr)
new.cols = str_replace_all(cols, "-", ".")
new.cols[12] = "AgeAtFirstClaim.80"
new.cols[82] = "CharlsonIndex.5"

dataT = dataY2
colnames(dataT) = new.cols

dataT$someDays = factor(
  ifelse(dataT$DaysInHospital==0 & !is.na(dataT$DaysInHospital), 0,
         ifelse(!is.na(dataT$DaysInHospital), 1, NA))
)
table(dataT$someDays)

col.list = paste(new.cols[3:90], collapse=" + ")
col.form = as.formula(paste("factor(someDays) ~ ", col.list, sep=""))
t = tree(formula = col.form , data = dataT)
plot(t); text(t)
