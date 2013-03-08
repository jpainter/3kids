# prediction

# d0 are weights for being hospitalized
  d0 = list(model0.x1$Theta1, model0.x1$Theta2)
  
# d1 are weights for, when hospitalize, the number of days hospitalized  
  d1 = list(model.x1$Theta1, model.x1$Theta2)

# dimensions
  d0.theta1 = as.matrix(as.data.frame(d0[1])); dim(d0.theta1)
  d0.theta2 = as.matrix(as.data.frame(d0[2])); dim(d0.theta2)
  d1.theta1 = as.matrix(as.data.frame(d1[1])); dim(d1.theta1)
  d1.theta2 = as.matrix(as.data.frame(d1[2])); dim(d1.theta2)
  
  load("dataY2.rda")
  data = dataY2
  rm(dataY2) 
  
  names(data)
  data.cols = c(3:90) 

# ===== predict first stage: hospitalized or not. 
  
  # outcome--in or out of hospital!
  data$someDays = factor(
    ifelse(data$DaysInHospital==0 & !is.na(data$DaysInHospital), 0,
           ifelse(!is.na(data$DaysInHospital), 1, NA))
  )
  table(data$someDays)

  # scale
#   preProcValues0 <- preProcess(data[, data.cols], method = c("center", "scale") )
  
  # remove columns with zero variance
  zero.var.cols0 = c("ProcedureGroup.SMCD", "ProcedureGroup.SO", 
                    "PrimaryConditionGroup.PNCRDZ", "PrimaryConditionGroup.RENAL1", 
                    "PrimaryConditionGroup.RENAL2", "PrimaryConditionGroup.SEPSIS")
  
  # update training columns
  nonZero.cols0 = setdiff(names(data[,data.cols]), zero.var.cols0)
  
  # set training and cv/test data
  data0 = predict(preProcValues0 , data[, data.cols] ) 
  X0 = as.matrix(data0[, nonZero.cols0])
  
  pred0 = nnet.predict( Theta1 = d0.theta1, Theta2 = d0.theta2, X = X0)
  predict0 = factor(pred0-1, levels=c(0, 1))
  y0 = data[, "someDays"]
  table(y0, predict0, useNA='always')
 
  
  # ===== predict second stage: days hospitalized. 
  
  # outcome--in or out of hospital!
#   data$someDays = factor(data$DaysInHospital)
#   table(data$someDays)
#   
  someDays = as.numeric(predict0)>1
  
  # scale
#   preProcValues1 <- preProcess(data[someDays, data.cols], method = c("center", "scale") )
  
  # remove columns with zero variance
  zero.var.cols1 =  c("ProcedureGroup.", "ProcedureGroup.ANES", "ProcedureGroup.SAS", "ProcedureGroup.SCS", 
                     "ProcedureGroup.SMCD", "ProcedureGroup.SMS", "ProcedureGroup.SNS", "ProcedureGroup.SO", 
                     "ProcedureGroup.SRS", "ProcedureGroup.SUS", "PrimaryConditionGroup.CANCRA", "PrimaryConditionGroup.CANCRM", 
                     "PrimaryConditionGroup.CATAST", "PrimaryConditionGroup.CHF", "PrimaryConditionGroup.FLaELEC", 
                     "PrimaryConditionGroup.HIPFX", "PrimaryConditionGroup.METAB1", "PrimaryConditionGroup.PERINTL", 
                     "PrimaryConditionGroup.PNCRDZ", "PrimaryConditionGroup.RENAL1", "PrimaryConditionGroup.RENAL2", 
                     "PrimaryConditionGroup.SEPSIS", "CharlsonIndex.3-4", "CharlsonIndex.5+")
  
  
  # update training columns
  nonZero.cols1 = setdiff(names(data[, data.cols]), zero.var.cols1)
  
  # set training and cv/test data
  data1 = predict(preProcValues1 , data[, data.cols] ) 
  X1 = as.matrix(data1[, nonZero.cols1 ])
  
  predict1 = nnet.predict( Theta1 = d1.theta1, Theta2 = d1.theta2, X = X1)
  y1 = data[, "DaysInHospital"]
  table(y1, predict1, useNA='always')
  
  # Final aggregate
  days.pred = ifelse(predict0 == 0 , 0, predict1)
  table(data$DaysInHospital, days.pred, useNA="always")
  
  # TARGET
  target$DaysInHospital = days.pred
  
# =====  nnet.predict.cla function...
  nnet.predict = function(Theta1, Theta2, X){
      num_labels <- nrow(Theta2);
      m <- nrow(X);
      if (is.null(m)) {
        m <- 1; X <- t(X);
      }
      p = rep(0, m);
      
      h1 <- sigmoid(cbind(rep(1, m), X) %*% t(Theta1));
      h2 <- sigmoid(cbind(rep(1, m), h1) %*% t(Theta2));
      p = apply(h2, 1, which.max);
  }
  
  