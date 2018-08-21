Case_stat<- function(fullmodel,train,test,n){

  model_iteration_train <- fullmodel

  #model_iteration_train <- step(fullmodel,k=3.8415)

  #Predictions
  #pred_train_iteration = predict(model_iteration_train, newdata = train, type =  'response')
  pred_train_iteration = predict(model_iteration_train, newdata = train,type = 'response')
  train_KStat = train #90 rows
  train_KStat$pred = pred_train_iteration
  pred_train_iteration
  summary(train)
  KStat_train = subset(train_KStat, select = c('DV', 'pred'))
  KStat_train = KStat_train[order(-KStat_train$pred), ]
  KStat_train$row = seq(1, nrow(KStat_train), 1)

  a_iteration = as.numeric(floor(nrow(train_KStat) / n), 0)



  # ******************** AUC ********************
  ROCR1_Iteration2 = ROCR::prediction(pred_train_iteration, train$DV)# to transform the input data into a standardized format
  auctrain_Iteration2 = as.numeric(ROCR::performance( ROCR1_Iteration2, "auc")@y.values)#predictor evaluations


  ###############################################################################################################
  ###************************************ 12. Data for Tableau********************************************
  #*************************12. A. Gains Table **********************************************************
  #Gains Table
  KStat_train$flag<-sample(0, replace=TRUE, size=nrow(train_KStat))

  y=a_iteration
  j=0
  for(i in 1:n){
    KStat_train$flag[(j+1):y]<-i
    j=y
    if(i==n & y<nrow(train_KStat))
    {
      KStat_train$flag[(y+1):nrow(train_KStat)]<-i
    }
    y=y+a_iteration

  }

  a_iteration
  Responders = 0

  for (i in 1:20)
  {
    Responders[i] = sum(KStat_train$DV[KStat_train$flag == i])
  }

  kstat = data.frame(Responders)

  ##Variables
  for (i in 1:20)
  {
    kstat$Non_Responders[i] = (sum(KStat_train$flag == i) - kstat$Responders[i])
  }

  kstat$Responders_Percentage = ((kstat$Responders) / sum(kstat$Responders)) *
    100

  kstat$Non_Responders_Percentage = ((kstat$Non_Responders) / sum(kstat$Non_Responders)) *
    100

  #*****Add the Risk indicator to the employee
  ##R-Cumm
  kstat$R_Cumm[20] = kstat$Responders_Percentage[nrow(kstat)]

  num = c(19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

  for (i in num)
  {
    kstat$R_Cumm[i] = kstat$R_Cumm[i + 1] + kstat$Responders_Percentage[i]
  }

  ##NR-Cumm

  kstat$NR_Cumm[20] = kstat$Non_Responders_Percentage[nrow(kstat)]

  num = c(19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

  for (i in num)
  {
    kstat$NR_Cumm[i] = kstat$NR_Cumm[i + 1] + kstat$Non_Responders_Percentage[i]
  }

  kstat$KS = round((kstat$NR_Cumm - kstat$R_Cumm), 2)

  num1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)


  kstat$Model_Lift[20] = 100

  for (i in num)
  {
    kstat$Model_Lift[i] = 100 - kstat$R_Cumm[i + 1]
  }

  ##Decile determination

  for (i in num1)
  {
    if (kstat$KS[i + 1] > kstat$KS[i])
    {
      count = i + 1
    }
    else
    {
      break
    }
  }

  library(SDMTools)
  optimum_threshold = optim.thresh(KStat_train$DV, KStat_train$pred)
  thresh = optimum_threshold$`max.sensitivity+specificity`

  return(thresh)
}
