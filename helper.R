scale_test = function(test){
  load("data/data_no_scale.RData")
  if("AST"%in% colnames(test)){
    test$AST = log(test$AST)#log transform skewed values
    test$AST = test$AST/64 # scale with the root mean square from training dataset
  }
  if("creatinine_value"%in% colnames(test)){
    test$creatinine_value = log(test$creatinine_value)
    test$creatinine_value = test$creatinine_value/0.65
  }
  if("RRSD"%in% colnames(test)){
    test$RRSD = log(test$RRSD)
    test$RRSD[is.infinite(test$RRSD)] =  -1
    test$RRSD = test$RRSD/1.07
  }
  if("creatinine_value"%in% colnames(test)){
    test$creatinine_value = log(test$creatinine_value)
    test$creatinine_value = test$creatinine_value/0.65
  }
  if("diag_GA"%in% colnames(test)){
    test$diag_GA = test$diag_GA/251.54
  }
  if("age"%in% colnames(test)){
    test$age= test$age/30.74
  }
  if("EPIS_PARA_COUNT"%in% colnames(test)){
    test$EPIS_PARA_COUNT= test$EPIS_PARA_COUNT/1.36
  }
  if("White_Blood_Cell_Count"%in% colnames(test)){
    test$White_Blood_Cell_Count= test$White_Blood_Cell_Count/11.35
  }
  if("BPDiaMean"%in% colnames(test)){
    test$BPDiaMean= test$BPDiaMean/84.95
  }
  if("Platelet_Count"%in% colnames(test)){
    test$Platelet_Count= test$Platelet_Count/231.77
  }
  if("BPSysMean"%in% colnames(test)){
    test$BPSysMean= test$PBPSysMean/139.60
  }

}

selu = function(x){
  alpha=1.67326324 
  scale=1.05070098
  
  if(x>=0){
    return(scale * x)
  }else{
    return(scale * alpha * (exp(x) - 1))
  }
}


calculate_theta = function(W,b, test, time_test, sta_test, cv.tr, predTrain){
  W = t(W)
  b = t(b)
  
  n_test = nrow(test)
  n_train = nrow(predTrain)
  n_node = nrow(W)
  
  image_hidden_test = matrix(rep(0,n_node*n_test),nrow = n_test) # 58 is the sample size ; 50 is sqrt(number of features)   
  for (i in 1:n_test){
    for (j in 1:n_node){
      image_hidden_test[i,j] = selu(sum(test[i,]*W[j,])+b[j])
    }
  }
  
  # image_hidden_train = matrix(rep(0,n_node*n_train),nrow = n_train)
  # for (i in 1:n_train){
  #   for (j in 1:n_node){
  #     image_hidden_train[i,j] = selu(sum(train[i,]*W[j,])+b[j])
  #   }
  # }
  # 
  # Ymatrix_train = Surv(time_train[,1],sta_train[,1])
  # 
  # cv.tr=cv.glmnet(as.matrix(image_hidden_train),Ymatrix_train,family='cox',alpha=0.9,nfolds=10)
  # predTrain=predict(cv.tr,as.matrix(image_hidden_train),s=cv.tr$lambda.min,type='response')
  predTest<-predict(cv.tr,as.matrix(image_hidden_test),s=cv.tr$lambda.min,type='response')
  
  #scale theta
  pred_df = rbind(predTrain, predTest)
  pred_df = scale(pred_df)
  predTrain = pred_df[1:n_train]
  predTest = pred_df[(n_train+1):nrow(pred_df)]

  return(list(predTrain, predTest))
}


# calculate percentile
# take the result list produced by get_theta
# output a dataframe of test theta and precentile in train
percentile = function(result){
  # get train theta
  predTrain = result[[1]]
  predTest = result[[2]]
  percent = c()
  for(i in 1:length(predTest)){
    percent[i] = round(sum(predTest[i]>predTrain)/length(predTrain),4)*100
  }
  percentile = data.frame(predTest, percent)
  return(percentile)
}

