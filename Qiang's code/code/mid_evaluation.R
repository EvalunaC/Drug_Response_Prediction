######################### PCC ###########################################

#==========================================================================================================
#========   2.1 Validation using "80/20" cross validation in training data only ===========================
#==========================================================================================================
library(caret)
library(pRRophetic)
library(ridge)
library(MASS)
library(plyr)
library(dplyr)
library(glmnet)
library(randomForest)
library(pls)
library(deepnet)
library(ranger)
library(kknn)
library(rfinterval)
library(e1071)

### GDSC1
GDSC<-read.csv("/home/Qiang/DrugResponse/GDSC1/GDSC1_IC50_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(GDSC)<-gsub("/","",rownames(GDSC)) ## Lapatinib=40

### GDSC2
GDSC<-read.csv("/home/Qiang/DrugResponse/GDSC2/GDSC2_IC50_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(GDSC) ##Lapatinib=81

#com_drug_GDSC<-intersect(possibleDrugs1,possibleDrugs2)
#write.csv(com_drug_GDSC,file="com_drug_GDSC.csv")

### CTRP1
CTRP<-read.csv("/home/Qiang/DrugResponse/CTRP1/CTRP1_AUC_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(CTRP)

### CTRP2
CTRP<-read.csv("/home/Qiang/DrugResponse/CTRP2/CTRP2_AUC_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(CTRP)<-gsub("[[:punct:]]", "-",  rownames(CTRP))

#com_drug_CTRP<-intersect(possibleDrugs3,possibleDrugs4)
#write.csv(com_drug_CTRP,file="com_drug_CTRP.csv")
#############################
setwd("/home/Qiang/DrugResponse/CTRP2/Output/mid_evaluation/PCC")

library("parallel")

doPredict<-function(drug){

  file <- paste("/home/Qiang/DrugResponse/CTRP2/CTRP2_Frame/RData/",possibleDrugs[drug],"_trainFrame.RData", sep="",collapse = NULL)
  load(file)
  drug_data <- trainFrame
  #========================================
  #=====    Split the data     ============
  #========================================
  train_length <- round(dim(drug_data)[1]*0.8)
  trainFrame <- drug_data[1:train_length,]
  testFrame <- drug_data[(train_length+1):dim(drug_data)[1],-1]
 
  #========================================
  #  Then use  ==1.0.1. Building Models==
  #========================================
  set.seed(1000)
   
  model_treebag<-train(Resp~.,data=trainFrame,method = 'treebag',trControl=trainControl("cv",number=10))
  preds_treebag<-predict(model_treebag,testFrame)
  
  }

allPreds_treebag<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_treebag)<-possibleDrugs
save(allPreds_treebag,file="preds_treebag.RData")



#########################

## GR
model_GR <- linearRidge(Resp ~ ., data = trainFrame)
preds_GR<-predict(model_GR,testFrame) 

allPreds_GR<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_GR)<-possibleDrugs
save(allPreds_GR,file="preds_GR.RData")

## RF
model_rf <- train(Resp~.,data=trainFrame,
                  method = "rf",
                  ntree = 50,
                  tuneGrid = data.frame(mtry = 2),
                  nodesize = 5,
                  importance = TRUE,
                  metric = "RMSE",
                  trControl = trainControl(method = "oob", seed = c(1,1)),
                  allowParallel = FALSE)
preds_rf<-predict(model_rf,testFrame)

allPreds_rf<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_rf)<-possibleDrugs
save(allPreds_rf,file="preds_rf.RData")

## General RF
model_rfG <- randomForest(Resp~.,data=trainFrame, importance = TRUE)
preds_rfG <- predict(model_rfG, testFrame)

allPreds_rfG<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_rfG)<-possibleDrugs
save(allPreds_rfG,file="preds_rfG.RData")

## Ranger
model_ranger<-ranger(Resp~.,data=trainFrame,num.trees = 50,splitrule="variance",seed =1000)
preds_ranger<-predict(model_ranger,testFrame)$predictions

allPreds_ranger<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_ranger)<-possibleDrugs
save(allPreds_ranger,file="preds_ranger.RData")

## PCR
model_pcr<-train(Resp~.,data=trainFrame,method="pcr",trControl=trainControl("cv",number=10))
preds_pcr<-predict(model_pcr,testFrame)

allPreds_pcr<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_pcr)<-possibleDrugs
save(allPreds_pcr,file="preds_pcr.RData")


## PLS
model_pls<-train(Resp~.,data=trainFrame,method="pls",trControl=trainControl("cv",number=10))
preds_pls<-predict(model_pls,testFrame)

allPreds_pls<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_pls)<-possibleDrugs
save(allPreds_pls,file="preds_pls.RData")


## KNN
model_KNN<-train(Resp~.,data=trainFrame,method="knn",trControl=trainControl("cv",number=10))
preds_kNN<-predict(model_KNN,testFrame)

allPreds_KNN<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_KNN)<-possibleDrugs
save(allPreds_KNN,file="preds_KNN.RData")


## KKNN
model_KKNN<-train(Resp~.,data=trainFrame,method="kknn",trControl=trainControl("cv",number=10))
preds_KKNN<-predict(model_KKNN,testFrame)

allPreds_KKNN<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_KKNN)<-possibleDrugs
save(allPreds_KKNN,file="preds_KKNN.RData")


## SVM
model_svm<-svm(Resp~.,data=trainFrame,kernel="sigmoid",cross=10,scale=FALSE)
preds_svm<-predict(model_svm,testFrame)

allPreds_svm<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_svm)<-possibleDrugs
save(allPreds_svm,file="preds_svm.RData")

## SVM Linear
model_svmLinear2<-train(Resp~.,data=trainFrame,method = 'svmLinear2',kernel="sigmoid",trControl=trainControl("cv",number=10))
preds_svmLinear2<-predict(model_svmLinear2,testFrame)

allPreds_svmLinear2<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_svmLinear2)<-possibleDrugs
save(allPreds_svmLinear2,file="preds_svmLinear2.RData")


## EN
model_EN<-train(Resp~.,data=trainFrame,method="glmnet",trControl=trainControl("cv",number=10))
preds_EN<-predict(model_EN,testFrame)

allPreds_EN<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_EN)<-possibleDrugs
save(allPreds_EN,file="preds_EN.RData")


## Ridge GLM
cv_output_0 <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0,type.measure="mse",nfolds=10)
best_lam_0 <- cv_output_0$lambda.min
model_ridgeglm<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0, lambda=best_lam_0)
preds_ridgeglm<- predict(model_ridgeglm, s = best_lam_0, newx=as.matrix(testFrame))

allPreds_ridgeglm<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_ridgeglm)<-possibleDrugs
save(allPreds_ridgeglm,file="preds_ridgeglm.RData")


## EN GLM
cv_output <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0.5,type.measure="mse",nfolds=10)
best_lam <- cv_output$lambda.min
model_ENglm<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0.5, lambda=best_lam)
preds_ENglm<- predict(model_ENglm, s = best_lam, newx=as.matrix(testFrame))

allPreds_ENglm<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_ENglm)<-possibleDrugs
save(allPreds_ENglm,file="preds_ENglm.RData")


## Lasso
cv_output_1 <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=1,type.measure="mse",nfolds=10)
best_lam_1 <- cv_output_1$lambda.min
model_Lasso<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=1, lambda=best_lam_1)
preds_Lasso<- predict(model_Lasso, s = best_lam_1, newx=as.matrix(testFrame))

allPreds_Lasso<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_Lasso)<-possibleDrugs
save(allPreds_Lasso,file="preds_Lasso.RData")


## rfinv1
model_rfinv1<- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "oob", alpha = 0.05,symmetry = TRUE)
preds_rfinv1<- model_rfinv1$testPred

allPreds_rfinv1<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_rfinv1)<-possibleDrugs
save(allPreds_rfinv1,file="preds_rfinv1.RData")


## rfinv2
model_rfinv2<- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "split-conformal", alpha = 0.05,seed = 1000)
preds_rfinv2<- model_rfinv2$testPred

allPreds_rfinv2<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_rfinv2)<-possibleDrugs
save(allPreds_rfinv2,file="preds_rfinv2.RData")


## rfinv3
model_rfinv3<- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "quantreg", alpha = 0.05)
preds_rfinv3<- model_rfinv3$testPred

allPreds_rfinv3<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_rfinv3)<-possibleDrugs
save(allPreds_rfinv3,file="preds_rfinv3.RData")


## treebag
model_treebag<-train(Resp~.,data=trainFrame,method = 'treebag',trControl=trainControl("cv",number=10))
preds_treebag<-predict(model_treebag,testFrame)

allPreds_treebag<-mclapply(1:length(possibleDrugs),doPredict,mc.cores=10)
names(allPreds_treebag)<-possibleDrugs
save(allPreds_treebag,file="preds_treebag.RData")    


############### Get the actual response for each drug
testResp<-list()
for(i in 1:length(possibleDrugs)){
  print(i)
  drug <- possibleDrugs[i]
  
  file <- paste("/home/Qiang/DrugResponse/CTRP2/CTRP2_Frame/RData/",possibleDrugs[i],"_trainFrame.RData", sep="",collapse = NULL)
  load(file)
  drug_data <- trainFrame
  #========================================
  #=====    Split the data     ============
  #========================================
  train_length <- round(dim(drug_data)[1]*0.8)
  trainFrame <- drug_data[1:train_length,]
  testFrame <- drug_data[(train_length+1):dim(drug_data)[1],-1]
  testResp[[i]] <- drug_data[(train_length+1):dim(drug_data)[1],1]
}

names(testResp)<-possibleDrugs
save(testResp,file="/home/Qiang/DrugResponse/CTRP2/Output/mid_evaluation/PCC/Combine/testResp.RData")
