set.seed(1000)

########## GDSC1
load("/home/Qiang/DrugResponse/GDSC1/GDSC1_Frame_BRCA/RData/Lapatinib_trainFrame.RData")
load("/home/Qiang/DrugResponse/GDSC1/GDSC1_Frame_BRCA/RData/Lapatinib_testFrame.RData")
transForm<-2.999942
offset<-4.465461

DL<-read.csv(file="/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/PCC/Yiqing/Lapatinib_DLpredict.csv")

A<-(DL[,1])^(1/transForm)-offset
write.csv(A,file="/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/PCC/Yiqing/Lapatinib_DLpredict.csv",row.names = F)

####### GDSC2
load("/home/Qiang/DrugResponse/GDSC2/GDSC2_Frame_BRCA/RData/Lapatinib_trainFrame.RData")
load("/home/Qiang/DrugResponse/GDSC2/GDSC2_Frame_BRCA/RData/Lapatinib_testFrame.RData")
transForm<-0.8897623
offset<-2.944905

DL<-read.csv(file="/home/Qiang/DrugResponse/GDSC2/Output/mid_evaluation/PCC/Yiqing/Lapatinib_DLpredict.csv")

A<-(DL[,1])^(1/transForm)-offset
write.csv(A,file="/home/Qiang/DrugResponse/GDSC2/Output/mid_evaluation/PCC/Yiqing/Lapatinib_DLpredict.csv",row.names = F)

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

method_list <- c("GR paper linear Ridge",
                 "Random Forest",
                 "General Random Forest",
                 "Ranger",
                 "Principle Component Regression",
                 "Partial Least Square",
                 "K-nearest neighbors",
                 "Weighted K-nearest neighbors ",
                 "Support Vector Machine",
                 "SVM Linear",
                 "Elastic Net",
                 "Ridge GLM",
                 "EN GLM",
                 "Lasso",
                 "RF+Lasso2019(out-of-bag)",
                 "RF+Lasso2019(split-conformal)",
                 "RF+Lasso2019(quantile regression forest)",
                 "Tree Bag",
                 "h2o deep learning"
                 )

model_GR <- linearRidge(Resp ~ ., data = trainFrame)
model_rf <- train(Resp~.,data=trainFrame,
                  method = "rf",
                  ntree = 50,
                  tuneGrid = data.frame(mtry = 2),
                  nodesize = 5,
                  importance = TRUE,
                  metric = "RMSE",
                  trControl = trainControl(method = "oob", seed = c(1,1)),
                  allowParallel = FALSE)
model_rfG <- randomForest(Resp~.,data=trainFrame, importance = TRUE)
model_ranger<-ranger(Resp~.,data=trainFrame,num.trees = 50,splitrule="variance",seed =1000)
model_pcr<-train(Resp~.,data=trainFrame,method="pcr",trControl=trainControl("cv",number=10))
model_pls<-train(Resp~.,data=trainFrame,method="pls",trControl=trainControl("cv",number=10))
model_KNN<-train(Resp~.,data=trainFrame,method="knn",trControl=trainControl("cv",number=10))
model_KKNN<-train(Resp~.,data=trainFrame,method="kknn",trControl=trainControl("cv",number=10))
model_svm<-svm(Resp~.,data=trainFrame,kernel="sigmoid",cross=10,scale=FALSE)
model_svmLinear2<-train(Resp~.,data=trainFrame,method = 'svmLinear2',trControl=trainControl("cv",number=10))
model_EN<-train(Resp~.,data=trainFrame,method="glmnet",trControl=trainControl("cv",number=10))

cv_output_0 <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0,type.measure="mse",nfolds=10)
best_lam_0 <- cv_output_0$lambda.min
model_ridgeglm<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0, lambda=best_lam_0)

cv_output <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0.5,type.measure="mse",nfolds=10)
best_lam <- cv_output$lambda.min
model_ENglm<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0.5, lambda=best_lam)

cv_output_1 <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=1,type.measure="mse",nfolds=10)
best_lam_1 <- cv_output_1$lambda.min
model_Lasso<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=1, lambda=best_lam_1)

model_rfinv1<- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "oob", alpha = 0.05,symmetry = TRUE)
model_rfinv2<- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "split-conformal", alpha = 0.05,seed = 1000)
model_rfinv3<- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "quantreg", alpha = 0.05)
model_treebag<-train(Resp~.,data=trainFrame,method = 'treebag',trControl=trainControl("cv",number=10))

preds_GR<-(predict(model_GR,testFrame))^(1/transForm)-offset
preds_rf<-(predict(model_rf,testFrame))^(1/transForm)-offset
preds_rfG <- (predict(model_rfG, testFrame))^(1/transForm)-offset
preds_ranger<-(predict(model_ranger,testFrame)$predictions)^(1/transForm)-offset
preds_pcr<-(predict(model_pcr,testFrame))^(1/transForm)-offset
preds_pls<-(predict(model_pls,testFrame))^(1/transForm)-offset
preds_KNN<-(predict(model_KNN,testFrame))^(1/transForm)-offset
preds_KKNN<-(predict(model_KKNN,testFrame))^(1/transForm)-offset
preds_svm<-(predict(model_svm,testFrame))^(1/transForm)-offset
preds_svmLinear2<-(predict(model_svmLinear2,testFrame))^(1/transForm)-offset
preds_EN<-(predict(model_EN,testFrame))^(1/transForm)-offset
preds_ridgeglm <- (predict(model_ridgeglm, s = best_lam_0, newx=as.matrix(testFrame)))^(1/transForm)-offset
preds_ENglm<- (predict(model_ENglm, s = best_lam, newx=as.matrix(testFrame)))^(1/transForm)-offset
preds_Lasso <- (predict(model_Lasso, s = best_lam_1, newx=as.matrix(testFrame)))^(1/transForm)-offset
preds_rfinv1 <- (model_rfinv1$testPred)^(1/transForm)-offset
preds_rfinv2 <- (model_rfinv2$testPred)^(1/transForm)-offset
preds_rfinv3 <- (model_rfinv3$testPred)^(1/transForm)-offset
preds_treebag<-(predict(model_treebag,testFrame))^(1/transForm)-offset

## GDSC1
preds_h2o<-t(read.csv("/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/PCC/h2o/BRCA/Lapatinib_Preds.csv",header=T))

##GDSC2
preds_h2o<-t(read.csv("/home/Qiang/DrugResponse/GDSC2/h2o/BRCA_final/Lapatinib_Preds.csv",header=T))


preds_from_testFrame<- list(preds_GR,
                            preds_rf,
                            preds_rfG,
                            preds_ranger,
                            preds_pcr,
                            preds_pls,
                            preds_KNN,
                            preds_KKNN,
                            preds_svm,
                            preds_svmLinear2,
                            preds_EN,
                            preds_ridgeglm,
                            preds_ENglm,
                            preds_Lasso,
                            preds_rfinv1,
                            preds_rfinv2,
                            preds_rfinv3,
                            preds_treebag,
                            preds_h2o)

names(preds_from_testFrame)<-method_list

save(preds_from_testFrame,file="/home/Qiang/DrugResponse/GDSC2/Output/BRCA_Validation/Clinical_boxplot.RData")


##### Clinical outcome
# From "breast_cancer_analysis.html"
load("/home/Qiang/DrugResponse/BRCA.RData")

clinDataBrca <- read.delim("/home/Qiang/DrugResponse/GROriginal/dataIn/clinical/nationwidechildrens.org_clinical_patient_brca.txt", as.is=T)
her2status <- clinDataBrca[, "her2_status_by_ihc"]
names(her2status) <- clinDataBrca[, "bcr_patient_barcode"]

sampleNames <- colnames(BRCA)
theTumorSamples <- which(substring(sampleNames, 14, 16) == "01A") # identify the tumor samples, tumor samples annotated as "01" by TCGA, normal samples as "10".
newNames <- gsub(".", "-", substring(colnames(BRCA), 1, 12), fixed=T)

pdf("/home/Qiang/DrugResponse/GDSC2/Output/BRCA_Validation/GDSC2_Lapatinib_ERBB2_clinical_Validation.pdf", width=8, height=10)
par(mfrow=c(4,5))
for (i in 1:19){
  bcaPreds <- preds_from_testFrame[i,]

  #bcaPreds<-read.csv(file="/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/PCC/Yiqing/Lapatinib_DLpredict.csv")
  #bcaPreds<-t(bcaPreds)
  names(bcaPreds) <- newNames
  bcaPreds <- bcaPreds[theTumorSamples] # Only include the tumor samples in this analysis. Results on normal samples are meaningless.
  sampsInBothDatasets <- clinDataBrca[, "bcr_patient_barcode"][clinDataBrca[, "bcr_patient_barcode"] %in% newNames]
  
  her2Neg <- which(her2status[sampsInBothDatasets] == "Negative")
  her2Pos <- which(her2status[sampsInBothDatasets] == "Positive")
  her2Equiv <- which(her2status[sampsInBothDatasets] == "Equivocal")
  (wiltest <- wilcox.test(bcaPreds[sampsInBothDatasets][her2Neg], bcaPreds[sampsInBothDatasets][her2Pos]))
  (t_test<-t.test(bcaPreds[sampsInBothDatasets][her2Neg], bcaPreds[sampsInBothDatasets][her2Pos]))
  
  boxplot(list(Negative=bcaPreds[sampsInBothDatasets][her2Neg], Equivocal=bcaPreds[sampsInBothDatasets][her2Equiv], Positive=bcaPreds[sampsInBothDatasets][her2Pos]), las=1, col=c("#66c2a5", "#fc8d62", "#8da0cb"), pch=20, width=c(.75, .75, .75), ylab="Predicted Lapatinib Sensitivity", xlab=paste(method_list[i], "\n t_test p = ", signif(t_test$p.value,digits = 3), sep=""),cex.axis=.75, outcol="#00000033")
}

dev.off()

#boxplot(list(Negative=bcaPreds[sampsInBothDatasets][her2Neg], Equivocal=bcaPreds[sampsInBothDatasets][her2Equiv], Positive=bcaPreds[sampsInBothDatasets][her2Pos]), las=1, col=c("#66c2a5", "#fc8d62", "#8da0cb"), pch=20, width=c(.75, .75, .75), ylab="Predicted Lapatinib Sensitivity", xlab=paste("NN", "\n t_test p = ", signif(t_test$p.value,digits = 3), sep=""),cex.axis=.75, outcol="#00000033")
