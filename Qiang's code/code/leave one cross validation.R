load("/home/Qiang/DrugResponse/CCLE_mat.RData")
load("/home/Qiang/DrugResponse/TCGA_Log2_New.RData")

#com_gene1 <- rownames(CCLE_2018_mat)[rownames(CCLE_2018_mat) %in% rownames(TCGA_Log2_New)]

#com_gene<-intersect(rownames(CCLE_2018_mat),rownames(TCGA_Log2_New))

#CCLE_2018_mat<-CCLE_2018_mat[com_gene,]

#save(CCLE_2018_mat,file="CCLE_2018_comGene.RData")


theLens <- numeric()

for(i in 1:length(possibleDrugs))
{
  #CCLETrainData <- getCGPinfo_New(possibleDrugs[i]) # get the IC50 and expression data for this drug/tissueType
  CCLETrainData <- getCGPinfo_CTRP(possibleDrugs[i])
  theLens[i] <- length(CCLETrainData$drug_IC50_train)
}

names(theLens) <- possibleDrugs

save(theLens,file="theLens.RData")


##################

source("/home/Qiang/DrugResponse/Code/GR_CV.R")

load("/home/Qiang/DrugResponse/GDSC2/Output/mid_evaluation/L-CV/CCLE_2018_comGene.RData")

getCGPinfo_New<-function(drug){
  whichNas <- which(is.na(GDSC[drug,]))
  drug_IC50<-GDSC[drug,][-whichNas]
  
  commonCellLines<-intersect(colnames(CCLE_2018_mat),colnames(drug_IC50))
  CCLE_train<-CCLE_2018_mat[,commonCellLines]
  drug_IC50_train<-drug_IC50[commonCellLines]
  
  CCLE_train<-as.matrix(CCLE_train)
  drug_IC50_train<-as.numeric(drug_IC50_train)
  return(list(drug_IC50_train=drug_IC50_train, CCLE_train=CCLE_train))
}

getCGPinfo_CTRP<-function(drug){
  whichNas <- which(is.na(CTRP[drug,]))
  if (length(whichNas)==0) {drug_IC50<-CTRP[drug,]} else {drug_IC50<-CTRP[drug,][-whichNas]}
  
  commonCellLines<-intersect(colnames(CCLE_2018_mat),colnames(drug_IC50))
  CCLE_train<-CCLE_2018_mat[,commonCellLines]
  drug_IC50_train<-drug_IC50[commonCellLines]
  
  CCLE_train<-as.matrix(CCLE_train)
  drug_IC50_train<-as.numeric(drug_IC50_train)
  return(list(drug_IC50_train=drug_IC50_train, CCLE_train=CCLE_train))
}



doVariableSelection <- function(exprMat, removeLowVaryingGenes)
{
  vars <- apply(exprMat, 1, var)
  return(order(vars, decreasing=TRUE)[seq(1:as.integer(nrow(exprMat)*(1-removeLowVaryingGenes)))])
}

doCv_Own <- function(num, cvFold, minNumSamples,methods)
{
  # print(num)
  if(theLens[num] > 40)
  {
    return(GR_CV(possibleDrugs[num], cvFold=cvFold, minNumSamples=minNumSamples, methods=methods))
  }
  else(return(NULL))
}

load("/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/L-CV/theLens.RData")

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

set.seed(1000)

loocvOut_all_GR <- mclapply(1:length(possibleDrugs), doCv_Own, cvFold=10, minNumSamples=0, methods="GR",mc.cores=10)
names(loocvOut_all_GR) <- possibleDrugs
setwd("/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/L-CV")
save(loocvOut_all_GR,file="LCV_GR.RData")


corP <- numeric()
corSpear <- numeric()
#nAll <- numeric()

for(i in 1:length(loocvOut_all_GR))
{
  if((!is.null(loocvOut_all_GR[[i]][[1]])))
  {
    # correlations on all tumors.
    theCor <- cor.test(loocvOut_all_GR[[i]][[1]], loocvOut_all_GR[[i]][[2]])
    corP[i] <- theCor$p.value
    corSpear[i] <- theCor$estimate
   # nAll[i] <- length(loocvOut_all_GR[[i]][[1]])
  }
}
names(corP) <- possibleDrugs
names(corSpear) <- possibleDrugs

nAll<-theLens
m <- cbind(nAll, corSpear, corP)
colnames(m) <- c("N All", "Spearman Correlation All", "Spearman P-value All")

write.csv(m, file="LCV_GR_P.csv")

corP <- numeric()
corSpear <- numeric()
#nAll <- numeric()

for(i in 1:length(loocvOut_all_rfinv2))
{
  if((!is.null(loocvOut_all_rfinv2[[i]][[1]])))
  {
    # correlations on all tumors.
    theCor <- cor.test(loocvOut_all_rfinv2[[i]][[1]], loocvOut_all_rfinv2[[i]][[2]])
    corP[i] <- theCor$p.value
    corSpear[i] <- theCor$estimate
    # nAll[i] <- length(loocvOut_all_rfinv2[[i]][[1]])
  }
}
names(corP) <- possibleDrugs
names(corSpear) <- possibleDrugs

nAll<-theLens
m <- cbind(nAll, corSpear, corP)
colnames(m) <- c("N All", "Spearman Correlation All", "Spearman P-value All")

write.csv(m, file="LCV_rfinv2_P.csv")

mean(m[,2],na.rm =T)
median(m[,2],na.rm =T)
IQR(m[,2],na.rm =T)

eval_result_train<-function(true,preds){
  MAE <- mean(abs(true - preds))
  SSE <- sum((true - preds)^2)
  SST <- sum((true - mean(true))^2)
  SSM <- sum((preds-mean(true))^2)
  R2 <- 1 - SSE / SST
  RMSE = sqrt(SSE/length(preds))
  NRMSE=RMSE/(max(true)-min(true))
  results<-data.frame(RMSE=RMSE,MAE=MAE,R2=R2,NRMSE=NRMSE)
  return(results)
}

Stats_table<-data.frame()

  for (i in 1:length(possibleDrugs)) {
    print(i)
    
    true=as.numeric(loocvOut_all_GR[[i]][[2]])
    preds=as.numeric(loocvOut_all_GR[[i]][[1]])
    
    if (length(which(is.na(preds)))!=0){
      true<-true[-which(is.na(preds))]
      preds<-preds[-which(is.na(preds))]
    }
    
    Stats_table0 <- data.frame(
      drug = possibleDrugs[i],
      Stats=eval_result_train(true,preds)
    )
    Stats_table <- rbind(Stats_table,Stats_table0)
  }

write.csv(Stats_table,file="Stats_Combine_GR.csv")

mean_RMSE<-mean(Stats_table[,2],na.rm =T)
median_RMSE<-median(Stats_table[,2],na.rm =T)
IQR_RMSE<-IQR(Stats_table[,2],na.rm =T)

mean_MAE<-mean(Stats_table[,3],na.rm =T)
median_MAE<-median(Stats_table[,3],na.rm =T)
IQR_MAE<-IQR(Stats_table[,3],na.rm =T)

mean_R2<-mean(Stats_table[,4],na.rm =T)
median_R2<-median(Stats_table[,4],na.rm =T)
IQR_R2<-IQR(Stats_table[,4],na.rm =T)

mean_NRMSE<-mean(Stats_table[,5],na.rm =T)
median_NRMSE<-median(Stats_table[,5],na.rm =T)
IQR_NRMSE<-IQR(Stats_table[,5],na.rm =T)



