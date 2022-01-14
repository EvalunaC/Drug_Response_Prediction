### GDSC1
GDSC<-read.csv("/home/Qiang/DrugResponse/GDSC1/GDSC1_IC50_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(GDSC)<-gsub("/","",rownames(GDSC)) ## Lapatinib=40

### GDSC2
GDSC<-read.csv("/home/Qiang/DrugResponse/GDSC2/GDSC2_IC50_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(GDSC) ##Lapatinib=81

### CTRP1
CTRP<-read.csv("/home/Qiang/DrugResponse/CTRP1/CTRP1_AUC_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(CTRP)

### CTRP2
CTRP<-read.csv("/home/Qiang/DrugResponse/CTRP2/CTRP2_AUC_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(CTRP)<-gsub("[[:punct:]]", "-",  rownames(CTRP))


load("/home/Qiang/DrugResponse/CTRP2/Output/mid_evaluation/PCC/Combine/Test_Combine_CTRP2.RData")
load("/home/Qiang/DrugResponse/CTRP2/Output/mid_evaluation/PCC/Combine/testResp.RData")

################################## Stats ##############################
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

setwd("/home/Qiang/DrugResponse/CTRP2/Output")

Stats_table<-data.frame()
for (j in 1:length(Test_Combine)) {
  for (i in 1:length(possibleDrugs)) {
    print(c(j,i))
    Stats_table0 <- data.frame(
      drug = possibleDrugs[i],
      method = names(Test_Combine)[j],
      Stats=eval_result_train(true=as.numeric(testResp[[i]]),preds=as.numeric(unlist(Test_Combine[[j]][i])))
    )
    Stats_table <- rbind(Stats_table,Stats_table0)
  }
}

write.csv(Stats_table,file="Stats_Combine_CTRP2.csv")

Stats_mean<-aggregate(Stats_table[,3:6],list(Stats_table$method),mean)

########################## PCC #################################
PCC_table<-data.frame()
for (j in 1:length(Test_Combine)){
  for (i in 1:length(possibleDrugs)){
    print(c(j,i))
    PCC_table0 <- data.frame(
      drug = possibleDrugs[i],
      method = names(Test_Combine)[j],
      PCC_CV28=cor(as.numeric(testResp[[i]]), as.numeric(unlist(Test_Combine[[j]][i])),method = "pearson")
    )
    PCC_table <- rbind(PCC_table,PCC_table0)
  }
}
write.csv(PCC_table,file="PCC_Combine_CTRP2.csv")

##################### SCC #################################
SCC_table<-data.frame()
for (j in 1:length(Test_Combine)){
  for (i in 1:length(possibleDrugs)){
    print(c(j,i))
    SCC_table0 <- data.frame(
      drug = possibleDrugs[i],
      method = names(Test_Combine)[j],
      SCC_CV28=cor(as.numeric(testResp[[i]]), as.numeric(unlist(Test_Combine[[j]][i])),method = "spearman")
    )
    SCC_table <- rbind(SCC_table,SCC_table0)
  }
}
write.csv(SCC_table,file="SCC_Combine_CTRP2.csv")

SCC_mean<-aggregate(SCC_table[,3],list(SCC_table$method),mean)
SCC_median<-aggregate(SCC_table[,3],list(SCC_table$method),median)


mean(SCC_table[,2])
median(SCC_table[,2])
sum(SCC_table[,2]>0.5)

## h2o
testResp1<-testResp[order(names(testResp))]
ldf1<-ldf[order(names(ldf))]

PCC_table<-data.frame()
  for (i in 1:length(testResp1)){
    print(i)
    PCC_table0 <- data.frame(
      drug = names(testResp1)[i],
      PCC_CV28=cor(as.numeric(testResp1[[i]]), as.numeric(unlist(ldf1[[i]])),method = "pearson")
    )
    PCC_table <- rbind(PCC_table,PCC_table0)
  }

write.csv(PCC_table,file="/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/PCC/h2o/PCC_h2o.csv")

mean(PCC_table[,2])
median(PCC_table[,2])
sum(PCC_table[,2]>0.5)

#### h2o
Stats_table<-data.frame()
for (i in 1:length(testResp1)) {
  Stats_table0 <- data.frame(
    drug = names(testResp1)[i],
    Stats=eval_result_train(true=as.numeric(testResp1[[i]]),preds=as.numeric(unlist(ldf1[[i]])))
  )
  Stats_table <- rbind(Stats_table,Stats_table0)
}

write.csv(Stats_table,file="/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/PCC/h2o/Stats_h2o.csv")

mean(Stats_table[,2])
mean(Stats_table[,3])
mean(Stats_table[,4])
mean(Stats_table[,5])

#========================================
#                 Plotting
#========================================
library("ggpubr")
library(ggplot2)
require(gridExtra)
library(cowplot)

PCC_table <- read.csv("/home/Qiang/DrugResponse/GDSC1/Output/PCC_Combine_GDSC1.csv", header = TRUE,na.strings="NA")
PCC_mean<-aggregate(PCC_table[,3],list(PCC_table$method),mean)
PCC_median<-aggregate(PCC_table[,3],list(PCC_table$method),median)

filename = paste("/home/Qiang/DrugResponse/GDSC2/Output/GDSC2_PCC.pdf", sep="",collapse = NULL)
pdf(filename,width=10, height=10)
ggviolin(subset(PCC_table, !is.na(PCC_CV28)), x = "method", y = "PCC_CV28", title="Drugs CV28 PCC Violinplot",
         color = "method", ylim = c(-0.5, 0.9),
         ylab = "PCC (Closer to 1 is better)", xlab = "method",
         repel=TRUE,draw_quantiles = 0.5,trim=FALSE,add = "jitter")+  # outlier.shape = NA
  scale_x_discrete(guide = guide_axis(angle = 20))+
  stat_summary(fun.data = function(x) data.frame(y=0.05, label =signif(median(x),digits = 3)), geom="text") +
  theme(legend.position="none")

ggboxplot(PCC_table, x = "method", y = "PCC_CV28",title="GDSC2 Drugs CV28 PCC Boxplot",
          color = "method", ylim = c(-0.5, 0.9),font.label = list(size = 4),
          ylab = "Pearson Cor Coef \n(Closer to 1 is better)", xlab = "method",repel=TRUE)+
  scale_x_discrete(guide = guide_axis(angle = 20))+ rremove("legend")
dev.off()



