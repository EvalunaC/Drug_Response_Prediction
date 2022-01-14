set.seed(1000)

library(pRRophetic)
library(car)

#load("/home/Qiang/DrugResponse/BRCA.RData")
#load("/home/Qiang/DrugResponse/Independent_datasets/GSE16446_median.RData")

load("/home/Qiang/DrugResponse/TCGA_Log2_New.RData")
load("/home/Qiang/DrugResponse/CCLE_mat.RData")

### GDSC1
GDSC<-read.csv("/home/Qiang/DrugResponse/GDSC1/GDSC1_IC50_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(GDSC)<-gsub("/","",rownames(GDSC))
colnames(GDSC)<- gsub("^X", "",  colnames(GDSC))

### GDSC2
GDSC<-read.csv("/home/Qiang/DrugResponse/GDSC2/GDSC2_IC50_matrix_New.csv", header=T,row.names=1)
possibleDrugs<-rownames(GDSC)
colnames(GDSC)<- gsub("^X", "",  colnames(GDSC))


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

doVariableSelection <- function(exprMat, removeLowVaryingGenes)
{
  vars <- apply(exprMat, 1, var)
  return(order(vars, decreasing=TRUE)[seq(1:as.integer(nrow(exprMat)*(1-removeLowVaryingGenes)))])
}

######################################################
# Obtain medication list and prepare data from here
######################################################

getDrugData <- function(input_medication_name){

  CCLETrainData<-getCGPinfo_New(input_medication_name)

  trainingExprData<-CCLETrainData$CCLE_train
  trainingPtype<-CCLETrainData$drug_IC50_train
  #testExprData<-BRCA  ### If only need the BRCA as the test matrix, then use this line.
  testExprData<-TCGA_Log2_New   #### Treat the whole TCGA as the test matrix. 
  selection=1
  batchCorrect="standardize"
  removeLowVaryingGenes=0.2
  removeLowVaringGenesFrom="rawData"
  printOutput=TRUE
  powerTransformPhenotype=TRUE

  trainExprMat=trainingExprData
  testExprMat=testExprData

  homData <- homogenizeData(testExprData, trainingExprData,
                            batchCorrect = batchCorrect, selection = selection, printOutput = printOutput)
  keepRows <- seq(1:nrow(homData$train))
  evaluabeGenes <- rownames(homData$test)
  keepRowsTrain <- doVariableSelection(trainingExprData[evaluabeGenes,
  ], removeLowVaryingGenes = removeLowVaryingGenes)
  keepRowsTest <- doVariableSelection(testExprData[evaluabeGenes,
  ], removeLowVaryingGenes = removeLowVaryingGenes)
  keepRows <- intersect(keepRowsTrain, keepRowsTest)
  numberGenesRemoved <- nrow(homData$test) - length(keepRows)
  if (printOutput)
    cat(paste("\n", numberGenesRemoved, "low variabilty genes filtered."))
  offset = 0
  if (powerTransformPhenotype) {
    if (min(trainingPtype) < 0) {
      offset <- -min(trainingPtype) + 1
      trainingPtype <- trainingPtype + offset
    }
    transForm <- powerTransform(trainingPtype)[[6]]
    trainingPtype <- trainingPtype^transForm
  }

  trainFrame <- data.frame(Resp = trainingPtype, t(homData$train[keepRows,]))
  testFrame <- data.frame(t(homData$test[keepRows, ]))
  return(list(trainFrame=trainFrame,transForm=transForm,testFrame=testFrame,offset=offset))
  }


transForm_num<-data.frame()
for (i in 1:length(possibleDrugs)){
  print(i)
  A<-getDrugData(possibleDrugs[i])
  trainFrame<-A[[1]]
  transForm<-A[[2]]
  testFrame<-A[[3]]
  offset<-A[[4]]
  save(trainFrame,file=paste("/home/Qiang/DrugResponse/GDSC2/GDSC2_Frame/RData/",possibleDrugs[i],"_trainFrame.RData", sep="",collapse = NULL))
  save(testFrame,file=paste("/home/Qiang/DrugResponse/GDSC2/GDSC2_Frame/RData/",possibleDrugs[i],"_testFrame.RData", sep="",collapse = NULL))
  transForm_num0<-data.frame(drug=possibleDrugs[i],transForm=transForm,offset=offset)
  transForm_num<-rbind(transForm_num,transForm_num0)
}
setwd("/home/Qiang/DrugResponse/GDSC2/GDSC2_Frame")
write.csv(transForm_num,file="transForm_GDSC2.csv")


#### since GDSC1 and GDSC2 use IC50, so we make powerTransformPhenotype=TRUE; 
### but CTRP1 and CTRP2 use the AUC as drug response, so we make powerTransformPhenotype=TRUE
