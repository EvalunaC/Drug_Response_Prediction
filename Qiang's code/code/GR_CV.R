GR_CV<-function (drug, testExprData = NULL, cvFold = 10, 
                 powerTransformPhenotype = TRUE, batchCorrect = "none", removeLowVaryingGenes = 0.2, 
                 minNumSamples = 0, selection = 1, methods="GR") 
{
  CCLETrainData <- getCGPinfo_New(drug)
  cvOut <- GR_ByCV(CCLETrainData$CCLE_train, 
                   CCLETrainData$drug_IC50_train, testExprData = testExprData, cvFold = cvFold, 
                   powerTransformPhenotype = powerTransformPhenotype, batchCorrect = batchCorrect, 
                   removeLowVaryingGenes = removeLowVaryingGenes, minNumSamples = minNumSamples, 
                   selection = selection,methods=methods)
  return(cvOut)
}


GR_ByCV<-function (trainingExprData, trainingPtype, testExprData = -1, 
                   cvFold = -1, powerTransformPhenotype = TRUE, batchCorrect = "eb", 
                   removeLowVaryingGenes = 0.2, minNumSamples = 10,selection = 1, methods="GR") 
{    if ((ncol(trainingExprData) < minNumSamples)) {
  stop(paste("There are less than", minNumSamples, "samples in your test or training set. It is strongly recommended that you use larger numbers of samples in order to (a) correct for intrinsic difference in your training and test sets and (b) fit a reliable model. To supress this message, change the \"minNumSamples\" parameter to this function."))
}
  
  if (is.null(testExprData)) {
    homData <- list()
    homData$selection <- selection
    homData$train <- trainingExprData
  }
  
  nTrain <- ncol(trainingExprData)
  predPtype <- numeric()
  
  randTestSamplesIndex <- sample(1:nTrain)
  sampleGroup <- rep(cvFold, nTrain)
  groupSize <- as.integer(nTrain/cvFold)
  for (j in 1:(cvFold - 1)) {
    sampleGroup[(((j - 1) * groupSize) + 1):(j * groupSize)] <- rep(j, 
                                                                    groupSize)
  }
  cvGroupIndexList <- split(randTestSamplesIndex, sampleGroup)
  for (j in 1:cvFold) {
    testCvSet <- homData$train[, cvGroupIndexList[[j]]]
    trainCvSet <- homData$train[, unlist(cvGroupIndexList[-j])]
    trainPtypeCv <- trainingPtype[unlist(cvGroupIndexList[-j])]
    predPtype <- c(predPtype, calcPhenotype_Own(trainCvSet, 
                                            trainPtypeCv, testCvSet, batchCorrect = "none", 
                                            minNumSamples = 0, selection = homData$selection, 
                                            removeLowVaryingGenes = removeLowVaryingGenes, 
                                            powerTransformPhenotype = powerTransformPhenotype, 
                                            printOutput = FALSE,methods=methods))
    cat(paste("\n", j, " of ", cvFold, " iterations complete.", 
              sep = ""))
  }
  predPtype <- predPtype[order(randTestSamplesIndex)]
  
  
  finalData <- list(cvPtype = predPtype, realPtype = trainingPtype)
  class(finalData) <- "pRRopheticCv"
  return(finalData)
}

calcPhenotype_Own<-function (trainingExprData, trainingPtype, testExprData, batchCorrect = "eb", 
          powerTransformPhenotype = TRUE, removeLowVaryingGenes = 0.2, 
          minNumSamples = 10, selection = -1, printOutput = TRUE, removeLowVaringGenesFrom = "homogenizeData",methods="GR") 
{
  if (class(testExprData) != "matrix") 
    stop("ERROR: \"testExprData\" must be a matrix.")
  if (class(trainingExprData) != "matrix") 
    stop("ERROR: \"trainingExprData\" must be a matrix.")
  if (class(trainingPtype) != "numeric") 
    stop("ERROR: \"trainingPtype\" must be a numeric vector.")
  if (ncol(trainingExprData) != length(trainingPtype)) 
    stop("The training phenotype must be of the same length as the number of columns of the training expressin matrix.")
  if ((ncol(trainingExprData) < minNumSamples) || (ncol(testExprData) < 
                                                   minNumSamples)) {
    stop(paste("There are less than", minNumSamples, "samples in your test or training set. It is strongly recommended that you use larger numbers of samples in order to (a) correct for batch effects and (b) fit a reliable model. To supress this message, change the \"minNumSamples\" parameter to this function."))
  }
  homData <- homogenizeData(testExprData, trainingExprData, 
                            batchCorrect = batchCorrect, selection = selection, printOutput = printOutput)
  if (!(removeLowVaringGenesFrom %in% c("homogenizeData", "rawData"))) {
    stop("\"removeLowVaringGenesFrom\" must be one of \"homogenizeData\", \"rawData\"")
  }
  keepRows <- seq(1:nrow(homData$train))
  if (removeLowVaryingGenes > 0 && removeLowVaryingGenes < 
      1) {
    if (removeLowVaringGenesFrom == "homogenizeData") {
      keepRows <- doVariableSelection(cbind(homData$test, 
                                            homData$train), removeLowVaryingGenes = removeLowVaryingGenes)
      numberGenesRemoved <- nrow(homData$test) - length(keepRows)
      if (printOutput) 
        cat(paste("\n", numberGenesRemoved, "low variabilty genes filtered."))
    }
    else if (removeLowVaringGenesFrom == "rawData") {
      evaluabeGenes <- rownames(homData$test)
      keepRowsTrain <- doVariableSelection(trainingExprData[evaluabeGenes, 
      ], removeLowVaryingGenes = removeLowVaryingGenes)
      keepRowsTest <- doVariableSelection(testExprData[evaluabeGenes, 
      ], removeLowVaryingGenes = removeLowVaryingGenes)
      keepRows <- intersect(keepRowsTrain, keepRowsTest)
      numberGenesRemoved <- nrow(homData$test) - length(keepRows)
      if (printOutput) 
        cat(paste("\n", numberGenesRemoved, "low variabilty genes filtered."))
    }
  }
  offset = 0
  if (powerTransformPhenotype) {
    if (min(trainingPtype) < 0) {
      offset <- -min(trainingPtype) + 1
      trainingPtype <- trainingPtype + offset
    }
    transForm <- powerTransform(trainingPtype)[[6]]
    trainingPtype <- trainingPtype^transForm
  }
  if (printOutput) 
    cat("\nFitting Ridge Regression model... ")
  trainFrame <- data.frame(Resp = trainingPtype, t(homData$train[keepRows, 
  ]))
  
  if (methods=="GR") {
    Model <- linearRidge(Resp ~ ., data = trainFrame)
  }
  
  if (methods=="rfG") {
    Model <- randomForest(Resp~.,data=trainFrame, importance = TRUE)
  }
  
  if (methods=="pcr") {
    Model <- train(Resp~.,data=trainFrame,method="pcr",trControl=trainControl("cv",number=10))
  }
  
  if (methods=="pls") {
    Model <- train(Resp~.,data=trainFrame,method="pls",trControl=trainControl("cv",number=10))
  }
  
  if (methods=="Lasso") {
    cv_output_1 <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=1,type.measure="mse",nfolds=10)
    best_lam <- cv_output_1$lambda.min
    Model<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=1, lambda=best_lam)
  }
  
  if (methods=="ENglm") {
    cv_output <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0.5,type.measure="mse",nfolds=10)
    best_lam <- cv_output$lambda.min
    Model<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0.5, lambda=best_lam)
  }
  
  if (methods=="Ridgeglm") {
    cv_output_0 <- cv.glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0,type.measure="mse",nfolds=10)
    best_lam <- cv_output_0$lambda.min
    Model<- glmnet(as.matrix(trainFrame[,-1]),as.matrix(trainFrame$Resp),alpha=0, lambda=best_lam)
  }
  
  if (printOutput) 
    cat("Done\n\nCalculating predicted phenotype...")
  #totBeta <- sum(abs(coef(rrModel)))
  #eachBeta <- abs(coef(rrModel))
  #eachContribution <- eachBeta/totBeta

    if (class(homData$test) == "numeric") {
    n <- names(homData$test)
    homData$test <- matrix(homData$test, ncol = 1)
    rownames(homData$test) <- n
    testFrame <- data.frame(t(homData$test[keepRows, ]))
    
    if (methods=="GR" | methods=="rfG" | methods=="pcr" | methods=="pls"){
      preds <- predict(Model, newdata = rbind(testFrame, testFrame))[1] 
    }
    
    if (methods=="Lasso" | methods=="ENglm" | methods=="Ridgeglm"){
      preds<- predict(Model, s = best_lam, newx=as.matrix(testFrame))
    }
    
    if (methods=="rfinv2"){
     Model <- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                            method = "split-conformal", alpha = 0.05,seed = 1000)
     preds<- Model$testPred 
    }
    
    if (methods=="rfinv1"){
      Model <- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "oob", alpha = 0.05,symmetry = TRUE)
      preds<- Model$testPred 
    }
    
    if (methods=="rfinv3"){
      Model <- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "quantreg", alpha = 0.05)
      preds<- Model$testPred 
    }
  }
  else {
    testFrame <- data.frame(t(homData$test[keepRows, ]))
    if (methods=="GR" | methods=="rfG" | methods=="pcr" | methods=="pls"){
      preds <- predict(Model, newdata = testFrame)
    }
    
    if (methods=="Lasso" | methods=="ENglm" | methods=="Ridgeglm"){
      preds<- predict(Model, s = best_lam, newx=as.matrix(testFrame))
    }
    
    if (methods=="rfinv2"){
      Model <- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "split-conformal", alpha = 0.05,seed = 1000)
      preds<- Model$testPred 
    }
    
    if (methods=="rfinv1"){
      Model <- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "oob", alpha = 0.05,symmetry = TRUE)
      preds<- Model$testPred 
    }
    
    if (methods=="rfinv3"){
      Model <- rfinterval(Resp~.,train_data=trainFrame, test_data = testFrame,
                          method = "quantreg", alpha = 0.05)
      preds<- Model$testPred 
    }
  }
  
  if (powerTransformPhenotype) {
    preds <- preds^(1/transForm)
    preds <- preds - offset
  }
  
  if (printOutput) 
    cat("Done\n\n")
  return(preds)
}

