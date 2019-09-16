# Creator: Austin M. Smith (amsmith11@usf.edu)

# Journal:  Biological Invasions   

# Title: "Machine learning for game bird site selection: case for Chukar.

#Authors:  Austin M. Smith, Wendell P. Cropper Jr., Michael Moutlon

# Discription: Analysis of site-level factors [in an attempt] to model habitat suitability of 
#              introducted Phaesianids to Washington State: case for Alectoris chuckar 



###-----------------------------------------------------------------------------------------------------------------------



### code chunk number 1: clear
###################################################


rm(list=ls(all=TRUE))



###-----------------------------------------------------------------------------------------------------------------------


# Import packages
######## ######## ######## ######## ######## ######## ######## ########



library(caret)  # used ot create individual models 
library(caretEnsemble) # used to create ensemble from individual models
library(kernlab) # used to build support vector machine 
library(randomForest) #  used to build random forest  model 
library(gbm) #  used to build gradient boosted tree package 
library(nnet) # used to build neural network model 
library(ggplot2) # graphing package 
library(lattice) #?
library(pROC)
library(plyr)
library(dplyr)


###-----------------------------------------------------------------------------------------------------------------------


# Import data sets
######## ######## ######## ######## ######## ######## ######## ########


source("State-dfs-A.chukar.R")


###-----------------------------------------------------------------------------------------------------------------------



### Correlation matrix for input data 
######## ######## ######## ######## ######## ######## ######## ########
# 
# 
# library(corrplot)
# 
# 
# Forms data into large matrix for computational purposes
# A <- as.matrix( Galbreath.ds[ 2:22 ] )
# A
# 
# # The correaltion matrix for Matrix A
# Corr_A = cor( A )
# 
# 
# # Visual representaion/ plot of Corr_A
# par(mar=c(1,1,1,1))
# corrplot( cor( Corr_A ),
#           #order = "hclust" ,
#           tl.col = 'blue',
#           tl.cex = .75 ,
#           addCoefasPercent = TRUE ,
#           title = "Correlation Between Input Variables" )



###-----------------------------------------------------------------------------------------------------------------------



#### Partition data for training, method = Cross-validation 
######## ######## ######## ######## ######## ######## ######## ########


set.seed(60720194) # Run 5 folds, 5 reps


####  Stratified sampling
# TrainingDataIndex <- createDataPartition( Galbreath.ds$Chukar.Occurrence , 
#                                           p=0.80 , 
#                                           list = FALSE )
# 
# 
# 
# # Create training  and  testing partitions
# 
# trainingData <- Galbreath.ds[ TrainingDataIndex, ]
# 
# testData <- Galbreath.ds[ -TrainingDataIndex, ]

trainingData <- Galbreath.ds
test.Data <- data.frame( test.WA.NoGal[ 2:23] )


# set the training parameters for model learning
# set the training parameters for model learning
TrainingParameters <- trainControl( method = "repeatedcv",  # cross vailidation -  repeated 5 times
                                    number = 5  ,           # number of folds/
                                    repeats = 5 ,          # repeat procedure 5 times
                                    classProbs =  TRUE,     # outputs for choropleth
                                    savePredictions = TRUE,
                                    
                                    summaryFunction = twoClassSummary) # needed to  calcuate AUC
#TrainingParameters



# Remove column "names" so models don't read it as a variable
#train.Data <- data.frame( trainingData[ 2:23 ] )
#train.Data
#test.Data <- data.frame( testData[ 2:23] )
#test.Data
#dim(test.Data)


train.Data <- data.frame( trainingData[ 2:23 ] )
test.Data <- data.frame( test.WA.NoGal[ 2:23] )


###-----------------------------------------------------------------------------------------------------------------------


# Builid all models through caretEnsemble
######## ######## ######## ######## ######## ######## ######## ########

model.Ensemble.list <- caretList(Chukar.Occurrence ~ . , 
                                 data = train.Data ,
                                 trControl = TrainingParameters ,
                                 metric = "ROC",
                                 methodList=c("glm","gbm", "nnet", "rf", "svmPoly"))

model.Ensemble.list

# View Correlation of trained models results to see if good for ensembling 
# model.corr<- modelCor(resamples(model.Ensemble.list))
# model.corr



# Run the ensemble model 
model.Ensemble <- caretEnsemble(model.Ensemble.list, 
                                trControl = TrainingParameters ,
                                metric = "ROC",
                                preProcess = c( "scale", "center" ))

summary(model.Ensemble)  # Collection of ROC Scores al ALLL models - Training 



###-----------------------------------------------------------------------------------------------------------------------


#### Testing Set for indiviual models  
######## ######## ######## ######## ######## ######## ######## ########


# Define each model in ensemble for simplicity.
# Each model and the statistics for model selection.
# GLM <- model.Ensemble$models$glm
# SVM <- model.Ensemble$models$svmPoly
# RF <- model.Ensemble$models$rf
# ANN <- model.Ensemble$models$nnet
# GBM <- model.Ensemble$models$gbm
# 
# 
# # Calls each model.
# GLM 
# SVM 
# RF 
# ANN 
# GBM 



######## Calacuate Test Set  #########



# This is for the testing set, WA 
# Calculate AUC for all six models and compare 
library("caTools")
test.probs <- lapply(model.Ensemble.list, predict, newdata=test.Data, type="prob")  # WA test data 
test.probs <- lapply(test.probs, function(x) x[,"P"])
test.probs <- data.frame(test.probs)
ens.probs <- predict(model.Ensemble, newdata=test.Data, type="prob")
test.probs$ensemble <-  ens.probs
par(mar=c(3,3,3,3))
caTools::colAUC(test.probs, test.Data$Chukar.Occurrence, plotROC=TRUE) # creates ROC curves plots

# Above generates AUC for test data 


## TO   MEASURE ACCURACY FOR TEST DATA
## -----------------------------------------------------------

# # Run the test.Data through each model
## Calculates P/A
test.GLM <- predict( model.Ensemble$models$glm  , newdata = test.Data )
test.GBM <- predict( model.Ensemble$models$gbm  , newdata = test.Data )
test.ANN <-  predict( model.Ensemble$models$nnet , newdata = test.Data )
test.RF <- predict( model.Ensemble$models$rf  , newdata = test.Data )
test.SVM <- predict( model.Ensemble$models$svmPoly  , newdata = test.Data )
test.Ensemble<- predict( model.Ensemble  , newdata = test.Data )


# creates 
confusionMatrix(test.GLM  , test.Data$Chukar.Occurrence, positive="P" )
confusionMatrix(test.GBM , test.Data$Chukar.Occurrence, positive="P" )
confusionMatrix(test.ANN  , test.Data$Chukar.Occurrence, positive="P" )
confusionMatrix(test.RF  , test.Data$Chukar.Occurrence, positive="P" )
confusionMatrix(test.SVM , test.Data$Chukar.Occurrence, positive="P" )
confusionMatrix(test.Ensemble  , test.Data$Chukar.Occurrence, positive="P" )






 
# rf.ROC <- roc(predictor=test.RF$A,
#                response=test.Data$Chukar.Occurrence,
#                levels=rev(levels(test.Data$Chukar.Occurrence)))
# rf.ROC$auc
# 
# 
# # Testing Set for Ensemble  for P/A
# # Run the test.Data through each model
# model.test <- predict( model.Ensemble  , newdata = test.Data, type="prob" )
# model.test 


# #### Confusion Matrix
# cm.model.test <- confusionMatrix( table( model.test , test.Data$Chukar.Occurrence ) )
# cm.model.test








###-----------------------------------------------------------------------------------------------------------------------
#
#
# ###-----------------------------------------------------------------------------------------------------------------------
# # OREGON VALIDATION SET
# ###-----------------------------------------------------------------------------------------------------------------------
# 
# 
###-----------------------------------------------------------------------------------------------------------------------


## PREDICT OR for individual models
######## ######## ######## ######## ######## ######## ######## ########


#Produces data.frame for OR probabilities 
model.val <- lapply(model.Ensemble.list, predict, newdata=OR.ds, type="prob")
model.val <- lapply(model.val, function(x) x[,"P"])
model.val <- data.frame(model.val)
ens.val <- predict(model.Ensemble, newdata=OR.ds, type="prob")
model.val$ensemble <-  ens.val
par(mar=c(1,1,1,1))
caTools::colAUC(model.val, OR.Original$Chukar.Occurrence, plotROC=TRUE) # creates ROC curves plots

model.val


OR.val.probs  <- cbind.data.frame(True, model.val) 
names(OR.val.probs) <- c( "Quadname", "True", "GLM", "GBM", "ANN", "RF", "SVM", "Ensemble" )


## Validation of OR for individual models Ensemble  
######## ######## ######## ######## ######## ######## ######## ########

# Creates data.frames with P/A
Val.GLM <-data.frame(predict( model.Ensemble$models$glm, newdata = OR.ds ))
Val.GBM <-data.frame(predict( model.Ensemble$models$gbm,  newdata = OR.ds ))
Val.ANN <-data.frame(predict( model.Ensemble$models$nnet ,  newdata = OR.ds ))
Val.RF <-data.frame(predict( model.Ensemble$models$rf,  newdata = OR.ds ))
Val.SVM <-data.frame(predict( model.Ensemble$models$svmPoly, newdata = OR.ds ))
Val.Ensemble <- data.frame(predict( model.Ensemble  , newdata = OR.ds  ))

# 



# Pull from orginal OR.Data for comparative purposes
True <- data.frame(OR.Original[1], OR.Original[23])

# Create single data.frame with all OR.ds P/A predictions
Complete.results <- cbind.data.frame(True, Val.GLM, Val.GBM, Val.ANN, Val.RF, Val.SVM, Val.Ensemble )
names(Complete.results) <- c( "Quadname", "True", "GLM", "GBM", "ANN","RF", "SVM", "Ensemble" )
Complete.results




# conusion matrix 
confusionMatrix(Complete.results$GLM, Complete.results$True, positive="P" )
confusionMatrix(Complete.results$GBM , Complete.results$True, positive="P" )
confusionMatrix(Complete.results$ANN , Complete.results$True, positive="P" )
confusionMatrix(Complete.results$RF , Complete.results$True, positive="P" )
confusionMatrix(Complete.results$SVM , Complete.results$True, positive="P" )
confusionMatrix(Complete.results$Ensemble , Complete.results$True, positive="P" )



###-----------------------------------------------------------------------------------------------------------------------


# WRITE CSV FILES
######## ######## ######## ######## ######## ######## ######## ########



### OREGON

#OR P/A
write.csv(Complete.results,  "Complete-OR-Val.PA-Data-Original-2.csv")


#OR Probs
write.csv(OR.val.probs,  "Complete-OR-Val.Prob-Data-Original-2.csv")


###-----------------------------------------------------------------------------------------------------------------------


# SAVE ENVIRONMENT

save.image('A.chukar-Ensemble-HS.Model-Original.rda')



###-----------------------------------------------------------------------------------------------------------------------


