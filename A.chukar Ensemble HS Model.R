# Creator: Austin M. Smith

# Article to ESA journal:  Ecological Applications 

# Title: "Using machine learning algorithms to evaluate habitat quality for Chukar Partridge 
#         management in Washington state"

#Authors:  Austin M. Smith, Wendell P. Cropper Jr., Michael Moutlon

# Discription: Analysis of site-level factors in an attempt to model habiat suitability of 
#              introducted Phaesianids to Washington State: case for Alectoris chuckar 



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



###-----------------------------------------------------------------------------------------------------------------------



# Import and manipulate data sets
######## ######## ######## ######## ######## ######## ######## ########



# All Quadrangles for the Washington state 24K layer 
WA.chukar <- A_chukar_HS_variables_WA_24K # include ALL WA and ALL Variables 


#  Choose the Variables needed for model
WA.Data <-  data.frame(  WA.chukar[2:20],  WA.chukar[24:25], WA.chukar[31], WA.chukar[39:40]  )
WA.Data[ is.na( WA.Data) ] <- 0
WA.Data



#### Review Data
sum( is.na( WA.Data ) ) # Check for NAs



# Select subset of data for training the model 
# Here we use the quadrangles from the counties specified in Galbreath 1954 

Galbreath.ds <- WA.Data
Galbreath.ds[ is.na( Galbreath.ds ) ] <- 0  ###  Substititue '0' for all 'N/A'
Galbreath.ds <- subset( Galbreath.ds, Galbreath == "Y" )
Galbreath.ds


# Confirm Data has correct input and statistics 
sum( is.na( Galbreath.ds) )
summary( Galbreath.ds )
dim( Galbreath.ds )




# Validation Data - Select OR counties 
# We will use the Our model to predict/define the P/A of chukar in OR based on the training on WA data
OR.chukar <-  A_chukar_HS_variables_OR_24K # FULL Data

OR.Data <- data.frame( OR.chukar[ 3:20], OR.chukar[24:25], OR.chukar[31], OR.chukar[39]  ) 
OR.Data[ is.na( OR.Data ) ] <- 0  ###  Substititue '0' for all 'N/A'
OR.Data



dim( OR.Data )
summary( OR.Data )
sum( is.na( OR.Data) )

OR.ds <- OR.Data[1:21]



###-----------------------------------------------------------------------------------------------------------------------



### Correlation matrix for input data 
######## ######## ######## ######## ######## ######## ######## ########


library(corrplot)


# Forms data into large matrix for computational purposes 
A <- as.matrix( Galbreath.ds[ 2:22 ] )
A

# The correaltion matrix for Matrix A
Corr_A = cor( A )


# Visual representaion/ plot of Corr_A 
par(mar=c(1,1,1,1))
corrplot( cor( Corr_A ), 
          order = "original" ,  
          tl.col = 'blue', 
          tl.cex = .75 ,
          addCoefasPercent = TRUE , 
          title = "Correlation Between Input Variables" )



###-----------------------------------------------------------------------------------------------------------------------



#### Partition data for training, method = Cross-validation 
######## ######## ######## ######## ######## ######## ######## ########

set.seed(02252019)

####  Stratified sampling
TrainingDataIndex <- createDataPartition( Galbreath.ds$Chukar.Occurrence , 
                                          p=0.80 , 
                                          list = FALSE )



####  Create Training Data 
trainingData <- Galbreath.ds[ TrainingDataIndex, ]


testData <- Galbreath.ds[ -TrainingDataIndex, ]


TrainingParameters <- trainControl( method = "repeatedcv", 
                                    number = 10 , 
                                    repeats = 10 ,
                                    classProbs =  TRUE,
                                    savePredictions = TRUE)
#TrainingParameters





# Remove labels for models 
train.Data <- data.frame( trainingData[ 2:23 ] )
train.Data


test.Data <- data.frame( testData[ 2:23] )
test.Data
dim(test.Data)



###-----------------------------------------------------------------------------------------------------------------------



# Builid all models , through caretEnsemble
######## ######## ######## ######## ######## ######## ######## ########

model.Ensemble.list <- caretList(Chukar.Occurrence ~ . , 
                                 data = train.Data ,
                                 trControl = TrainingParameters ,
                                 preProcess = c( "scale", "center" ),
                                 methodList=c("glm", "svmPoly", "rf", "nnet", "gbm"))

model.Ensemble.list 




# View Correlation of trained models results to see if good for ensembling 
model.corr<- modelCor(resamples(model.Ensemble.list))
model.corr

#Plot the correlation matrix for model.corr
par(mar=c(2,2,2,2))
corrplot( model.corr, 
          order = "original" ,
          method = "number",
          type = "lower",
          tl.col = 'blue', 
          tl.cex = 1 ,
          addCoefasPercent = TRUE , 
          title = "Correlation of model list " )




# Run the ensemble model 
model.Ensemble <- caretEnsemble(model.Ensemble.list, 
                                metric="Accuracy",
                                trControl = TrainingParameters ,
                                preProcess = c( "scale", "center" ))
        



###-----------------------------------------------------------------------------------------------------------------------



#### Testing Set for indiviual models for P/A  
######## ######## ######## ######## ######## ######## ######## ########

# Define each model in ensemble for simplicity
GLM <- model.Ensemble$models$glm
SVM <- model.Ensemble$models$svmPoly
RF <- model.Ensemble$models$rf
ANN <- model.Ensemble$models$nnet
GBM <- model.Ensemble$models$gbm


# Run the test.Data through each model  
test.GLM <- predict( GLM  , newdata = test.Data )
test.SVM <- predict( SVM  , newdata = test.Data )
test.RF <-  predict( RF  , newdata = test.Data )
test.ANN <- predict( ANN  , newdata = test.Data )
test.GBM <- predict( GBM  , newdata = test.Data )


# Confusion Matrices to see perfromance measurements
cm.GLM <- confusionMatrix( table( test.GLM , test.Data$Chukar.Occurrence ) )
cm.SVM <- confusionMatrix( table( test.SVM , test.Data$Chukar.Occurrence ) )
cm.RF <- confusionMatrix( table( test.RF , test.Data$Chukar.Occurrence ) )
cm.ANN <- confusionMatrix( table( test.ANN , test.Data$Chukar.Occurrence ) )
cm.GBM <- confusionMatrix( table( test.GBM , test.Data$Chukar.Occurrence ) )


# View cofusion matrices
cm.GLM 
cm.SVM 
cm.RF 
cm.ANN 
cm.GBM 



# Testing Set for Ensemble  for P/A
 

# Run the test.Data through each model
model.test <- predict( model.Ensemble  , newdata = test.Data )
model.test 

#### Confusion Matrix
cm.model.test <- confusionMatrix( table( model.test , test.Data$Chukar.Occurrence ) )
cm.model.test




# Calculate AUC for all six models and compare  
library("caTools")
test.probs <- lapply(model.Ensemble.list, predict, newdata=test.Data, type="prob")
test.probs <- lapply(test.probs, function(x) x[,"P"])
test.probs <- data.frame(test.probs)
ens.probs <- predict(model.Ensemble, newdata=test.Data, type="prob")
test.probs$ensemble <- ens.probs
par(mar=c(8,8,8,8))
caTools::colAUC(test.probs, test.Data$Chukar.Occurrence, plotROC=TRUE) # creates ROC curves plots

test.probs




###-----------------------------------------------------------------------------------------------------------------------



## Validation of OR for individual models
######## ######## ######## ######## ######## ######## ######## ########

# Creates data.frames with P/A
Val.GLM <-data.frame(predict( GLM, newdata = OR.ds ))
Val.SVM <-data.frame(predict( SVM, newdata = OR.ds ))
Val.RF <-data.frame(predict( RF , newdata = OR.ds ))
Val.ANN <-data.frame(predict( ANN, newdata = OR.ds ))
Val.GBM <-data.frame(predict( GBM, newdata = OR.ds ))


# View data frames
Val.GLM
Val.SVM
Val.RF
Val.ANN
Val.GBM



## Validation of OR for individual models Ensemble  
######## ######## ######## ######## ######## ######## ######## ########


# Run the test.Data through each model
Val.Ensemble <- data.frame(predict( model.Ensemble  , newdata = OR.ds  ))
Val.Ensemble


# Pull from orginal WA.Data for comparative purposes
True <- data.frame(OR.chukar[2], OR.Data[22])

# Create single data.frame with all OR.ds P/A predictions
Complete.results <- cbind.data.frame(True, Val.GLM, Val.SVM, Val.RF, Val.ANN, Val.GBM, Val.Ensemble )
names(Complete.results) <- c( "Quadname", "True", "GLM", "SVM", "RF", "ANN", "GBM", "Ensemble" )
Complete.results

write.csv(Complete.results,  "Complete resultsVal")




#Produces data.frame for OR probabilities 
model.val <- lapply(model.Ensemble.list, predict, newdata=OR.ds, type="prob")
model.val <- lapply(model.val, function(x) x[,"P"])
model.val <- data.frame(model.val)
ens.val <- predict(model.Ensemble, newdata=OR.ds, type="prob")
model.val$ensemble <- ens.val
model.val



###-----------------------------------------------------------------------------------------------------------------------



########  Data For Full-Set Washington DEMs Probabilities
######## ######## ######## ######## ######## ######## ######## ########



#Produces data.frame for OR probabilities 
WA.probs<- lapply(model.Ensemble.list, predict, newdata=WA.Data, type="prob")
WA.probs <- lapply(WA.probs, function(x) x[,"P"])
WA.probs <- data.frame(WA.probs)
WA.Ens <- predict(model.Ensemble, newdata=WA.Data, type="prob")
WA.probs$ensemble <- WA.Ens
names(WA.probs) <- c(  "GLM",  "SVM", "RF", "ANN" , "GBM", "Ensemble")
WA.probs


# Create single data.frame with all WA probabilities 
Complete.WA.Data <- cbind.data.frame(WA.Data[1], WA.probs )
names(Complete.WA.Data) <- c( "Quadname",  "GLM", "SVM", "RF", "ANN", "GBM", "Ensemble" )
Complete.WA.Data

write.csv(Complete.WA.Data,  "Complete.WA.Data")

###-----------------------------------------------------------------------------------------------------------------------
