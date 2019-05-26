# Creator: Austin M. Smith (amsmith11@usf.edu)

# Article to ESA journal:  Ecological Applications 

# Title: "Using machine learning algorithms to evaluate habitat quality for Chukar Partridge 
#         management in Washington state"

#Authors:  Austin M. Smith, Wendell P. Cropper Jr., Michael Moutlon

# Discription: Analysis of site-level factors [in an attempt] to model habiat suitability of 
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



# Import and manipulate data sets
######## ######## ######## ######## ######## ######## ######## ########



# All Quadrangles for the Washington state 24K layer 

WA.chukar <- read.csv("A.chukar-Variables-WA-24K.csv", header = T)



# Create columns based on sum of variables
# Some data types can be grouped 

WA.chukar$col45 <- WA.chukar$EN_FOR + WA.chukar$EB_FOR + WA.chukar$DN_FOR + WA.chukar$DB_FOR + WA.chukar$MIX_FOR # sum of forest types to create one FOR-variable
WA.chukar$col46 <- WA.chukar$CL_SHB + WA.chukar$OP_SHB # sum of forest types to create one SHB-variable
WA.chukar$col47 <- WA.chukar$CROP + WA.chukar$NAT_CROP # sum of forest types to create one CROP-variable




###  The following is to organize the columns so variable groups are clustered (eg. landcover types are together)
### The order does not matter for the models - just personal preference 

WA.chukar <- data.frame(WA.chukar[2:3],  WA.chukar[45:46], WA.chukar[11:14], 
                        WA.chukar[16], WA.chukar[47], WA.chukar[18:44] )

names(WA.chukar) <- c( "name",
                      
                      "WAT","FOR","SHB","WOOD_SAV","SAV","GRA","WETL",
                      "URB","CROP","SNOW","BAR",
                  
                      "ELEV_MAX","ELEV_AVE","ELEV_MIN","SMAX", "SAVG", "ASP",
                  
                      "BIO_1", "BIO_2", "BIO_3", "BIO_4",
                      "BIO_5", "BIO_6", "BIO_7", "BIO_8","BIO_9", 
                      "BIO_10", "BIO_11", "BIO_12","BIO_13", "BIO_14", 
                      "BIO_15", "BIO_16","BIO_17", "BIO_18", "BIO_19" )


WA.chukar


# Now to upload occurrence/ suitability information for each quadrangle
# created  using QGIS (Version "....") with USGS Gap Analysis data and 24K quadrangle layer
WA.OCC <- read.csv ("A.chukar-GapAnalysis-Occurrence-24K-WA.csv", header = TRUE)  


# Combine the dataframes 
WA.chukar <- merge( WA.chukar, WA.OCC, by = "name", all = TRUE )
WA.chukar <- data.frame ( WA.chukar[1:37], WA.chukar[39:41])


#  Choose the variables needed for model
WA.Data <-  data.frame(  WA.chukar[1:19],  WA.chukar[23:24], WA.chukar[30], WA.chukar[38:39]  )
WA.Data[ is.na( WA.Data) ] <- 0
WA.Data



#### Review Data
sum( is.na( WA.Data ) ) # Check for NAs



# Select subset of data for training the model 
# Here we use the quadrangles from the counties specified in Galbreath 1954 


Galbreath.ds <- subset( WA.Data, Galbreath == "Y" )
Galbreath.ds <- Galbreath.ds[1:23]
Galbreath.ds[ is.na( Galbreath.ds ) ] <- 0 
Galbreath.ds





# Confirm Data has correct input and statistics 
sum( is.na( Galbreath.ds) )
summary( Galbreath.ds )
dim( Galbreath.ds )




# Validation Data - Select OR counties 
# We will use this to predict/define the P/A of chukar in OR based on the training on Galbreath data

OR.chukar <- read.csv("A.chukar-Variables-OR-24K.csv", header = T)

# Combined similar landcover types to simplify dataframe
OR.chukar$col45 <- OR.chukar$EN_FOR + OR.chukar$EB_FOR + OR.chukar$DN_FOR + OR.chukar$DB_FOR + OR.chukar$MIX_FOR # sum of forest types to create one FOR-variable
OR.chukar$col46 <- OR.chukar$CL_SHB + OR.chukar$OP_SHB # sum of forest types to create one SHB-variable
OR.chukar$col47 <- OR.chukar$CROP + OR.chukar$NAT_CROP # sum of forest types to create one CROP-variable


# Organize it to match the training data.frame
OR.chukar <- data.frame(OR.chukar[2:3],  OR.chukar[45:46], OR.chukar[11:14], OR.chukar[16], OR.chukar[47], OR.chukar[18:44] )

names(OR.chukar) <- c( "name",
                      
                      "WAT","FOR","SHB","WOOD_SAV","SAV","GRA","WETL",
                      "URB","CROP","SNOW","BAR",
                      
                      "ELEV_MAX","ELEV_AVE","ELEV_MIN","SMAX", "SAVG", "ASP",
                      
                      "BIO_1", "BIO_2", "BIO_3", "BIO_4",
                      "BIO_5", "BIO_6", "BIO_7", "BIO_8","BIO_9", 
                      "BIO_10", "BIO_11", "BIO_12","BIO_13", "BIO_14", 
                      "BIO_15", "BIO_16","BIO_17", "BIO_18", "BIO_19" )


#OR.chukar


# Import occurrence/ suitabiliy data
OR.OCC <- read.csv ("A.chukar-GapAnalysis-Occurrence-24K-OR.csv", header = TRUE)  # created  using QGIS with USGS Gap Analysis data and 24K layer

# add occurrence data to dataframe
OR.chukar <- merge( OR.chukar, OR.OCC, by = "name", all = TRUE )



# Choose the Variables needed for model

OR.Data <-  data.frame(  OR.chukar[1:19],  OR.chukar[23:24], OR.chukar[30], OR.chukar[39]  )
OR.Data[ is.na( OR.Data) ] <- 0
OR.Data # 2043 - need to reduce 

OR.Data <- OR.Data[1:2011,] # Rmoves last set of rows that have no data


#Check data 
#dim( OR.Data )
#summary( OR.Data )
#sum( is.na( OR.Data) )


U <- unique(OR.Data$name)




OR.Data<- OR.Datau[-c(1919) ]

OR.Data$name[duplicated(OR.Data$name)]

OR.Data <- OR.Data %>% distinct(name, .keep_all = TRUE)


# Remove column "names" so models don't read it as a variable 
OR.ds <- OR.Data[2:22]  



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
          order = "hclust" ,  
          tl.col = 'blue', 
          tl.cex = .75 ,
          addCoefasPercent = TRUE , 
          title = "Correlation Between Input Variables" )



###-----------------------------------------------------------------------------------------------------------------------



#### Partition data for training, method = Cross-validation 
######## ######## ######## ######## ######## ######## ######## ########

#set.seed(02252019)
set.seed(05092019)

####  Stratified sampling
TrainingDataIndex <- createDataPartition( Galbreath.ds$Chukar.Occurrence , 
                                          p=0.80 , 
                                          list = FALSE )



# Create training  and  testing partitions

trainingData <- Galbreath.ds[ TrainingDataIndex, ]

testData <- Galbreath.ds[ -TrainingDataIndex, ]


# set the training parameters for model learning
TrainingParameters <- trainControl( method = "repeatedcv",  # cross vailidation
                                    number = 10 ,           # number of folds/
                                    repeats = 10 ,          # repeat procedure 10 times
                                    classProbs =  TRUE,     # outputs for choropleth
                                    savePredictions = TRUE) # needed to  calcuate AUC
#TrainingParameters





# Remove column "names" so models don't read it as a variable

train.Data <- data.frame( trainingData[ 2:23 ] )
#train.Data

test.Data <- data.frame( testData[ 2:23] )
#test.Data
#dim(test.Data)



###-----------------------------------------------------------------------------------------------------------------------



# Builid all models through caretEnsemble
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
        


summary(model.Ensemble)
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

#write.csv(Complete.results,  "Complete-OR-Val-Data.csv")




#Produces data.frame for OR probabilities 
model.val <- lapply(model.Ensemble.list, predict, newdata=OR.ds, type="prob")
model.val <- lapply(model.val, function(x) x[,"P"])
model.val <- data.frame(model.val)
ens.val <- predict(model.Ensemble, newdata=OR.ds, type="prob")
model.val$ensemble <- ens.val
model.val

OrVal <- cbind.data.frame(OR.Data[1], model.val)
names(OrVal) <- c( "Quadname",  "GLM", "SVM", "RF", "ANN", "GBM", "Ensemble" )
write.csv(OrVal,  "Complete-OR-Val-Data.csv")

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

write.csv(Complete.WA.Data,  "Complete-WA-Val-Data.csv")

###-----------------------------------------------------------------------------------------------------------------------
save.image('A.chukar-Ensemble-HS.Model.rda')
