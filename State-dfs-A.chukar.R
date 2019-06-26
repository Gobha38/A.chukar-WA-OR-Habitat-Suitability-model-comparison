#  Script used to manipulate data sets for models.
# Makes it easier to call for other work and simplfies model script



# Import packages
######## ######## ######## ######## ######## ######## ######## ########


# library(caret)  # used ot create individual models 
# library(caretEnsemble) # used to create ensemble from individual models
# library(kernlab) # used to build support vector machine 
# library(randomForest) #  used to build random forest  model 
# library(gbm) #  used to build gradient boosted tree package 
# library(nnet) # used to build neural network model 
# library(ggplot2) # graphing package 
# library(lattice) #?
# library(pROC)
# library(plyr)
# library(dplyr)


###-----------------------------------------------------------------------------------------------------------------------


# Import and manipulate data sets
######## ######## ######## ######## ######## ######## ######## ########



### WASHINGTON ###



# All Quadrangles for the Washington state 24K layer 

WA.chukar <- read.csv("A.chukar-Variables-WA-24K.csv", header = T)


# Create columns based on sum of variables
# Some data types can be grouped 

#Sum of Forest
WA.chukar$col45 <- WA.chukar$EN_FOR + WA.chukar$EB_FOR + WA.chukar$DN_FOR + WA.chukar$DB_FOR + WA.chukar$MIX_FOR # sum of forest types to create one FOR-variable
#Sum of Shrubland
WA.chukar$col46 <- WA.chukar$CL_SHB + WA.chukar$OP_SHB # sum of forest types to create one SHB-variable
# Sum of Cropland
WA.chukar$col47 <- WA.chukar$CROP + WA.chukar$NAT_CROP # sum of forest types to create one CROP-variable

#########

# Now to upload occurrence/ suitability information for each quadrangle
# created  using QGIS (Version "....") with USGS Gap Analysis data and 24K quadrangle layer
WA.OCC <- read.csv ("A.chukar-GapAnalysis-Occurrence-24K-WA.csv", header = TRUE)  


# Combine the dataframes 
WA.chukar <- merge( WA.chukar, WA.OCC, by = "name", all = TRUE )
#WA.chukar <- data.frame (WA.chukar[,1], WA.chukar[3:47], WA.chukar[49:50])

#####



WA.Original <- data.frame(WA.chukar[1], WA.chukar[3],  WA.chukar[45:46], WA.chukar[11:14], 
                          WA.chukar[16], WA.chukar[47], WA.chukar[18:26], WA.chukar[30:31], 
                          WA.chukar[37], WA.chukar[49:50] )
names(WA.Original) <- c( "name",
                         
                         "WAT","FOR","SHB","WOOD_SAV","SAV","GRA","WETL",
                         "URB","CROP","SNOW","BAR",
                         
                         "ELEV_MAX","ELEV_AVE","ELEV_MIN","SMAX", "SAVG", "ASP",
                         
                         "BIO_1", "BIO_5", "BIO_6", "BIO_12",
                         
                         "Chukar.Occurrence", "Galbreath")




# Select subset of data for training the model 
# Here we use the quadrangles from the counties specified in Galbreath 1954 
Galbreath.ds <- subset( WA.Original, Galbreath == "Y" )
Galbreath.ds <- Galbreath.ds[1:23]# remove Galbreath column for models
Galbreath.ds[ is.na( Galbreath.ds ) ] <- 0 


# Confirm Data has correct input and statistics 
sum( is.na( Galbreath.ds) )
summary( Galbreath.ds )
dim( Galbreath.ds )


# Create data set to test model performance 
test.WA.NoGal <- subset( WA.Original, Galbreath == "N" )
test.WA.NoGal <- test.WA.NoGal[1:23] # remove Galbreath column for models
test.WA.NoGal[ is.na( test.WA.NoGal ) ] <- 0 
test.WA.NoGal  


# Confirm Data has correct input and statistics 
sum( is.na( test.WA.NoGal) )
summary( test.WA.NoGal )
dim( test.WA.NoGal )


###-----------------------------------------------------------------------------------------------------------------------


###-----------------------------------------------------------------------------------------------------------------------
# OREGON VALIDATION SET
###-----------------------------------------------------------------------------------------------------------------------


# Validation Data - Select OR counties 
# We will use this to predict/define the P/A of chukar in OR based on the training on Galbreath data
OR.chukar <- read.csv("A.chukar-Variables-OR-24K.csv", header = T)


# Combined similar landcover types to simplify dataframe

#Sum of Forest
OR.chukar$col45 <- OR.chukar$EN_FOR + OR.chukar$EB_FOR + OR.chukar$DN_FOR + OR.chukar$DB_FOR + OR.chukar$MIX_FOR # sum of forest types to create one FOR-variable
#Sum of Shrubland
OR.chukar$col46 <- OR.chukar$CL_SHB + OR.chukar$OP_SHB # sum of forest types to create one SHB-variable
#Sum of Cropland 
OR.chukar$col47 <- OR.chukar$CROP + OR.chukar$NAT_CROP # sum of forest types to create one CROP-variable

#########


# Import occurrence/ suitabiliy data
OR.OCC <- read.csv ("A.chukar-GapAnalysis-Occurrence-24K-OR.csv", header = TRUE)  # created  using QGIS with USGS Gap Analysis data and 24K layer


# add occurrence data to dataframe
OR.chukar <- merge( OR.chukar, OR.OCC, by = "name", all = TRUE )



OR.Original <- data.frame(OR.chukar[1], OR.chukar[3],  OR.chukar[45:46], OR.chukar[11:14], 
                          OR.chukar[16], OR.chukar[47], OR.chukar[18:26], OR.chukar[30:31], 
                          OR.chukar[37], OR.chukar[49] )
names(OR.Original) <- c( "name",
                         
                         "WAT","FOR","SHB","WOOD_SAV","SAV","GRA","WETL",
                         "URB","CROP","SNOW","BAR",
                         
                         "ELEV_MAX","ELEV_AVE","ELEV_MIN","SMAX", "SAVG", "ASP",
                         
                         "BIO_1", "BIO_5", "BIO_6", "BIO_12",
                         
                         "Chukar.Occurrence")


#OR.Original[ is.na( OR.Original) ] <- 0
OR.Original # 2043 - need to reduce 
OR.Original <- OR.Original[1:2011,] # Rmoves last set of rows that have no data
#OR.Original<- OR.Original[-c(1919) ]

OR.Original$name[duplicated(OR.Original$name)]

OR.Original <- OR.Original %>% distinct(name, .keep_all = TRUE)
OR.Original <- na.omit(OR.Original)

summary (OR.Original)
write.csv(OR.Original, "OR.Original.csv")     # Refiguring data produces 
OR.Original <- read.csv("OR.Original.csv", header = T)
OR.Original<- OR.Original[2:24]
summary(OR.Original)

# Remove column "names" so models don't read it as a variable 
OR.ds <- OR.Original[2:22]


###-----------------------------------------------------------------------------------------------------------------------


###-----------------------------------------------------------------------------------------------------------------------
### NEW MEXICO
###-----------------------------------------------------------------------------------------------------------------------


###-----------------------------------------------------------------------------------------------------------------------


# Import NM data - raw
NM.chukar <- read.csv("A.chukar-Variables-NM-24K.csv", header = T)
NM.chukar$col44<- rep(0,nrow(NM.chukar)) # make new column 
NM.chukar <- data.frame(NM.chukar[2:4],  NM.chukar[,44], NM.chukar[5:43] )





# Combined similar landcover types to simplify dataframe
NM.chukar$col44 <- NM.chukar$EN_FOR  + NM.chukar$NM.chukar...44.+  NM.chukar$DN_FOR + NM.chukar$DB_FOR + NM.chukar$MIX_FOR # sum of forest types to create one FOR-variable
NM.chukar$col45 <- NM.chukar$CL_SHB + NM.chukar$OP_SHB # sum of forest types to create one SHB-variable
NM.chukar$col46<- NM.chukar$CROP + NM.chukar$NAT_CROP # sum of forest types to create one CROP-variable


#NM.OCC <- read.csv ("A.chukar-GapAnalysis-Occurrence-24K-NM.csv", header = TRUE) 


#NM.chukar <- merge( NM.chukar, NM.OCC, by = "name", all = TRUE )

# Organize it to match the training data.frame
NM.chukar <- data.frame(NM.chukar[1:2],  NM.chukar[44:45], NM.chukar[10:13], NM.chukar[15], NM.chukar[46], NM.chukar[17:43] )
names(NM.chukar) <- c( "name",
                       
                       "WAT","FOR","SHB","WOOD_SAV","SAV","GRA","WETL",
                       "URB","CROP","SNOW","BAR",
                       
                       "ELEV_MAX","ELEV_AVE","ELEV_MIN","SMAX", "SAVG", "ASP",
                       
                       "BIO_1", "BIO_2", "BIO_3", "BIO_4",
                       "BIO_5", "BIO_6", "BIO_7", "BIO_8","BIO_9", 
                       "BIO_10", "BIO_11", "BIO_12","BIO_13", "BIO_14", 
                       "BIO_15", "BIO_16","BIO_17", "BIO_18", "BIO_19" )
#NM.chukar


# Choose the Variables needed for model - Original run 
NM.Original <-  data.frame(  NM.chukar[1:19],  NM.chukar[23:24], NM.chukar[30] )
NM.Original[ is.na( NM.Original) ] <- 0
NM.Original # 2068 - need to reduce 
NM.Original$name[duplicated(NM.Original$name)]
NM.Original <- NM.Original %>% distinct(name, .keep_all = TRUE)

NM.OCC <- read.csv ("A.chukar-GapAnalysis-Occurrence-24K-NM.csv", header = TRUE) 
NM.OCC <-NM.OCC[2:3]


NM.Original <- merge( NM.Original, NM.OCC, by = "name", all = TRUE )


# Remove column "names" so models don't read it as a variable 
NM.ds <- NM.Original[2:22] 




