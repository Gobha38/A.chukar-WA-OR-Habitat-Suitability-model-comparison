library(sf)
library(rgdal)
library(maptools)
library(GISTools)

library(rgdal)
library(ggplot2)
library(dplyr)


library(ggplot2)
library(scales)
library(ggmap)
library(viridis)


### Upload WA shapefile
wa <- readShapePoly( "./map_indexes_QD24K_wa_3666222_01/map_indexes\\quads24k_a_wa.shp")
wa
 

#read in data from spreadsheet
#x <- read.csv("Complete-WA-Val-Data.csv")
x <- read.csv("Complete-WA-Val-Data-FULL.csv")

x <- read.csv("Complete-WA-Val-Data-BioClim.csv")





# round the values 
#xround<- round(x[3:8],2) #the "-1" excludes column 1
#round(x[3:8],2) 

# add 
x<- data.frame(x[1:2], xround)








summary(x)

.
joined_WA <- merge(wa, x, by.x="QUADNAME", by.y="Quadname")

summary(joined)

plot(joined$Ensemble)


#US3@data$id <- rownames(US3@data)
#joined@data$RF <- rownames(joined@data)

#wa.ff <- fortify(joined, region = "QUADNAME")
#summary(wa.ff)

#join <- merge(wa, x2, by.x="QUADNAME", by.y="Quadname")




l <- as.data.frame( or$QUADNAME)

or <- readShapePoly( "./map_indexes_QD24K_or_3667989_01/map_indexes\\quads24k_a_or.shp")
plot(or, add = T)
unique(or$QUADNAME)
or$QUADNAME[duplicated(or$QUADNAME)]

#read in data from spreadsheet

x2 <- read.csv("Complete-WA-Val.Percent-Data-Clump_All_BIO-5X1.csv")
model.val


# round the values 
#xround<- round(x[3:8],2) #the "-1" excludes column 1
#yround <- round(y[3:8],2) 

# add 
x22 <- data.frame(x2)

summary(y)

joined_OR <- merge(or,Tdf, by.x="QUADNAME", by.y="Quadname")



joined_OR$Ensemble









nm <- readShapePoly( "map_indexes_QD24K_nm_3714544_01/map_indexes\\quads24k_a_nm.shp")
plot(nm, add = T)
unique(or$QUADNAME)
or$QUADNAME[duplicated(or$QUADNAME)]

#read in data from spreadsheet

x3 <- read.csv("Complete-NM-Val.Prob-Data-Original-10X10.csv")

x3 <- NMNMv

joined_NM <- merge(nm, x3, by.x="QUADNAME", by.y="Quadname")









par(mar=c(1,1,1,1))



library(colorRamps)
#shades3 <- auto.shading(joined$Ensemble, n = 11,  cols =  colorRamps::green2red)
shades <- shading(c(.1, .20,.30,.40, .50, .60, .70,.80,.90,1.00), 
                   cols = rev(brewer.pal(11, "RdYlBu")))


shades <- shading(c(.20,.40, .60, .80,1.00), 
                  cols = rev(brewer.pal(6, "RdYlBu")))

choro.WA.GLM <- choropleth(joined_WA, joined_WA$GLM, shades)
choro.WA.SVM <- choropleth(joined_WA, joined_WA$SVM, shades)
choro.WA.RF <- choropleth(joined_WA, joined_WA$RF, shades)
choro.WA.ANN <- choropleth(joined_WA, joined_WA$ANN, shades)
choro.WA.GBM <- choropleth(joined_WA, joined_WA$GBM, shades)
choro.WA.Ensemble <- choropleth(joined_WA, joined_WA$Ensemble, shades)




choro.OR.GLM <- choropleth(joined_OR, joined_OR$GLM, shades)
choro.OR.SVM <- choropleth(joined_OR, joined_OR$SVM, shades)
choro.OR.RF <- choropleth(joined_OR, joined_OR$RF, shades)
choro.OR.ANN <- choropleth(joined_OR, joined_OR$ANN, shades)
choro.OR.GBM <- choropleth(joined_OR, joined_OR$GBM, shades)
choro.OR.Ensemble <- choropleth(joined_OR, joined_OR$Ensemble, shades)


par(mfrow=c(3,2))
choro.NM.GLM <- choropleth(joined_NM, joined_NM$GLM, shades)
choro.NM.SVM <- choropleth(joined_NM, joined_NM$SVM, shades)
choro.NM.RF <- choropleth(joined_NM, joined_NM$RF, shades)
choro.NM.ANN <- choropleth(joined_NM, joined_NM$ANN, shades)
choro.NM.GBM <- choropleth(joined_NM, joined_NM$GBM, shades)
choro.NM.Ensemble <- choropleth(joined_NM, joined_NM$Ensemble, shades)



#Produces data.frame for WA probabilities
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






###-----------------------------------------------------------------------------------------------------------------------


###-----------------------------------------------------------------------------------------------------------------------
### NEW MEXICO
###-----------------------------------------------------------------------------------------------------------------------


###-----------------------------------------------------------------------------------------------------------------------


## Validation of NM for individual models
######## ######## ######## ######## ######## ######## ######## ########


# # Creates data.frames with P/A
# NMv.GLM <-data.frame(predict( GLM, newdata = NM.ds, type="prob" ))
# NMv.SVM <-data.frame(predict( SVM, newdata = NM.ds, type="prob" ))
# NMv.RF <-data.frame(predict( RF , newdata = NM.ds, type="prob" ))
# NMv.ANN <-data.frame(predict( ANN, newdata = NM.ds, type="prob" ))
# NMv.GBM <-data.frame(predict( GBM, newdata = NM.ds, type="prob" ))
# 
# 
# # View data frames
# NMv.GLM
# NMv.SVM
# NMv.RF
# NMv.ANN
# NMv.GBM


## NM validation of NM for individual models Ensemble  
######## ######## ######## ######## ######## ######## ######## ########


# Run the test.Data through each model
# NMv.Ensemble <- data.frame(predict( model.Ensemble  , newdata = NM.ds, type="prob"  ))
# NMv.Ensemble


# Pull from orginal WA.Data for comparative purposes
# NM_Quadname <- data.frame(NM.Data[,1] )

# Create single data.frame with all NM.ds P/A predictions
Complete.results.NM <- cbind.data.frame(NM_Quadname, NMv.GLM, NMv.SVM, NMv.RF, NMv.ANN, NMv.GBM, NMv.Ensemble )
names(Complete.results.NM) <- c( "Quadname",  "GLM", "SVM", "RF", "ANN", "GBM", "Ensemble" )
Complete.results.NM


#Produces data.frame for NM probabilities 
model.NMv <- lapply(model.Ensemble.list, predict, newdata=NM.ds, type="prob")
model.NMv <- lapply(model.NMv, function(x) x[,"P"])
model.NMv <- data.frame(model.NMv)
ens.NMv <- predict(model.Ensemble, newdata=NM.ds, type="prob")
model.NMv$ensemble <- 1- ens.NMv
par(mar=c(1,1,1,1))
caTools::colAUC(model.NMv, NM.Original$Chukar.Occurrence, plotROC=TRUE) # creates ROC curves plots


model.NMv

NMNMv <- cbind.data.frame(NM.Original$name, model.NMv)
names(NMNMv) <- c( "Quadname",  "GLM", "SVM", "RF", "ANN", "GBM", "Ensemble" )

