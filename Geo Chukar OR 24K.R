# Author: Austin M. Smith, M.Sc
# Title:  Chukar Habitat Model 



###################################################
### code chunk number 1: clear
###################################################


rm(list=ls(all=TRUE))


######## ########  Geospatial Packages ######## ########

# data processing

library(dplyr)
library(foreign) # for reading dbfs
library(ggplot2)
library(gridExtra) # to arrange grid plots
library(magrittr)
#library(tidyr)

# spatial
library(dismo) #map raster on Google Map
library(GISTools)
library(raster)
library(rasterVis)
library(rgdal)
library(rgeos)
library(sf)
library(spatial)

# other 
library(tmap)
library(maps)



######## ########  Landcover Raster File ######## ########
######## ########  ##########################  ######## ########


# Import LCT Raster 

LCTR <- raster("/Users/Austin/Downloads/LCType.tif", values = TRUE)  # From macbook
#LCTR


#set model crs for all layers
# Refer to this for all layers 
Model_CRS <- crs(LCTR)
#Model_CRS

# Layer currently in gardient form
#change colors to match MODIS

#LCcolors <- c("blue","seagreen3","seagreen4","springgreen2","springgreen1","forestgreen",
#              "darkolivegreen3","darkolivegreen3","darkkhaki","khaki3","orange1","mediumaquamarine",
#              "goldenrod1","red","goldenrod2","white","gray")


# Create plot to check 
#par(mar=c(1,1,1,1))
#plot(LCTR, col = LCcolors,
#     axes = FALSE, 
#     box = FALSE,
#     legend = FALSE )


##### USGS 1:24,000 Quadrangle Polygon

Quad24_OR <-readShapePoly("/Users/Austin/Downloads/map_indexes_QD24K_or_3669477_01/map_indexes\\quads24k_a_or.shp")
Quad24_OR

# Check  crs of layer
# should come up NA - Set to  Model_CRS
crs(Quad24_OR) <- Model_CRS

Quad24_OR # Check if "coord. ref." updated

### Plot 24K WA to Global Map

# Plot Files around WA
# Will look off, especially using zoom function in RStudio
# Need to fix this 

#plot(LCTR, 
#     axes = FALSE,
#     box = FALSE,
#     col = LCcolors, xlim=c(-124.75, -116.875), ylim=c(45.5, 49),
#     legend = FALSE)
#
#plot(Quad24_OR, add =TRUE)



######## ########  Collect Landcover per Quad  ######## ########
######## ########  ##########################  ######## ########

### Counts pixels of all variables for each quadrangle.
### Should create a list organized 

e1 <- extract(LCTR, Quad24_OR, method ='simple')

class(e1)  # a list
length(e1) # 1921

### Function to tabulate land use by region and return 
### a data.frame

tabFunc<-function(indx, extracted, region, regname) {
  dat<-as.data.frame(table(extracted[[indx]]))
  dat$name<-region[[regname]][[indx]]
  return(dat)
}
###


#Apply Function 
tabs<-lapply(seq(e1), tabFunc, e1, Quad24_OR, "QUADNAME")
tabs<-do.call("rbind",tabs )
tabs

#Rename Tabs
tabs$Var1<-factor(tabs$Var1, levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                  labels = c( "WAT","EN_FOR","EB_FOR","DN_FOR","DB_FOR","MIX_FOR",
                              "CL_SHB","OP_SHB","WOOD_SAV","SAV","GRA","WETL",
                              "CROP","URB","NAT_CROP","SNOW","BAR"))


# Organize data into sheet (like excel)
library(tidyr)
Sheet1OR<- data.frame(tabs%>%
                      group_by(name) %>% # group by region
                      mutate(totcells=sum(Freq), # how many cells overall
                             #percent.area=round(1*Freq/1,2)) %>%
                             percent.area=round(100*Freq/totcells,2)) %>%
#                      ungroup() %>%
                      #percent.area=round(100*Freq/totcells,2)) %>%#cells by landuse/total cells
                      dplyr::select(-c(Freq, totcells)) %>% # there is a select func in raster so need to specify
                      mutate(id = 1:n()) %>%
                      ungroup() %>%
                      spread(key=Var1, value=percent.area, fill=0)
)# make wide format

#Sheet1OR # check sheet


### 
wat <- aggregate(WAT~name,data=Sheet1OR,FUN=sum)
enfor <- aggregate(EN_FOR~name,data=Sheet1OR,FUN=sum)
ebfor <- aggregate(EB_FOR~name,data=Sheet1OR,FUN=sum)
dnfor <- aggregate(DN_FOR~name,data=Sheet1OR,FUN=sum)
dbfor <- aggregate(DB_FOR~name,data=Sheet1OR,FUN=sum)
mixfor <- aggregate(MIX_FOR~name,data=Sheet1OR,FUN=sum)

clshb <- aggregate(CL_SHB~name,data=Sheet1OR,FUN=sum)
opshb <- aggregate(OP_SHB~name,data=Sheet1OR,FUN=sum)
woodsav <- aggregate(WOOD_SAV~name,data=Sheet1OR,FUN=sum)
sav <- aggregate(SAV~name,data=Sheet1OR,FUN=sum)
gra <- aggregate(GRA~name,data=Sheet1OR,FUN=sum)

wetl <- aggregate(WETL~name,data=Sheet1OR,FUN=sum)
crop <- aggregate(CROP~name,data=Sheet1OR,FUN=sum)
urb <- aggregate(URB~name,data=Sheet1OR,FUN=sum)
natcrop <- aggregate(NAT_CROP~name,data=Sheet1OR,FUN=sum)
snow <- aggregate(SNOW~name,data=Sheet1OR,FUN=sum)
bar <- aggregate(BAR~name,data=Sheet1OR,FUN=sum)
  
Sheet1 <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                 list( wat, enfor, ebfor, dnfor, dbfor, mixfor,
                       clshb, opshb, woodsav, sav, gra,
                       wetl, crop, urb, natcrop, snow, bar) )



Sheet1


detach(package:tidyr, unload=TRUE)



######## ########  Elevation/ Slope  per Quad  ######## ########
######## ########  ##########################  ######## ########

### Here we use the package 'elevatr'.  
### package information can be found at:
### https://github.com/jhollist/elevatr

library(elevatr)

elevation <- get_elev_raster(Quad24_OR, z = 9)  #extracts elevation data per pixel per quadrangel 
par(mar=c(1,1,1,1))
plot(elevation,
     axes = FALSE,
     box = FALSE,
     legend = FALSE)
#plot(elevation, xlim=c(-124.75, -116.875), ylim=c(45.5, 49))  # plots elevation data 
plot(Quad24_OR, add= TRUE) # plot WA layer to see how it overlaps

### Extract information 

e2 <- extract(elevation, Quad24_OR, method ='simple') # extracts elevation data per pixel per quadrangel 
e2
#lenge2<- length(e2)
output <- matrix(unlist(e2), ncol = lenge2, byrow = FALSE) #

e2max <- extract(elevation, Quad24_OR, method ='simple', fun = max)
e2mean <- extract(elevation, Quad24_OR, method ='simple', fun = mean)
e2min <- extract(elevation, Quad24_OR, method ='simple', fun = min)

e2max
e2mean
e2min

class(e2)  # a list
## [1] "list"
length(e2) 

### Create Tables of collected data ###
#lappl

#Elevation Maximun per quad
tabs_elevMax<-lapply(seq(e2max), tabFunc, e2max, Quad24_OR, "QUADNAME")
tabs_elevMax<-do.call("rbind", tabs_elevMax )
tabs_elevMax

#Elevation Average per quad
tabs_elevMean<-lapply(seq(e2mean), tabFunc, e2mean, Quad24_OR, "QUADNAME")
tabs_elevMean<-do.call("rbind", tabs_elevMean )
tabs_elevMean

#Elevation Minimum per quad
tabs_elevMin<-lapply(seq(e2min), tabFunc, e2min, Quad24_OR, "QUADNAME")
tabs_elevMin<-do.call("rbind", tabs_elevMin )
tabs_elevMin


#Combine and Reduce to necessary information
ELEV <- merge( merge( tabs_elevMax, tabs_elevMean, by = "name", all = TRUE ), tabs_elevMin, by = "name", all = TRUE )
#ELEV
names(ELEV) <- c("name", "ELEV_MAX","freq1","ELEV_AVE","freq2","ELEV_MIN","freq3" )
ELEV <- ELEV[c(1,2,4,6)]
ELEV


Datacb1 <- merge( Sheet1, ELEV, by = "name", all = TRUE )
Datacb1
#write.csv(Datacb1, "Data sample 1")


### Next, slope and aspect are calculated.
### Both are ecologicially signifiant variables to Chukar studies
### These are claculated thru the 'raster' package 


asp <- terrain(elevation, opt = "aspect", unit = "degrees", df = F)
slo <- terrain(elevation, opt = "slope", unit = "degrees", df = F)

SMAX <- extract( slo,  Quad24_OR, method ='simple', fun = max )
SAVG <- extract( slo,  Quad24_OR, method ='simple', fun = mean )
ASP <- extract(asp,  Quad24_OR,  method ='simple', fun = mean )


SMAX 
SAVG
ASP


SLOPE <- cbind.data.frame(Quad24_OR$QUADNAME, SMAX, SAVG, ASP)
names(SLOPE) <- c("name", "SMAX", "SAVG", "ASP" )
SLOPE


Datacb2 <- merge( Datacb1, SLOPE, by = "name", all = TRUE )
Datacb2
##########################################################################



######## ########  Bioclim: Climate Data  ######## ########
######## ########  #####################  ######## ########


# Variables most signficant to Chukar establishment 
Bio1 <- raster('./wc2/wc2.0_bio_10m_01.tif') # Annual Mean Temperature
Bio5 <- raster('./wc2/wc2.0_bio_10m_05.tif') # Max Temperature of Warmest Month
Bio6 <- raster('./wc2/wc2.0_bio_10m_06.tif') # Min Temperature of Coldest Month
Bio12 <- raster('./wc2/wc2.0_bio_10m_12.tif')# Annual Precipitation

# additional variables to consider 
Bio2 <- raster('./wc2/wc2.0_bio_10m_02.tif') # Mean Diurnal Range (Mean of monthly (max temp - min temp))
Bio3 <- raster('./wc2/wc2.0_bio_10m_03.tif') # Isothermality (BIO2/BIO7) (* 100)
Bio4 <- raster('./wc2/wc2.0_bio_10m_04.tif') # Temperature Seasonality (standard deviation *100)
Bio7 <- raster('./wc2/wc2.0_bio_10m_07.tif') # Temperature Annual Range (BIO5-BIO6)
Bio8 <- raster('./wc2/wc2.0_bio_10m_08.tif') #  Mean Temperature of Wettest Quarter
Bio9 <- raster('./wc2/wc2.0_bio_10m_09.tif') # Mean Temperature of Driest Quarter
Bio10 <- raster('./wc2/wc2.0_bio_10m_10.tif') # Mean Temperature of Warmest Quarter
Bio11 <- raster('./wc2/wc2.0_bio_10m_11.tif') # Mean Temperature of Coldest Quarter
Bio13 <- raster('./wc2/wc2.0_bio_10m_13.tif') # Precipitation of Wettest Month
Bio14 <- raster('./wc2/wc2.0_bio_10m_14.tif') # Precipitation of Driest Month
Bio15 <- raster('./wc2/wc2.0_bio_10m_15.tif')# Precipitation Seasonality (Coefficient of Variation)
Bio16 <- raster('./wc2/wc2.0_bio_10m_16.tif') # Precipitation of Wettest Quarter
Bio17 <- raster('./wc2/wc2.0_bio_10m_17.tif') # Precipitation of Driest Quarter
Bio18 <- raster('./wc2/wc2.0_bio_10m_18.tif') # Precipitation of Warmest Quarter
Bio19 <- raster('./wc2/wc2.0_bio_10m_19.tif') # Precipitation of Coldest Quarter


# Extract/ Calculate bio layers measurements for quad-layer 

BIO_1 <- extract(Bio1, Quad24_OR, method ='simple', fun = mean)
BIO_2 <- extract(Bio2, Quad24_OR, method ='simple', fun = mean)
BIO_3 <- extract(Bio3, Quad24_OR, method ='simple', fun = mean)
BIO_4 <- extract(Bio4, Quad24_OR, method ='simple', fun = mean)
BIO_5 <- extract(Bio5, Quad24_OR, method ='simple', fun = max) # Using max generated supremum (LUB)
BIO_6 <- extract(Bio6, Quad24_OR, method ='simple', fun = min) # Using max generated infimum  (GLB)
BIO_7 <- extract(Bio7, Quad24_OR, method ='simple', fun = mean)
BIO_8 <- extract(Bio8, Quad24_OR, method ='simple', fun = mean)
BIO_9 <- extract(Bio9, Quad24_OR, method ='simple', fun = mean)
BIO_10 <- extract(Bio10, Quad24_OR, method ='simple', fun = mean)
BIO_11 <- extract(Bio11, Quad24_OR, method ='simple', fun = mean)
BIO_12 <- extract(Bio12, Quad24_OR, method ='simple', fun = mean)
BIO_13 <- extract(Bio13, Quad24_OR, method ='simple', fun = mean)
BIO_14 <- extract(Bio14, Quad24_OR, method ='simple', fun = mean)
BIO_15 <- extract(Bio15, Quad24_OR, method ='simple', fun = mean)
BIO_16 <- extract(Bio16, Quad24_OR, method ='simple', fun = mean)
BIO_17 <- extract(Bio17, Quad24_OR, method ='simple', fun = mean)
BIO_18 <- extract(Bio18, Quad24_OR, method ='simple', fun = mean)
BIO_19 <- extract(Bio19, Quad24_OR, method ='simple', fun = mean)


library(tidyr)

# Combine  climate data to add to landcover data 
CLIM <- cbind.data.frame(Quad24_OR$QUADNAME, BIO_1, BIO_2, BIO_3, BIO_4, BIO_5, 
                         BIO_6, BIO_7, BIO_8,BIO_9, BIO_10, BIO_11, BIO_12, BIO_13, 
                         BIO_14, BIO_15, BIO_16, BIO_17, BIO_18, BIO_19 )
names(CLIM) <- c("name", "BIO_1", "BIO_2", "BIO_3", "BIO_4",
                 "BIO_5", "BIO_6", "BIO_7", "BIO_8","BIO_9", 
                 "BIO_10", "BIO_11", "BIO_12","BIO_13", "BIO_14", 
                 "BIO_15", "BIO_16","BIO_17", "BIO_18", "BIO_19" )
CLIM


### #### #### #### #### ###
### Thesis variables
### Should all variables be average?
### May resort to these for final model 

#TMAX <- extract(Bio5, Quad24_WA, method ='simple', fun = max)
#TAVG <- extract(Bio1, Quad24_WA, method ='simple', fun = mean)
#TMIN <- extract(Bio6, Quad24_WA, method ='simple', fun = min)
#PREC <- extract(Bio12,Quad24_WA, method ='simple', fun = mean)


# Check file information  
#TMAX
#TAVG
#TMIN
#PREC

# Combine  climate data to add to landcover data 
#CLIM <- cbind.data.frame(Quad24_WA$QUADNAME, TMAX, TAVG, TMIN, PREC)
#names(CLIM) <- c("name", "TMAX", "TAVG", "TMIN", "PREC" )
#CLIM

# Add CLIM data to Sheet1 (Landcover data)
Datacb3 <- merge( Datacb2, CLIM, by = "name", all = TRUE )
Datacb3

write.csv(Datacb3, "A.chukar-Variables-OR-24K")

detach(package:tidyr, unload=TRUE)
##########################################################################

#library(corrplot)

#A <- as.matrix( CLIM[ 2:20 ] ) 
#A <- as.matrix( Datacb3[ 2:43 ] )
#A <- as.matrix( Datacb3 )
#A <- mapply(A, FUN=as.numeric)
#A<- matrix(data=A, ncol=42, nrow=1421)


#typeof(A)

#chukar P/A 

#Corr_A<- cor(A, use="complete.obs")

#corrplot( cor( Corr_A ), 
#          order = "original" ,  
#          tl.col = 'blue', 
#          tl.cex = .75 ,
#          addCoefasPercent = TRUE , 
#          title = "Correlation Between Input Variables" 
#)

#pca()

#WAds <- merge( Datacb3, AcOCC, by = "name", all = TRUE )
#Datacb2


##########################################################################

save.image('Geospatial-A.chukar-24K-OR.rda')

