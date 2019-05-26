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
x <- read.csv("Complete-WA-Val-Data.csv")

# round the values 
#xround<- round(x[3:8],2) #the "-1" excludes column 1
round(x[3:8],2) 

# add 
x<- data.frame(x[1:2], xround)

summary(x)

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
x2 <- read.csv("Complete-OR-Val-Data.csv")

# round the values 
#xround<- round(x[3:8],2) #the "-1" excludes column 1
#yround <- round(y[3:8],2) 

# add 
x22 <- data.frame(x2)

summary(y)

joined_OR <- merge(or, x2, by.x="QUADNAME", by.y="Quadname")



joined_OR$Ensemble















library(colorRamps)
#shades3 <- auto.shading(joined$Ensemble, n = 11,  cols =  colorRamps::green2red)
shades <- shading(c(.1, .20,.30,.40, .50, .60, .70,.80,.90,1.00), 
                   cols = rev(brewer.pal(11, "RdYlBu")))


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
choro.OR.Ensemble <- choropleth(joined_OR, joined_OR$Ensemble, shades, add = T)




oregon.union <- unionSpatialPolygons(joined_WA, joined_OR)




par(mar=c(0.5,0.5,0.5,0.5))

par(mar=c(8,8,8,8))






library(leaflet)
