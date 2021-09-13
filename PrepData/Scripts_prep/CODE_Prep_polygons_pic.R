
# Prepping polygons to do whole state in Pastures tool
#  adds PIC label and centroids needed for leaflet
# 20 July 2021

library(ggplot2)
library(reshape2)
library(rgdal)
library(raster)

# NEED TO CHOSE PGR AND FOO HERE, THEN Rest is pretty GENERIC.  

#  set up for PGR
startDir <- "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_polygons"
setwd(startDir)


longTbl <- read.csv("./AgRegion_PIC_table/CPE.CPE_PROPERTY_PIC_V_LOOKUP.csv")

# Need to add a column for number of PICs
 #sort by PID
longTbl <- longTbl[order(longTbl$PROPERTY_ID),c("PROPERTY_ID", "PIC")]
 #number for how many pics per PID
longTbl$numPICs <- sequence(rle(longTbl$PROPERTY_ID)$lengths)


longTbl[longTbl$numPICs > 7,]
  # this one has 14 pics
longTbl[longTbl$PROPERTY_ID == longTbl[longTbl$numPICs ==14,"PROPERTY_ID"],]

# add total number of PICs as a new column
maxValue <- aggregate(numPICs ~ PROPERTY_ID, data=longTbl, max)
colnames(maxValue) <- c("prop", "maxNumPIC")

newLong <- merge(longTbl, maxValue, by.x="PROPERTY_ID", by.y="prop")

  #reshape
      #test
wideTbl <- reshape(newLong[1:50,], timevar="numPICs",idvar=c("PROPERTY_ID", "maxNumPIC"), direction="wide" )
nrow(wideTbl)

  #for reals
wideTbl <- reshape(newLong, timevar="numPICs",idvar=c("PROPERTY_ID", "maxNumPIC"), direction="wide" )
nrow(wideTbl)


# create new column with Pic in text
wideTbl$PicLabel <- NA
#wideTbl <- wideTbl[1:50,]

#### Function to concatenate PICs where more than one
FUN <- function(df, do.this){
  switch(  
  do.this,  
 out1 = {txt <- wideTbl[j, "PIC.1"]},  
 out2 = {txt <- paste(wideTbl[j,3], wideTbl[j,4], sep="/")},  
 out3 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], sep="/")},  
 out4 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6], sep="/")},
 out5 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7], sep="/")},
 out6 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8], sep="/")},
 out7 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9], sep="/")},
 out8 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9],wideTbl[j,10], sep="/")},
 out9 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9],wideTbl[j,10],wideTbl[j,11], sep="/")},
 out10 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9],wideTbl[j,10],wideTbl[j,11],wideTbl[j,12], sep="/")},
 out11 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9],wideTbl[j,10],wideTbl[j,11],wideTbl[j,12],wideTbl[j,13], sep="/")},
 out12 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9],wideTbl[j,10],wideTbl[j,11],wideTbl[j,12],wideTbl[j,13],wideTbl[j,14], sep="/")},
 out13 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9],wideTbl[j,10],wideTbl[j,11],wideTbl[j,12],wideTbl[j,13],wideTbl[j,14],wideTbl[j,15], sep="/")},
 out14 = {txt <- paste(wideTbl[j,3],wideTbl[j,4],wideTbl[j,5], wideTbl[j,6],wideTbl[j,7],wideTbl[j,8],wideTbl[j,9],wideTbl[j,10],wideTbl[j,11],wideTbl[j,12],wideTbl[j,13],wideTbl[j,14],wideTbl[j,15],wideTbl[j,16], sep="/")},
 stop("Enter something that switches me!")
)
  return(txt)
}
####

for (j in 1:nrow(wideTbl)){
   wideTbl[j,"PicLabel"]  <- FUN(widTbl[j,], wideTbl[j, "maxNumPIC"])
   #outTxt <-FUN(widTbl[j,], wideTbl[j, "maxNumPIC"])
   #print(outTxt)
}

outTable <- wideTbl[, c("PROPERTY_ID", "maxNumPIC", "PicLabel")]

#write.csv(outTable, "PropID_PIC_lookup.csv")
#########################################################
outTable <- read.csv("PropID_PIC_lookup.csv")

#read in property shapefile

#inShp <- readOGR("./AgProps_extracted_5July2021", "AgProps_extracted_5July2021")
inShp <- readOGR("./AgProps_3September2021", "AgProps_3September2021")
#PROPERTY_I             PROPERTY_N ID1
inShp <- inShp[,c("PROPERTY_I", "PROPERTY_N")]
inShp$ID1 <- row.names(inShp)

shpTbl <- inShp@data
nrow(shpTbl)
shpTbl$PROPERTY_I <- as.integer(shpTbl$PROPERTY_I )

summary(as.factor(shpTbl$PROPERTY_I %in% outTable$PROPERTY_ID)) # so 10918 have a PIC

sitesWithPIC <- shpTbl$PROPERTY_I %in% outTable$PROPERTY_ID
  #split dataset
tbl.with <- shpTbl[sitesWithPIC,]
tbl.without<- shpTbl[!sitesWithPIC,]
   tbl.without$ID1 <- NA
   tbl.without$maxNumPIC<- NA
   tbl.without$PicLabel<- NA

#merge with PIC file, keeping all polygons
tbl.withPic <- merge(tbl.with, outTable, by.x="PROPERTY_I", by.y="PROPERTY_ID")
newOut <- as.data.frame(rbind(tbl.withPic, tbl.without))

plot(inShp[inShp$PROPERTY_I == 1398420,])

# FOUND WEBPAGE SAYING USE SPATIAL OBJECT, **NOT** object@data.  Otherwise all incoorect. 
newShp <- merge(inShp, newOut[,c("PROPERTY_I", "ID1", "PicLabel")], by="PROPERTY_I")

plot(newShp[newShp$PROPERTY_I == 1398420,])

############
#fix the projection
crs_wgs84 <- CRS(SRS_string = "EPSG:4326") # WGS84 has EPSG code 4326
proj4string(newShp) <- crs_wgs84

## Clean up for Shiny further: 
trueCentroids = rgeos::gCentroid(newShp,id=newShp$ID1, byid=TRUE)
newShp$centLong <- coordinates(trueCentroids)[,"x"]
newShp$centLat <- coordinates(trueCentroids)[,"y"]

writeOGR(newShp, "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_polygons", layer="Props_for_shiny_3Sep2021", driver="ESRI Shapefile")
saveRDS(newShp, "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/ShinyPastures/data/OtherData/Props_shape.rds")

