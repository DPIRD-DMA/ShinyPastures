
#  this reads in PGR rasters and makes cumulative rasters
# 13 August 2021

library(raster)
library(rgdal)

startDir <- "C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github"
setwd(startDir)

############################################
#    writing cumulative rasters
############################################

inDir <- "C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github/ShinyPastures/PrepData/PGR"
outDir2 <- "C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github/ShinyPastures/PrepData/PGR_cumulative"

ThisYearOnly <- "TRUE"

if(ThisYearOnly == "TRUE") {
   ListDir <- paste0(inDir, "/y2021")
   ListOutDir <- paste0(outDir2, "/y2021")
}else{
    #fix this up - currently this is manually determined.
   ListDir <- list.dirs(inDir)[-1]
   ListOutDir <- list.dirs(outDir2)[-1] # WATCH THIS - AT THE MOMENT, DIFF YEARS AVAILABLE in PGR and PGR_cummulative
   #ListOutDir <- ListOutDir[13:18] #2016 to 2021
   #ListDir <- ListDir[c(13,15,17,19,20,21)]  # need this because of extra dirs currently in my folder
}

#Loop over directories   -- Check if still has a colortable?  would this stop them showing up in qgis? and check for high values/adding
for(j in 1:length(ListDir)){
   #j=1  
  print(ListDir[j])
   all.files2<-list.files(ListDir[j], pattern="PGR2*", full.names=FALSE) #pattern=".tif$",
 
   # changed prefix just to be clear for new raster names
   all.files3 <- paste0("Add", substr(all.files2, 4, nchar(all.files2)))
   # save as cumulative values - Then we can summarise and graph directly

   # each year starts as all 0's
   rBase <- 0 * raster(paste0(ListDir[j], "/", all.files2[1]))

   #Loop over files in that directory
   for(i in 1:length(all.files2)){
     print(all.files2[i])
     r <- raster(paste0(ListDir[j], "/", all.files2[i]))
     rBase <- (r * 7) + rBase # growth this week + previous week
     #plot(r,breaks=breakpoints,col=colors)
     writeRaster(rBase, paste0(ListOutDir[j], "/",all.files3[i]), datatype='INT2U', format = "GTiff", overwrite=TRUE )
   }
} #end loop over Dirs
