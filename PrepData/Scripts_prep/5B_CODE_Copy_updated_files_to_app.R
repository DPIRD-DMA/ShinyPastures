# Edited to save as new file types, to try to make faster. 14 Sep 2021

# final step to update data in ShinyPastures app
# 1-June-2021

## use for update after adding a new image. 
## This copies the newly created files into correct directories for Shiny Pastures.  
## After this, need to update the server.  

# Copy new images from DataPrep to app folder
###############################################################################################################
# images  ## THESE WILL CHANGE.  HOW TO AUTOMATE?  ONLY PART THAT WILL CHANGE UNTIL PATHS MOVE   ##############
    #  Could use old/new list of images in the directory?? Find file dates?

#pImage <- "./PrepData/PGR/y2021/PGR20210810 10 August 2021.tif"
#fImage <- "./PrepData/FOO/y2021/FOO20210810 10 August 2021.tif"
lf <- list.files("./PrepData/PGR/y2021", pattern=".tif$", full.names=TRUE)
pImage <- lf[length(lf)]

lf2 <- list.files("./PrepData/FOO/y2021", pattern=".tif$", full.names=TRUE)
fImage <- lf2[length(lf2)]

print(pImage)
print(fImage)
###########################################################################################################

# Directory for app data:
writeDir <- "./Shiny/data"

#data being copied and where its coming from: 

#Dirs with new data to copy:
inDir <- "./PrepData/PGR_cumulative"
inFoo <- "./PrepData/FOO"



#file base names - shouldn't change
p1 <-  "OtherYears_timeseries_Props_PGR"
p2 <-  "ThisYear_timeseries_Props_PGR"
f1 <- "OtherYears_timeseries_Props_FOO"
f2 <- "ThisYear_timeseries_Props_FOO"

###########
## COPY ##

 # copy images
file.copy(pImage, paste0(writeDir, "/PGR_for_shiny"))
file.copy(fImage, paste0(writeDir, "/FOO_for_shiny"))


## NEW FILE TYPES -- not saving previous drafts this to save space
#file.copy(paste0(writeDir, "/", p2, ".feather"), paste0(writeDir, "/PreviousDraft"), copy.date=TRUE, overwrite=TRUE)
#file.copy(paste0(writeDir, "/", f2, ".feather"), paste0(writeDir, "/PreviousDraft"), copy.date=TRUE, overwrite=TRUE)

#  write feather version
df <- read.csv(paste0(inDir,  "/", p2, ".csv"))
feather::write_feather(df, paste0(writeDir,  "/", p2, ".feather"))

df1 <- read.csv(paste0(inFoo,  "/", f2, ".csv"))
feather::write_feather(df1, paste0(writeDir,  "/", f2, ".feather"))

 #copy most recent image, reprojected, to new folder: 

      #  delete tifs in this folder before copying
fn <- list.files(paste0(writeDir, "/Maps_for_shiny_Mercator"), pattern=".tif$", full.names=TRUE)
file.remove(fn)

# convert most recent raster to Mercator, and write to new directory for leaflet maps
pR <- raster(pImage)
pR1 <- projectRaster(pR, crs = CRS(SRS_string = "EPSG:3857"), method = "ngb")
writeRaster(pR1, paste0(writeDir, "/Maps_for_shiny_Mercator/", basename(pImage)), format="GTiff", overwrite=TRUE)


fR <- raster(fImage)
fR1 <- projectRaster(fR, crs = CRS(SRS_string = "EPSG:3857"), method = "ngb")
writeRaster(fR1, paste0(writeDir, "/Maps_for_shiny_Mercator/", basename(fImage)), format="GTiff", overwrite=TRUE)

# save as R format.  NO - rda's not working on shiny server, although work locally. 
#save(pR1, fR1, file="./Shiny/data/Maps_for_shiny_Mercator/leafMaps.rda")

# this saves as native r format - hopefully faster. 
#source("./PrepData/Scripts_prep/CODE_save_final_files_as_rda.R")
#SaveFilesAsRDA()


#######################################################################################
#only need to do this once a year

#file.copy(paste(writeDir, p1, sep="/"), paste0(writeDir, "/PreviousDraft"), copy.date=TRUE, overwrite=TRUE)
#file.copy(paste(writeDir, f1, sep="/"), paste0(writeDir, "/PreviousDraft"), copy.date=TRUE, overwrite=TRUE)
#file.copy(paste0(inDir,"/", p1, ".rds"), writeDir, copy.date=TRUE, overwrite=TRUE)
#file.copy(paste0(inFoo, "/",f1, ".rds"), writeDir, copy.date=TRUE, overwrite=TRUE)
df3<- read.csv(paste0(inDir,"/", p1, ".csv"))
df4 <-read.csv(paste0(inFoo,"/", f1, ".csv"))
feather::write_feather(df3, paste0(writeDir,  "/", p1, ".feather"))
feather::write_feather(df4, paste0(writeDir,  "/", f1, ".feather"))
 #summary tables
#file.copy(paste(inDir, "SummaryTable_timeseries_Props_PGR.rds", sep="/"), writeDir, copy.date=TRUE, overwrite=TRUE)
#file.copy(paste(inFoo, "SummaryTable_timeseries_Props_FOO.rds", sep="/"), writeDir, copy.date=TRUE, overwrite=TRUE)

df5<- read.csv(paste(inDir, "SummaryTable_timeseries_Props_PGR.csv", sep="/"))
df6 <-read.csv(paste(inFoo, "SummaryTable_timeseries_Props_FOO.csv", sep="/"))
feather::write_feather(df5, paste0(writeDir,  "/SummaryTable_timeseries_Props_PGR.feather"))
feather::write_feather(df6, paste0(writeDir,  "/SummaryTable_timeseries_Props_FOO.feather"))


Props <- readRDS("./Shiny/data/OtherData/Props_shape.rds")
# lists for selecting properties: script CODE_Property_PIC_list_formatting.R
propList <- readRDS("./Shiny/data/OtherData/propList.rds")
picList <- readRDS("./Shiny/data/OtherData/picList.rds")
 # loading in shiny
save(Props, propList, picList, file= "./Shiny/data/inPolys.rda" )

