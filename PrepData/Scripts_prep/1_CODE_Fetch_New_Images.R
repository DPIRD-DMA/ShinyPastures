
# 13 August 2021 Karen Holmes
# Pastures from Space -- Shiny

# This code checks for new image on server, copies, 
# adds 6 days to the date, and writes in date format:  "XXX20210512 12 May 2021.tif"

## next step, creating the tables, will use format "XXX20210512 12 May 2021.tif"

## code for date changes from other formats (other years or from other agencies)
# in  CODE_ImageDate_change_and_formatting.R

# libraries
library(rgdal)
library(sp)
library(raster)

##################
# set FOO or PGR #
##################

PrefixName <- c("PGR", "FOO") 

lapply(PrefixName, function(Prefix){
   # Prefix <- "PGR"
  
# GIS server
data.Dir = paste0("//Agspsrgis03/majorinf/Imagery/Satellite/FOOPGR/", Prefix, "/2021/New")  #  Need to use "New" as of May 2021
# local directory
out.dir<- paste0("C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github/ShinyPastures/PrepData/", Prefix, "/y2021")
# set working directory
setwd(out.dir)

## Check if any new images there
# make a list of all tif files in covariate directory
all.files <- list.files(path = data.Dir, pattern="tif$") 
#write.csv(all.files, paste0(out.dir, "/", "ServerFiles_when_last_checked.csv"), row.names=FALSE)
old.files <- read.csv(paste0(out.dir, "/", "ServerFiles_when_last_checked.csv"))
old.files <- old.files[,1]
  #this gets only those that were not in the list from last time checked. 
all.files <- all.files[-which(all.files %in% old.files)]
print(all.files)

if(length(all.files) == 0){
  print ("NO NEW IMAGES TO PROCESS")
} else {  # run the rest of the code to crop, rename, and write to my drive.

 # The rasters are in geographic coords, so reading in 1 to get the projection
 #rfile = all.files[1]
 #r1 <- raster(paste(data.Dir, rfile, sep = "/"))
 
 #loop over all rasters, copy to local drive, then re-name, add 6 days, and copy again 
 lapply(all.files, function(file){ 
    #file = all.files[1]
    print(file)
    inRaster <- raster(paste(data.Dir, file, sep = "/"))
    
    if(Prefix == "PGR"){
      # set 254, 255 as nodata (one was originally cloud, and one no data, I think?)
      inRaster[inRaster > 253] <- NA
    }
    
    proj4string(inRaster) <- CRS(SRS_string = "EPSG:4326")
    
      # changing name from :  "FOO 5 may 2021.tif"  to   "FOO20210511 11 May 2021.tif" (ADDING 6 DAYS)
      nameDate <- substr(file, 5, nchar(file)-4)
      fileDate <- as.Date(nameDate,format="%d %B %Y")
      newDate <- fileDate + 6
      formattedDate <- format (newDate,"%d %B %Y") 
      charDate <- as.character(formattedDate)
      newname <- paste0(Prefix, format (newDate, "%Y%m%d"), " ", charDate, ".tif")

      writeRaster(inRaster, filename=paste(out.dir, newname, sep = "/"), 
                format="GTiff", overwrite = TRUE)
 }) # end of files
} # end of loop over new files

 # update file to list of current images
all.files <- list.files(path = data.Dir, pattern="tif$") 
write.csv(all.files, paste0(out.dir, "/", "ServerFiles_when_last_checked.csv"), row.names=FALSE)

}) # end loop over PrefixName


