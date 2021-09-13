


# this is a loop to read in the rasters, make a change, and overwrite 

library(raster)
library(rgdal)

startDir <- "C:/ALL_PROJECTS/PastFromSpace_DRAFT4"
setwd(startDir)

DataDir <- "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/PGR/y2021"  # overwriting same files.  


Dirs <- list.dirs("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/PGR")[-1]
Dirs <- Dirs[-length(Dirs)]
######################################
# First part: set NA's, project, etc. 
######################################
### THIS MAY BE A LITTLE DIFF FROM COLOURS ON WEB.  DO NOT RUN, FOR TIME BEING.  (may just be => vs > break, or something)

# define same colors as Landgate (doesn't really matter here, but helpful later)
#breakpoints <- c(0, 10, 20, 30, 40, 50, 60, 70, 85, 100, 120)
#colors <- c("#fca4b7","#fcc89d","#fcfa4e","#79fc72","#00bdfc","#ad00fc", 
#              "#d358db","#ff009d", "#ff0062", "#ff8119", "#ffae00" )

for(z in 1:length(Dirs)){
  #z=1
  DataDir <- Dirs[z]
  #get files (original from Landgate) 
  all.files<-list.files(DataDir, pattern=".tif$", full.names=FALSE)
  
  # this chooses only the latest file to run.  
  #all.files <- all.files[length(all.files)] 
  
  # Set values to NA, re-write projection, and overwrite the file
  lapply(all.files, function(rf){
    #rf <- all.files[1]
    r <- raster(paste0(DataDir, "/", rf))
    r[r > 253] <- NA
    
    proj4string(r) <- CRS(SRS_string = "EPSG:4326") # WGS84 has EPSG code 4326
    writeRaster(r,paste0(DataDir, "/", rf), format = "GTiff", overwrite=TRUE )
  })
}

