

# Convert file names and dates from what was on server to new.  
   ### adds 6 days to the date, so presented as LAST day of the modis composite ###
# doing this to full SW dataset, see v4 on my system. 

# 12-August-2021
#  Karen Holmes


##########################################
# working out how to rename files with dates. 
# find names for re-naming rasters

# Editing file names, to get in same format that sorts correctly. 

##################################################################################
#  IMPORTANT:  CHANGING IMAGE DATES TO END OF WEEK.  THESE WILL DIFFER FROM SERVER.  
##################################################################################

#newdir<- "C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Data_Prep/FOO/y2020"

Prefix <- "FOO"

olddir <- paste0("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_", Prefix, "_orig")
setwd(olddir)

newdir<- paste0("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/", Prefix)
yearDir <- list.dirs(olddir, full.names=FALSE)[-1]

# read in one of the consistent rasters from Landgate to check colormap, copy to other files
r <- raster("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_PGR_orig/y2004/SWWAmodPGR20041020.tif")
colortable(r)

lapply(yearDir[18], function(theyear){
  #theyear <- "y2020"
  print(theyear)
  all.files <- list.files(path = paste0("./", theyear), pattern=".tif$", full.names = TRUE) 
  #all.files <- list.files(path = paste0("./", yearDir[18]), pattern=".tif$", full.names = TRUE)
  #print(all.files[1])
  
  lapply(all.files, function(file){
    # file <- all.files[1]
    print(basename(file))
    inRaster <- raster(file)
    
    #1# This formatting is for new Landgate version:  "SWWAmodPGR20041231.tif" to new standard: "PGR20041231 31 December 2004.tif"
    #nameDate <- substr(basename(file), 11, (nchar(basename(file))-4) )
    #fileDate <- as.Date(nameDate,format="%Y%m%d")
    #newDate <- fileDate + 6
    #formattedDate <- format (newDate,"%d %B %Y") 
    #charDate <- as.character(formattedDate)
    #newname <- paste0(Prefix, format(newDate, "%Y%m%d"), " ", charDate, ".tif")
    
    #2# This formatting is for new Landgate version:  "PGR 31 December 2004.tif" to new standard: "PGR20041231 31 December 2004.tif"
    nameDate <- substr(basename(file), 5, (nchar(basename(file))-4) )
    fileDate <- as.Date(nameDate,format="%d %B %Y")
    newDate <- fileDate + 6
    formattedDate <- format (newDate,"%d %B %Y") 
    charDate <- as.character(formattedDate)
    newname <- paste0(Prefix, format (newDate, "%Y%m%d"), " ", charDate, ".tif")
    
    #3# FOO - names correct, but needs 6 days added. 
    #nameDate <- substr(basename(file), 4, 11)
    #fileDate <- as.Date(nameDate,format="%Y%m%d")
    #newDate <- fileDate + 6
    #formattedDate <- format (newDate,"%d %B %Y") 
    #charDate <- as.character(formattedDate)
    #newname <- paste0(Prefix, format(newDate, "%Y%m%d"), " ", charDate, ".tif")
    
    writeRaster(inRaster, filename=paste(newdir,theyear, newname, sep = "/"), 
                format="GTiff", overwrite = TRUE)
  })
})



####################
newdir<- "C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Data_Prep/PGR/y2018_onserver"
all.files <- list.files(path = newdir, pattern=".tif$") 


#  changing pgr from pgr20210407 7 April 2021.tif  to pgr_20210407.tif
lapply(all.files, function(file){
  # file <- all.files[1]
  print(file)
  inRaster <- raster(paste(newdir, file, sep = "/"))
  
  part1 <- substr(file, 0, 3)
  part2 <- substr(file, 7, nchar(file))
  newname <- paste0(part1, "2", part2)# ".tif")
  
  writeRaster(inRaster, filename=paste(newdir, newname, sep = "/"), 
              format="GTiff", overwrite = TRUE)
})

###########################################################################
#  changing pgr from pgr_20210407.tif to "PGR20210407 7 April 2021.tif", to make consistent with FOO files 
newdir<- "C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Data_Prep/PGR/y2016"
all.files <- list.files(path = newdir, pattern=".tif$") 

lapply(all.files, function(file){
  # file <- all.files[1]
  print(file)
  inRaster <- raster(paste(newdir, file, sep = "/"))
  
  part1 <- substr(file, 0, 4)
  part2 <- substr(file, 5, 12)
  
  formattedDate <- as.Date(part2, format="%Y%m%d")
  newFormat <- format(formattedDate, format="%d %B %Y")
  
  charDate <- as.character(newFormat)
  newname <- paste0("PGR", part2, " ", charDate, ".tif")
  
  writeRaster(inRaster, filename=paste(newdir, newname, sep = "/"), 
              format="GTiff", overwrite = TRUE)
})

############################################################################
## get info from rasters
#  writing to a table

#write a function that produces a vector of information
rasSumStats <- function(infile){
  # file <- all.files[1]
  print(infile)
  inRaster <- raster(paste(dir1, infile, sep = "/"))
  
  return(data.frame(
    col1 <- substr(names(inRaster), 1, 3),
    col2 <- substr(names(inRaster), 4, 7),
    col3 <- names(inRaster),
    col4 <- dataType(inRaster),
    col5 <- inRaster@data@min,
    col6 <- inRaster@data@max,
    col7 <- length(unique(values(inRaster))) 
  ))
}

# this runs the function on 1 directory (named dir1)
#a <- do.call(rbind, lapply(all.files, rasSumStats))

# this is final table
outTable <- as.data.frame(matrix(NA, nrow=0, ncol=7))

#  loop over directories
indir<- list.dirs("C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Data_Prep/PGR")[-1]
indir<- list.dirs("C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Data_Prep/FOO")[-1]
#indir <- indir[1:2]

for(i in 1:length(indir)){
  dir1 <- indir[i]
  all.files <- list.files(path = dir1, pattern=".tif$") 
  # call function then bind output into final table
  a <- do.call(rbind, lapply(all.files, rasSumStats))
  outTable <- rbind(outTable, a)
} # end dir1

colnames(outTable) <- c("Dataset", "Year", "FileName", "Type", "MinValue", "MaxValue", "nValues")

write.csv(outTable, "C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Reporting/PGR_Raster_Summaries_9June2021.csv")
write.csv(outTable, "C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Reporting/FOO_Raster_Summaries_9June2021.csv")

#nameDate <- substr(all.files, 5, nchar(all.files) -4)
#formattedDate <- as.Date(nameDate, format="%d %B %Y")
#charDate <- as.character(formattedDate)
#newnames <- paste0("foo_", gsub("[-]", "", charDate))
