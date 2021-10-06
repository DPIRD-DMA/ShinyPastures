
# script to prep datasets for reading into shiny. 
   # PGR images
   # FOO images
   # ImageDates table
# this has to be saved in Github/ShinyPastures, as on my laptop

SaveFilesAsRDA <- function(){
   
    rasterPath <- "./PrepData/PGR/y2021"
    print("processing pgr")
    nameList <- list.files(rasterPath, pattern=".tif$", full.names=F)
    txtYear <- substr(nameList,4, 7)
    txtMonth <- substr(nameList,8, 9)
    txtDay <- substr(nameList,10, 11)
    rasterDate <- paste(txtYear, txtMonth, txtDay, sep="-")

   imageDatesTbl <- as.data.frame(cbind(1:length(rasterDate), rasterDate, txtYear, txtMonth, txtDay))
   imageDatesTbl$NiceDate <- format(as.Date(rasterDate), "%d-%b")   
   names(imageDatesTbl) <- c("rLayer", "rasterDate", "yr", "mth", "day", "NiceDate")
   saveRDS(imageDatesTbl, file="./Shiny/data/DatesTable.rds")

   #find raster names, read into a stack
   rasterList <- list.files(rasterPath, pattern=".tif$", full.names=T)
   ras <- stack(rasterList)
   names(ras)<- rasterDate
   pBrick <- brick(ras)
   names(pBrick)
   #write netcdf
   writeRaster(pBrick, "./Shiny/data/pgr.nc", overwrite=TRUE, format="CDF",     varname="PGR",
               longname="PGR -- raster stack to netCDF", xname="Longitude",   yname="Latitude", zname="Time (Month)")
   # testing output
   #tmp <- brick("./Shiny/data/pgr.nc")
   #tmp2 <- readRDS("./Shiny/data/DatesTable.rds")

   # FOO  
   # dates are identical to PGR, so use same:  imageDatesTbl 
   print("processing FOO")
   rasterPathFOO <- "./PrepData/FOO/y2021"

   #find raster names, read into a stack
   rasterListFOO <- list.files(rasterPathFOO, pattern=".tif$", full.names=T)
   rasFOO <- stack(rasterListFOO)
   names(rasFOO)<- rasterDate
   
   fBrick <- brick(rasFOO)
   names(fBrick)
   #write netcdf
   writeRaster(fBrick, "./Shiny/data/foo.nc", overwrite=TRUE, format="CDF",     varname="FOO",
               longname="FOO -- raster stack to netCDF", xname="Longitude",   yname="Latitude", zname="Time (Month)")

   # save as native r file 
   print("saving ...")
   #save(ras, rasFOO, imageDatesTbl, file="./Shiny/data/inRasters.rda")
 }  
