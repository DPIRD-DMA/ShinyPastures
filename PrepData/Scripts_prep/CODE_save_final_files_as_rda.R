
# script to prep datasets for reading into shiny. 
# this has to be saved in Github/ShinyPastures, as on my laptop

SaveFilesAsRDA <- function(){
    rasterPath <- "./Shiny/data/PGR_for_shiny"
    print("processing pgr")
    nameList <- list.files(rasterPath, pattern=".tif$", full.names=F)
    txtYear <- substr(nameList,4, 7)
    txtMonth <- substr(nameList,8, 9)
    txtDay <- substr(nameList,10, 11)
    rasterDate <- paste(txtYear, txtMonth, txtDay, sep="-")

   imageDatesTbl <- as.data.frame(cbind(1:length(rasterDate), rasterDate, txtYear, txtMonth, txtDay))
   imageDatesTbl$NiceDate <- format(as.Date(rasterDate), "%d-%b")   
   names(imageDatesTbl) <- c("rLayer", "rasterDate", "yr", "mth", "day", "NiceDate")

   #find raster names, read into a stack
   rasterList <- list.files(rasterPath, pattern=".tif$", full.names=T)
   ras <- stack(rasterList)
   names(ras)<- rasterDate

   # FOO  
   # dates are identical to PGR, so use same:  imageDatesTbl 
   print("processing FOO")
   rasterPathFOO <- "./Shiny/data/FOO_for_shiny"

   #find raster names, read into a stack
   rasterListFOO <- list.files(rasterPathFOO, pattern=".tif$", full.names=T)
   rasFOO <- stack(rasterListFOO)
   names(rasFOO)<- rasterDate

   # save as native r file 
   print("saving ...")
   save(ras, rasFOO, imageDatesTbl, file="./Shiny/data/inRasters.rda")
}