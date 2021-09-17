
# reproject all datasets into native leaflet projection to reduce loading time. 
### EPSG:3857 Web Mercator, all data previously in EPSG: 4326
# proj4string(inRaster) <- CRS(SRS_string = "EPSG:4326") # new format for assigning projection

  # will need to do this for click$x and click$y in app.r
#coordinates(myDataframe) <- value
#proj4string(myDataframe) <- CRS("+init=epsg:4326")
#newdf <- spTransform(myDataframe, CRS("+proj=longlat +ellps=GRS80"))
# keep as a sp points object?

library(raster)
library(rgdal)

# go through datafolders and reproject, save in new folder. 


#inFiles <- list.files("./data/PGR_for_shiny_dd", pattern=".tif$", full.names=TRUE)
#outPath <- "./data/PGR_for_shiny"

inFiles <- list.files("./data/FOO_for_shiny_dd", pattern=".tif$", full.names=TRUE)
outPath <- "./data/FOO_for_shiny"

lapply(inFiles, function(MyFile){
  #MyFile <- inFiles[1]
  print(basename(MyFile))
  r <- raster(MyFile)
  r2 <- projectRaster(r, crs = CRS(SRS_string = "EPSG:3857"), method = "ngb")
  writeRaster(r2, paste0(outPath, "/", basename(MyFile)), format="GTiff", overwrite=TRUE)
  
})

# shapefile
MyFile <- "./data/OtherData_dd/Props_shape.rds"
shp <- readRDS(MyFile)
projection(shp)
proj4string(shp) <- CRS(SRS_string = "EPSG:4326") 
outShp <- spTransform(shp, CRS(SRS_string = "EPSG:3857"))

saveRDS(outShp, paste0("./data/OtherData/", basename(MyFile)))

# try simplifying shapes
library(rmapshaper)

MyFile <- "./data/OtherData_dd/Props_shape.rds"
shp <- readRDS(MyFile)


#
inshp <- readOGR("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_polygons", "Props_for_shiny_16Aug2021")
inshp <- inshp[,c("PROPERTY_I","PROPERTY_N","PicLabel","centLong","centLat")]
saveRDS(inshp, "./OtherData/Props_shape.rds")
inshp<- shp

library(sf)
newsf <- st_as_sf(inshp)
saveRDS(newsf, "./data/OtherData/Props_sf.rds")
saveRDS(newsf, "./data/OtherData/sf_file.rds")
st_write(newsf, "./data/OtherData/sf_fileNew.shp", driver="ESRI Shapefile") 

inSF <- readRDS("./data/OtherData/sf_file.rds")
#st_write(newsf, "shapefile_out.shp", driver="ESRI Shapefile")  # create to a shapefile 
#st_write(newsf, " s.gpkg", driver="GPKG")  # Create a geopackage file
#list of drivers
(rgdal::ogrDrivers(), write == TRUE)


shp5 <- inshp
shp4 <- inshp
shp3 <- inshp
shp2 <- inshp
shp1 <- inshp
shp05 <- inshp
 ms_simplify(shp5, keep = 0.5, keep_shapes=TRUE)
 writeOGR(shp5, "./data/OtherData", "shp5", driver="ESRI Shapefile", overwrite_layer=TRUE)

ms_simplify(shp, keep = 0.4, keep_shapes=TRUE)
writeOGR(shp4, "./data/OtherData", "shp4", driver="ESRI Shapefile", overwrite_layer=TRUE)

ms_simplify(shp, keep = 0.3, keep_shapes=TRUE)
writeOGR(shp3, "./data/OtherData", "shp3", driver="ESRI Shapefile", overwrite_layer=TRUE)

ms_simplify(shp, keep = 0.2, keep_shapes=TRUE)
writeOGR(shp2, "./data/OtherData", "shp2", driver="ESRI Shapefile", overwrite_layer=TRUE)

ms_simplify(shp, keep = 0.1, keep_shapes=TRUE)
writeOGR(shp1, "./data/OtherData", "shp1", driver="ESRI Shapefile", overwrite_layer=TRUE)

row.names(shp05)<- row.names(shp05@data)
out <- ms_simplify(shp05, keep = 0.05, keep_shapes=TRUE)
writeOGR(out, "./data/OtherData", "shp05", driver="ESRI Shapefile", overwrite_layer=TRUE)

saveRDS(shp05, "./data/OtherData/shp05.rds")



###########

a <- readRDS("./data/OtherYears_timeseries_Props_PGR.rds") 

# save as different formats then test loading speed
library(utils)
library(readr)
library(data.table)
library(feather)

write.csv(a, "./data/Testing/other2.csv")
write_feather(a, "./data/Testing/other.feather")
fwrite(a, "./data/Testing/other.f")
saveRDS(a, "./data/Testing/other.rds")
write_csv(a, "./data/Testing/other.csv")
save(a,  file = "./data/Testing/other.RData")


library(microbenchmark)
benchmark <- microbenchmark(readCSV = utils::read.csv("./data/Testing/other2.csv"),
                            readrCSV = readr::read_csv("./data/Testing/other.csv", progress = F),
                            fread = data.table::fread("./data/Testing/other.f", showProgress = F),
                            loadRdata = base::load("./data/Testing/other.RData"),
                            readRds = base::readRDS("./data/Testing/other.rds"),
                            readFeather = feather::read_feather("./data/Testing/other.feather"), times = 10)
print(benchmark, signif = 2)

in.feather = feather::read_feather("./data/Testing/other.feather")


setwd("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/ShinyPastures")


## Test stack versus brick?? 


inFiles <- list.files("./Shiny/data/FOO_for_shiny_dd", pattern=".tif$", full.names=TRUE)
outPath <- "./data/FOO_for_shiny"

rasterListFOO <- list.files("./Shiny/data/FOO_for_shiny", pattern=".tif$", full.names=T)

b <- brick(stack(rasterListFOO))
writeRaster(b, "rstack.nc", overwrite=TRUE, format="CDF",     varname="Foo", varunit="foounit", 
            longname="FOO2021 -- raster stack to netCDF", xname="Longitude",   yname="Latitude", zname="Time (Month)")

outfile <- writeRaster(brick, filename='grid.tif', format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
benchmark <- microbenchmark(b1 = brick(stack(rasterListFOO)),
                            b2 = brick("rstack.nc"), times = 1)
print(benchmark, signif = 2)

##
latestPGR <- list.files("./Shiny/data/Maps_for_shiny_Mercator/", pattern="^PGR", full.names=TRUE)
latestFOO <- list.files("./Shiny/data/Maps_for_shiny_Mercator/", pattern="^FOO", full.names=TRUE)
pR1 <- raster(latestPGR) #in mercator for leaflet
fR1 <- raster(latestFOO)   #crs = CRS(SRS_string = "EPSG:3857")

saveRDS(pR1, file="pgr.rds")
library(microbenchmark)
benchmark <- microbenchmark(r1 = raster(latestPGR),
                            r2 = readRDS("pgr.rds"), times = 10) # try this again in shiny??
print(benchmark, signif = 2)





########################################
library(profvis)
library(shiny)
profvis({
  runExample(example = "06_tabsets", display.mode = "normal")
})


profvis({ runApp('.') }
        , prof_output = '../PrepData/TestingApp')


# Assign to variable
p <- profvis(prof_input = 'C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/TestingApp/file5e301de24a25.Rprof')

# Save as a webpage
htmlwidgets::saveWidget(p, "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/TestingApp/profile.html")

                      