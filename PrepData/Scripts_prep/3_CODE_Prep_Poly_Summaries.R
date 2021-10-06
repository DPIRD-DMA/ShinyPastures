# PGR raster summaries
#  Summarise data by polygon for plotting
#  Using cumulative rasters, so we get a cumulative avg/10/90 per time step
  # running on y2021 data, 7 April 2021.  Probably need to re-run on other years?  
  #  Using "ID2" as unique polygon id.  This dataset had dups removed.

   # ran same code on (non-cumulative) FOO images.
   # IMPORTANT:  I multiplied all FOO values by 10 to convert to actual units.  14-April-21
   # check throughout code for differences for pgr or foo. 

library(raster)
library(rgdal)
library(stringr)
#library(terra)
library(exactextractr)
library(sf)

######################
##  Choose dataset  ##
######################

startDir <-  "C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github/ShinyPastures/PrepData/PGR_cumulative"
Pfix <- "Add"  # or uncomment next lines

## OR ##

startDir <-  "C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github/ShinyPastures/PrepData/FOO"
Pfix <- "FOO"  #  if FOO, then it multiplies rasters by 10 to fix units.    


setwd(startDir)

#polys <- readOGR("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_polygons", layer = "Props_for_shiny_16Aug2021")
polys <- readOGR("C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github/ShinyPastures/PrepData/AgRegion_polygons", layer = "Props_for_shiny_3Sep2021")

# export a text file to use to look up polygon names (PID, PIC)
head(polys@data)
  #prop.lkup <- polys@data[,c("ID1_x", "PROPERTY_I", "PROPERTY_N","PicLabel")]
  #names(prop.lkup)<- c("ID1", "PID", "Name", "PIC")
  #prop.lkup$XID1 <- paste0("X",prop.lkup$ID1)
   #writing this to use in CODE_Prep_DataTable_for_plotting.R.  Check using correct shapefile.
    #pre-sorting so ID's are increasing in number
  #write.csv(prop.lkup[order(prop.lkup$ID1),], "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/OtherData/Lookup_Prop_IDs.txt", row.names=FALSE)
  
prop.lkup <- read.csv("C:/Users/kholmes/OneDrive - Department of Primary Industries and Regional Development/Documents/Github/ShinyPastures/PrepData/OtherData/Lookup_Prop_IDs.txt")

# need this spatial format for the faster extract function used below. 
polys.sf <- st_as_sfc(polys) 

#######
## CHECK POLYS NAME IS "ID1_x"
#########

### function  ###
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
#################

#ONLY NEED TO UPDATE LAST DIR, UNLESS RE-PROCESSING WHOLE TIMESERIES

# will need to loop over years
 ListYearDirs <- list.dirs(startDir)[-1]

# THIS YEAR ONLY
 ListYearDirs <- ListYearDirs[18]


# Next is a loop over the Directories  
lapply(ListYearDirs, function(inDir){
   #inDir <- ListYearDirs[1] 
  print(inDir)
  #get rasters 
  files1 <-  list.files(inDir, pattern=".tif$", full.names=T)
  s <- stack(files1) # remove testing files if need be:  files1[1:16]
  ImageNameList <- names(s)
  
  #  IMportant - next line for FOO only
  if(Pfix == "FOO"){
    print("this is FOO, so multiplying by 10 to fix units.")
     s <- s*10  # images saved by Landgate are Foo/10, so this converts back to real units
     #restore names
     names(s) <- ImageNameList
  }  
  
  # need to multiply value by coverage_fraction?
  
  er.exact <- exact_extract(s, polys.sf, progress = TRUE)  # polys need to be sf object
  names(er.exact) <- polys$ID1_x
  MeansList <- lapply(er.exact, function(a){
    colMeans(a, na.rm = TRUE)
  })

  #reformat them as a dataframe (df)
  df1 <- as.data.frame(do.call("cbind", MeansList)) 
  df <- round_df(df1, 0) 

  # get image name
  ImageName <- row.names(df)
  # extract the date info from the image name
  
    ### DIFFERENT FOR FOO
  #DATE <- substr(ImageName, 4, 11)
     ### PGR - converted to same as FOO name format
  DATE <- substr(ImageName, 4, 11)
  
  yr <- substr(DATE, 1, 4)
  mth <- substr(DATE, 5,6)
  
  # change formatting of date for later plotting
  DATE <- as.Date(DATE, format="%Y%m%d")  
  
  # combine with the dataframe
  RegionMean <- cbind(ImageName, DATE, yr, mth, df)
  row.names(RegionMean) <- NULL
  RegionMean <- RegionMean[1:(nrow(RegionMean)-1),] 
  
  
  
  if(Pfix=="FOO"){
    print("Writing FOO file")
    write.csv(RegionMean, paste0(startDir, "/",basename(inDir),"_PropStats_timeseries_FOO.csv"), row.names=FALSE)
    #saveRDS(RegionMean, paste0(startDir, "/",basename(inDir),"_PropStats_timeseries_FOO.rds"))
  } else{
    print("Writing PGR file")
    write.csv(RegionMean, paste0(startDir, "/",basename(inDir),"_PropStats_timeseries_PGR.csv"), row.names=FALSE)
    #saveRDS(RegionMean, paste0(startDir, "/",basename(inDir),"_PropStats_timeseries_PGR.rds"))
  }
  
  gc()
}) 

# is there any difference in formatting from csv or rds? AND this is used in next script, so NO NEED for RDS.
#  Yes, the rds reads in column names as integers, the csv adds an "X" in front.  
#a <- read.csv("y2021_PropStats_timeseries_PGR.csv")
#b <- readRDS("y2021_PropStats_timeseries_PGR.rds")
