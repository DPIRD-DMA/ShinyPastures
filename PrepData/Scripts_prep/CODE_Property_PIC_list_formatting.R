
# 16 August 2021, Karen Holmes
# set up datasets for shiny app so ready before startup. 
# making dataset for PIC, etc. used in autocomplete dropdown lists 

############### LOOK UPS FOR AUTOCOMPLETE SELECTION IN SIDE BAR  ##################################
startDir <- "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData"
setwd(startDir)

#Props <- readOGR("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_polygons", layer="Props_for_shiny_16Aug2021" )
Props <- readOGR("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_polygons", layer="Props_for_shiny_3Sep2021" )

# need to read in list of unique PIC, with repeating PID, so people select PIC and get a PID back. 
p <- read.csv("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/AgRegion_polygons/AgRegion_PIC_table/CPE.CPE_PROPERTY_PIC_V_LOOKUP.csv")
nrow(p)
u <- unique(p$PIC)
head(p[p$PIC %in% u,])
length(u) # so PIC repeats too.  

# are all PID in the shapefile also in the pic file?

summary(as.factor(Props$PROPERTY_I %in% p$PROPERTY_ID)) # 4998 of polys are not in the PIC file
head(Props[!(Props$PROPERTY_I %in% p$PROPERTY_ID),])
summary(as.factor(Props$PicLabel[!(Props$PROPERTY_I %in% p$PROPERTY_ID)])) # all have no PIC.  

# are all PID in the PIC table also in the shapefile?
summary(as.factor(  p$PROPERTY_ID %in% Props$PROPERTY_I)) # 12619 ARE in the polygons, 13226 are NOT

# if I remove all properties from shapefile that have no PIC - how many are we trying to match?
tmp <- Props@data[!is.na(Props@data$PicLabel),]  # this leaves 10918.  So should have slightly more than this in a lookup (if some PICs apply to more than 1 PID)

# So make look up of those PID that have a pic, and associate the ID1 which is the unique shapefile reference. 
namesDF <- p[, c("PROPERTY_ID", "PIC")]
# this is 
pWithID <- merge(namesDF, tmp, by.x="PROPERTY_ID", by.y="PROPERTY_I", all.y=TRUE)
head(pWithID)

length(unique(tmp$PROPERTY_I)) #10918 PIDs in shapefile with a PIC
length(unique(pWithID$PROPERTY_ID)) # after join, this many unique PIDS: 10918, this many rows: 12619. So it still has PIDs with different PIC.  Good. 

# So pWithID is appropriate to use in the scroll/lookup for PICs. Properties with NO pic are not in this list. 
pWithID$PICPID <- paste0( pWithID$PIC, " / ", pWithID$PROPERTY_ID)

# make as small as possible, and save as sorted list. 
outFile <- pWithID[,c("PROPERTY_ID", "PICPID")]

# complete list of PICs for autocomplete with dqshiny
#sortedPicList <- sort(outFile[,"PICPID"], decreasing = FALSE)

# re-ordered
outFile <- outFile[order(outFile$PICPID),]

saveRDS(outFile,  "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/ShinyPastures/data/OtherData/picList.rds") # for use in auto2

# complete list of properties for autocomplete with dqshiny
propNames <- as.character(Props$PROPERTY_I) 

saveRDS(propNames, "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/ShinyPastures/data/OtherData/propList.rds") # for use in auto1

