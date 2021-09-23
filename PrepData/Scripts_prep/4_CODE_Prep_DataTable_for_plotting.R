
# Formats data files for graphing in the ShinyPastures app.  

# revisiting 7-April-2021, on 2021 property data.  Have now removed duplicate polys
# edited 1-June-2021, to clean up and make easier to run.

# FIRST PART IS RUN AFTER EACH NEW IMAGE ADDED, SECOND PART ONLY AT END OF YEAR FOR MONTHLY SUMMARY


library(ggplot2)
library(reshape2)

# NEED TO CHOSE PGR AND FOO HERE, THEN Rest is pretty GENERIC. 

# Limit fileList in this first section if only doing THIS YEAR.  

#  set up for PGR
startDir <- "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/PGR_cumulative"
Prefix <- "PGR"

#   set up for FOO
startDir <- "C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/FOO"
Prefix="FOO"

########################
# set up files: 
setwd(startDir)
fileList <- list.files(startDir, pattern=paste0("*\\_PropStats_timeseries_", Prefix, ".csv$") )
print(fileList)
outFileName <- paste0("ThisYear_timeseries_Props_", Prefix)
outFileOthers <- paste0("OtherYears_timeseries_Props_", Prefix)  #this has other years for selection as "similar" to this year.

###if only doing weekly update, limit fileList to THIS YEAR
fileList <- fileList[18]

###########################

  #set up table names
fname <- paste0("dat", substr(fileList, 4, 5))
  #set up year starting date
stDate <- paste0("20", substr(fileList, 4, 5), "-01-01")


#########  Function That calculates day of the year, starting at 1-Jan
Yearly_Summary <- function(df){
  #df <- fileList[1]
  tmp <- as.data.frame(read.csv(df))
  tmp$DATE <- as.Date(tmp$DATE, format="%Y-%m-%d")   #Issues here? has dashes in R, writes as "/", reads as "-" ?
  tmp$DayOfYr <- as.numeric(tmp$DATE - as.Date(stDate[j], format= "%Y-%m-%d"))
  return(tmp)
}
########
#tmp2 <- Yearly_Summary(fileList[1])
#tmp2[1:10, 1:10]

# apply function to each file
for (j in 1:length(fileList)){
  assign(fname[j], Yearly_Summary(fileList[j]))
}

# if doing only 2021
if(length(fileList) == 1){
  dat <- dat21 #only this year
}else{
  print(paste0("Working on ", Prefix, " data.  more than 1 year detected - combining years into one file."))
dat <- rbind(dat04, dat05, dat06, dat07, dat08, dat09,dat10, dat11, dat12, dat13, dat14, dat15,dat16, dat17, dat18, dat19, dat20, dat21)
}

# re-shape so that Region is a column (flatten)
library(reshape2)
dat.melt1 <- melt(dat, variable.name = "Region", value.name= "Mean", id=c("ImageName", "DayOfYr","DATE", "yr", "mth"))
colnames(dat.melt1) <- c("ImageName","DayOfYr","DayOfYear","yr","mth","Region","Mean")
dat.melt$DayOfYear <- as.Date(dat.melt$DayOfYear)

  #dat.melt1[dat.melt1$yr == 2021,]
## Need to add in PID and PIC, from original shapefile data
prop.lkup <- read.csv("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/PrepData/OtherData/Lookup_Prop_IDs.txt")
head(dat.melt1) 
head(prop.lkup)
  # join the two tables (one to many left join)
dat.melt <- merge(x = dat.melt1, y = prop.lkup, by.x = c("Region"), by.y = c("XID1"), all.x = T)
head(dat.melt)



#dat.melt[1:100,]

# this year data 
thisyrdat <- dat.melt[dat.melt$yr == 2021, ]


### CHOOSE DATANAME ###  Could reduce number of columns written out here.
write.csv(thisyrdat, paste0(startDir, "/", outFileName, ".csv")) 
#saveRDS(thisyrdat, paste0(startDir, "/", outFileName, ".rds")) 


if(length(fileList) == 1){
  print("Done updating files for this year.")
} else{
  print("This year's file updated.  Now processing all the other years.")
# next file only  needs to be done once at start of year. #  reducing size of table -- only these columns needed for Summary Plots 
write.csv(dat.melt[dat.melt$yr !=2021,c("DayOfYr", "DayOfYear", "PID", "yr", "mth", "Mean")], paste0(startDir, "/", outFileOthers, ".csv")) 
#saveRDS(dat.melt[dat.melt$yr !=2021,c("DayOfYr","DayOfYear", "PID", "yr", "mth", "Mean")], paste0(startDir, "/", outFileOthers, ".rds")) 

# testing merge
# A <- cbind(1:10, "cat")
# colnames(A) <- c("id", "animal")
# B <- cbind(1:8, c("dog", "cat"))
# colnames(B) <- c("num", "pet")
# merge(x = A, y = B, by.x = c("id"), by.y = c("num"), all.x = T)  # all X included, and only where Y matches added on. 

########################################################################################################
## THIS PART IS DONE AT END OF YEAR
#########################################################################################################
## CALCULATING STATS OVER THE YEARS (minus THIS YEAR) FOR EACH MONTH

## Calculate mean/median and 90/10 and 25/75 by month. then in plot fit with smooth line 
library(dplyr)
  # buliding this without 2021, because thisyear=2021.  
mth.sum <-     dat.melt[dat.melt$yr !=2021, c("Region", "mth", "Mean")] %>% 
  group_by(Region, mth) %>% 
  summarise(mean_sum = mean(Mean), med_sum = median(Mean), q10 = quantile(Mean, prob=0.1, na.rm=TRUE), 
            q90 = quantile(Mean, prob=0.9, na.rm=TRUE),
            q25 = quantile(Mean, prob=0.25, na.rm=TRUE), q75 = quantile(Mean, prob=0.75, na.rm=TRUE) )

df.sum <- as.data.frame(mth.sum)


# table of month mid-point day, to use for plotting monthly mean, etc. 
mth.date <- as.data.frame(cbind(c(1, 2, 3, 4 ,5, 6, 7, 8, 9, 10, 11, 12),
                                c("2021-01-15","2021-02-14","2021-03-15","2021-04-15","2021-05-15","2021-06-15",
                                  "2021-07-15","2021-08-15","2021-09-15","2021-10-15","2021-11-15","2021-12-15")))
  # figure out the dayOfyear for mid month.
  names(mth.date) <- c("mth", "DayOfYear")
  mth.date$DayOfYear <- as.Date(mth.date$DayOfYear, format="%Y-%m-%d")
  #mth.date$DateWords <- format(mth.date$DayOfYear, format="%d %B %Y")
  mth.date$DayOfYr <- as.numeric(mth.date$DayOfYear - as.Date("2020-01-01", format= "%Y-%m-%d"))
  
   # merging tables 
  df1 <- merge(x = df.sum, y = mth.date[,c("mth", "DayOfYr", "DayOfYear")], by.x = c("mth"), by.y = c("mth"), all.y = T)
  df2 <- merge(x = df1, y = prop.lkup, by.x = c("Region"), by.y = c("XID1"), all.y = T)
#write out table - this is used for property PGR plots in rShiny
  # order by Region and month to make it easier to read
  head(df2[order(df2[,"Region"],df2[,"mth"]),])
  
  # Round values to 0 decimal places
  df2[, 3:8] <- round(df2[, 3:8], 0)
  
  
  if(Prefix == "PGR"){
    print("writing PGR files")
    write.csv(df2[order(df2[,"Region"],df2[,"mth"]),], paste0(startDir, "/SummaryTable_timeseries_Props_PGR.csv"), row.names=FALSE)
    #saveRDS(df2[order(df2[,"Region"],df2[,"mth"]),], paste0(startDir, "/SummaryTable_timeseries_Props_PGR.rds"))
    #need to save to feather format too
  } else{
   print("Writing FOO files")
    write.csv(df2[order(df2[,"Region"],df2[,"mth"]),], paste0(startDir, "/SummaryTable_timeseries_Props_FOO.csv"), row.names=FALSE)
    #saveRDS(df2[order(df2[,"Region"],df2[,"mth"]),], paste0(startDir, "/SummaryTable_timeseries_Props_FOO.rds"))
  } # end writing file if
}  # end if