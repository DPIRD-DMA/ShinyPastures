## new things to add into auto scripts - 
# 1. Feather format for tables
# 2 new matrix and rdata files to support extract from stack.  (BIG FILES). 

# 28 August 2021

#  saving files as feather format
library(feather)

setwd("C:/ALL_PROJECTS/PastFromSpace_DRAFT4/ShinyPastures/data")

FileList <- list.files(".", pattern=".rds$")


lapply(FileList, function(inFile){
  df <- readRDS(inFile)
  outName <- paste0(substr(inFile, 0, nchar(inFile) -4), ".feather")
  write_feather(df, outName)
})



# extract cell values quickly from big stack
#https://www.r-bloggers.com/2015/05/extract-values-from-numerous-rasters-in-less-time/

STACK <- ras
mat <- ff::ff(vmode="double",dim=c(ncell(STACK),nlayers(STACK)),filename="./data/RastStacks/PGR.stack.ffdata")

for(i in 1:nlayers(STACK)){
  mat[,i] <- STACK[[i]][]
}
save(mat,file="./data/RastStacks/PGR.stack.RData")

#  set up beforehand
ID_Raster <- raster(STACK[[1]])
ID_Raster[] <- 1:ncell(STACK[[1]])

# code in script - makes spatial point out of coords first
matpt <-as.data.frame(matrix(c(116.6674,-33.9029, 100), 1, 3, dimnames=list(c(1), c("lon", "lat", "val"))))
coordinates(matpt) <- ~lon+lat

ext_ID <- extract(ID_Raster,matpt)
ext2 <- mat[as.numeric(ext_ID),]


##
STACK <- rasFOO
matfoo <- ff::ff(vmode="double",dim=c(ncell(STACK),nlayers(STACK)),filename="./data/RastStacks/FOO.stack.ffdata")

for(i in 1:nlayers(STACK)){
  matfoo[,i] <- STACK[[i]][]
}
save(matfoo,file="./data/RastStacks/FOO.stack.RData")

