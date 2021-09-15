
# Plot up images as function for right hand map of PGR
# 3 march 2021

# 11 April - removing locality shapefile, and changing to give Props output

# 2-July-2021 - improving color map, adding esri imagery to plots

#inputs:  rasStack=ras, layernum = input$layer, shpname=shape, 
#         rgn = event$id, RasterDate = rasterDate, Props = prop

plotPGRmaps <- function(rasStack, layernum, rgn, prop, TblDates){  # newmarker
  #plotPGRmaps(ras, 11, "1264350", "X2021.03.17", Props, imageDatesTbl)
   #rasStack <- ras
  #  layernum <- "07-Sep" # coming in as NiceDate = 12-Jan
  #  rgn <- c(1264350)
  #  prop <- Props
  #  TblDates <- imageDatesTbl

    par(xpd = FALSE)
    par(oma=c(0, 0, 0, 0), mar=c(1,1,1,1))
    
    print(rasStack)
    
    # set breakpoints and colors for ranges of values - write function for this plotting.
    # using fixed legend (jpg) 
    
    bins.pgr <- c(0, 10, 20, 30, 40, 50, 60, 70, 85, 100, 120, 255)
    pcolor <- c("#fca4b7","#fcc89d","#fcfa4e","#79fc72","#00bdfc","#ad00fc", 
                "#d358db","#ff009d", "#ff0062", "#ff8119", "#ffae00", "#ffffff", alpha = 0.4 ) 
    transp <- c("#fca4b7","#fcc89d","#fcfa4e","#79fc72","#00bdfc","#ad00fc", 
                "#d358db","#ff009d", "#ff0062", "#ff8119", "#ffae00", "#ffffff", alpha = 0 )
    pcolor[1] <- transp[1]
    
    
    # Now "layernum" is a date, need to look up number
    rastNum <- TblDates[TblDates$NiceDate == layernum, 1]
    e <- extent(prop[prop$PROPERTY_I == rgn,])  #adding 0.2 expands the extent equally in all directions (buffer)
    
    ### THIS SECTION ADDS IMAGERY
       # location of imagery for plots of properties  
    #gdw  = rosm::as.tile_source("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}.jpg",
     #                           extension="jpg")
        # get tiles for this area
    #pbase <- osm.raster(prop[prop$PROPERTY_I == rgn,], crop=TRUE, zoomin= -3, type=gdw)
    #plot image, then "add=TRUE" to next plot, and adjust transparency (alpha) 
    #plotRGB(pbase, ext=e*1.5)
    #############################
    
    # plot selected raster, 0.7 opacity (alpha)
    plot(rasStack[[as.numeric(rastNum)]], ext = e*1.1,
         breaks=bins.pgr,col=pcolor, legend=FALSE,alpha=1,  asp=1, axes=F, box=T) # add=TRUE) # alpha=0.40  for semi transparent
    
    mtext(paste0("  ",TblDates[TblDates$NiceDate == layernum, "NiceDate"]),
          side = 3, adj=0, line = -2, cex=1.5)
    
    #mtext(citetext, side = 1, adj=0, line = 1, cex=0.8, col="darkgrey")
    #mtext(citetext, side = 1, cex=0.8, col="darkgrey")
    
    #### NEED NEXT 2 LINES FOR ESRI  ####
    #citetext <- "Image source: Esri, Maxar, Earthstar Geographics, USDA FSA, USGS, Aerogrid, IGN, IGP, and the GIS User Community"
    #mtext(citetext, side = 1, adj=0.5, cex=0.8, col="darkgrey")
    #############
    
    # plot polygons
    #plot(prop,border="black", lwd=1, add=TRUE)
    plot(prop[prop$PROPERTY_I == rgn,],border="black", lwd=3, add=TRUE)
    
} # end of function
    

  
  