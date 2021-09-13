
# Plot up images as function for FOO
# 12 April 2021



#inputs:  rasStack=ras, layernum = input$layer, shpname=shape, 
#         rgn = event$id, RasterDate = rasterDate, Props = prop

plotFOOmaps <- function(rasStack, layernum, rgn, prop, TblDates){  
  #plotPGRmaps(ras, 11, "1264350", "X2021.03.17", Props, imageDatesTbl)
    #rasStack <- rasFOO
    #layernum <- "17-Mar" # coming in as NiceDate = 12-Jan
    #rgn <- c(1264350)
    #RasterDate <- as.Date("2021-03-17")
    #prop <- Props
    #TblDates <- imageDatesTbl
    
  par(xpd = FALSE)
  par(oma=c(0, 0, 0, 0), mar=c(1,1,1,1))
    
    # set breakpoints and colors for ranges of values - write function for this plotting.
    
    bins.foo <- c(0, 5, 10, 30, 50, 75, 100, 150, 255)
    foocolor <- c("#FF0000","#FFFF00","#66FF66","#33CC33","#008000","#005200", "#0000FF","#9649FF",alpha = 0.4)
    transp <-   c("#FF0000","#FFFF00","#66FF66","#33CC33","#008000","#005200", "#0000FF","#9649FF", alpha = 0 )
    foocolor[1] <- transp[1]
    
    ##############
    # FOO units are kg DM/ha
    
    # Now "layernum" is a date, need to look up number
    rastNum <- TblDates[TblDates$NiceDate == layernum, 1]
    e <- extent(prop[prop$PROPERTY_I == rgn,])  #adding 0.2 expands the extent equally in all directions (buffer)
    
    #### NEXT SECTION ADDS IMAGERY
        # location of imagery for plots of properties  
    #gdw  = rosm::as.tile_source("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}.jpg",
    #                            extension="jpg")
    
        # get tiles for this area
    #pbase <- osm.raster(prop[prop$PROPERTY_I == rgn,], crop=TRUE, zoomin= -3, type=gdw)
        #plot image, then "add=TRUE" to next plot, and adjust transparency (alpha)
    #plotRGB(pbase, ext=e*1.5)
    ############################
    
    # plot selected raster, 0.7 opacity (alpha)
    plot(rasStack[[as.numeric(rastNum)]], ext = e*1.1,
         breaks=bins.foo,col=foocolor,
         legend=FALSE,alpha=1,  asp=1, axes=F, box=T) #add=TRUE) # alpha=0.40  for semi transparent
    
    mtext(paste0("  ",TblDates[TblDates$NiceDate == layernum, "NiceDate"]),
          side = 3, adj=0, line = -2, cex=1.5)
    
    #mtext(citetext, side = 1, adj=0, line = 1, cex=0.8, col="darkgrey")
    #mtext(citetext, side = 1, cex=0.8, col="darkgrey")
    
    #### NEED NEXT 2 LINES FOR ESRI ####
    #citetext <- "Image source: Esri, Maxar, Earthstar Geographics, USDA FSA, USGS, Aerogrid, IGN, IGP, and the GIS User Community"
    #mtext(citetext, side = 1, adj=0.5, cex=0.8, col="darkgrey")
    ##################################

    # plot polygons
    #plot(prop,border="black", lwd=1, add=TRUE)
    plot(prop[prop$PROPERTY_I == rgn,],border="black", lwd=3, add=TRUE)
  
}

#  writing legend out as separate image. See other script.