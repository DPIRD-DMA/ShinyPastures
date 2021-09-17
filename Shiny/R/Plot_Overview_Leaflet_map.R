

# function to make a leaflet map of the area with region polygons
# only input is shapefile of polygons, with "NAME" field
# flag it TRUE or FALSE, indicating if map has been clicked. If NOT, then draw Arthur river. 
# 3 March 2021
#10-April:  changing to use property shapefile.  
  # Arthur river zoom is about 8. 

# Leaflet map, with Esri imagery

LeafletPlot <- function(propname, rPGR, rFOO){
  
   #these will always be the most recent date.  Moving to new folder, with projection pre-done.  
  #latestPGR <- list.files("./data/Maps_for_shiny_Mercator/", pattern="^PGR", full.names=TRUE)
  #latestFOO <- list.files("./data/Maps_for_shiny_Mercator/", pattern="^FOO", full.names=TRUE)
  # rPGR <- raster(latestPGR)
  # rFOO <- raster(latestFOO)
   #rPGR <- ras[[nlayers(ras)]]
   #rFOO <- rasFOO[[nlayers(rasFOO)]]
   #propname <- Props
   #propname <- readRDS("./data/OtherData_dd/Props_shape.rds") #dd
   #propname <- propname[,c("PROPERTY_I","PROPERTY_N", "PicLabel","centLong","centLat")]
   #propname <- readRDS("./data/OtherData/Props_shape.rds") #mercator - doesn't work in leaflet
   #propname <- readRDS("./data/OtherData/Props_sf.rds") #dd  doesn't seem faster than sp object
   
     # need these for checkboxes to turn on maps
   rpgrName <- paste0("Pasture growth  ", format(as.Date(substr(names(rPGR), 4, 11), "%Y%m%d" ), "%d-%b-%y") )
   rfooName <- paste0("Feed on offer ", format(as.Date(substr(names(rFOO), 4, 11), "%Y%m%d" ), "%d-%b-%y") )
   
     # color palettes
   bins.pgr <- c(0, 10, 20, 30, 40, 50, 60, 70, 85, 100, 120, 254)
   pcolor <- colorBin(c("#fca4b7","#fcc89d","#fcfa4e","#79fc72","#00bdfc","#ad00fc", 
                        "#d358db","#ff009d", "#ff0062", "#ff8119", "#ffae00" ), 
                      domain = NULL, bins = bins.pgr, na.color = "#00000000")
 
   bins.foo <- c(0, 5, 10, 30, 50, 75, 100, 150, 256)
   foocolor <- colorBin(c("#FF0000","#FFFF00","#66FF66","#33CC33","#008000","#005200", 
                         "#0000FF","#9649FF"), domain = NULL, bins = bins.foo, na.color = "#00000000")
   

  #i <- function(a, b) {
  #  if(missing(a)) print("Missing A") 
  #}

    #if(missing(shp_selected)) {
    # print("Doesn't exist")
      #print(shp_selected)
#leaf <-  
  leaflet() %>% 
      setView(lat = -32.33, lng = 117.51, zoom = 7) %>%
      addProviderTiles("OpenStreetMap.HOT", group = "Street map")%>% #"Esri.WorldImagery" "Esri.WorldStreetMap" "Esri.DeLorme" "OpenTopoMap"
      addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% #"Esri.WorldImagery" "Esri.WorldStreetMap" "Esri.DeLorme" "OpenTopoMap"
      addProviderTiles("OpenTopoMap", group = "Topo map")%>% #"Esri.WorldImagery" "Esri.WorldStreetMap" "Esri.DeLorme" "OpenTopoMap"
      #inset map showing location
      addMiniMap(
        tiles = providers$OpenStreetMap.HOT,
        toggleDisplay = TRUE, zoomLevelOffset=-5) %>%
      # Button to zoom to broader scale.
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 8",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      #button to zoom to location
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    # scale bar
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric=TRUE, imperial = FALSE)) %>%
    
     # add PGR raster
      addRasterImage(rPGR, color=pcolor, opacity = 0.7,method="ngb", group=rpgrName, project=FALSE) %>%     #colors = "Spectral", LayerID=rasLeafClick, 
      addLegend("topright", title="kg DM/ha/day",
                colors = c("#fca4b7","#fcc89d","#fcfa4e","#79fc72","#00bdfc","#ad00fc", "#d358db","#ff009d", "#ff0062", "#ff8119", "#ffae00"), 
               labels= c("0 - 10", "10 - 20","20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 85", "85 - 100","100 - 120", " > 120"),
                group=rpgrName) %>%
      # add foo raster
      addRasterImage(rFOO, color=foocolor, opacity = 0.7,method="ngb", group=rfooName, project = FALSE) %>%     #colors = "Spectral", 
      addLegend("topright", title="kg DM/ha", colors = c("#FF0000","#FFFF00","#66FF66","#33CC33","#008000","#005200", 
                                       "#0000FF","#9649FF"), 
              labels= c("0 - 50", "50 - 100","100 - 300", "300 - 500", "500 - 750", "750 - 1000", "1000 - 1500", " > 1500"),
              group=rfooName) %>%
    
      addLayersControl(baseGroups = c("Street map", "Satellite", "Topo map"),
                       overlayGroups = c(rpgrName, rfooName), 
                       options = layersControlOptions(collapsed = FALSE))%>%
      hideGroup(c(rpgrName, rfooName)) %>%  #this makes default 'uncheck' 
  
    # add property polygons
    addPolygons(data = propname,   
                weight = 1,
                #color = "black",
                color = "#708090",  #slate grey
                smoothFactor = 0.3,
                fillOpacity = 0.0,
                highlight = highlightOptions(
                  weight = 3,
                  #color = "#ffea03", # bright yellow
                  color = "#FF4500",  # orange red
                  fillOpacity = 0.0,
                 bringToFront = TRUE),
               layerId = ~PROPERTY_I,
                #popup = paste("Property ID: ", propname$PROPERTY_I,"<br>",
                #              "Property Name ", propname$PROPERTY_N, "<br>",
                #              "PIC: ", propname$PIC, "<br>"),
               group= "Property view") %>% 
    groupOptions("Property view", zoomLevels = 10:20)
    
} # END FUNCTION

