
# 16-April21 - added selection boxes 
#  25 May - changing all selection properties, to get rid of "select" buttons
#        - also removing tool tips, given it is shown in box on the side. I think this is more important  
#        - because you can see it from the other tabs
options("rgdal_show_exportToProj4_warnings"="none") 
#suppressPackageStartupMessages()

library(raster)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(ggplot2)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(plotly)
#library(rosm)  # this for plotting esri images
library(rgeos)
library(dqshiny)
library(plyr)

#need?
#library(pracma) 
#library(ptw)
#library(mgcv)

source("./R/Plot_Overview_Leaflet_map.R")
source("./R/Plot_PGR_maps.R")
source("./R/Plot_FOO_maps.R")
source("./R/Plot_Region_Summary.R")
source("./R/Plot_Region_Summary_FOO.R")
source("./R/Plot_Pixels_FOO.R")
source("./R/Plot_Pixels_pgr.R")
source("./R/Calc_interpolate_dates.R")

##################################################################################
#### Read in datasets and prep   ################
# property shapefiles: some formatting done in script CODE_Property_PIC_list_formatting.R
#Props <- readOGR("./data/OtherData", 
#                 layer = "Props_for_shiny_16Aug2021" )
Props <- readRDS("./data/OtherData/Props_shape.rds")
# lists for selecting properties: script CODE_Property_PIC_list_formatting.R
propList <- readRDS("./data/OtherData/propList.rds")
picList <- readRDS("./data/OtherData/picList.rds")

####  RASTERS  ####
# PGR
rasterPath <- "./data/PGR_for_shiny"
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
rasterPathFOO <- "./data/FOO_for_shiny"

#find raster names, read into a stack
rasterListFOO <- list.files(rasterPathFOO, pattern=".tif$", full.names=T)
rasFOO <- stack(rasterListFOO)
names(rasFOO)<- rasterDate

#### read in summary tables for plotting

#DatSummary <- read.csv("./data/SummaryTable_timeseries_Props_PGR.csv")
#ThisYearDat <- read.csv("./data/ThisYear_timeseries_Props_PGR.csv")
#OtherYears <- read.csv("./data/OtherYears_timeseries_Props_PGR.csv") 

#FooSummary <- read.csv("./data/SummaryTable_timeseries_Props_FOO.csv") #updated 9-June-2021 to include all years
#ThisYearFOO <- read.csv("./data/ThisYear_timeseries_Props_FOO.csv")
#OtherYearsFOO <- read.csv("./data/OtherYears_timeseries_Props_FOO.csv")# same

#AS RDS files
#DatSummary <- readRDS("./data/SummaryTable_timeseries_Props_PGR.rds")
#ThisYearDat <- readRDS("./data/ThisYear_timeseries_Props_PGR.rds")
#OtherYears <- readRDS("./data/OtherYears_timeseries_Props_PGR.rds") 

#FooSummary <- readRDS("./data/SummaryTable_timeseries_Props_FOO.rds") 
#ThisYearFOO <- readRDS("./data/ThisYear_timeseries_Props_FOO.rds")
#OtherYearsFOO <- readRDS("./data/OtherYears_timeseries_Props_FOO.rds")

# as feather
DatSummary <- as.data.frame(feather::read_feather("./data/SummaryTable_timeseries_Props_PGR.feather"))
ThisYearDat <- as.data.frame(feather::read_feather("./data/ThisYear_timeseries_Props_PGR.feather"))
OtherYears <- as.data.frame(feather::read_feather("./data/OtherYears_timeseries_Props_PGR.feather")) 

FooSummary <- as.data.frame(feather::read_feather("./data/SummaryTable_timeseries_Props_FOO.feather")) 
ThisYearFOO <- as.data.frame(feather::read_feather("./data/ThisYear_timeseries_Props_FOO.feather"))
OtherYearsFOO <- as.data.frame(feather::read_feather("./data/OtherYears_timeseries_Props_FOO.feather"))

#load(file="./data/inPolys.rda")
#load(file="./data/inRasters.rda")

# teh rda files don't seem to work well for rasters or stack
#load(file="./data/Maps_for_shiny_Mercator/leafMaps.rda") #pR1 and fR1
latestPGR <- list.files("./data/Maps_for_shiny_Mercator/", pattern="^PGR", full.names=TRUE)
latestFOO <- list.files("./data/Maps_for_shiny_Mercator/", pattern="^FOO", full.names=TRUE)
 pR1 <- raster(latestPGR) #in mercator for leaflet
 fR1 <- raster(latestFOO)

#################################################################################################

# Define UI 
ui<-function(request){
  
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Pasture Production",
      titleWidth = 450
    ),
    
    
    dashboardSidebar(useShinyjs(),
                     tags$a(href='https://www.agric.wa.gov.au/sheep/understanding-pastures-space-south-west-western-australia',
                            #tags$img(src='DPIRD_white.jpg'), style = 'position:absolute; top:65px; left:10px;' ),
                            tags$img(src='DPIRD-logo-white2.png'), style = 'position:absolute; top:65px;left:10px;' ),
                     
                     absolutePanel(style = 'position:absolute; top:200px; left:1px; right:1px',
                                   
                                   #h4(tags$b("  Select 3 ways:", style= 'margin-left: 20px;')),
                                   br(),
                                   autocomplete_input("auto2", "PIC / Property", picList$PICPID, max_options = 1000, placeholder="Type your PIC (WXXX1234)"),
                                   autocomplete_input("auto1", "Property", propList, max_options = 1000, placeholder="Type your Property ID"),
                                   br(),
                                   br(),
                                   div(id='text_div', verbatimTextOutput("text"), style = 'margin-left: 15px;width:200px;'),
                                   tags$style(type='text/css', '#text{background-color: #fc9a44; white-space: pre-wrap;word-break: keep-all;color: black; font-size:100%}'), #pre-wrap;  nowrap gives slider 
                                   bookmarkButton(label="Bookmark...", title="Bookmark a property and get the url to go directly there next time.")
                     ), # end absolute panel
                     # adding this to see if it will get rid of crashes
                     tags$head(
                       HTML(
                         "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
                       )
                     )
    ), # end sidebar
    
    
    dashboardBody(
      fluidRow(
        #### THIS IS THE BIT TO FIXUP TO CHANGE TAB COLORS ###
        #tags$style(".nav-tabs {
        #        background-color: #006747;}
        #       .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
        #     background-color: transparent;border-color: transparent;}
        #       .nav-tabs-custom .nav-tabs li.active {border-top-color: #FFF;}"),
        tags$style("li a {font-size: 14px;font-weight: bold;}",
                   ".nav-tabs {background: #C0C0C0;}
                .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {background-color: #fff;
                                               border-color: #fff;                                                      
                                               }
                .nav-tabs-custom .nav-tabs li.active {border-top-color: 
                                                      #ffa500; 
                                                      border-top-width:thick
                                                      }",
                   type = 'text/css'," 
                .irs-grid-text {font-size: 11pt !important; transform: rotate(-45deg) translate(-10px);}  # text for sliderbar dates
                "
                   
                   
        ),      
        tabBox(
          width=800, height=700,id = "tabsetAll",
          tabPanel("Select location", status = "warning", solidHeader = TRUE,
                   h5(tags$i("< Zoom in to see property boundaries and click to select >")),  
                   #tags$style("li a {font-size: 18px;font-weight: bold;}"),
                   withSpinner(leafletOutput("map1", width = "100%", height=850))
          ),
          tabPanel("Pasture growth",
                   fluidRow(
                     box( 
                       height = 580, width=10,
                       title = "Growth this year", status = "warning", solidHeader = TRUE,
                       withSpinner(plotOutput("rasPlot", click = "rasPlot_click", width = "100%", height=400)),
                       
                       #with shinyWidget
                       sliderTextInput(
                         inputId = "layer1", 
                         label = h4("Image dates in 2021 "), #h4(tags$b("2021 image dates: "))
                         grid = TRUE, 
                         force_edges = TRUE,
                         choices = c(imageDatesTbl$NiceDate),
                         selected=imageDatesTbl[nrow(imageDatesTbl), "NiceDate"])
                     ),
                     box(height = 580, width=2,
                         tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
                         div(img(src="PGR_color_legend.jpg", style="width: 130px"), style = 'position:absolute; top:140px; left:-30px'  ),
                         h5(tags$i("1 pixel = 250m x 250m" ,style = 'position:absolute; top:400px; left:-20px' )),
                         h5(tags$i("= ~ 6ha", style = 'position:absolute; top:416px; left:19px' ))
                     )
                   ),
                   fluidRow( align="center",
                             useShinyjs(),
                             h5(tags$i("< Click on a map pixel to see values since 1-January in tab below. >")),
                             tabBox(
                               width=12, height=450,
                               title = "", id = "tabsetPGRgraph",
                               
                               tabPanel("Summary across whole property", value = "#panel1",
                                        plotlyOutput("RegionPlot", height = 350)
                               ),
                               tabPanel("Pixel values", status = "info", solidHeader = TRUE, value = "#panel2",
                                        # h5(tags$i("<You must click on a pixel in the map above to view data >")),
                                        box(width = 9,
                                            #h5("Pasture growth (weekly) for current year."),
                                            withSpinner(plotlyOutput("TSplot", height=350))),
                                        box(width = 3,
                                            tableOutput("df.pgr")
                                        )
                               )
                             ) # end tabBox 
                   )# end fluidRow
          ), 
          tabPanel("Feed on offer",
                   fluidRow(
                     box( 
                       height = 580, width=10,
                       title = "Feed on offer this year", status = "warning", solidHeader = TRUE,
                       #div(img(src="FOO_color_legend.jpg", style="width: 90px"), style="float:right;"  ),  #style="float:right;"   style = 'position:absolute; right:1px;'
                       withSpinner(plotOutput("fooPlot", click = "fooPlot_click", width = "100%", height=400)),
                       
                       #with shinyWidget
                       sliderTextInput(
                         inputId = "layerfoo", 
                         label = h4("Image dates in 2021 "), #h4(tags$b("2021 image dates: "))
                         grid = TRUE, 
                         force_edges = TRUE,
                         choices = c(imageDatesTbl$NiceDate),
                         selected=imageDatesTbl[nrow(imageDatesTbl), "NiceDate"])
                       #verbatimTextOutput(outputId = "rngSlider") # prints value in box
                     ),
                     box(height = 200, width=2, 
                         tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
                         div(img(src="FOO_color_legend.jpg", style="width: 130px"), style = 'position:absolute; top:170px; left:-20px' ), #style="float:right;"
                         h5(tags$i("1 pixel = 250m x 250m" ,style = 'position:absolute; top:400px; left:-20px' )),
                         h5(tags$i("= ~ 6ha", style = 'position:absolute; top:416px; left:19px' ))
                     )
                   ), # end Fluidrow
                   
                   fluidRow( align="center",
                             h5(tags$i("< Click on a map pixel to see values since 1-January in tab below. >")),
                             
                             tabBox(
                               width=12, height=450, id = "tabsetfoo",
                               
                               tabPanel("Summary across whole property", value = "#panel3",
                                        plotlyOutput("FOOGraph", height = 350)
                               ),
                               tabPanel("Pixel values", status = "info", solidHeader = TRUE, value = "#panel4",
                                        #h5(tags$i("<You must click on a pixel in the map above to view data >")),
                                        box(width = 9,
                                            #h5("Pasture growth (weekly) for current year."),
                                            withSpinner(plotlyOutput("TSplotFOO",height=350 ))),
                                        box(width = 3,
                                            tableOutput("df.foo")
                                        )
                               )
                             ) # end tabBox
                   ) # end fluidrow
          ), # END tabPanel FOO
          tabPanel("More information",
                   fluidRow(
                     tags$style("li a {font-size: 18px;font-weight: bold;}"),
                     tags$style(
                       ".block {
                          border-color: #00000;
                          border-style: solid;
                          #background-color: lightblue;
                          text-align: center;
                          margin: 100px;
                          min-height: 100px;
                          width: 400px;
                         }
                         "
                     ),
                     div(
                       class = "block",
                       #"Block 1",
                       br(),
                       span("This site is designed to supply Pastures From Space information at the property level."), 
                       span("Download a helpfile on site features here:"),
                       downloadLink("downloadData", 
                                    label = h4("Download"))
                     )
                   ),
                   
                   #h5(tags$i("< Zoom in to see property boundaries and click to select >")),  
                   
                   #h4("  This site is designed to supply Pastures From Space information at the property level. "),
                   #h4("  Download a helpfile on site features here: "),
                   
          ) # END tabpanel INFO
        ) # end main tabBox
      ) # end fluidRow that holds main box with everything in it. 
    ) # end dashboardBody
  ) # end Page
}  # end of ui function
#####################################################################
# Define server logic 

server <- function(input, output, session) {
  
  # Leaflet map, with Esri imagery, calling as function
  output$map1 <- renderLeaflet({
    LeafletPlot(Props, pR1, fR1) 
    #  LeafletPlot(Props, ras[[nlayers(ras)]], rasFOO[[nlayers(rasFOO)]]) 
  })
  
  proxy <- leafletProxy("map1")
  
  #  Bookmarks - url
  onBookmark(function(state) {
    state$values$currentPID <- v$data 
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    v$data  <- state$values$currentPID
  })
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$
  #select PID for all plots
  v <- reactiveValues(data = NULL)
  
  ### Get v from Leaf Map Click: useClick
  # and add highlighted polygon on click
  observeEvent(input$map1_shape_click, { 
    if(input$map1_shape_click[1] != ""){
      
      v$data <-input$map1_shape_click
      #print(paste(c("this is map click ", v$data, "Zoom is ", input$map1_zoom)) )
      #show('text-div')
      
      #if(is.null(v$data[1]))
      # return()
      
      #Select the polygon based on the PID (in v$data[1])
      selected <- Props[Props@data$PROPERTY_I == v$data[1],]
      #print(selected )
      
      newzoom <- if(input$map1_zoom > 10){
        newzoom <- input$map1_zoom
      } else{
        newzoom <- 10
      } 
      
      #change style upon click event
      if(v$data[1] == "Selected"){
        proxy %>% removeShape(layerId = "Selected") 
      } else {
        proxy %>%
          setView(lng = selected$centLong, lat = selected$centLat, zoom = newzoom) %>%
          addPolygons(data = selected,
                      fill=FALSE, #fillColor = "yellow", fillOpacity = .6,
                      color = "#000000",
                      opacity = 1,
                      weight = 3,
                      stroke = T,
                      layerId = "Selected")}
    } # end of what to do if no click ""
  }) # end obs useClick
  
  ##
  # PID select box
  observeEvent(input$auto1, {  #usePID
    if(input$auto1!=""){
      v$data <- as.integer(input$auto1) #added as.integer for autocomplete trial
      #print(paste("this is pickPID ", v$data, "Zoom is ", input$map1_zoom))
      #show('text-div')
      
      #subset countries by click point
      selected <- Props[Props@data$PROPERTY_I == v$data[1],] 
      #print(selected)
      
      newzoom <- if(input$map1_zoom > 10){
        newzoom <- input$map1_zoom
      } else{
        newzoom <- 10
      } 
      
      #change style upon changing selection
      if(v$data[1] == "Selected"){
        proxy %>% removeShape(layerId = "Selected")
      } else {
        proxy %>%
          setView(lng = selected$centLong, lat = selected$centLat, zoom = newzoom) %>%
          addPolygons(data = selected,
                      fill=FALSE, #fillColor = "yellow", fillOpacity = .6,
                      color = "#000000",
                      opacity = 1,
                      weight = 3,
                      stroke = T,
                      layerId = "Selected")#%>%
      }
    } # end of "if input ""
  })  # end obs - usePID
  
  #Pic select box   #usePIC
  observeEvent(input$auto2, {
    if(input$auto2!=""){
      v$data <- picList[picList$PICPID == input$auto2, "PROPERTY_ID"]
      #print(paste("this is pickPIC ", v$data, "Zoom is ", input$map1_zoom))
      #show('text-div')
      
      #subset countries by click point
      selected <- Props[Props@data$PROPERTY_I == v$data[1],]
      #print(selected )
      
      newzoom <- if(input$map1_zoom > 10){
        newzoom <- input$map1_zoom
      } else{
        newzoom <- 10
      } 
      
      #change style upon click event
      if(v$data[1] == "Selected"){
        proxy %>% removeShape(layerId = "Selected")
      } else {
        proxy %>%
          setView(lng = selected$centLong, lat = selected$centLat, zoom = newzoom) %>%
          addPolygons(data = selected,
                      fill=FALSE, #fillColor = "yellow",fillOpacity = .6,
                      color = "#000000",
                      opacity = 1,
                      weight = 3,
                      layerId = "Selected") #%>%
      }
    } #end check for empty auto2 ""
  }) # end Obs - usePIC 
  
  # this is the orange box in the sidebar
  output$text <- renderText({
    req(v$data[1])
    paste0("Property: ", v$data[1], 
           "\n     PIC: ", Props@data[Props@data$PROPERTY_I == v$data[1], "PicLabel"],
           "\n    Name: ", Props@data[Props@data$PROPERTY_I == v$data[1], "PROPERTY_N"])
  }) 
  
  # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # use v$data for all raster plots 
  
  output$rasPlot <- renderPlot({
    req(v$data[1])
    plotPGRmaps(ras, input$layer1, v$data[1], Props, imageDatesTbl)
  })
  
  output$fooPlot <- renderPlot({
    req(v$data[1])
    plotFOOmaps(rasFOO, input$layerfoo, v$data[1], Props, imageDatesTbl)
  })
  
  #PGR When region selected, plot Region Seasonal patterns in bottom left graph. Uses static tables.
  output$RegionPlot <- renderPlotly({
    req(v$data[1])
    plotRegionSummary(DatSummary, ThisYearDat, OtherYears, v$data[1])
    #}
  }) 
  
  #FOO When region selected, plot Region Seasonal patterns in bottom left graph. Uses static tables.
  output$FOOGraph <- renderPlotly({
    req(v$data[1])
    plotRegionSummaryFOO(FooSummary, ThisYearFOO, OtherYearsFOO, v$data[1])
  })
  
  #### Below works with pixel values on PGR map stack. ### 
  # Gets click location coordinates
  Coords <- reactive({
    req(input$rasPlot_click$x)
    c(input$rasPlot_click$x, input$rasPlot_click$y)
  })
  
  observeEvent(input$rasPlot_click$x, {
    #print(paste("old coords: ", Coords()))
    # this activates the 'pixel' panel when plot is clicked
    updateTabsetPanel(session=getDefaultReactiveDomain(), "tabsetPGRgraph", selected = "#panel2") 
  })
  
  # click on PGR raster stack returns 'value' - this is a list of raster values extracted at click location (Coords).  
  value  <- eventReactive(input$rasPlot_click$x,{
    req(input$rasPlot_click$x)
    extract(ras,cellFromXY(ras,Coords()))
  })
  
  # make table of values for the pixel (values through stack)
  output$df.pgr <- renderTable({
    req(input$rasPlot_click$x)
    valdf <-  data.frame("Layer" = imageDatesTbl$NiceDate, "Value" = unlist(value())[1,])
    #valdf <-  data.frame("Layer" = imageDatesTbl$NiceDate, "Value" = value()[1,])
    colnames(valdf) <- c("Date","kg DM/ha/day")
    rownames(valdf)  <- 1:nlayers(ras)
    valdf
  }, digits=0)
  
  
  #output$help_text <- renderUI({
  #  HTML( "<b>Click on the map above to get the pixel values since 1-January.</b>")
  #})
  
  # PGR Time Series Plot
  output$TSplot <- renderPlotly({
    req(input$rasPlot_click$x)
    plotPixel_pgrTS(cbind(imageDatesTbl, unlist(value())[1,]), input$rasPlot_click$x, input$rasPlot_click$y)
  }) # end PGR graph plotting
  
  
  #### below works with FOO ####
  observeEvent(input$fooPlot_click$x, {
    req(input$fooPlot_click$x)
    #     print(paste("FOOmap coords: ", input$fooPlot_click$x, input$fooPlot_click$y))
    ###      # on click, activate 'pixel' table
    updateTabsetPanel(session=getDefaultReactiveDomain(), "tabsetfoo", selected = "#panel4") 
  })
  
  # get the click coordinates
  CoordsFOO <- reactive({
    req(input$fooPlot_click$x)
    c(input$fooPlot_click$x, input$fooPlot_click$y)
  }) 
  
  # click on FOO raster stack returns 'value'
  valueFOO  <- eventReactive(input$fooPlot_click$x,{
    req(input$fooPlot_click$x)
    extract(rasFOO,cellFromXY(rasFOO,CoordsFOO()))
  })
  
  # FOO make table of values for the pixel (values through stack)
  output$df.foo <- renderTable({
    req(input$fooPlot_click$x)
    fdf <-  data.frame("Layer" = imageDatesTbl$NiceDate, "Value" = unlist(valueFOO())[1,])
    fdf$Value <- fdf$Value * 10
    #fdf <-  data.frame("Layer" = imageDatesTbl$NiceDate, "Value" = valueFOO()[1,])
    colnames(fdf) <- c("Date","kg DM/ha")
    rownames(fdf)  <- 1:nlayers(rasFOO)
    fdf
  }, digits=0)
  
  # FOO Time Series Plot
  output$TSplotFOO <- renderPlotly({
    req(input$fooPlot_click$x)
    plotPixel_fooTS(cbind(imageDatesTbl, 10*(unlist(valueFOO())[1,])), input$fooPlot_click$x, input$fooPlot_click$y)
  }) 
  
  output$downloadData <- downloadHandler(
    filename = "How to use ShinyPastures.pdf",
    content = function(file) {
      file.copy("www/How to use ShinyPastures tool.pdf", file)
    }
  )
  
  #writing values to the console for debugging
  #observeEvent(valueFOO(), {
  #  print(paste(c("FOO pix ", 10*valueFOO()[1,] )) )
  #    print(fdf) 
  #    print(paste(c(input$fooPlot_click$x, input$fooPlot_click$y) ))
  #  })
  
  
  #  observe({
  #    reactiveValuesToList(input)
  #    session$doBookmark()
  #  })
  #  onBookmarked(updateQueryString)
} # End server

###############################################
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")