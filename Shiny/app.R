
# changed how observeEvents work - now nested events, and single Coords used and updated by pgr and foo plot clicks. 
# 24Sep2021

options("rgdal_show_exportToProj4_warnings"="none")  #suppressPackageStartupMessages()

library(raster) #
library(ncdf4)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal) #
library(ggplot2) 
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(plotly)
#library(rosm)  # this for plotting esri images
library(rgeos) #
library(dqshiny) #  drop down boxes
library(plyr) # i think ggplot needs this??

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

Props <- readRDS("./data/OtherData/Props_shape.rds")

# lists for selecting properties: script CODE_Property_PIC_list_formatting.R
propList <- readRDS("./data/OtherData/propList.rds")
picList <- readRDS("./data/OtherData/picList.rds")

# dates
imageDatesTbl <- readRDS("./data/DatesTable.rds")

#read brick - assign names as imageDatesTbl$rasterDate. netcdf files much faster. 
ras <- brick("./data/pgr.nc")
names(ras) <- imageDatesTbl$rasterDate
rasFOO <- brick("./data/foo.nc")
names(rasFOO) <- imageDatesTbl$rasterDate

# as feather
DatSummary <- as.data.frame(feather::read_feather("./data/SummaryTable_timeseries_Props_PGR.feather"))
ThisYearDat <- as.data.frame(feather::read_feather("./data/ThisYear_timeseries_Props_PGR.feather"))
OtherYears <- as.data.frame(feather::read_feather("./data/OtherYears_timeseries_Props_PGR.feather")) 

FooSummary <- as.data.frame(feather::read_feather("./data/SummaryTable_timeseries_Props_FOO.feather")) 
ThisYearFOO <- as.data.frame(feather::read_feather("./data/ThisYear_timeseries_Props_FOO.feather"))
OtherYearsFOO <- as.data.frame(feather::read_feather("./data/OtherYears_timeseries_Props_FOO.feather"))

# Rasters added to leaflet map:
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
                       plotOutput("rasPlot", click = "rasPlot_click", width = "100%", height=400), #withSpinner()
                       
                       #with shinyWidget
                       sliderTextInput(
                         inputId = "layer1", 
                         label = h4("Image dates in 2021 "), #h4(tags$b("2021 image dates: "))
                         grid = TRUE, 
                         force_edges = TRUE,
                         choices = c(imageDatesTbl$NiceDate),
                         selected=imageDatesTbl[nrow(imageDatesTbl), "NiceDate"],
                         animate=TRUE, animationOptions(
                           interval = 1000,
                           loop = FALSE,
                           playButton = TRUE,
                           pauseButton = TRUE
                         ))
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
                               width=12, height=450, id = "tabsetPGRgraph",
                               
                               tabPanel("Summary across whole property", value = "#panel1",
                                        plotlyOutput("RegionPlot", height = 350)
                               ),
                               tabPanel("Pixel values", status = "info", solidHeader = TRUE, value = "#panel2",
                                        # h5(tags$i("<You must click on a pixel in the map above to view data >")),
                                        box(width = 9,
                                            #h5("Pasture growth (weekly) for current year."),
                                            plotlyOutput("TSplot", height=350)), #withSpinner()
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
                       plotOutput("fooPlot", click = "fooPlot_click", width = "100%", height=400), #withSpinner()
                       
                       #with shinyWidget
                       sliderTextInput(
                         inputId = "layerfoo", 
                         label = h4("Image dates in 2021 "), #h4(tags$b("2021 image dates: "))
                         grid = TRUE, 
                         force_edges = TRUE,
                         choices = c(imageDatesTbl$NiceDate),
                         selected=imageDatesTbl[nrow(imageDatesTbl), "NiceDate"],
                         animate=TRUE, animationOptions(
                           interval = 1000,
                           loop = FALSE,
                           playButton = TRUE,
                           pauseButton = TRUE)
                         )
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
                                            plotlyOutput("TSplotFOO",height=350)), # withSpinner()
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
                   )
                   
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
      print(paste(c("this is map click ", v$data, "Zoom is ", input$map1_zoom)) )
      
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
          #setView(lng = selected$centLong, lat = selected$centLat, zoom = newzoom) %>%
          flyTo(lng = selected$centLong, lat = selected$centLat, zoom = newzoom,
                options=list(duration=1, easeLinearity=0.25)) %>%
          addPolygons(data = selected,
                      fill=FALSE, #fillColor = "yellow", fillOpacity = .6,
                      color = "#000000",
                      opacity = 1,
                      weight = 3,
                      stroke = T,
                      layerId = "Selected")
        }
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
                      layerId = "Selected")
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
                      layerId = "Selected")
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
 
  observeEvent(v$data[1], {
    # plot PGR map
    output$rasPlot <- renderPlot({
      req(v$data[1])
      plotPGRmaps(ras, input$layer1, v$data[1], Props, imageDatesTbl)
    })
    #plot foo map
    output$fooPlot <- renderPlot({
      req(v$data[1])
      plotFOOmaps(rasFOO, input$layerfoo, v$data[1], Props, imageDatesTbl)
    })
  
    #PGR When region selected, plot Region Seasonal patterns in bottom left graph. Uses static tables.
    output$RegionPlot <- renderPlotly({
      req(v$data[1])
      plotRegionSummary(DatSummary, ThisYearDat, OtherYears, v$data[1])
    }) 
  
    #FOO When region selected, plot Region Seasonal patterns in bottom left graph. Uses static tables.
    output$FOOGraph <- renderPlotly({
      req(v$data[1])
      plotRegionSummaryFOO(FooSummary, ThisYearFOO, OtherYearsFOO, v$data[1])
    })
  
    # if new property clicked, set focus back on first panel graph
    observeEvent(v$data[1],{
      updateTabsetPanel(session=getDefaultReactiveDomain(), "tabsetPGRgraph", selected = "#panel1")
      updateTabsetPanel(session=getDefaultReactiveDomain(), "tabsetfoo", selected = "#panel3") 
      Coords$data <- NA
    })
  
    # setting 1 coords value to use for pgr and foo maps
    Coords <- reactiveValues(data = NULL)
    
    #### Below works with pixel values on PGR map stack. ###
  
    observeEvent(input$rasPlot_click$x, {
      updateTabsetPanel(session=getDefaultReactiveDomain(), "tabsetPGRgraph", selected = "#panel2")
      
    Coords$data <- c(input$rasPlot_click$x, input$rasPlot_click$y)
    #print(Coords$data)

      value <- extract(ras,cellFromXY(ras,Coords$data))
      #print(value)
      
      # make table of values for the pixel (values through stack)
      output$df.pgr <- renderTable({
        #valdf <-  data.frame("Layer" = imageDatesTbl$NiceDate, "Value" = unlist(value())[1,])
        valdf <-  data.frame("Layer" = imageDatesTbl$NiceDate, "Value" = value[1,])
        colnames(valdf) <- c("Date","kg DM/ha/day")
        rownames(valdf)  <- 1:nlayers(ras)
        valdf
      }, digits=0)

      # PGR Time Series Plot
      output$TSplot <- renderPlotly({
        if(!is.null(value)){
          plotPixel_pgrTS(cbind(imageDatesTbl, value[1,]), Coords$data)
        }
      }) # end PGR graph plotting
     
    }) # end observeEvent click

    
    #### below works with FOO plotclick ####
    
    observeEvent(input$fooPlot_click$x, {
      updateTabsetPanel(session=getDefaultReactiveDomain(), "tabsetfoo", selected = "#panel4") 
      
      Coords$data <- c(input$fooPlot_click$x, input$fooPlot_click$y)
      #print(paste0("foo click ", Coords$data))
      
      valueFOO <- extract(rasFOO,cellFromXY(rasFOO,Coords$data))
      #print(valueFOO)
  
       # FOO make table of values for the pixel (values through stack)
      output$df.foo <- renderTable({
         if(!is.null(valueFOO)){
           fdf <-  data.frame("Layer" = imageDatesTbl$NiceDate, "Value" = valueFOO[1,])
           fdf$Value <- fdf$Value * 10
           colnames(fdf) <- c("Date","kg DM/ha")
           rownames(fdf)  <- 1:nlayers(rasFOO)
           fdf
          }
      }, digits=0)
  
      # FOO Time Series Plot
     output$TSplotFOO <- renderPlotly({
       if(!is.null(valueFOO)){
          plotPixel_fooTS(cbind(imageDatesTbl, 10*(valueFOO[1,])), Coords$data)
        }
      }) 
   }) # end observeEvent for Foo click
    
  }) # end observeEvent v[1] for all plots
  
  
  output$downloadData <- downloadHandler(
    filename = "How to use ShinyPastures.pdf",
    content = function(file) {
      file.copy("www/How to use ShinyPastures tool.pdf", file)
    }
  )
  
} # End server

###############################################
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")