# function for plotting timeseries at pixel after clicking on map
#  PGR
#  15-may-2021

#how to add manual legends to ggplot2
#https://community.rstudio.com/t/adding-manual-legend-to-ggplot2/41651/3

# read in tables for plotting

#yData <- yDat
#clickX <- input$fooPlot_click$x
#clickY <- input$fooPlot_click$y

#inData1 <- as.data.frame(cbind(imageDatesTbl, yData))

#yData <-  c(10,10,10,21,24,35, 40, 41, 42, 43, 70, 74, 75, 76, 81, 91)
#clickX <- 356
#clickY <- 623
  #plotPixel_fooTS(yData, clickX, clickY)

plotPixel_pgrTS <- function( yData, clickXY){ 
  #print(paste0( "This is clicks from pgrPlot: ",clickXY))
  
  #hide("help_text")
  
    inData1 <- as.data.frame(yData)
    colnames(inData1)[7] <- "Pasture.Growth"
    colnames(inData1)[1] <- "Week"
    inData1[,1] <- as.numeric(inData1[,1])


    #Set height of coordinates text
    coord.text.y = round_any(max(inData1$Pasture.Growth)+10,10,f=floor)
    
    if(is.numeric(clickXY)){
      coordLabel <- paste0("Location: ", round(clickXY[1], 3),", ", round(clickXY[2], 3))
    }else{
      coordLabel <- "<NA>"
    }
    
    
    pltTS <-   ggplot(data=inData1, 
                  aes(x=Week, y=Pasture.Growth)) + 
           geom_point(size = 3, color="orangered3")+
           geom_line(size = 1, color="orange")+
           labs(title="Current season pasture growth rate" ,
                x=NULL, y = "kg Dry Matter /ha/day")+
           annotate(geom="text", x=10, y=coord.text.y, col="black", fontface = "bold", 
                    label= coordLabel) +
           scale_x_continuous(limit = c(0, 53), breaks=c(1,5,9,13,17,22, 26, 30,35,39, 44,48),  
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"))+
           theme_bw()+
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 

    ggplotly(pltTS)
    
  }


