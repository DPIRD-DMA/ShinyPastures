#Edited for plotting FOO timeseries summaries  12-April-2021

# function to plot historical PGR data.  "region" is from click on left map.
# this assumes all column names are standard, etc. 
#3-March-2021

# read in tables for plotting

#inTable <- as.data.frame(feather::read_feather("./data/SummaryTable_timeseries_Props_FOO.feather"))
#inThisYear <- as.data.frame(feather::read_feather("./data/ThisYear_timeseries_Props_FOO.feather"))
#OtherYears <- as.data.frame(feather::read_feather("./data/OtherYears_timeseries_Props_FOO.feather"))
#PropID <- 1351810   

#inTable <- read.csv("C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Data_Prep/FOO/SummaryTable_timeseries_Props_FOO_x10_ALLYEARS.csv")
#OtherYears <- read.csv("C:/ALL_PROJECTS/PastFromSpace_DRAFT2/Data_Prep/FOO/OtherYears_timeseries_Props_FOO_x10_ALLYEARS.csv")

#ListOfProps <- namesDF

#$$$$$$$$$$$$$$$$$$$$$$$$$
plotRegionSummaryFOO <- function(inTable, inThisYear, OtherYears, PropID){  #event$id


      inData1 <- inTable[inTable$PID == PropID,]
    inData2 <- inThisYear[inThisYear$PID == PropID,]
    inOtherYears <- OtherYears[OtherYears$PID == PropID,]
    
    inData1$DayOfYear <- as.Date(inData1$DayOfYear)
    
    #this should be changed in original datatables
    #inData1$DayOfYear <- as.Date(inData1$DayOfYr, origin="2021-01-01")
    #inData2$DayOfYear <- as.Date(inData2$DayOfYr, origin="2021-01-01")
    #inOtherYears$DayOfYear <- as.Date(inOtherYears$DayOfYr, origin="2021-01-01")
    
    
    # sort out current date and most similar past year
    #this year's day and Mean
    dayTarget <- inData2[nrow(inData2),c("DayOfYr")]
    mnTarget <- inData2[nrow(inData2),c("Mean")]
    #set target date to be +/- 10 days from most current image
    targetDate <- c(dayTarget -10, dayTarget +10)
    #make a small table of all Means in correct timeframe
    newdata <- subset(inOtherYears, DayOfYr > targetDate[1] & DayOfYr < targetDate[2],
                      select=c(DayOfYear, yr, Mean))
    
    # i want 3 closest years
    #olist <- order(abs(newdata$Mean))
    # order the rows starting at closest value to ThisYear today's date.
    newdata <- newdata[order(abs(newdata$Mean - mnTarget)),]
    unik <- !duplicated(newdata[,c( "yr")])  ## logical vector of unique values
    #seq_along(tmp[,c("yr")])[unik]  ## indices
    #get list of unique years, still in order of closest to today
    topyrs <- newdata[unik,] ## the values

        # if there are more than 3, cap it at 3.  
    #if(nrow(topyrs) > 3) topyrs <- topyrs[1:3,]
    if(nrow(topyrs) > 1) topyrs <- topyrs[1,]
    
    # This is the final dataset for our "analogue" years (closest 3)
    # get data from "inOtherYears" for teh selected years and start date. 
    #tmpOtherYr <- inOtherYears[inOtherYears$yr %in% topyrs$yr, ]
    #tmpOtherYr <- tmpOtherYr[tmpOtherYr$DayOfYr >= min(topyrs$DayOfYr), ]
    
    tmpYr1 <- inOtherYears[inOtherYears$yr == topyrs$yr[1],]
    tmpYr1 <- tmpYr1[tmpYr1$DayOfYear >= min(topyrs[1, 'DayOfYear']), ]
    tmpYr1 <- tmpYr1[tmpYr1$DayOfYear < as.Date("2022-01-01"),]
    #tmpYr2 <- inOtherYears[inOtherYears$yr == topyrs$yr[2],]
    #tmpYr2 <- tmpYr2[tmpYr2$DayOfYr >= min(topyrs[2, 'DayOfYr']), ]
    
    #tmpYr3 <- inOtherYears[inOtherYears$yr == topyrs$yr[3],]
    #tmpYr3 <- tmpYr3[tmpYr3$DayOfYr >= min(topyrs[3, 'DayOfYr']), ]
    
    tmpYr1$DayOfYear <- as.Date(paste0("2021-",substr(tmpYr1$DayOfYear, 6, nchar(tmpYr1$DayOfYear)) ))
    # List dates of images in ThisYear and inOtherYears analogue (tmpYr1)
    
    DatesList <- unique(c(as.Date(inData2$DayOfYear), tmpYr1$DayOfYear))
 
    
    # now send to new function to interpolate values to use in graphs
    InterpTable <- InterpTimeSeries(inData1, DatesList )
    #print(InterpTable)
    
     ####  Plotly 
    # start with bottom line 
    fig <-plot_ly(InterpTable, x = ~DayOfYear, y = ~q10, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgba(80,80,80,0.4)'),
                  showlegend = T, name = '10%', hoverinfo =paste("name", "y"))
    # add next line, and fill down to bottom line (so yellow line, grey fill)
    fig <- fig %>% add_trace(y = ~InterpTable$q25, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(100,100,100,0.2)', line = list(color = 'rgba(255, 255, 151, 0.3)'),
                             showlegend = T, name = '25%')
    # thrid line is the median, with markers, and fill down to 25.
    fig <- fig %>% add_trace(y = ~InterpTable$med_sum, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(200, 200, 200, 0.2)',   #'rgba(255, 255, 151, 0.4)'
                             line = list(color = 'rgba(0, 0, 0, 0.8)', width =1 ),   #green 'rgba(0, 150, 0, 0.4)'
                             showlegend = T, name = '50% (median)')
    #4th line is 75%, and fill down to median
    fig <- fig %>% add_trace(y = ~InterpTable$q75, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(93, 255, 23, 0.2)', line = list(color = 'rgba(93, 255, 23, 0.4)'),
                             showlegend = T, name = '75%')
    #5th line is 90%, and fill down to 75%
    fig <- fig %>% add_trace(y = ~InterpTable$q90, type = 'scatter', mode = 'lines', 
                             fill = 'tonexty', fillcolor='rgba(0, 150, 0, 0.3)',line = list(color = 'rgba(0, 150, 0, 0.4)'),
                             showlegend = T, name = '90%')
    
    # other similar year is last line as.Date(~inData1$DayOfYr, origin = "2021-01-01")
    # 3 years with value closest to most recent image plotted
    fig <- fig %>% add_trace(x=~tmpYr1$DayOfYear,  y = ~tmpYr1$Mean, type = 'scatter', mode = 'lines',
                             line = list(color = 'rgba(122, 0, 255, 1)',width = 3, dash='dot'),
                             showlegend = T, name = paste0(topyrs$yr[1]))
   # fig <- fig %>% add_trace(x=~tmpYr2$DayOfYr,  y = ~tmpYr2$Mean, type = 'scatter', mode = 'lines',
   #                          line = list(color = 'rgba(127, 222, 255, 1)',width = 3, dash='dot'),
   #                          showlegend = T, name = paste0(topyrs$yr[2]))
   # fig <- fig %>% add_trace(x=~tmpYr3$DayOfYr,  y = ~tmpYr3$Mean, type = 'scatter', mode = 'lines',
   #                          line = list(color = 'rgba(255, 21, 255, 1)',width = 3, dash='dot'),
   #                          showlegend = T, name = paste0(topyrs$yr[3]))
    
    # "This Year line - in purple
    fig <- fig %>% add_trace(x= ~as.Date(inData2$DayOfYear), y = ~inData2$Mean,  type = 'scatter', mode = 'markers+lines',
                             marker = list(color = 'rgba(122, 0, 255, 1)', size = 8),
                             line = list(color = 'rgba(122, 0, 255, 1)',width = 3),
                             showlegend = T, name = 'This year')
    # layout settings
    fig <- fig %>% layout(title = FALSE, 
                          paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(255,255,255)',
                          xaxis = list(title = "Day of the year",
                                       gridcolor = 'rgb(227,227,227)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(227,227,227)',
                                       ticks = 'outside',
                                       type = 'date',
                                       tickformat = "%d-%b",
                                       zeroline = FALSE),
                          yaxis = list(title = "Feed (kg DM/ha)",
                                       gridcolor = 'rgb(227,227,227)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(227,227,227)',
                                       ticks = 'outside',
                                       tickformat=",", # number format.  This has 2 sig digits: ",.2r"
                                       zeroline = FALSE),
                          hovermode = "x unified")
    
    fig <- fig %>%
      add_annotations(
        xref="paper", yref="paper", x=0.0, y=1.1,
        text="Feed on offer:  2004 to present",
        font = list(color = "black", 
                    #style="bold",
                    size = 16),
        showarrow=F)
    
    fig <- fig %>%
      add_annotations(
        xref="paper", yref="paper", x=0.1, y=0.9,
        #x=35, y=round_any((max(inData1$q25) - min(inData1$q25))/3,1000,f=floor), 
        text=paste0("Property:  ", PropID,
                    "
PIC: ", inData2$PIC[1]), 
        font = list(color = "black", 
                    style="bold",
                    size = 14), 
        showarrow=F, align='right') 
 
    #find month with maximum median sum. 
    maxMedianIndex <- InterpTable$med_sum == max(InterpTable$med_sum,na.rm=TRUE)
    
    fig <- fig %>%
      add_annotations(
        xref="paper", yref="paper", x=0.22, y=0.4,
        text=paste0("Typical maximum FOO:
      90%: ", formatC(InterpTable[maxMedianIndex,"q90"], format="f", big.mark = ",", digits=0),
                    "
      75%: ", formatC(InterpTable[maxMedianIndex,"q75"], format="f", big.mark = ",", digits=0),
                    "
      50%: ", formatC(InterpTable[maxMedianIndex,"med_sum"], format="f", big.mark = ",", digits=0),
                    "
      25%: ", formatC(InterpTable[maxMedianIndex,"q25"], format="f", big.mark = ",", digits=0),
                    "
      10%: " , formatC(InterpTable[maxMedianIndex,"q10"], format="f", big.mark = ",", digits=0)),
        font = list(color = "black", 
                    size = 12),
        align='left')
    fig  
}

