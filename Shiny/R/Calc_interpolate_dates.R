
#  Interpolate summary values on same dates as ThisYear and OtherYear data for plotting
#  INputs are subset tables from "Plot_Region_Summary.R" 
#  these need DayOfYear = a date -- 2021-01-01, already declared as.Date. 
#   25-Aug-2021

#inData1 
#inData2
#tmpYr1
#DatesList <- unique(c(inData2$DayOfYear, tmpYr1$DayOfYear))


InterpTimeSeries <- function(SummaryData, ListOfDates){
  #SummaryData <- inData1
  #ListOfDates <- DatesList
  interpDatTable <- cbind(data.frame(approx(SummaryData$DayOfYear,SummaryData$med_sum, xout = ListOfDates, 
                                 rule = 2, method = "linear", ties = mean)),
  
                        data.frame(approx(SummaryData$DayOfYear,SummaryData$q10, xout = ListOfDates, 
                                     rule = 2, method = "linear", ties = mean))[2],
  
                        data.frame(approx(SummaryData$DayOfYear,SummaryData$q25, xout = ListOfDates, 
                                          rule = 2, method = "linear", ties = mean))[2],
  
                        data.frame(approx(SummaryData$DayOfYear,SummaryData$q75, xout = ListOfDates, 
                                          rule = 2, method = "linear", ties = mean))[2],
  
                        data.frame(approx(SummaryData$DayOfYear,SummaryData$q90, xout = ListOfDates, 
                                          rule = 2, method = "linear", ties = mean))[2])
  colnames(interpDatTable) <- c("DayOfYear", "med_sum", "q10", "q25", "q75", "q90")
  
  for (i in 2:6){
    interpDatTable[,i] <- round(interpDatTable[,i], 0)
  }
  
  return(interpDatTable)
}


#a <- InterpTimeSeries(inData1, DatesList)
