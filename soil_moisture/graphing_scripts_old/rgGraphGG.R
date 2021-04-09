
library("readxl")
library("ggplot2")
rgData <- read_excel("soil moisture data.xlsx")
rgData$Date <- as.Date(rgData$Date)
rgData$"ni101" <- as.numeric(rgData$`NI-101 (m^3/m^3)`)
rgData$"test" <- as.numeric(rgData$test)


# Graphs the soil moisture curves provided into a file, named after the treatments selected to plot.
# Only plots the values for days between startDate and endDate.
# @parameter ... : a variable number of strings Should be the name of a specific soil moisture sensor,
#                  for example AO (Arborist Chips Outflow) or CI (Control Inflow)
# @parameter graphFile: boolean value that determines whether or not graph should show up in the plot window
#                       of RStudio or written to a png file.
# @parameter startDate: a string that contains the earliest date you want to plot from. Must be of the format
#                       'YYYY-MM-DD'. Default value is 2020-02-01 (Feburary 1st, 2020)  
# @parameter endDate: a string that contains the latest day you want to plot to. Must be of the form
#                     'YYYY-MM-DD'. Default value is 2020-07-01 (July 1st, 2020)
# @return: None. Just writes to a file.

graphMoisture <- function(..., graphFile=TRUE, startDate="2020-02-01", endDate="2020-07-01") {
  x <- list(...)
  plotData = getAvg(x, startDate, endDate)
  plotMoisture(plotData, length(x), graphFile, diff=FALSE)
}


# Graphs the difference of outflow and inflow of soil moisture curves for the provided treatments. Has
# the same graphFile, startDate, and endDate parameters as before
# @parameter ...: a variable number of strings. Each string should be the name of a treatment. Must be the
#                 first letter of the treatment, for example: "C" for control, "A" for arborist chips.
# @parameter graphFile: boolean value that determines whether or not graph should show up in the plot window
#                       of RStudio or written to a png file.
# @parameter startDate: a string that contains the earliest date you want to plot from. Must be of the format
#                       'YYYY-MM-DD'. Default value is 2020-02-01 (Feburary 1st, 2020)  
# @parameter endDate: a string that contains the latest day you want to plot to. Must be of the form
#                     'YYYY-MM-DD'. Default value is 2020-07-01 (July 1st, 2020)
# @return: None. Writes to a file.

graphMoistureDiff <- function(..., graphFile=TRUE, startDate="2020-02-01", endDate="2020-07-01") {
  x <- list(...)
  plotData = getDiff(x, startDate, endDate)
  plotMoisture(plotData, length(x), graphFile, diff=TRUE)
}



# Takes a list of treatment names, a start date, end date, and then returns a dataframe that is
# rgData but containing only dates between startDate and endDate
# @parameter treatmentNames : a list of treatment names. "CO" for Control Outflow, "AI" for Arborist Chip Inflow.
# @parameter startDate : a string representing the earliest day to be plotted
# @parameter endDate: a string representing the last day to be plotted
#
# @return: a dataframe containing the average soil moisture values, grouped by TREATMENT. Also filters out
#          rows where soil moisture is negative.

getAvg <- function(treatmentNames, startDate, endDate) {
      x <- treatmentNames
      rgSelectedData <- pruneDataFrame(startDate, endDate, rgData)
      plotData <-data.frame(Date = rgSelectedData$Date) 
      for (id in x) {
          idError(id)
          id_columns <- c(NULL, NULL, NULL, NULL)
          i <- 1
          for (name in names(rgSelectedData)) { #Get all unique rg ids, like CO, CI, MO, MI, etc
            if (grepl(id, name, fixed=TRUE)) {
              id_columns[i] <- name
              i <- i+1
            }
          }
          
          if (length(id_columns) == 4) { #Average the values for each treatment
            avgMoisture <- rowMeans(rgSelectedData[id_columns])
          } else if (length(id_columns) == 1) {
            avgMoisture <- rgSelectedData$id_columns[1]
          } else {
            stop("Something went wrong")
          }
          plotData[[id]] <- avgMoisture
      }
      return(plotData)
}


# Returns a dataframe with the difference between the outflow and inflow soil moistures for each
# treatment provided in treatmentNames (a list of strings). Will also only contain rows from startDate
# to endDate.
# @parameter treatmentNames: a list of strings. Each string is a treatment name, the first letter of the treatment.
# @parameter startDate : a string representing the earliest day to be plotted
# @parameter endDate: a string representing the last day to be plotted
#
# @return: a dataframe that contains one column for each value in treatmentNames. The column represents the
#          average difference between the outflow soil moisture and inflow soil moisture for a given treatment.
#          Also filters out any negative soil moisture readings.

getDiff <- function(treatmentNames, startDate, endDate) {
    x <- treatmentNames
    rgSelectedData <- pruneDataFrame(startDate, endDate, rgData)
    plotData <-data.frame(Date = rgSelectedData$Date) 
    for (id in x) {
        idErrorDiff(id)
        id_columns <- c(NULL, NULL, NULL, NULL)
        i <- 1
        outId <- paste(id, "O", sep="")
        for (name in names(rgSelectedData)) { #Get all unique rg ids, like CO, MO, NO, AO, etc
            if (grepl(outId, name, fixed=TRUE)) {
              id_columns[i] <- name
              i <- i+1
            }
        }
        
        if (length(id_columns) == 4) { #Average the values for each treatment
            avgMoistureOut <- rowMeans(rgSelectedData[id_columns])
        } else if (length(id_columns) == 1) {
            avgMoisture <- rgSelectedData$id_columns[1]
        } else {
            stop("Something went wrong")
        }
        
        i <- 1
        inId <- paste(id, "I", sep="")
        for (name in names(rgSelectedData)) { #Get all unique rg ids, like CI, NI, AI, MI, etc
            if (grepl(inId, name, fixed=TRUE)) {
                id_columns[i] <- name
                i <- i+1
            }
        }
        
        if (length(id_columns) == 4) { #Average the values for each treatment
          avgMoistureIn <- rowMeans(rgSelectedData[id_columns])
        } else if (length(id_columns) == 1) {
          avgMoisture <- rgSelectedData$id_columns[1]
        } else {
          stop("Something went wrong")
        }
        
        
        avgMoistureDiff = avgMoistureOut - avgMoistureIn
        plotData[[id]] <- avgMoistureDiff
    }
    return(plotData)
}




# Plots the data of the given dataframe. Assumes the first column is Dates which is the x axis, then plots
# the remaining columns as soil moisture readings on the y axis. 
#
# @parameter dataframe: a dataframe containing the averages of the treatments 
# @parameter numArgs: the number of treatments we wish to plot
# @parameter graphFile: boolean value that determines whether or not graph should show up in the plot window
#                       of RStudio or written to a png file.
# @parameter diff: boolean value that determines whether or not we are plotting the difference between outflow
#                  inflow or not.
#
# @ return: None. Writes to a graphfile.

plotMoisture <- function(dataframe, numArgs, graphFile, diff) {
    ids <- colnames(dataframe)
    if (numArgs == 1) {
        if (graphFile) {
          if (diff) {
            fileName = paste("graphs/", ids[2], "_", "diff.png", sep="")
            ylabel <- ylab("Soil Moisture Diff (Out - In)")
          } else {
            fileName = paste("graphs/", ids[2], ".png", sep="")
            ylabel <- ylab("Soil Moisture")
          }
          png(file = fileName, height = 4, width = 6, units="in", res=1200)
        }
        plot <- ggplot(data=dataframe)
        point1 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[2]]], colour=getName(ids[2])), size=.005)
        xlabel <- xlab("Time")
        title <- ggtitle(ids[2])
        legendlabels <- scale_colour_manual(breaks = getName(ids[2]), values = c("#ff0000"), name="Treatment")
        legendformat <- guides(colour=guide_legend(override.aes=list(size=3)))
        plot <- plot + point1 + title + legendlabels + xlabel + ylabel + legendformat
        print(plot)
        plot
        if (graphFile) {
          dev.off()
        }
        
    } else if (numArgs == 2) {
        if (graphFile) {
          if (diff) {
            fileName = paste("graphs/", ids[2], "_", ids[3], "_", "diff.png", sep="")
            ylabel <- ylab("Soil Moisture Diff (Out - In)")
          } else {
            fileName = paste("graphs/", ids[2], "_", ids[3], ".png", sep="")
            ylabel <- ylab("Soil Moisture")
          }
          png(filename = fileName, width=6, height = 4, units = "in", res = 1200)
        }
        plot <- ggplot(data=dataframe)
        point1 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[2]]],  colour = getName(ids[2])), size=.005)
        point2 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[3]]], colour = getName(ids[3])), size=.005)
        xlabel <- xlab("Time")
        title <- ggtitle(paste(ids[2], ids[3]))
        legendlabels <- scale_colour_manual(breaks = getName(ids[2:3]), values = c("#ff0000", "#00ff00"), name="Treatment")
        legendformat <- guides(colour=guide_legend(override.aes=list(size=3)))
        plot <- plot+ point1+ point2 + title + ylabel + xlabel + legendlabels + legendformat
        print(plot)
        if (graphFile) {
          dev.off()
        }
    
    } else if (numArgs==3) {
        if (graphFile) {
          if (diff) {
            fileName = paste("graphs/", ids[2], "_", ids[3], "_", ids[4], "_", "diff.png", sep="")
            ylabel <- ylab("Soil Moisture Diff (Out - In)")
          } else {
            fileName = paste("graphs/", ids[2], "_", ids[3], "_", ids[4], "_", ".png", sep="")
            ylabel <- ylab("Soil Moisture Diff (Out - In)")
          }
          png(filename = fileName, width=6, height = 4, units = "in", res=1200)
        }
        plot <- ggplot(data=dataframe)
        point1 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[2]]], colour = getName(ids[2])), size=.005)
        point2 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[3]]], colour = getName(ids[3])), size=.005)
        point3 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[4]]], colour = getName(ids[4])), size=.005)
        title <- ggtitle(paste(ids[2], ids[3], ids[4]))
        xlabel <- xlab("Time")
        legendlabels <- scale_colour_manual(breaks = getName(ids[2:4]), values = c("#ff0000", "#00ff00", "#0000ff"), name="Treatment")
        legendformat <- guides(colour=guide_legend(override.aes=list(size=3)))
        plot <- plot + point1 + point2 + point3 + title + legendlabels + ylabel + xlabel + legendformat
        print(plot)
        if (graphFile) {
          dev.off()
        }
      
    } else if (numArgs ==4) {
        if (graphFile) {
          if (diff) {
            fileName = paste("graphs/", ids[2], "_", ids[3], "_", ids[4], "_", ids[5], "_", "diff.png", sep="")
            ylabel <- ylab("Soil Moisture Diff (Out - In)")
          } else {
            fileName = paste("graphs/", ids[2], "_", ids[3], "_", ids[4], "_", ids[5], ".png", sep="")
            ylabel <- ylab("Soil Moisture")
          }
          png(filename = fileName, width=6, height = 4, units="in", res=1200)
        }
        plot <- ggplot(data=dataframe, fill=FALSE)
        point1 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[2]]], colour=getName(ids[2])), size = .005)
        point2 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[3]]], colour=getName(ids[3])), size=.005)
        point3 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[4]]], colour=getName(ids[4])), size=.005)
        point4 <- geom_point(aes(x=dataframe$Date, y=dataframe[[ids[5]]], colour = getName(ids[5])), size=.005)
        xlabel <- xlab("Time")
        title <- ggtitle(paste(ids[2], ids[3], ids[4], ids[5]))
        legendlabels <- scale_colour_manual(breaks = getName(ids[2:5]), values = c("#ff0000", "#00ff00", "#0000ff", "#87cefa"), name="Treatment")
        legendformat <- guides(colour=guide_legend(override.aes=list(size=3)))
        plot <- plot + point1 + point2 + point3 + point4 + ylabel + xlabel + title + legendlabels + legendformat
        print(plot)
        if (graphFile) {
          dev.off()
        }
    } else {
      stop("x should be at least 1 argument long")
    }
  
  
}


# A function that takes in a dataframe, a start date, enddate, and returns a dataframe
# with only that range of dates. Also replaces any negative moisture readings with NAs.
#
# @parameter startDate: a string of the form YEAR-MO-DA
# @parameter endDate: a string of the form YEAR-MO-DA
# @parameter dataframe: the dataframe read in from the file 'soil moisture data.xlsx'
#
# @return: the mutated dataframe. DOES NOT RETURN A NEW DATAFRAME.
#
# Errors: If either date is of the wrong format, an error is thrown. If the start date occurs after the end date, an error is thrown.

pruneDataFrame <- function(startDate, endDate, dataframe) {
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  dates <- dataframe$Date
  print(dates)
  if (!(startDate %in% dates)) {
    stop(paste(startDate, "is not a valid date. Make sure that your date occurs within the bounds of the data and it is in the right format:\n YEAR-MO-DA\n2020-07-03 is a valid date"))
  } else if(!(endDate %in% dates)) {
    stop(paste(startDate, "is not a valid date. Make sure that your date occurs within the bounds of the data and it is in the right format:\n YEAR-MO-DA\n2020-07-03 is a valid date"))
  } else if (startDate > endDate) {
    stop("Your start date is later than your end date!")
  }
  startIndex <- match(startDate, dates)
  endIndex <- match(endDate, dates)
  dataframe[dataframe < 0] <- NA
  return(dataframe[startIndex:endIndex,])
}



# A function that takes in an ID and stops the program if it is not a correct ID.
# The correct ids are given in idset.
# @param: a 2 letter ID representing the treatment and the out/in soil moisture sensor
# returns: none
# Errors: if the ID is not a valid treatment/(in/out) combination, stops the program.

idError <- function(id) {
  idSet <- c("CO", "CI", "NO", "NI", "MO", "MI", "AO", "AI")
  if (!(id %in% idSet)) {
    stop(paste(id, "is not a valid treatment/outflow id. Treatment/outflow ids\nmust be in the form of\n[capitalized initial of treatment][capitalized initial of inflow our outflow].\nFor example, carbon outflow would be CO"))
  }
}

idErrorDiff <- function(id) {
    idSet <- c("C", "N", "M", "A")
    if (!(id %in% idSet)) {
      stop(paste(id, "is not a valid treatment id. Treatment ids\nmust be in the form of\n[capitalized initial of treatment].\nFor example, carbon would be C"))
    }
  
}



# A function that takes in a list of ids and returns a list of the corresponding name
# to each ID.
# @parameter ids: a list of ids such as AO, NO, AI. The items of the list must be strings
#
# @return : a list of strings of the corresponding names to each id, such as 'Arborist Chips Out'.
#
# Errors: if the id is not recognised (not of form [Treatment first character][input/output first character]),
# program is stopped.

getName <- function(ids) {
    names <- character(length(ids))
    for (i in 1:length(ids)) {
        name <- ""
        if (substr(ids[i], 1, 1) == "C") {
          name <- paste(name, "Control", sep="")
        } else if (substr(ids[i], 1, 1) == "A") {
          name <- paste(name, "Arborist Chips", sep="")
        } else if (substr(ids[i], 1, 1) == "M") {
          name <- paste(name, "Medium Bark", sep="")
        } else if (substr(ids[i], 1, 1) == "N") {
          name <- paste(name, "Nuggets", sep="")
        } else {
          stop("The ID is formatted incorrectly")
        }
        
        if (nchar(ids[1]) == 2) {
            if (substr(ids[i], 2, 2) == "O") {
              name <- paste(name, "Out")
            } else if (substr(ids[i],2, 2) == "I") {
              name <- paste(name, "In")
            } else {
              stop("The ID is formatted incorrectly")
            }
        }
        names[i] <- name
    }
    return(as.vector(unlist(names)))
}


graphMoisture("CO", "MO", graphFile=TRUE)

