
library("readxl")
library("ggplot2")
rgData <- read_excel("soil moisture data.xlsx")
#rgData$Date <- as.character(rgData$Date)

graphMoisturePDF <- function(..., graphFile=TRUE, startDate="2020-02-01", endDate="2020-06-01") {
    x <- list(...)
    colors <- colorRampPalette(c("red", "green"))(4)
    first <- TRUE
    c <-1
    rgData <- selectDates(startDate, endDate, rgData)
    for (id in x) {
        
        
        idError(id)
        id_columns <- c(NULL, NULL, NULL, NULL)
        i <- 1
        for (name in names(rgData)) { #Get all unique rg ids, like CO, CI, MO, MI, etc
            if (grepl(id, name, fixed=TRUE)) {
                id_columns[i] <- name
                i <- i+1
            }
        }
        if (length(id_columns) == 4) { #Average the values for each treatment
            avgMoisture <- rowMeans(rgData[id_columns])
        } else if (length(id_columns) == 1) {
            avgMoisture <- rgData$id_columns[1]
        } else {
            stop("Something went wrong")
        }
        
        date <- rgData$Date
        if (first) {
            main_string <- paste("Soil Moisture versus Time")
            xlabel <- "Time"
            ylabel <- "Soil Moisture (m^3/m^3)"
            first = FALSE
            if (graphFile) {
                pdf(file = paste("graphs/", paste(unlist(x), collapse="_"), "_graph.pdf", sep=""), width = 6, height = 4)
            }
            plot(date, avgMoisture, pch=19, cex=.2, main=main_string, xlab=xlabel, ylab=ylabel, col= colors[c]) #We have to plot the first time
            
        } else {
            points(date, avgMoisture, col=colors[c], cex = .2) #For every additional curve, we add to the plot object we created above
        }
        c <- c+1
    }
    print(as.vector(unlist(x)))
    legend("topright", legend=as.vector(unlist(x)), col=as.vector(unlist(colors)), pch=15)
    if (graphFile) {
        dev.off()
    }
}



selectDates <- function(startDate, endDate, dataframe) {
    startDate <- as.Date(startDate)
    endDate <- as.Date(endDate)
    dates <- as.Date(rgData$Date)
    if (!(startDate %in% dates)) {
        stop(paste(startDate, "is not a valid date. Make sure that your date occurs within the bounds of the data and it is in the right format:\n YEAR-MO-DA\n2020-07-03 is a valid date"))
    } else if(!(endDate %in% dates)) {
        stop(paste(startDate, "is not a valid date. Make sure that your date occurs within the bounds of the data and it is in the right format:\n YEAR-MO-DA\n2020-07-03 is a valid date"))
    } else if (startDate > endDate) {
        stop("Your start date is later than your end date!")
    }
    startIndex <- match(startDate, dates)
    endIndex <- match(endDate, dates)
    #dataframe[dataframe < 0] <- NA
    return(dataframe[startIndex:endIndex,])
}



idError <- function(id) {
    idSet <- c("CO", "CI", "NO", "NI", "MO", "MI", "AO", "AI")
    if (!(id %in% idSet)) {
        stop(paste(id, "is not a valid treatment/outflow id. Treatment/outflow ids\nmust be in the form of\n[capitalized initial of treatment][capitalized initial of inflow our outflow].\nFor example, carbon outflow would be CO"))
    }
    
}

graphMoisturePDF("CO", "MO", graphFile=TRUE, startDate="2020-02-01", endDate = "2020-07-01")
