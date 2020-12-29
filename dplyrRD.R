library(tidyverse)
library(dplyr)
library("ggplot2")
library(matrixStats)

rg = read_csv("data/combined.csv")
names(rg)



# Graphs the soil moisture curves provided into a file, named after the treatments selected+ to plot.
# Only plots the values for days between startDate and endDate.
# @parameter ... : a variable number of strings Should be the name of a specific soil moisture sensor,
#                  for example AO (Arborist Chips Outflow) or CI (Control Inflow)
# @parameter startDate: a string that contains the earliest date you want to plot from. Must be of the format
#                       'YYYY-MM-DD'. Default value is 2020-02-01 (Feburary 1st, 2020)  
# @parameter endDate: a string that contains the latest day you want to plot to. Must be of the form
#                     'YYYY-MM-DD'. Default value is 2020-11-01 (November 1st, 2020)
# @return: None. Just writes to a file.
graphRGMoisture <- function(..., startDate="2020-02-01", endDate="2020-11-01") {
    treatmentNames <- list(...)
    startDate <- as.Date(startDate)
    endDate <- as.Date(endDate)
    summaryRG <- rg %>% mutate(X1 = NULL, Source.Name = NULL, 'Line#' = NULL, "m^3/m^3,  Soil Moisture Stn" = NULL) %>%
        mutate(Date = as.Date(Date, format="%m/%d/%y %H:%M")) %>%
        rename_all(funs(str_replace_all(., " ", "_"))) %>%  #Renaming columns
        rename_all(funs(str_replace_all(.,"-", "_"))) %>%
        rename_all(funs(str_replace_all(., "(_\\(m\\^3/m\\^3\\))", ""))) %>%
        group_by(Date) %>% summarise_if(is.numeric, mean) %>%#Grouping data by day, taking MEAN instead of MEDIAN
        filter(Date > startDate) %>% filter(Date < endDate) #Removing rows outside our targeted date range
    summaryRG[summaryRG < 0] <- NA #Removing negative soil moisture values

    
    ids <- c("AI", "AO", "CI", "CO", "MI","MO", "NI", "NO")
    for (id in ids) { #Average soil moistures of the 4 replications
        cols_to_mean <- c(NULL, NULL, NULL, NULL) #The four RGs with the same treatment
        i <- 1
        for (col_name in names(summaryRG)) {
            if (grepl(id, col_name, fixed=TRUE)) {
                cols_to_mean[i] <- col_name
                i <- i+1
            }
        }
        avgMoisture <- rowMeans(summaryRG[cols_to_mean])
        summaryRG[paste(id, "_avg", sep="")] <- avgMoisture
    }
    
    plot <- ggplot(data=summaryRG, aes(x=Date))
    lines <- ""
    title <- ""
    colors <-list()
    fileName <- ""
    for (treatmentName in treatmentNames) {
        lines <- paste(lines, "geom_line(aes(y=", sep="")
        col_name <- paste(treatmentName, "_avg", sep="")
        lines <- paste(lines, col_name, ",color='", getFullName(treatmentName), "')) + ", sep="")
        title <- paste(title, treatmentName, sep=" ")
        colors <- append(colors, getColor(treatmentName))
        fileName <- paste(fileName, treatmentName, sep="_")
    }
    fileName <- substr(fileName, 2, nchar(fileName))
    title <- paste(title, "Soil Moisture vs Time")
    lines <- substr(lines, 1, nchar(lines)-3)
    lines <- paste("plot <- plot +", lines, sep="")
    eval(parse(text=lines))
    legend_pls <- scale_color_manual(values=unlist(colors))
    labels <- labs(color="Treatments")
    xlabel <- xlab("Time")
    ylabel <- ylab("Soil Moisture")
    title <- ggtitle(title)
    
    plot <- plot + title + ylabel + xlabel + legend_pls + labels
    fileName <- paste("graphs/", fileName, ".png", sep="")
    png(file = fileName, height = 4, width = 6, units="in", res=1200)
    print(plot)
    dev.off()
}

getColor <- function(treatmentName) {
    treatmentNames <- list("AO", "NO", "MO", "CO", "AI", "NI", "CI", "MI")
    if (!(treatmentName %in% treatmentNames)){
        stop("The treatment name provided is not valid")
    } else {
        io <- substring(treatmentName, 2, 2)
        type <- substring(treatmentName, 1, 1)
        if (io == "I") {
            color <- switch(type, "A" = "#58fc89", "N" = "#fcdc0a", "C"="#fc6464", "M" ="#02fcfc")
        } else {
            color <- switch(type, "A" = "#037024", "N" = "#a08b01", "C"="#990000", "M" ="#00b7b7")
        }
        return(color)
    }
}





getFullName <- function(treatmentName) {
    treatmentNames <- list("AO", "NO", "MO", "CO", "AI", "NI", "CI", "MI")
    if (!(treatmentName %in% treatmentNames)) {
        stop("The treatment name provided is not valid")
    } else {
        type <- substring(treatmentName, 1, 1)
        fullName <- switch(type, "A" = "Arborist Chips", "N" = "Nuggets", "C"="Control", "M" ="Medium Bark")
        
        io <- substring(treatmentName, 2, 2)
        end <- switch(io, "I" = "In", "O"="Out")
        return(paste(fullName, end))
        
    }
}