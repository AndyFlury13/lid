#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Helper functions

library(tidyverse)
library(dplyr)
library("ggplot2")
library(matrixStats)

rg = read_csv("data/moistureVStime.csv")
names(rg)



# Given a treatment name, returns the color the treatment will be graphed with
# @parameter treatemntName: a two character string denoting the treatment. Example: "AO", "NO", "AI"
# @parameter ribbon: a boolean indicating whether we should return the color for the ribbon of a line. FALSE for the line itself
# @return: a string containing the hex code of the color we will graph the treatment as.
# @stop: if the inputted treatmentName is not a valid string, then we will throw an error.

getColor <- function(treatmentName, ribbon) {
    treatmentNames <- list("AO", "NO", "MO", "CO", "AI", "NI", "CI", "MI")
    if (!(treatmentName %in% treatmentNames)){
        stop("The treatment name provided is not valid")
    } else {
        io <- substring(treatmentName, 2, 2)
        type <- substring(treatmentName, 1, 1)
        if (io == "I") {
            if (ribbon) {
                color <- switch(type, "A" = "#a9fcc2", "N" = "#fcef99", "C"="#f9b6b6", "M" ="#a9fcfc")
            } else {
                color <- switch(type, "A" = "#58fc89", "N" = "#fcdc0a", "C"="#fc6464", "M" ="#02fcfc")
            }
        } else {
            if (ribbon) {
                color <- switch(type, "A" = "#648c70", "N" = "#c6bd83", "C"="#bc7a7a", "M" ="#71baba")
            } else {
                color <- switch(type, "A" = "#037024", "N" = "#a08b01", "C"="#990000", "M" ="#00b7b7")
            }
        }
        return(color)
    }
}

# Returns the correctly calculated SD of the given df. Not for general use, just a helper function
# @parameter df: should be a df whose columns are the SDs of the 4 replicate treatments. These SDs are SD of each day.
# @return : returns a vector with the SDs corretly aggregated. They are aggregated in the following way:
#                   sdVector = sum(daily_sd ** 2)/ 4 over every day

getSD <- function(df) {
    df <- df ** 2
    sdAvg <- rowMeans(df)
    
    sdAvg <- sdAvg** .5
    return(sdAvg)
}

# Given a treatment name, returns the full treatment name that will be displayed on the grah.
# @parameter treatemntName: a two character string denoting the treatment. Example: "AO", "NO", "AI"
# @return: a string containing the full name of the treatment. For example, "Arborist Chips Out", "Nuggets Out, "Arborist Chips In"
# @stop: if the inputted treatmentName is not a valid string, then we will throw an error.

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



library(shiny)

# Define UI for application`` that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Rain Garden Soil Moisture"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("startDate", "Starting Date:", value = "2020-02-01"),
            dateInput("endDate", "Ending Date:", value = "2020-11-01"),
            checkboxGroupInput("treatments", "Treatments",
                               c("Arborist Chips In" = "AI",
                                 "Control In" = "CI",
                                 "Medium Bark In" = "MI",
                                 "Nuggets In" = "NI",
                                 "Arborist Chips Out" = "AO",
                                 "Control Out" = "CO",
                                 "Medium Bark Out" = "MO",
                                 "Nuggets Out" = "NO"
                                 )),
            checkboxInput("ribbons", "St Dev Ribbons", TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("timeSeries"),
           span(textOutput("dateErrMessage"), style="color:red")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$dateErrMessage <- renderText({
        diff <- as.numeric(difftime(input$endDate, input$startDate, units = "days"))
        if (input$startDate < as.Date("2020-01-27") || input$endDate < as.Date("2020-01-27")) {
            print("what")
            "Our data does not include values from before January 27th, 2020."
        } else if (diff < 3) {
            if (diff >= 0) {
                "The start and end dates you inputted are too close together. Your end date must be at least 3 days after your start date."
            } else {
                "Your start date is after your end date. Your end date must be at least 3 days after your start date."
            }
        }
    })
    output$timeSeries <- renderPlot({
        # generate bins based on input$bins from ui.R
        #treatmentNames <- processInput(input)
        graphRG(input$treatments, input$startDate, input$endDate, input$ribbons)
    })
}

graphRG <- function(treatmentNames, startDate, endDate, ribbonBoolean) {
    diff <- as.numeric(difftime(endDate, startDate, units = "days"))
    if (diff < 3 || startDate < as.Date("2020-01-27") || endDate < as.Date("2020-01-27")) {
        df <- data.frame()
        plot <- ggplot(df) + geom_point() + xlim(as.Date("2020-02-01"), as.Date("2020-11-01")) + ylim(0, .35)
        print(plot)
        return()
    }
    summaryRG <- rg %>% mutate(X1 = NULL, Source.Name = NULL, 'Line#' = NULL, "m^3/m^3,  Soil Moisture Stn" = NULL) %>%
        mutate(Date = as.Date(Date, format="%m/%d/%y %H:%M")) %>%
        rename_all(funs(str_replace_all(., " ", "_"))) %>%  #Renaming columns
        rename_all(funs(str_replace_all(.,"-", "_"))) %>%
        rename_all(funs(str_replace_all(., "(_\\(m\\^3/m\\^3\\))", ""))) %>%
        group_by(Date) %>% summarise_if(is.numeric, funs(mean, sd)) %>%#Grouping data by day, taking MEAN instead of MEDIAN
        filter(Date > startDate) %>% filter(Date < endDate) #Removing rows outside our targeted date range
    summaryRG[summaryRG < 0] <- NA #Removing negative soil moisture values
    
    ids <- c("AI", "AO", "CI", "CO", "MI","MO", "NI", "NO")
    for (id in ids) { #Average soil moistures of the 4 replications
        replicates <- c() #The four RGs with the same treatment
        replicatesSD <- c()
        for (col_name in names(summaryRG)) {
            if (grepl(id, col_name, fixed=TRUE)) {
                if (grepl("sd", col_name, fixed=TRUE)) {
                    replicatesSD <- c(replicatesSD, col_name)
                } else {
                    replicates <- c(replicates, col_name)
                }
            }
        }
        
        avgMoisture <- rowMeans(summaryRG[replicates])
        sdMoisture <- getSD(summaryRG[replicatesSD])
        summaryRG[paste(id, "_avg", sep="")] <- avgMoisture
        summaryRG[paste(id, "_sd" ,sep="")] <- sdMoisture
    }
    if (is.null(treatmentNames)) {
        plot <- ggplot(data=summaryRG, aes(x=Date, y = MO_avg))
        xlabel <- xlab("Time")
        ylabel <- ylab("Soil Moisture")
        plot <- plot + xlabel + ylabel + geom_blank()
        print(plot)
        return()
    }
    plot <- ggplot(data=summaryRG, aes(x=Date))
    lines <- ""
    ribbons <- ""
    title <- ""
    colors <-list()
    fileName <- ""
    
    
    for (treatmentName in treatmentNames) {
        lines <- paste(lines, "geom_line(aes(y=", sep="")
        ribbons <- paste(ribbons, "geom_ribbon(aes(ymin=", sep="")
        
        col_name <- paste(treatmentName, "_avg", sep="")
        col_sd_name <- paste(treatmentName, "_sd", sep="")
        lines <- paste(lines, col_name, ",color='", getFullName(treatmentName), "')) + ", sep="")
        ribbons <- paste(ribbons, col_name, "-", col_sd_name, ", ymax=", col_name, "+", col_sd_name, "), fill='", getColor(treatmentName, TRUE), "') + ", sep="")
        colors[[treatmentName]] <- getColor(treatmentName, FALSE)
        fileName <- paste(fileName, treatmentName, sep="_")
    }
    colors <- unlist(colors[order(names(colors))], use.names=FALSE)
    fileName <- substr(fileName, 2, nchar(fileName))
    title <-("Soil Moisture vs Time")
    lines <- substr(lines, 1, nchar(lines)-3)
    ribbons <- substr(ribbons, 1, nchar(ribbons)-3)
    if (ribbonBoolean) {
        lines <- paste("plot <- plot +", ribbons, "+", lines, sep="")
    } else {
        lines <- paste("plot <- plot + ", lines, sep = "")
    }
    eval(parse(text=lines))
    legend_pls <- scale_color_manual(values=colors)
    labels <- labs(color="Treatments")
    xlabel <- xlab("Time")
    ylabel <- ylab("Soil Moisture")
    title <- ggtitle(title)
    
    plot <- plot + title + ylabel + xlabel + legend_pls + labels# + ribbon
    fileName <- paste("graphs/", fileName, ".png", sep="")
    print(plot)
}

# Run the application 
shinyApp(ui = ui, server = server)
