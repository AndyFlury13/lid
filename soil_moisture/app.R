#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(plotly)
library(tidyverse)
library(dplyr)
library("ggplot2")
library(matrixStats)
library(stats)
library(reshape2)
library(shiny)
rg = read_csv("data/moistureVStime.csv")


############# HELPER FUNCTIONS ############# 

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
                color <- switch(type, "A" = "#a4edb9", "N" = "#fcdc0a", "C"="#f9b6b6", "M" ="#a9fcfc")
            } else {
                color <- switch(type, "A" = "#54f082", "N" = "#e6cc22", "C"="#fc6464", "M" ="#04c4c4")
            }
        } else {
            if (ribbon) {
                color <- switch(type, "A" = "#76a885", "N" = "#c6bd83", "C"="#bc7a7a", "M" ="#84caf0")
            } else {
                color <- switch(type, "A" = "#037024", "N" = "#a08b01", "C"="#990000", "M" ="#0092e0")
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


# Row-wise function. Returns the standard error of the vector x. Used with the apply function.
# @parameter x
# @return : the standard error of the vector, a float.
getSE <- function(x) {
    return(sd(x) / length(x))
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


# Fill in the NaN values/gaps in our SD column, thus filling up the gaps in the ribbon.
# We will interpolate the missing values with a simple linear approximation, drawn from the 
# the "ends" of the gap. We can try to do this inplace or just return the df if it's easier.
interpolateRibbon <- function(df) {
    for (col_name in names(df)) {
        gap_found <- FALSE
        if (grepl("se", col_name, fixed=TRUE)) {
            if (nchar(col_name) != 5) {
                next
            }
            y_col = substr(col_name, 1, nchar(col_name)-2)
            y_col = paste(y_col, "avg", sep="")
            max_col_name = paste(y_col, "_ribbon_max", sep="")
            min_col_name = paste(y_col, "_ribbon_min", sep="")
            df[max_col_name] = df[y_col] + df[col_name]
            df[min_col_name] = df[y_col] - df[col_name]
            for (i in (1:length(df[[max_col_name]]))) {
                val = df[[max_col_name]][i];
                if (is.na(val) || is.null(val)) {
                    if (!gap_found) {
                        gap_start_y = df[[max_col_name]][i-1];
                        gap_start_x = i-1
                        gap_found = TRUE;
                    }
                } else {
                    if (gap_found) {
                        gap_end_x = i;
                        gap_end_y = df[[max_col_name]][i];
                        linear_approx = interpolate(gap_start_x, gap_start_y, gap_end_x, gap_end_y)
                        for (x_value in (gap_start_x: gap_end_x)) {
                            
                            df[[max_col_name]][x_value] <- (linear_approx[1] * x_value) + linear_approx[2]
                        }
                        gap_found = FALSE
                    }
                }
            }
            if (gap_found) {
                gap_end_x = length(df[[max_col_name]]) 
                gap_end_y = gap_start_y
                linear_approx = interpolate(gap_start_x, gap_start_y, gap_end_x, gap_end_y)
                for (x_value in (gap_start_x: gap_end_x)) {
                    
                    df[[max_col_name]][x_value] <- (linear_approx[1] * x_value) + linear_approx[2]
                }
            }
            gap_found = FALSE;
            for (i in (1:length(df[[min_col_name]]))) {
                val = df[[min_col_name]][i];
                if (is.na(val) || is.null(val)) {
                    if (!gap_found) {
                        gap_start_y = df[[min_col_name]][i-1];
                        gap_start_x = i-1
                        gap_found = TRUE;
                    }
                } else {
                    if (gap_found) {
                        gap_end_x = i;
                        gap_end_y = df[[min_col_name]][i];
                        linear_approx = interpolate(gap_start_x, gap_start_y, gap_end_x, gap_end_y)
                        for (x_value in (gap_start_x: gap_end_x)) {
                            df[[min_col_name]][x_value] <- (linear_approx[1] * x_value) + linear_approx[2]
                        }
                        gap_found = FALSE
                    }
                }
            }
            if (gap_found) {
                gap_end_x = length(df[[min_col_name]]) 
                gap_end_y = gap_start_y
                linear_approx = interpolate(gap_start_x, gap_start_y, gap_end_x, gap_end_y)
                for (x_value in (gap_start_x: gap_end_x)) {
                    
                    df[[min_col_name]][x_value] <- (linear_approx[1] * x_value) + linear_approx[2]
                }
            }
        }
    }
    return(df)
}


interpolate <- function(x1, y1, x2, y2) {
    m = (y2 - y1) / (x2-x1)
    b = m * -1* x1 + y1
    return(c(m, b))
}


# Graph the raingarden/raingarden treatment over the given dates using GGPlotly.
# @parameter treatmentNames: a list of the treatments we wish to display
# @parameter startDate: a string of the start date. Should be of the format YYYY-MM-DD, like "2020-01-27"
# @parameter endDate: a string of the end date. See startDate
# @ribbonBoolean: whether or not to display the standard error ribbons.
graphRG <- function(treatmentNames, startDate, endDate, ribbonBoolean) {
    diff <- as.numeric(difftime(endDate, startDate, units = "days"))
    
    ### Error handling
    if (diff < 3 || startDate < as.Date("2020-01-27") || endDate < as.Date("2020-01-27")) {
        df <- data.frame()
        plot <- ggplot(df) + geom_point() + xlim(as.Date("2020-02-01"), as.Date("2020-11-01")) + ylim(0, .35)
        return(plot)
    }
    
    ### Clean up the dataframe
    data <- rg %>% mutate(X1 = NULL, Source.Name = NULL, 'Line#' = NULL, "m^3/m^3,  Soil Moisture Stn" = NULL) %>%
        mutate(Date = as.Date(Date, format="%m/%d/%y %H:%M")) %>%
        select(-c(line)) %>%
        rename_all(funs(str_replace_all(., " ", "_"))) %>%
        rename_all(funs(str_replace_all(.,"-", "_"))) %>%
        rename_all(funs(str_replace_all(., "(_\\(m\\^3/m\\^3\\))", ""))) %>%
        group_by(Date) %>% summarise_all(list(median=median, iqr=IQR), na.rm=TRUE) %>%
        filter(Date > startDate) %>% filter(Date < endDate)
    data[data < 0] <- NA
    
    
    ### Take treatment-wise averages
    ids <- c("AI", "AO", "CI", "CO", "MI","MO", "NI", "NO")
    for (id in ids) { 
        replicates <- c()
        replicatesIQR <- c()
        for (col_name in names(data)) {
            if (grepl(id, col_name, fixed=TRUE)) {
                if (!(grepl("iqr", col_name, fixed=TRUE))) {
                    replicates <- c(replicates, col_name)
                }
            }
        }
        sdMoisture <- apply(data[replicates], 1, getSE)
        avgMoisture <- rowMeans(data[replicates])
        data[paste(id, "_avg", sep="")] <- avgMoisture
        data[paste(id, "_se" ,sep="")] <- sdMoisture
    }
    
    ### More error handling
    if (is.null(treatmentNames)) {
        plot <- ggplot(data=data, aes(x=Date, y = MO_avg))
        xlabel <- xlab("Time")
        ylabel <- ylab("Soil Moisture")
        plot <- plot + xlabel + ylabel + geom_blank()
        return(plot)
    }
    
    ### Interpolate null ribbon values
    data <- interpolateRibbon(data)
    
    ### Draw plots
    plot <- ggplot(data=data, aes(x=Date))
    lines <- ""
    ribbons <- ""
    title <- ""
    colors <-list()
    for (treatmentName in treatmentNames) {
        lines <- paste(lines, "geom_line(aes(y=", sep="")
        ribbons <- paste(ribbons, "geom_ribbon(aes(ymin=", sep="")
        ribbon_min_name = paste(treatmentName, "_avg_ribbon_min", sep="")
        ribbon_max_name = paste(treatmentName, "_avg_ribbon_max", sep="")
        col_name <- paste(treatmentName, "_avg", sep="")
        col_sd_name <- paste(treatmentName, "_se", sep="")
        lines <- paste(lines, col_name, ",color='", getFullName(treatmentName), "')) + ", sep="")
        ribbons <- paste(ribbons, ribbon_min_name, ", ymax=", ribbon_max_name, "), fill='", getColor(treatmentName, TRUE), "') + ", sep="")
        colors[[treatmentName]] <- getColor(treatmentName, FALSE)
    }
    colors <- unlist(colors[order(names(colors))], use.names=FALSE)
    lines <- substr(lines, 1, nchar(lines)-3)
    ribbons <- substr(ribbons, 1, nchar(ribbons)-3)
    if (ribbonBoolean) {
        lines <- paste("plot <- plot +", ribbons, "+", lines, sep="")
    } else {
        lines <- paste("plot <- plot + ", lines, sep = "")
    }
    print(lines)
    eval(parse(text=lines))
    legend_pls <- scale_color_manual(values=colors)
    labels <- labs(color="Treatments")
    xlabel <- xlab("Time")
    ylabel <- ylab("Soil Moisture")
    title <- ggtitle("Moisture vs Time")
    plot <- plot + ylabel + xlabel + legend_pls + labels + title
    return(plot)
}


# Plot P-values over time.
# @parameter startDate: a string of the start date. Should be of the format YYYY-MM-DD, like "2020-01-27"
# @parameter endDate: a string of the end date. See startDate
# @param gran: a string, either "day", "month", or "season". Specifies how to bin the data
graphAnova <- function(startDate, endDate, gran) {
    
    ### Clean up dataframe. Based on bin, perform summary stat
    data = rg %>% mutate(X1 = NULL, Source.Name = NULL, 'Line#' = NULL, "m^3/m^3,  Soil Moisture Stn" = NULL) %>%
        mutate(Date = as.Date(Date, format="%m/%d/%y %H:%M")) %>%
        select(-c(line)) %>%
        rename_all(funs(str_replace_all(., " ", "_"))) %>%
        rename_all(funs(str_replace_all(.,"-", "_"))) %>%
        rename_all(funs(str_replace_all(., "(_\\(m\\^3/m\\^3\\))", "")))
    if (gran == "day") {
        data = data %>% filter(Date > startDate) %>% filter(Date < endDate)
        times = seq(as.Date(startDate), as.Date(endDate), by=gran)
    } else if (gran == "month") {
        data = data %>% group_by(Date) %>% summarise_all(list(median=median, iqr=IQR), na.rm=TRUE) %>%
            filter(Date > startDate) %>% filter(Date < endDate)
        times = seq(as.Date(startDate), as.Date(endDate), by=gran)
    }
    data[data < 0] <- NA
    
    ### Take treatment-wise averages
    ids <- c("AI", "AO", "CI", "CO", "MI","MO", "NI", "NO")
    for (id in ids) { #Average soil moistures of the 4 replications
        replicates <- c() #The four RGs with the same treatment
        replicatesIQR <- c()
        for (col_name in names(data)) {
            if (grepl(id, col_name, fixed=TRUE)) {
                if (!(grepl("iqr", col_name, fixed=TRUE))) {
                    replicates <- c(replicates, col_name)
                }
            }
        }
        avgMoisture <- rowMeans(data[replicates])
        data[paste(id, "_avg", sep="")] <- avgMoisture
    }
    
    ### Perform P-test for each bin. Choose outlet over inlet
    data = data[c("Date", "AO_avg", "NO_avg", "MO_avg", "CO_avg")]
    anova_df = data.frame(date=double(), pvals=double())
    for (i in 1:(length(times) - 1)) {
        step_start = times[i]
        step_end = times[i+1]
        step_data = data %>% filter(Date >= step_start) %>% filter(Date < step_end)
        step_data_vals = step_data[c("AO_avg", "NO_avg", "MO_avg", "CO_avg")]
        date = format(step_data$'Date'[1])
        if(dim(step_data_vals)[1]  == 0) {
            next
        }
        step_data = step_data[ , c(2, 3, 4, 5)]
        step_data = melt(step_data, na.rm=TRUE)
        pval = summary(aov(value ~ variable, data=step_data))[[1]][["Pr(>F)"]][[1]]
        print(summary(aov(value ~ variable, data=step_data))[[1]])
        new_row = c(date, pval)
        anova_df[nrow(anova_df)+1,] <- new_row
    }
    
    ### Draw plots
    anova_df <- anova_df %>% mutate(date = as.Date(date, format="%Y-%m-%d"))
    plot <- ggplot(data=anova_df, aes(x=date))
    plot <- plot + geom_point(aes(y=pval)) + ylab("P-Value") + xlab("Date") + ggtitle('ANOVA')
    return(plot)
}

############################################


############# USER INTERFACE ###############

ui <- fluidPage(

    # Application title
    titlePanel("Mulch Study Soil Moisture"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("startDate", "Starting Date:", value = "2020-02-01"),
            dateInput("endDate", "Ending Date:", value = "2021-02-18"),
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
            checkboxInput("ribbons", "St Err Ribbons", FALSE),
            radioButtons("gran", "ANOVA Bins:",
                         c("Day" = "day",
                           "Month" = "month",
                           "Season" = "season")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("timeSeries"),
           span(textOutput("dateErrMessage"), style="color:red"),
           plotlyOutput("anova")
        )
    )
)
############################################


############## SERVER LOGIC ################

server <- function(input, output) {
    output$dateErrMessage <- renderText({
        diff <- as.numeric(difftime(input$endDate, input$startDate, units = "days"))
        if (input$startDate < as.Date("2020-01-27") || input$endDate < as.Date("2020-01-27")) {
            "Our data does not include values from before January 27th, 2020."
        } else if (diff < 3) {
            if (diff >= 0) {
                "The start and end dates you inputted are too close together. Your end date must be at least 3 days after your start date."
            } else {
                "Your start date is after your end date. Your end date must be at least 3 days after your start date."
            }
        }
    })
    output$timeSeries <- renderPlotly({
        
        p <- graphRG(input$treatments, input$startDate, input$endDate, input$ribbons)
        print(class(1))
        print(class(p))
        ggplotly(p, )
    })
    output$anova <- renderPlotly({
        p <- graphAnova(input$startDate, input$endDate, input$gran)
        ggplotly(p, )
    })
}

shinyApp(ui = ui, server = server)
