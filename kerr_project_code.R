library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinyWidgets)
library(usmap)

ui <- fluidPage(
  
  #Main title page header
  titlePanel("NNDSS Data Visualization"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Select a file
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line
      tags$hr(),
      # Input: Checkbox if file has header
      #checkboxInput("header", "Header", TRUE),
      # Input: Select separator
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
    #  # Input: Select quotes
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
    #  # Horizontal line
      tags$hr(),
    #  # Input: Select number of rows to display
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
    
    # Select state/region
    selectInput("selectra", label = h3("Select Reporting Area"), 
                choices = list(
                  "ALABAMA", "ALASKA", "AMERICAN SAMOA", "ARIZONA", 
                    "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", 
                    "DELAWARE", "DISTRICT OF COLUMBIA", "EAST NORTH CENTRAL", 
                    "EAST SOUTH CENTRAL", "FLORIDA", "GEORGIA", "GUAM", 
                    "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", 
                    "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", 
                    "MASSACHUSETTS", "MICHIGAN", "MIDDLE ATLANTIC", 
                    "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", 
                    "MOUNTAIN", "NEBRASKA", "NEVADA", "NEW ENGLAND", 
                    "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", 
                    "NEW YORK CITY", "NON-US RESIDENTS", "NORTH CAROLINA", 
                    "NORTH DAKOTA", "NORTHERN MARIANA ISLANDS", "OHIO", 
                    "OKLAHOMA", "OREGON", "PACIFIC", "PENNSYLVANIA", 
                    "PUERTO RICO", "RHODE ISLAND", "SOUTH ATLANTIC", 
                    "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", 
                    "TOTAL", "U.S. VIRGIN ISLANDS", "US RESIDENTS", 
                    "US TERRITORIES", "UTAH", "VERMONT", "VIRGINIA", 
                    "WASHINGTON", "WEST NORTH CENTRAL", "WEST SOUTH CENTRAL", 
                    "WEST VIRGINIA", "WISCONSIN", "WYOMING"), 
                selected = "TOTAL"),
    #Select disease
    selectInput("selectdis", label = h3("Disease"), 
                choices = list(
                  "Gonorrhea" = "Gonorrhea", 
                  "H. Influenza" = 'Haemophilus.influenza'), 
                selected = "Gonorrhea"),
    # Select time period
    selectInput("selecttime", label = h3("Time period"), 
                choices = list(
                  "Current Week" = "Current.week", 
                  "Cumulative from 2021" = "Cum.2021",
                  "Cumulative from 2020" = "Cum.2020"), 
                selected = "Current.week", multiple = TRUE),
    # Select type of disease (e.g., invasive)
    selectInput("selecttype", label = h3("Type of Disease"), 
                choices = c("", 
                            "Invasive disease" = "invasive.disease"), 
                selected = NULL),
    # Select age group
    selectInput("selectage", label = h3("Age"), 
                choices = c("",
                            "All ages" = "All.ages",
                            "5 and under" = "5.years"), 
                selected = NULL),
    # Select serotype
    selectInput("selectsero", label = h3("Serotype"), 
                choices = c("",
                            "All serotypes" = "all.serotypes",
                            "Serotype B" = "Serotype.b"), 
                selected = NULL)
  
    
     ),
  # what is displayed on the main page
  mainPanel(
    tableOutput("contents"),
    plotOutput("plot")
   
    )
  )
)

server <- function(input, output){
  
  output$contents <- renderTable({
    req(input$file1)
    
    # make sure dataframe reads correctly
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = TRUE,
                       sep = input$sep,
                       quote = input$quote)
        df <- filter(df, Reporting.Area == input$selectra)
        df <- select(df, Reporting.Area, MMWR.Week, contains(input$selectdis) &
                     contains(input$selecttime))
        if (nchar(input$selecttype) > 0) {
          df <- select(df, Reporting.Area, MMWR.Week, contains(input$selecttype))
        }
        if (nchar(input$selectage) > 0) {
          df <- select(df, Reporting.Area, MMWR.Week, contains(input$selectage))
        }
        if (nchar(input$selectsero) > 0) {
          df <- select(df, Reporting.Area, MMWR.Week, contains(input$selectsero))
        }
        ## not interested in the flagged columns, going to remove
        df <- select(df, -contains("flag"))   
        
        ## if two time periods are chosen, do the following
        if (length(input$selecttime)==2){
          df <- rename(df, location = Reporting.Area,week = MMWR.Week)

          ## use the column names of the selected times
          colnames(df)[grep(input$selecttime[1], colnames(df))] <- input$selecttime[1]
          colnames(df)[grep(input$selecttime[2], colnames(df))] <- input$selecttime[2]
          
          
          df[, input$selecttime[1]] <- as.numeric(gsub(",", "", df[, input$selecttime[1]]))
          df[, input$selecttime[2]] <- as.numeric(gsub(",", "", df[, input$selecttime[2]]))
          
          
          df <- arrange(df, week)
          ## comparison line plot of disease vs time
          output$plot <- renderPlot({
            ggplot(df, aes(x=week)) + 
              geom_line(aes(y = df[, input$selecttime[1]], color = input$selecttime[1])) + 
              geom_line(aes(y = df[, input$selecttime[2]], color = input$selecttime[2])) +
              ylab("Raw Case Counts") 

          }, height = 400,width = 600)
          
          ## if 3 time periods are selected, do the following:
            ## honestly shouldn't do this. in the readme.
        } else if (length(input$selecttime) == 3){ 
          df <- rename(df, location = Reporting.Area,week = MMWR.Week)
          
          ## use the column names of the selected times
          colnames(df)[grep(input$selecttime[1], colnames(df))] <- input$selecttime[1]
          colnames(df)[grep(input$selecttime[2], colnames(df))] <- input$selecttime[2]
          colnames(df)[grep(input$selecttime[3], colnames(df))] <- input$selecttime[3]
          
          df[, input$selecttime[1]] <- as.numeric(gsub(",", "", df[, input$selecttime[1]]))
          df[, input$selecttime[2]] <- as.numeric(gsub(",", "", df[, input$selecttime[2]]))
          df[, input$selecttime[3]] <- as.numeric(gsub(",", "", df[, input$selecttime[3]]))

          df <- arrange(df, week)
          ## comparison line plot of disease vs time
          output$plot <- renderPlot({
            ggplot(df, aes(x=week)) + 
              geom_line(aes(y = df[, input$selecttime[1]], color = input$selecttime[1])) + 
              geom_line(aes(y = df[, input$selecttime[2]], color = input$selecttime[2])) +
              geom_line(aes(y = df[, input$selecttime[3]], color = input$selecttime[3])) +
              ylab("Raw Case Counts")
          }, height = 400,width = 600)
          
          } else {
        ## if only one time period is selected, make a normal plot
            df <- rename(df, location = Reporting.Area,week = MMWR.Week)
            
            ## use the column names of the selected times
            colnames(df)[grep(input$selecttime[1], colnames(df))] <- input$selecttime[1]
            df[, input$selecttime[1]] <- as.numeric(gsub(",", "", df[, input$selecttime[1]]))
            df <- arrange(df, week)
          
          output$plot <- renderPlot({
            ggplot() +
              geom_line(data = df, aes(x=week, y=df[, input$selecttime[1]], group=location, color = input$selecttime[1]))+
              geom_point() +
              ylab("Raw Case Counts") 
              
          }, height = 400,width = 600)
        }
        
       
          
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
}

shinyApp(ui, server)
