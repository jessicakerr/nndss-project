library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinyWidgets)
NNDSS_test_dataset <- read.csv("~/OneDrive - University of Pittsburgh/PhD/Classes/2022/BIOST 2094 - Advanced R Computing/NNDSS Project/nndss-project/NNDSS_test_dataset.csv")


ui <- pageWithSidebar(
  
  headerPanel("NNDSS Summary Statistics"),
  
  
  
  sidebarPanel(
    shinyWidgets::pickerInput(
      inputId = "pick1",
      label = "Disease Type",
      choices = colnames(NNDSS_test_document()),
      options = list('actions-box' = TRUE),
      multiple = TRUE
    ),
    
  ),
  mainPanel(

    
  )
)

server <- function(input, output){
  
  
  
}

shinyApp(ui, server)
