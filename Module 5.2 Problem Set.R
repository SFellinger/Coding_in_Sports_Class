##### SCROLL DOWN TO THE VERY BOTTOM TO SEE PROBLEM SET


#getwd()
#setwd("/Users/stevenfellinger/Desktop/Coding Course/Data")


# load packages

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)

# Import data / CSV & Remove Extra Column "V1"

TestTrackMan=fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))
TestTrackMan <- subset(TestTrackMan, select = -c(...1))


# Converts Character Date into Proper Format

TestTrackMan$Date <- mdy(TestTrackMan$Date)


### Link to a list of various Shiny Inputs we can use

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/



# Start of the UI - Part 1 of App Structure

ui <- navbarPage(
  
  "Title of App", theme = "flatly",
  
  tabPanel("Pitchers",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("Team", label = "Choose Team",
                           
                           choices = levels(as.factor(TestTrackMan$PitcherTeam))),
               
               selectInput("Pitcher", label = "Choose Pitcher",
                           
                           choices = levels(as.factor(TestTrackMan$Pitcher))),
               
               dateRangeInput("Date", label = "Choose Date Range",
                              start = min(TestTrackMan$Date),
                              end = max(TestTrackMan$Date),
                              min = min(TestTrackMan$Date),
                              max = max(TestTrackMan$Date),
                              format = "yyyy-mm-dd",
                              separator = "to"),
               
               checkboxGroupInput("Pitch", label = "Choose Pitch Type(s)", 
                                  choices = levels(as.factor(TestTrackMan$TaggedPitchType))),
               
               
               width = 2),
             
             
             mainPanel(
               
               
             )),
           
  ),
  
)



# Start of the Server - Part 2 of App Structure

server = function(input, output, session) {
  
  # Input Reactions -- Pitcher Tab
  
  
  # Pitchers Based on Team    
  
  observeEvent(
    input$Team,
    updateSelectInput(session,
                      "Pitcher", "Choose Pitcher",
                      choices = levels(factor(filter(TestTrackMan,
                                                     PitcherTeam == isolate(input$Team))$Pitcher))))
  
  
  # Date Range Based on When Pitcher Threw
  
  observeEvent(
    input$Pitcher,
    updateDateRangeInput(session,
                         "Date", "Choose Date Range",
                         start = min(TestTrackMan$Date),
                         end = max(TestTrackMan$Date)))
  
  
  # Pitch Types Based on Pitcher
  
  observeEvent(
    input$Pitcher,
    updateCheckboxGroupInput(session,
                             "Pitch", "Choose Pitch Type(s)",
                             choices = levels(factor(filter(TestTrackMan,
                                                            Pitcher == isolate(input$Pitcher))$TaggedPitchType))))
  
  
  
  
  
}


# ShinyApp - Part 3 of App Structure

shinyApp(ui = ui, server = server)





########## Problem Set ##############

#Create the Following:

# 1) Create a linking between hitting data, on the "Hitters" tab just like we did here for "Pitchers"
##### SCROLL DOWN TO THE VERY BOTTOM TO SEE PROBLEM SET


#getwd()
#setwd("/Users/stevenfellinger/Desktop/Coding Course/Data")


# load packages

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)

# Import data / CSV & Remove Extra Column "V1"

TestTrackMan=fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))
TestTrackMan <- subset(TestTrackMan, select = -c(...1))


# Converts Character Date into Proper Format

TestTrackMan$Date <- mdy(TestTrackMan$Date)


### Link to a list of various Shiny Inputs we can use

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/



# Start of the UI - Part 1 of App Structure

ui <- navbarPage(
  
  "Title of App", theme = "flatly",
  
  tabPanel("Hitters",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("Team", label = "Choose Team",
                           
                           choices = levels(as.factor(TestTrackMan$BatterTeam))),
               
               selectInput("Hitter", label = "Choose Hitter",
                           
                           choices = levels(as.factor(TestTrackMan$BatterID))),
               
               dateRangeInput("Date", label = "Choose Date Range",
                              start = min(TestTrackMan$Date),
                              end = max(TestTrackMan$Date),
                              min = min(TestTrackMan$Date),
                              max = max(TestTrackMan$Date),
                              format = "yyyy-mm-dd",
                              separator = "to"),
               
               checkboxGroupInput("Pitch", label = "Choose Pitch Type(s)", 
                                  choices = levels(as.factor(TestTrackMan$TaggedPitchType))),
               
               
               width = 2),
             
             
             mainPanel(
               
               
             )),
           
  ),
  
)



# Start of the Server - Part 2 of App Structure

server = function(input, output, session) {
  
  # Input Reactions -- Hitter Tab
  
  
  # Hitters Based on Team    
  
  observeEvent(
    input$Team,
    updateSelectInput(session,
                      "Hitter", "Choose Hitter",
                      choices = levels(factor(filter(TestTrackMan,
                                                     BatterTeam == isolate(input$Hitter))$BatterID))))
  
  
  # Date Range Based on When Hitter Batted
  
  observeEvent(
    input$Hitter,
    updateDateRangeInput(session,
                         "Date", "Choose Date Range",
                         start = min(TestTrackMan$Date),
                         end = max(TestTrackMan$Date)))
  
  
  # Pitch Types Based on Hitter
  
  observeEvent(
    input$Hitter,
    updateCheckboxGroupInput(session,
                             "Pitch", "Choose Pitch Type(s)",
                             choices = levels(factor(filter(TestTrackMan,
                                                            Hitter == isolate(input$Hitter))$TaggedPitchType))))
  
  
  
  
  
}


# ShinyApp - Part 3 of App Structure

shinyApp(ui = ui, server = server)
