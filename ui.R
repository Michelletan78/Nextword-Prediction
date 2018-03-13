# Load packages for Shiny UI 
library(shiny)

#### Shiny UI code

shinyUI <- fluidPage(
                       # Title for Shiny App
                       headerPanel("Next Word Prediction - Coursera Capstone Project"),
                       
                       sidebarPanel(
                         hr(),
                         textInput("inputText", "Enter your text here:",value = "")
                       ),
                       
                       hr(),
                       
                       mainPanel(
                         hr(),
                         hr(),
                         h5("Suggested/ Predicted word:"),
                         verbatimTextOutput("prediction"))
                       
                     )