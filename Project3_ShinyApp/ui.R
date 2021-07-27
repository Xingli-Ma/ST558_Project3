#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Read in the American black bear data
ABB <- read.csv("ABBdata.csv", stringsAsFactors = FALSE)
print(str(ABB))

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("American black bear distribution data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type="tab",
                        tabPanel("About", textOutput("about")),
                        tabPanel("Data", tableOutput("ABBdata")),
                        tabPanel("Data Exploration", plotOutput("explore")),
                        tabPanel("Modeling Info", ),
                        tabPanel("Modeling Fitting", ),
                        tabPanel("Prediction", ),
                        tabPanel("Summary", verbatimTextOutput("summ")),
                        tabPanel("Plot", plotOutput("plot"))
                
            )
        )
    )
))
