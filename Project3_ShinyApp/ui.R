#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
library(shiny)
library(shinydashboard)
require(knitr)
library(kableExtra)
library(tibble)
library(caret)
library(ggplot2)
require(corrplot)
library(tidyverse)
library(DT)


dashboardPage(skin="red",
              
    dashboardHeader(
        title="ST558 Project 3: American Black Bear Distribution Data Analysis", titleWidth=2000),
    
    dashboardSidebar(
        sidebarMenu(
            tags$style(HTML(".sidebar-menu li a { font-size: 20px;}")),
            menuItem("About", tabName="about"),
            menuItem("Data", tabName="data"),
            menuItem("Data Exploration", tabName="explore"),
            menuItem("Modeling", tabName="model")
        )
    ),
    dashboardBody(
        tabItems(
# ______________________________________________________________________________________
            # About tab
            tabItem(tabName="about",
                    h3("The purpose of the app"),
                    h4("This shiny app is created to demonstrate the American black bear data information, to allow users to explore the data and make customized summaries. It also fits three models on the training set of the data, and select the best model to make predictions on the testing set."),
                    h3("The data and data source"),
                    imageOutput("picture", height = "auto"),
                    uiOutput("url"),
                    h4("The American black bear distribution data come from the iNaturalist database, provided by Brent Pease at NC State University. The data set consists of a randomly-selected set of 927 (50 km2) regions from Western North America. It has 10 variables:"),
                    tags$li("Y: Number of black bear reports"), 
                    tags$li("N: Number of surveys submitted"), 
                    tags$li("forest: Proportion of the region that is forest"),
                    tags$li("grassland: Proportion of the region that is grassland"), 
                    tags$li("cropland: Proportion of the region that is cropland"), 
                    tags$li("temp: Annual average temperature"), 
                    tags$li("precip: Annual average precipitation"), 
                    tags$li("humanPop: Human population"), 
                    tags$li("protected: Indicator of whether the region includes protected lands"), 
                    tags$li("ecoregion: MARINE WEST COAST FOREST, MEDITERRANEAN CALIFORNIA, NORTH AMERICAN DESERTS, NORTHWESTERN FORESTED MOUNTAINS"), 
                    h3("The functionalities of the app"), 
                    h4("This shiny app has four tabs: About, Data, Data Exploration, and Modeling. The About page has a brief introduction about this app and its functionalities, as well as data description and data source. The Data page allows users to view and subset the data and download and save the data. The Data Exploration page generates different types of plots that can be specified by users and create summaries. The Modeling page fits three statistical models, including modeling approaches. The Modeling page has three tabs: Modeling Info, Model Fitting, and Prediction. More information relevant to each tab are available at the beginning of each page.")
                    ),
            
# ______________________________________________________________________________________
            # Data tab
            tabItem(tabName = "data", 
                    fluidRow(
                        column(3,
                            box(width = 10,
                                checkboxGroupInput(inputId = "sel_var",
                                                   label = "Select Variables",
                                                   choices = list(
                                                       "Number of black bear" = "Y",
                                                       "Number of surveys" = "N",
                                                       "Proportion of forest" = "forest",
                                                       "Proportion of grassland" = "grassland",
                                                       "Proportion cropland" = "cropland",
                                                       "Annual average temperature" = "temp",
                                                       "Annual average precipitation" = "precip",
                                                       "Human population" = "humanPop",
                                                       "Protected lands" = "protected",
                                                       "Ecoregion" = "ecoregion"
                                                   ),
                                                   selected = list(
                                                       "Number of black bear" = "Y",
                                                       "Number of surveys" = "N",
                                                       "Protected lands" = "protected",
                                                       "Ecoregion" = "ecoregion"
                                                       )
                                                   ), # end of checkbox
                                checkboxGroupInput(inputId = "select_ecoregion",
                                                   label = "Select Ecoregion",
                                                   choices = list(
                                                       "MARINE WEST COAST FOREST",
                                                       "MEDITERRANEAN CALIFORNIA",
                                                       "NORTH AMERICAN DESERTS",
                                                       "NORTHWESTERN FORESTED MOUNTAINS"
                                                   ),
                                                   selected = list(
                                                       "MARINE WEST COAST FOREST",
                                                       "MEDITERRANEAN CALIFORNIA",
                                                       "NORTH AMERICAN DESERTS",
                                                       "NORTHWESTERN FORESTED MOUNTAINS"
                                                   )
                                    
                                )
                                ) #end of box
                            ), # end of column 3
                                column(9,
                                       DTOutput(outputId = "dt_table"),
                                       br(),
                                       br(),
                                       downloadButton(
                                           outputId = "dt_download", label="Download data",
                                           style="color: #fff; background-color: green; border-color: Black;")
                                       ) # end of column 9
                                
                            ) # end of fluidRow
                        ), # end of data tab
            
# ______________________________________________________________________________________
            # Data Exploration tab
            tabItem(tabName = "explore", 
                    fluidPage(
                        # Set tab style
                        tags$head(
                            tags$style(type='text/css', 
                                       ".nav-tabs {font-size: 20px}")),
                        tabsetPanel(type="tab",
                                    tabPanel("Numerical Summaries",
                                             fluidRow(
                                                 column(3, 
                                                     fluidRow(box(width = 12,
                                                     h3("Number of black bear by ecoregion"),
                                                        tableOutput("dt_ecoregion"),
                                                        br(),
                                                        br(),
                                                     )
                                                     ),
                                                     fluidRow(box(width = 12,
                                                     h3("Number of black bear by whether or not protected lands"),
                                                        tableOutput("dt_protected")
                                                        )
                                                     )
                                                     ),# end of column 3
                                                 column(9,box(width = 15,
                                                                        checkboxGroupInput(inputId = "sel_var_2",
                                                                                           label = "Select Variables for Summary",
                                                                                           choices = list(
                                                                                               "Number of black bear" = "Y",
                                                                                               "Number of surveys" = "N",
                                                                                               "Proportion of forest" = "forest",
                                                                                               "Proportion of grassland" = "grassland",
                                                                                               "Proportion cropland" = "cropland",
                                                                                               "Annual average temperature" = "temp",
                                                                                               "Annual average precipitation" = "precip",
                                                                                               "Human population" = "humanPop",
                                                                                               "Protected lands" = "protected",
                                                                                               "Ecoregion" = "ecoregion"
                                                                                           ),
                                                                                           selected = list(
                                                                                               "Number of black bear" = "Y",
                                                                                               "Number of surveys" = "N",
                                                                                               "Protected lands" = "protected",
                                                                                               "Ecoregion" = "ecoregion"
                                                                                           )
                                                                        ), # end of checkbox
                                                                        h3("Numerical summaries on the data"),
                                                                        verbatimTextOutput("dt_sum")
                                                 ) # end of box
                                                  # end of wellPanel
                                                 
                                                 
                                                 
                                                 ) # end of column 9
                                                 ) # end of fluidRow
                                             ), # end of Numerical Summaries tab
                                    
                                    tabPanel("Graphical Summaries",
                                             fluidRow(
                                                 column(6,
                                                        h3("Stacked barplot of number of black bear by protected lands and ecoregion"),
                                                        plotOutput("barPlot"),
                                                        br()
                                                        ),
                                                 column(6,
                                                        h3("Stacked barplot of number of black bear by protected lands and ecoregion"),
                                                        plotOutput("corrPlot"),
                                                        br()
                                                        )
                                                 ), # end of fluidRow
                                             fluidRow(
                                                 column(6, 
                                                        box(width = 15,
                                                            # select inputs with a drop-down list for user to select
                                                            selectInput(inputId="var", label="Select Variable to Create Histogram",
                                                                        choices = list(
                                                                            "Proportion of forest" = "forest",
                                                                            "Proportion of grassland" = "grassland",
                                                                            "Proportion cropland" = "cropland",
                                                                            "Annual average temperature" = "temp",
                                                                            "Annual average precipitation" = "precip",
                                                                            "Human population" = "humanPop"
                                                                            )
                                                                        ),
                                                            # create dynamic title
                                                            uiOutput("title1"),
                                                            plotOutput("hisPlot"),
                                                            br()
                                                        ) # end of box
                                                        ), 
                                                        
                                                 column(6,
                                                        box(width = 15,
                                                     uiOutput("title2"),
                                                     plotOutput("hisPlotReg"),
                                                     br()
                                                     )# end of box
                                                 ) # end of column 9
                                             ) # end of fluidRow
                                    ) # end of tabPanel
                        ) # end of tabsetPanel
                    ) # end of fluidPage
            ), # end of data exploratory tab
# ______________________________________________________________________________________
            # Modeling tab
tabItem(tabName = "model", 
        fluidPage(
            # Set tab style
            tags$head(
                tags$style(type='text/css', 
                           ".nav-tabs {font-size: 20px}")),
            tabsetPanel(type="tab",
                        tabPanel("Modeling Info", textOutput("info"),
                                 fluidRow(
h4("There are three supervised learning models fitted on the data. In all three models, the data will be modeled as:"),
withMathJax("$$Yi \\sim Binomial(N_i, p_i)$$"),
h4("where Yi is the number of reported black bears in a location, Ni is the number of surveys in that location, and pi is the probability of a black bear observed in that location. There are a total of 8 predictors can be included in the models based on user selection."),
                                     column(4, wellPanel(
h3("Model 1: Generalized Linear Regression Model"),
h4("This model is fitted with Y/N as response, other variables as presictors."),
br(),
tags$b("Logistic Regression Model Equantion is as follows:"),
withMathJax("$$logit(p_i)= \\beta_0 + \\sum^k_{j = 1} x_i \\beta_j + \\epsilon_i$$"),
br()
)),

    column(4, wellPanel(
h3("Model 2: Regression Tree Model"),
h4("This model is fitted with Y/N as response, other variables as presictors."),
br(),
tags$b("Regression Tree Model Equantion is as follows:"),
withMathJax("$$logit(p_i)= \\beta_0 + \\sum^k_{j = 1} x_i \\beta_j + \\epsilon_i$$"),
br()
)),

    column(4, wellPanel(
h3("Model 3: Random Forest Model"),
h4("This model is fitted with Y/N as response, other variables as presictors."),
br(),
tags$b("Random Forest Model Equantion is as follows:"),
withMathJax("$$logit(p_i)= \\beta_0 + \\sum^k_{j = 1} x_i \\beta_j + \\epsilon_i$$"),
br()
))
                                     
                                 
                                 
                                 
                                 
                                 )),
                        
                        
                        
                        
                        tabPanel("Modeling Fitting", tableOutput("fitting"),
                                 
                                 fluidRow(
                                     column(3,
                                            box(width = 12,
                                                
                                                sliderInput("percent", "Training Set (percent)",
                                                            min = 0, max = 100, value = 80, step = 5),
                                                br(),
                                                br(),
                                                checkboxGroupInput(inputId = "sel_var",
                                                                   label = "Select Variables",
                                                                   choices = list(
                                                                       "Number of black bear" = "Y",
                                                                       "Number of surveys" = "N",
                                                                       "Proportion of forest" = "forest",
                                                                       "Proportion of grassland" = "grassland",
                                                                       "Proportion cropland" = "cropland",
                                                                       "Annual average temperature" = "temp",
                                                                       "Annual average precipitation" = "precip",
                                                                       "Human population" = "humanPop",
                                                                       "Protected lands" = "protected",
                                                                       "Ecoregion" = "ecoregion"
                                                                   ),
                                                                   selected = list(
                                                                       "Number of black bear" = "Y",
                                                                       "Number of surveys" = "N",
                                                                       "Protected lands" = "protected",
                                                                       "Ecoregion" = "ecoregion"
                                                                   )
                                                ), # end of checkbox
                                                )))
                                 
                                 
                                 ),
                        tabPanel("Prediction", tableOutput("prediction"),
                                 fluidPage(
                                 theme = "bootstrap.css",
                                 fluidRow(
                                     column(8, align="center", offset = 2,
                                            textInput("string", label="",value = "", width = "100%"),
                                            tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                                     )
                                 ),
                                 fluidRow(
                                     column(6, align="center", offset = 3,
                                            actionButton("button",label = textOutput("prediction")),
                                            tags$style(type='text/css', "#button { vertical-align: middle; height: 50px; width: 100%; font-size: 30px;}"))))
                                 
                                 
                                 ))))
            
            )
        )
    )
