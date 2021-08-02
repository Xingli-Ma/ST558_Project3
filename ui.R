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
library(plotly)
require(corrplot)
library(tidyverse)
library(DT)

# Read data and clean up data  
ABBdata <- read.csv("ABBdata.csv")
# Convert categorical variable to factor
ABBdata$protected <- as.factor(ABBdata$protected)
ABBdata$ecoregion <- as.factor(ABBdata$ecoregion)
ABBdata <- ABBdata[,-1]

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
        tags$head(
            tags$style(
                HTML(
                    '#best_model {font-weight: bold; color: red;}', '#predict {color: red; font-size: 24px;}', '#fit {color: red; font-size: 24px;}' ))),
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
                        checkboxGroupInput(inputId = "sel_var",
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
                h3("The correlation plot between numeric variables"),
                plotOutput("corrPlot"),
                br()
                )
                ), # end of fluidRow
            fluidRow(
                column(6, 
                box(width = 15,
                selectInput(inputId="his_var", label="Select Variable to Create Histogram",
                choices = list(
                "Proportion of forest" = "forest",
                "Proportion of grassland" = "grassland",
                "Proportion cropland" = "cropland",
                "Annual average temperature" = "temp",
                "Annual average precipitation" = "precip",
                "Human population" = "humanPop"
                )),
                # create dynamic title
                uiOutput("title1"),
                plotOutput("hisPlot"),
                br()
                ) # end of box
                ), # end of column
                
                column(6,
                box(width = 15,
                selectInput(inputId="choice", label="Choose Variables to Make Scatter Plot", 
                choices = list(
                    "Number of surveys" = "N",
                    "Proportion of forest" = "forest",
                    "Proportion of grassland" = "grassland",
                    "Proportion cropland" = "cropland",
                    "Annual average temperature" = "temp",
                    "Annual average precipitation" = "precip",
                    "Human population" = "humanPop",
                    "Protected lands" = "protected",
                    "Ecoregion" = "ecoregion")),
                plotlyOutput("graph"),
                br()
                )# end of box
                ) # end of column 6
            ) # end of fluidRow
        ) # end of tabPanel
    ) # end of tabsetPanel
    ) # end of fluidPage
    ), # end of data exploratory tab
# ______________________________________________________________________________________
            # Modeling tab
# Model Info page
tabItem(tabName = "model", 
        fluidPage(
            # Set tab style
            tags$head(
                tags$style(type='text/css', 
                           ".nav-tabs {font-size: 20px}")),
            tabsetPanel(type="tab",
                        tabPanel("Modeling Info", textOutput("info"),
                                 fluidRow(wellPanel(
h3("There are three supervised learning models fitted on the data. In all three models, the data will be modeled as:"),
h3(withMathJax("$$Yi \\sim Binomial(N_i, p_i)$$")),
h3("where ", em("Yi"), " is the number of reported black bears in a location, ", em("Ni"), " is the number of surveys in that location, and ", em("pi"), " is the probability of a black bear observed in that location. There are a total of 8 predictors can be included in the models based on user selection."),
h3("In the Model Fitting section, all three models will be fitted on the traing set(A specific percent of the data that can be chosen by user. The user can also choose the variables that they want to include in the models). Cross validation will be used for selecting models where appropriate, and Model summaries will be reported. Then, the three models will be compared on the testing set and the best model was selected, which RMSE is the smallest. Finaly,in the Prediction section, the user will use the best model to make predictions on the response variable.")
)),
    fluidRow(
    column(4, wellPanel(
h2(tags$b("Model 1: Generalized Linear Model")),
h3("A logistic regression model is fitted with ", em("pi(Yi/Ni)"), " as response, other variables as predictors."),
br(),
h3("The logistic regression model equation is as follows:"),
h3(withMathJax("$$logit(p_i)= \\beta_0 + \\sum^k_{j = 1} x_i \\beta_j + \\epsilon_i$$")),
br()
)),

    column(4, wellPanel(
h2(tags$b("Model 2: Boosted Tree Model")),
h3("Boosting trees grow sequentially. Each subsequent tree is grown on a modified version of original data. Predictions update as trees are grown. They are slowly training trees to avoid overfit. This regression tree model is fitted with ", em("pi(Yi/Ni)"), " as response, other variables as predictors."),
br()
)),

    column(4, wellPanel(
h2(tags$b("Model 3: Random Forest Model")),
h3("This model use a rondom subset of predictors that user selected to fit each bootstrap tree to reduce possibal colinerity between predictors. This model is aldo fitted with ", em("pi(Yi/Ni)"), " as response, a rondom subset of other variables as predictors."),
br()))

                                 )
),
# ______________________________________________________________________________________
# Model Fitting tab
                tabPanel("Model Fitting", tableOutput("fitting"),
                        fluidRow(
                        column(4,
                               
                        box(width = 12,
                        sliderInput("percent", "Training Set (percent of data set)",min = 0, max = 100, value = 80, step = 5)),
                        
                        box(width = 12,
                        checkboxGroupInput(inputId = "sel_pre",
                        label = "Select Predictors",
                        choices = list(
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
                        "Protected lands" = "protected",
                        "Ecoregion" = "ecoregion"
                        )
                        ), # end of checkbox
                        ), # end of box
                        
                        box(width = 12,
                            sliderInput("fold", "Choose the Number of Folds for Cross Validation",min = 1, max = 10, value = 5, step = 1)
                            ),
                        
                        box(width = 12,
                            h3("Please click ", actionButton("fit", "Fit Models"), "to see model results"),
                            h3("Comparing model results and selecting the best model that has the smallest RMSE value."),
                            tableOutput("test_results"),
                            h3(verbatimTextOutput("best_model"))
                            )
                        ), #end of column 3
                        
                        column(8,
                            fluidRow(wellPanel(
                                h3("Model 1 output"),
                                verbatimTextOutput("dt_fit1")
                                )
                                ),
                            fluidRow(wellPanel(
                                h3("Model 2 output"),
                                verbatimTextOutput("dt_fit2")
                                )
                                ),
                            fluidRow(wellPanel(
                                h3("Model 3 output"),
                                verbatimTextOutput("dt_fit3")
                                )
                                ),
                            ) # end of column 9
                        ) # end of fluidRow
                    ), # end of fitting tab
# ______________________________________________________________________________________
# Model Prediction tab
        tabPanel("Prediction", tableOutput("prediction"),
            fluidRow( h2("Step 1:"), 
                           h3("Please input the value for each predictor to make prediction, which is the probability of an American black bear being observed in your survey location (50 km2 area)."),
                     br(),
                     column(3, fluidRow( numericInput("forest",label="Proportion of the region that is forest (please input a value between 0 and 1)", value = median(ABBdata$forest), min = 0, max = 1)),
                            br(),
                               fluidRow( numericInput("grassland",label="Proportion of the region that is grassland (please input a value between 0 and 1)", value = median(ABBdata$grassland), min = 0, max = 1))
                            ),
                     column(3, fluidRow( numericInput("cropland",label="Proportion of the region that is cropland (please input a value between 0 and 1)", value = median(ABBdata$cropland), min = 0, max = 1)),
                            br(),
                               fluidRow( numericInput("temp",label="Annual average temperature (please enter the temperature on the Fahrenheit scale)", value = median(ABBdata$temp), min = -100, max = 300))
                            ),
                     column(3, fluidRow( numericInput("precip",label="Annual average precipitation (please enter the precipitation in millimeters)", value =median(ABBdata$precip), min = 0, max = 10000)),
                            br(),
                               fluidRow( numericInput("humanPop",label= "Human population (please enter the human population in your survey location)", value = median(ABBdata$humanPop), min = 0, max = 1000000))
                            ),
                     column(3, fluidRow( selectizeInput("protected", "Protected lands (please select the Indicator of whether the region includes protected lands)", selected = as.character(0), choices = levels(ABB$protected))),
                            br(),
                               fluidRow( selectizeInput("ecoregion", "Ecoregion (please select the ecoregion that your survey location belongs to)", selected = "MARINE WEST COAST FOREST", choices = levels(ABB$ecoregion)))
                            )
                    ),
            fluidRow( h2("Step 2:"), 
                            h3("Choose Model to Make Prediction"),
                            radioButtons("model", "Select the Model Type", choices = list("Generalized Linear Model" = "linear", "Boosted Tree Model" = "tree", "Random Forest Model" = "rf"), selected = "rf")),
                      
            fluidRow( h2("Step 3:"),
                      h3("Please click ", actionButton("predict", "Predict"), " to make prediction."),
                      p("(Note: Remember to click Fit Models on the Model Fitting page, input the values for the above predictors, select the model type before you click the Predict button.)"),
                      h3("The prediction value is: "),
                      verbatimTextOutput("pred"))
                     
                
        )
            
            ))))
        )
    )
