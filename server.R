#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

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
library(dplyr)
library(DT)

# Read data and clean up data  
ABBdata <- read.csv("ABBdata.csv")
# Convert categorical variable to factor
ABBdata$protected <- as.factor(ABBdata$protected)
ABBdata$ecoregion <- as.factor(ABBdata$ecoregion)
ABBdata <- ABBdata[,-1]

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    #__________________________________________________________________________________________
    # About page
    # render image
    output$picture <- renderImage({
        return(list(src = "iNaturalist.jpg", contentType = "image/jpg", alt = "picture")) 
        }, deleteFile=FALSE)
    # render ui
    output$url <- renderUI({
        tagList(a("https://www.inaturalist.org", href="https://www.inaturalist.org"))
    })
    #__________________________________________________________________________________________
    # Data page
    data <- reactive({
        ABB <- ABBdata %>% filter(ecoregion %in% (input$select_ecoregion)) %>% select(as.vector(input$sel_var))
    })
    # render table
    output$dt_table <- renderDT({
        data()
        })
    # allow user to download the data set
    output$dt_download <- downloadHandler(
        filename = paste("ABBdata-download-", Sys.Date(), ".csv", sep=""),
        content = function(file) {
            write.csv(data(), file, row.names = FALSE)
        }
    )
    #__________________________________________________________________________________________
    # Data Exploration page
    # Numerical summaries
    # render summary table
    data_sum <- reactive({
        ABB_sum <- ABBdata %>% select(as.vector(input$sel_var))
    })
    output$dt_sum <- renderPrint({
        summary(data_sum())
    })
    
    # render contingency table
    output$dt_ecoregion <- function(){
        ABBdata %>% dplyr::group_by(ecoregion) %>% dplyr::summarise(black_bear = sum(Y)) %>% knitr::kable("html") %>% kable_styling("striped", full_width = F)
    }
    
    # Contingency table comparing number of black bear reports by whether or not protected lands
    # render contingency table
    output$dt_protected <- function(){
        ABBdata %>% dplyr::group_by(protected) %>% dplyr::summarise(black_bear = sum(Y)) %>% knitr::kable("html") %>% kable_styling("striped", full_width = F)
    }
    
    # Graphical summaries
    # Stacked barplot of number of black bear by protected lands and ecoregion 
    output$barPlot <- renderPlot({
    g <- ggplot(ABBdata, aes(x = protected, fill = ecoregion))
    g + geom_bar(aes(weight = Y), position = "stack") +
        labs(x = "Protected Lands", y = "Number of Black Bear") +
        scale_fill_discrete(name = "Ecoregion")
    })
    
    # Create correlation plot
    output$corrPlot <- renderPlot({
    # Create correlation matrix
    CM <- cor(ABBdata[, c("Y","N","forest","grassland","cropland","temp","precip","humanPop")])
    # Plot the correlation matrix
    corrplot(round(CM,2), method="circle")
    })
    
    #create dynamic histogram title1
    output$title1 <- renderUI({
        text1 <- paste0("Histogram for ", str_to_title(input$his_var))
        h3(text1)
    })
    
    # Create a histogram plot for select variable
    output$hisPlot <- renderPlot({
    h <- ggplot(ABBdata, aes_string(x=input$his_var))
    h + geom_histogram(bins=20, aes(y=..density..)) + 
        geom_density(stat="density", adjust=0.4, lwd=2, colour= "red") +
        xlab("Number of Black Bear") + ylab("Density")
    })
    
    #create dynamic histogram title2
    output$title2 <- renderUI({
        text2 <- paste0("Histogram for ", str_to_title(input$var), " by Ecoregion")
        h3(text2)
    })
    
    #____________________________________________________________________________________________
    # Create a histogram plot for select variable by ecoregion
    #output$hisPlotReg <- renderPlot({
        # "forest", "grassland", "cropland", "temp", "precip", "humanPop"
    #hr <- ggplot(ABBdata, aes_string(x=input$var))
    #hr + geom_histogram(bins=20, aes(y=..density..)) + 
        #geom_density(stat="density", adjust=0.4, lwd=2, colour= "red") +
        #facet_wrap(~ ecoregion, ncol = 2) +
        #xlab("Number of Black Bear") + ylab("Density")
    #})
    #_________________________________________________________________________________________
    
    output$graph <- renderPlotly({
        plot_ly(ABBdata, x = ~get(input$choice), y = ~Y, type = 'scatter', mode = 'markers') %>%
            layout(xaxis = list(title = input$choice), yaxis = list(title = "Number of Black Bear"))
        
    })
    #__________________________________________________________________________________________
    # Model page
    # Model Info tab
    # Model Fitting tab
    ABB_new <- ABBdata
    ABB_new$P <- ABB$Y/ABB$N
    # Remove Y variable
    ABB_new <- ABB_new[,-1]
    # Model 1: Generalized Linear Regression Model
    # Define training control
    
    # Split the data into training and testing sets, and save them as reactive values
    data_list <- reactive({
        # set seed for reproducible
        set.seed(558)
        ABB_user_select <- ABB_new %>% select(P, N, as.vector(input$sel_pre))
        ABBIndex <- createDataPartition(ABB_user_select$P, p = (input$percent)/100, list = FALSE)
        ABB_train <- ABB_user_select[ABBIndex, ]
        ABB_test <- ABB_user_select[-ABBIndex, ]
        list(ABB_train, ABB_test)
        })
    
    results_list <- reactive({
        # Fit model 1: Generalized Linear Regression Model
        fit1 <- train(P ~ .-N, data = data_list()[[1]],
                      method = "glm",
                      family = "binomial",
                      weights = data_list()[[1]]$N,
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = input$fold))
        # Fit model 2: Boosted Tree Model
        fit2 <- train(P~ ., data = select(data_list()[[1]],-c(N)),
                      method = "gbm",
                      preProcess = c("center", "scale"),
                      verbose = FALSE,
                      trControl = trainControl(method = "cv", number = input$fold))
        # Fit model 3: Random Forest Model
        fit3 <- train(P~ ., data = select(data_list()[[1]],-c(N)),
                      method = "rf",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = input$fold))
        model_fits <- list(fit1, fit2, fit3)
        model_fits
    })
    
    output$dt_fit1 <- renderPrint({
        results_list()[[1]]
    })
    
    
    output$dt_fit2 <- renderPrint({
        results_list()[[2]]
    })
    
    
    output$dt_fit3 <- renderPrint({
        results_list()[[3]]
    })
    
    # Comparing and Selecting Models
    output$test_results <- function(){
        
        # Making predictions on testing set
        predfit1 <- predict(results_list()[[1]], newdata = data_list()[[2]])
        predfit2 <- predict(results_list()[[2]], newdata = select(data_list()[[2]],-c(N)))
        predfit3 <- predict(results_list()[[3]], newdata = select(data_list()[[2]],-c(N)))
        
        # Evaluate the model performances by comparing the testing RMSE values
        testResults <- rbind(postResample(predfit1, data_list()[[2]]$P),
                         postResample(predfit2, data_list()[[2]]$P),
                         postResample(predfit3, data_list()[[2]]$P))
        testResults <- data.frame(testResults)
        row.names(testResults) <- c("Generalized Linear Regression",
                                "Boosted Tree",
                                "Random Forest")
        
        # Show RMSE values for all models
        testResults %>% knitr::kable("html") %>% kable_styling("striped", full_width = F)
    }
    
    # Declear the best model
    output$best_model <- renderPrint({
    bestModel <- rownames(testResults[testResults$RMSE == min(testResults$RMSE), ])
    paste("The best model is:", bestModel, "!")
    })
    
    #__________________________________________________________________________________________
    # Model Prediction
    
    # Create a data frame given user input values, and store it as reactive values.
    new_data <- reactive({
        newData <- data.frame(c(1), c(input$forest), c(input$grassland), c(input$cropland), c(input$temp), c(input$precip), c(input$humanPop), c(as.character(input$protected)),c(input$ecoregion), stringsAsFactors=FALSE)
        colnames(newData) <- c("N", "forest", "grassland", "cropland", "temp", "precip", "humanPop", "protected", "ecoregion")
        newData
    })
    
    # Make prediction
    output$pred <- renderPrint({
        if(input$model == "linear"){
            pred <- predict(results_list()[[1]], newdata = new_data())
            pred[[1]]
        } else if(input$model == "tree"){
            pred <- predict(results_list()[[2]], newdata = new_data())
            pred[[1]]
        } else if(input$model == "rf"){
            pred <- predict(results_list()[[3]], newdata = new_data())
            pred[[1]]
        }
    })
    
})
