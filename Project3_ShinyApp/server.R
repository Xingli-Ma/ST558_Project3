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
print(str(ABBdata))

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
        ABB_sum <- ABBdata %>% select(as.vector(input$sel_var_2))
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
        text1 <- paste0("Histogram for ", str_to_title(input$var))
        h3(text1)
    })
    
    # Create a histogram plot for select variable
    output$hisPlot <- renderPlot({
    h <- ggplot(ABBdata, aes_string(x=input$var))
    h + geom_histogram(bins=20, aes(y=..density..)) + 
        geom_density(stat="density", adjust=0.4, lwd=2, colour= "red") +
        xlab("Number of Black Bear") + ylab("Density")
    })
    
    #create dynamic histogram title2
    output$title2 <- renderUI({
        text2 <- paste0("Histogram for ", str_to_title(input$var), " by Ecoregion")
        h3(text2)
    })
    
    # Create a histogram plot for select variable by ecoregion
    output$hisPlotReg <- renderPlot({
        # "forest", "grassland", "cropland", "temp", "precip", "humanPop"
    hr <- ggplot(ABBdata, aes_string(x=input$var))
    hr + geom_histogram(bins=20, aes(y=..density..)) + 
        geom_density(stat="density", adjust=0.4, lwd=2, colour= "red") +
        facet_wrap(~ ecoregion, ncol = 2) +
        xlab("Number of Black Bear") + ylab("Density")
    })
    
    
    #__________________________________________________________________________________________
    # Model page
    # Model Info tab

    # set seed for reproducible
    #set.seed(558)
    #train <- reactive({sample(1:nrow(ABBdata), size = nrow(ABBdata)*as.numeric(input$percent)/100)})
    #test <- dplyr::setdiff(1:nrow(ABBdata),train())
    #train_set <- ABBdata[train,]
    #test_set <- ABBdata[test,]
    
})
