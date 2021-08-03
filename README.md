## Title: ST558 Project 3: R Shiny Dashboard: American Black Bear Data Analysis    

### Introduction  
This Shiny App includes four sections: About, Data, Data Exploration, and Modeling. The Modeling section has three pages: Modeling Info, Model Fitting, and Prediction. It can show a lot of dynamic and interactive information based on user inputs.

This App is created to demonstrate the American black bear data information and allow users to explore the data and make customized summaries. The user can fit three supervised models on the training set of the data and select the best model based on the model performance on the testing set. Finally, the user can choose one of the three models to make a prediction: the probability of an American black bear being observed in your survey location based on new data that the user collected.

### The following R packages are required to run this Shiny App.    

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

### To install all above packages that are needed, please run this line of code:    

install.packages(c("shiny", "shinydashboard", "knitr", "kableExtra", "tibble", "caret", "ggplot2", "plotly", "corrplot", "tidyverse", "DT"))    

### To run this app, please run the following code:    

shiny::runGitHub("ST558_Project3", "Xingli-Ma", ref="main")    





