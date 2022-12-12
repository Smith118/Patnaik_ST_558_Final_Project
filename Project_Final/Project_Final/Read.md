Read Me
================
SP
2022-12-12

## About this App

This App is created to implement basic steps of machine learning
algorithms using R Shiny App to demonstrate its usage for development of
tools for analysing dataset. This App will perform some basic
exploratory analysis of th data and performs basic implementation of
machine learning models, including model fitting and predictions with
comparison of the test results.

## Run the Code

Use this code to run the APP in your environment. Make sure to download
packages for usage.

`shiny::runGitHub("Smith118/Patnaik_ST_558_Final_Project", "rstudio", subdir = "/Project_Final/Project_Final/")`

## Included Page and Tab Items in the App

-   About page : The About page has links to the data and brief summary
    about the App and its usage.
-   Data Page: This page displays the data and can be used to sbset or
    filter data for viewing and downloading purpose.
-   Exploration Page: Performs some basic graphical and tabular summary.
-   Modelling Page: This has 3 tabs in it. a) Modelling Info: Talks
    briefly about concepts of Multiple Linear regression , Regression
    Trees and Random Forest Models. - Model Fitting: In this tab , Test
    Train split, modelling inputs and model fits are generated - Model
    Predcitions: Modelling outcomes and comparisos have been dome here,

## About Dataset

The data used here is the “Daily_Demand_Forecasting_Orders”. This data
is created to do some prediction based modelling for the total target
sales for a logistics company. The dataset has following variables.

-   @attribute Week_of_the_month {1.0, 2.0, 3.0, 4.0, 5.0}  
-   @attribute Day_of_the_week\_(Monday_to_Friday) {2.0, 3.0, 4.0, 5.0,
    6.0}  
-   @attribute Non_urgent_order integer  
-   @attribute Urgent_order integer  
-   @attribute Order_type_A integer  
-   @attribute Order_type_B integer  
-   @attribute Order_type_C integer  
-   @attribute Fiscal_sector_orders integer  
-   @attribute Orders_from_the_traffic_controller_sector integer  
-   @attribute Banking_orders\_(1) integer  
-   @attribute Banking_orders\_(2) integer  
-   @attribute Banking_orders\_(3) integer  
-   @attribute Target\_(Total_orders) integer  

This is basically a times series data, but can be used for regression
purposes as well. The response variable here is target variable only.
The predictor variables are usually log of the orders recived from each
industry type that contributes to the total orders. Hence, we have used
this App to demonstrate prediction outcomes for target variable only.

This is a very basic data and may have limited scope in terms of EDA-
like histograms may not be possible as it is already binned. But pair
plots can be generated and time series plots can be generated here to
look at data trends.

Link to dataset:
<https://archive.ics.uci.edu/ml/datasets/Daily+Demand+Forecasting+Orders>

## R Packages Used.

Following packages have been used here.   library(shiny) - For launching
and using shiny App  
library(shinydashboard)- For creation of dashboard.  
library(tidyverse)- Use its packages as applicable  
library(readr)- Use ias packages if applicable  
library(lubridate) - Use its packages if applicable  
library(caret)- for machine learning implementation  
library(DT)- Creation of Data Tables  
library(shinyWidgets)-For Shiny App  
library(caTools)-Use its packages as applicable  
library(shinyr)-For Shiny App  
library(rpart)-For Regression tree  
library(randomForest)-For Regression Forest  
library(Metrics)–For comparison metrics  For Regression Forest  
library(GGally)- For Pair plots  
