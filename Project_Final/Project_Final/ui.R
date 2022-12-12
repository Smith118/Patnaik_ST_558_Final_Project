#
# ST 558 Final Project 
# SMITALI PATNAIK
# 12.12.2022
#


library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(lubridate) 
library(caret)
library(DT)
library(shinyWidgets)
library(caTools)
library(shinyr)
library(rpart)
library(randomForest)
library(Metrics)
library(ggcorrplot)
library(GGally)

para <- 1:ncol(df1)
names(para) <- names(df1)


dashboardPage(skin="black",
              #TITLE
              dashboardHeader(title="Machine Learning App Demo.",titleWidth=1000),
              
              #TABS AS PER INSTRUCTIONS
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("play-circle", verify_fa = FALSE)),
                menuItem("Data", tabName = "data", icon = icon("play-circle", verify_fa = FALSE)),
                menuItem("Data Exploration", tabName = "dataexploration", icon= icon("play-circle",verify_fa = FALSE)),
                menuItem("Modelling", tabName = "modelling", icon = icon("play-circle",verify_fa = FALSE))
              )),
              
              # BODY 
              dashboardBody(
                tabItems(
                  #TAB1-ABOUT
                  tabItem(tabName = "about",
                          fluidRow(
                            withMathJax(),
                            column(6,
                                   h1("Machine Learning App Demo"
                                   ),
                                   box(background="black",width=15,
                                       h4("Logistics form nervous sytem of a nation , as every industry relies on its services, 
                                       whether its medical, military , or manufacturing. They have the most important role.
                                       planning avaliable resources to manage services is very important and thuslogidtic companies have to
                                       a make startegy on how to deploy those resources. In this case, predicitve models come into picture.
                                       They use histrical data and trends to understand the orders they get and hence, plan and manage resources based on 
                                       prediction results to boost both services as well as profits. This app is made to showcase similar excercise using a 
                                       globally available dataset.",align = "justified"),
                                       br(),
                                       h4("This app implements some basic machine learning methods on the Daily_Demand_Forecasting_Orders
                                       dataset. Information on this dataset can be found below. 
                                           "),
                                       wellPanel(
                                         helpText(a("Click Here to Download Data",
                                                    href="https://archive.ics.uci.edu/ml/datasets/Daily+Demand+Forecasting+Orders")
                                         )),
                                       br(),
                                       h4("The App has 3 main tabs."),
                                       h4("The First tab shows the information about the Data. Here dataframe can be explored with
                                       its columns and rows. The type of data can be seen.
                                       and select functions can be performed like subsetting the data based on row and columns. 
                                       The User can make selections on the inputs and removed with **backspace key**.
                                       The selections are equipped with submit/update view , more chioces will be seen on selecting inputs to enhance
                                       usbility.
                                       The next tab is Data Exploration tab. This can be used to do basic summary statiscs like sum and 
                                       mean on the dataset based on categories (if needed). And is equipped to make select plots.
                                       The thid tab is the Modelling tab. This tab is further divided into 4 sections. 1st 
                                       one has brief summary on the modelling we are doing and guides through the modelling process.We
                                       have performed 3 regression based models- Multiple Linear Regression Model, Ragression Trees and Random Forest.
                                       The results of model fit can be found at the Model Fitting tab.
                                       The next tab is the prediction result tab which shows the sumary statistics for the prediction made with test data.
                                       The comparison metric used here is : RMSE for the fit and prediction comparison.",align="justified"),
                                       h4 ( "The model fit and prediction output are stated through their summaries. And parameters like C results, Variable 
                                       importance and RMSe , MSE etc can be referred from the summary results directly. "
                                        
                                       )
                                       
                                   )
                                   
                            ),
                            column(6,
                                   status = "primary", solidHeader = TRUE,
                                   imageOutput("pic", height = "auto"),
                                   h1("CASE STUDY: Machine Learning Predictions with Daily Demand Forecasting Orders Dataset."),
                                   box(background="black",width=9
                                   )))) ,
                  
                  
                  
                  #TAB2-DATA 
                  tabItem(tabName = "data",
                          fluidRow(
                            column(4,
                                   radioButtons("Criteria", label = h5("Select the Filter Type",style = "font-weight:bold"),
                                                choices = list("All" = 1, 
                                                               "Rows" = 2, 
                                                               "Columns" = 3,
                                                               "Select Rows & Columns"= 4),
                                                selected = 1)),
                            h4("Selection boxes based on your criteria- Options visible on select & submit"),
                            br(),
                            
                            column(4,
                                   conditionalPanel(
                                     condition = "input.Criteria == '2' | input.Criteria == '4'",
                                     numericInput("nums1", 'Select number of rows',value=10, min=1,max=100))),
                            
                            column(4,
                                   conditionalPanel(
                                     condition = "input.Criteria == '3' |input.Criteria =='4' ",
                                     numericInput("nums2", 'Enter number of columns',value=10, min=1,max=100))),
                            br(),
                            br(),
                            submitButton("Update View", icon("refresh")),
                            dataTableOutput("tab", width = 1500) 
                          )
                  ),
                  tabItem(tabName = "dataexploration",
                          fluidRow(
                            column(7,                                      
                                   box(width=15,
                                       
                                       #background="black",
                                       solidHeader=TRUE,
                                       selectInput("ins", "Select Variable for X (not for Pair plot)",
                                                   c("week","day","non_urgent","urgent","typeA",
                                                     "typeB","typeC","fiscal","traffic" ,"banking1",
                                                     "banking2" ,"banking3","target"),selected='week'),
                                       selectInput("outs", "Select Variable for Y (not for Pair plot)",
                                                   c("week","day","non_urgent","urgent","typeA",
                                                     "typeB","typeC","fiscal","traffic" ,"banking1",
                                                     "banking2" ,"banking3","target"),selected='week'),
                                       
                                       selectInput("ply", "Select Type of plot",
                                                   choices=list("Scatter Plot"=1,"Bar plot"=2,"Box Plot"=3,"Pair Plot"=4, selected = 1)),
                                       radioButtons("cat", label = h5("Plot Category wise?",style = "font-weight:bold"),
                                                    choices = list("No" = 1, 
                                                                   "Yes" = 2),selected=1),
                                       h4("Select Category variable to plot"),
                                       conditionalPanel(
                                         condition = "input.cat == '2'",
                                         selectInput("catvar", "Select Variable for Category",
                                                     c("day","week"),selected='week')),
                                       br(),
                                       submitButton("Update View", icon("refresh")),
                                       plotOutput("plot1")
                                   )
                            ),
                            column(5,
                                   box(width=10,
                                       solidHeader=TRUE,           
                                       selectInput("act", "Select Type of statistic",
                                                   choices=list("Mean"=1,"Sum"=2,"StdDev"=3,"ALL"=4 ,selected = 1)) ,
                                       radioButtons("filter", label = h5("Plot Category wise?",style = "font-weight:bold"),
                                                    choices = list("No" = 1, 
                                                                   "Yes" = 2),selected=1),
                                       h4("Select Category variable for grouping"),
                                       conditionalPanel(
                                         condition = "input.act == '2'",
                                         selectInput("var1", "Select Variable for Category",
                                                     c("day","week"),selected='week')),
                                       submitButton("Update View", icon("refresh")),
                                       br(),
                                       br(),
                                       dataTableOutput('summ')
                                       
                                   )
                                   
                                   
                                   
                            )
                            
                            
                          )),
                  
                  tabItem(tabName = "modelling",
                          tabsetPanel(type = "tabs",
                                      tabPanel(title="Modelling Info",
                                               fluidRow(
                                                 h2("About Models"),
                                                 box(width=10,
                                                
                                                 h4("The Modelling tab has been created to test three types of Regressions- Multiple LinearRegression Model, 
                                                    Regression Tree and a Random Forest Model. The Data has 12 set of predictos and 1 response variable called 
                                                    'target' which is the target orders to be fulfilled. The set of predictors has been kept open for the user
                                                    to choose , however the target variable is fixed to as the response variable as the data does not have any 
                                                    oppirtunites to choose other variables as the predictors. As they are not outcomes of any event.
                                                    
                                                    The Regression Models have been evaluated with same set of predictors to enable comparison however,Random 
                                                    Forest and Decision trees have the option for cross validation to find best parameters. Howver, user is 
                                                    allowed to choose some settings for the Regression Tree and Random Forest models to see differet outcomes.
                                                    
                                                    The user is also allowed to choose the percentage for train and test set, this allows for flexibility in 
                                                    deciding the split based on requirement.
                                                    "),
                                                 column(12,
                                                 h3("Multiple Linear Regression"),
                                                 box(
                                                 h4(" Multiple Linear Regression is extension to the Linear Regression that we see every day.
                                                 This is formulated as: 
                                                 "),
                                                 h4("\\(Y\\)=\\(\\beta\\)0 + \\(\\beta\\)1 * \\(X\\)1 + \\(\\beta\\)2 * \\(X\\)2......"),
                                                 h4(", where Y is the predicted reponsese value for the linear model with regression coefficients 
                                                     in X with their slopes and intercept (\\beta\\)0 .Regression works with following assumptions:"),
                                                  h4("1.Response is linearly related to the combination or transformations of predicotr variables"),
                                                  h4("2.The residuals follow a normal distribution"),
                                                  h4("3.Residuals have uniform variance"),
                                                 h4("MLM models are easy to implement and work perfectly when the predictors are known to have 
                                                    linear relationship with response variables. However, they are prone to noise and over fitting 
                                                    and may not perform well when the predictors are themselves highly correlated.
                                                    "),
                                                 ),
                                                 box(
                                                 h3("Regression Tree Model"),
                                                 h4("Regression trees are decision trees in which theresponse values \\(Y\\) can take continuous values 
                                                 instead of class labels in leaves. Regression trees are used to explain the decisions, 
                                                 and identify possible events tthat can occur, and see best outcome thorugh required metric. The 
                                                 advantages of Regression trees is that they are capacle in explaining
                                                 which attributes are used and how the attributes are used to obtain the outcome.The disdvantages
                                                 are that it is prone to overfitting and may get very complex.
                                                 ")
                                                 
                                                 
                                                 ),
                                                 
                                                 box(
                                                   h3("Random Forest"),
                                                   h4("Random Forest combins multiple decision trees to make a forest.
                                                   Random Forest model concept used relation that,  multiple uncorrelated models 
                                                   cab perform much better togther than they do alone.
                                                   Each tree gives a classification , and then the  
                                                   forest chooses the classification with the majority of the classification. When using Random
                                                   Forest for regression, the forest picks the average of the outputs of all trees. Random Forest models
                                                   can be accurate and may be give a good R^2. The advantge is that , it can reduce overfitting and 
                                                   but has disadvatnge that it still may rlead to large complex models. 
                                                 
                                                 ")
                                                   
                                                   
                                                 )
                                                    )
                                                ) )
                                               
                                               ),
                                      
                                    
                                      
                                      tabPanel(title="Model fitting",
                                               fixedRow(div(style = "height:10px"),
                                                        column(4,
                                                               h4("Choose Variables, and Train/test settings."),
                                                               numericInput("prop", 'Select percentage',value=0.7,min=0.5, max=1,step=0.1),
                                                               selectInput("columns", "Select Variable for X",
                                                                           c("week","day","non_urgent","urgent","typeA",
                                                                             "typeB","typeC","fiscal","traffic" ,"banking1",
                                                                             "banking2" ,"banking3","target","day1","week1" ),multiple=TRUE)),
                                                        column(4,
                                                               h4("Settings for Regression Tree:(note cv is inbuilt in 'rpart')"),
                                                               numericInput("mins", 'Select MinSplit',value=5,max=13, min=1,step=1),
                                                               numericInput("dep", 'Select MaxDepth',value=5,max=13, min=1,step=1),
                                                               numericInput("cp", 'Select Cp',value=0.001, min=0.001,step=0.001),
                                                               numericInput("cvt", 'Select cross validation number',value=5,max=50, min=1,step=1)),
                                                        column(4,
                                                               h4("Setting for Random Forest"),
                                                               numericInput("tn", 'Select ntree',value=5,max=13, min=1,step=1),
                                                               numericInput("trym", 'Select mtry',value=5,max=13, min=1,step=1),
                                                               numericInput("cvn", 'Select cross validation number',value=5,max=50, min=1,step=1)),
                                                        column(12,
                                                               submitButton("Update View", icon("refresh")),
                                                               verbatimTextOutput("value"),
                                                               fixedRow(    
                                                          column(12, 
                                                                 box(width=12,
                                                          dataTableOutput("infitrmse") ),     
                                                                 column(4,
                                                                        box(width=15,
                                                                            #sliderInput(
                                                                            # "prop",
                                                                            # label = h3("Use this to get Train/Test Split %"),
                                                                            # min = 0,
                                                                            # step=0.1,
                                                                            # max = 1,
                                                                            #  value =0.75),
                                                                            #textOutput('result'),
                                                                            ##DT::dataTableOutput('dftable'),
                                                                            h4("Multiple Linear Regression"),
                                                                            plotOutput('resultlm'),
                                                                            br(),
                                                                            verbatimTextOutput('lmsummary')
                                                                            )),
                                                                 column(4,
                                                                        box(width=15,
                                                                            h4("Regression Tree"),
                                                                            plotOutput('resulttree'),
                                                                            br(),
                                                                            verbatimTextOutput('rtreesummary')
                                                                            )),
                                                                 
                                                                 column(4,
                                                                        box(width=15,
                                                                            h4("Random Forest"),
                                                                            plotOutput('resultrf'),
                                                                            br(),
                                                                            verbatimTextOutput('rfsummary')
                                                                            ))
                                                               ))))),
                                      tabPanel(title="Predictions",
                                               fluidRow( 
                                                 h3("Test Prediction Results")),
                                               br(),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 box(width=15,height=200,
                                                     title='Basic Summary Statistics',
                                                     column(4,
                                                            box(width=10,height=10,
                                                                h3("Multiple Linear Regression"),
                                                                verbatimTextOutput('lmpredsummary')
                                                            )),
                                                     
                                                     column(4,
                                                            box(width=10,height=10,
                                                                h3("Regression Tree"),
                                                                verbatimTextOutput('treepredsummary'))),
                                                     column(4,
                                                            box(width=10,height=10,
                                                                h3("Random Forest"),
                                                                verbatimTextOutput('rfpredsummary')
                                                                
                                                            )
                                                     ))),
                                               br(), 
                                               br(),
                                               fluidRow(
                                                 box(width=5,height=10,
                                                     column(6,
                                                            h3("Test Prediction Comparisons RMSE"),
                                                            dataTableOutput("fitrmse")
                                                            
                                                            
                                                     )#,
                                                     #column(6,
                                                     #       h3("Test Prediction Comparisons Data"),
                                                     #        
                                                     #      plotOutput("databp") 
                                                     
                                                     #)
                                                     
                                                 )
                                                 
                                                 
                                                 
                                                 
                                               ))))
                  
                )))









