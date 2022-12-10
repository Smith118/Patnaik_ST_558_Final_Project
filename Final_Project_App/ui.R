#
# ST 558 Final Project 
# SMITALI PATNAIK
#


library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(lubridate) 
library(caret)
library(DT)
library(shinyWidgets)
library(shinyWidgets)
library(caTools)

para <- 1:ncol(df1)
names(para) <- names(df1)


dashboardPage(skin="black",
              #TITLE
              dashboardHeader(title="Supervised Learning Case Study Daily_Demand_Forecasting_Orders",titleWidth=1000),
              
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
                                   h1("Predictions Model using Supervised Machine Learning Methoda"),
                                   box(background="yellow",width=9,
                                       h4("This page describes machine learning methods for the data being used"),
                                   ),
                                   
                            )
                          )
                  ),

                  #TAB2-DATA 
                  tabItem(tabName = "data",
                          fluidRow(
                            radioButtons("Criteria", label = h5("Select the Filter Type",style = "font-weight:bold"),
                                         choices = list("All" = 1, 
                                                        "Rows" = 2, 
                                                        "Columns" = 3,
                                                        "Select Rows & Columns"=4),
                                         selected = 1),
                            h4("Selection boxes based on your criteria"),
                            column(2,
                                   conditionalPanel(
                                     condition = "input.Criteria == '2' | input.Criteria == '4' ",
                                     numericInput("nums1", 'Select number of rows',value=10, min=1,max=100))),
                            
                            column(4,
                                   conditionalPanel(
                                     condition = "input.Criteria == '3' |input.Criteria =='4' ",
                                     numericInput("nums2", 'Enter number of columns',value=10, min=1,max=100))),
                            dataTableOutput("tab", width = 1500) ,
                          )
                  ),
                  tabItem(tabName = "dataexploration",
                          fluidRow(
                            column(5,                                                 
                                   box(width=50,
                                       title="Scatter Plot",
                                       background="black",
                                       solidHeader=TRUE,
                                        selectInput("ply", "Select Type of plot",
                                                       choices=list("Scatter"=1,"Bar"=2, selected = NULL)),
                                         conditionalPanel(
                                          condition = "input.ply == '1' | input.ply == '2' ",
                                         
                                           selectInput("ins", "Select Variable for X",
                                                       c("week","day","non_urgent","urgent","typeA",
                                                         "typeB","typeC","fiscal","traffic" ,"banking1",
                                                         "banking2" ,"banking3","target","day1","week1" ,"target1")),
                                           selectInput("outs", "Select Variable for Y",
                                                       c("week","day","non_urgent","urgent","typeA",
                                                         "typeB","typeC","fiscal","traffic" ,"banking1",
                                                         "banking2" ,"banking3","target","day1","week1" ,"target1")),
                                           radioButtons("cat", label = h5("Plot Category wise?",style = "font-weight:bold"),
                                                        choices = list("No" = 1, 
                                                                       "Yes" = 2), selected = 1),
                                           h4("Select Category variable to plot"),
                                           conditionalPanel(
                                             condition = "input.cat == '1'",
                                             selectInput("catvar", "Select Variable for Category",
                                                         c("day1","week1" ,"target1"))),
                                         ),
                                             br(),
                                             br(),
                                             plotOutput("plot1")
                                         
                                       
                                   )))),
                  
                  tabItem(tabName = "modelling",
                          fluidRow(
                                   tabBox(
                                   title="Split, Modelling and Predictions ",
                                   #tabsetPanel("Modelling inf",
                                      tabPanel("Modelling Info","text")),
                                   
                                      tabPanel("Regression Fits",
                                            selectInput("columns", "Select Variable for Y",
                                                          c("week","day","non_urgent","urgent","typeA",
                                                            "typeB","typeC","fiscal","traffic" ,"banking1",
                                                            "banking2" ,"banking3","target","day1","week1" ,"target1"),multiple=TRUE),
                                              #sliderInput(
                                               # "prop",
                                               # label = h3("Use this to get Train/Test Split %"),
                                               # min = 0,
                                               # step=0.1,
                                               # max = 1,
                                              #  value =0.75),
                                              numericInput("prop", 'Select percentage',value=0, min=1,step=1),
                                               br(),
                                               br(),
                                               #textOutput('result'),
                                               ##DT::dataTableOutput('dftable'),
                                               h3("Multiple Linear Regression"),
                                               verbatimTextOutput('lmsummary'),
                                               plotOutput('importance')),
                                    tabPanel(
                                      "Decision Tree",
                                     numericInput("fold", 'Select percentage',value=3, min=1,step=5),
                                     numericInput("rep", 'Select percentage',value=3, min=1,step=5),
                                     verbatimTextOutput('rtreesummary'),
                                      ))))
                                   
                                   
                                   ))
                                  
                  
                  
                  
               

