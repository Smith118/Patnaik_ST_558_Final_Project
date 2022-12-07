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


dashboardPage(skin="black",
              
              #TITLE
              dashboardHeader(title="Supervised Learning Case Study Daily_Demand_Forecasting_Orders",titleWidth=1000),
              
              #TABS AS PER INSTRUCTIONS
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("play-circle", verify_fa = FALSE)),
                menuItem("Data", tabName = "data", icon = icon("play-circle", verify_fa = FALSE))
                #menuItem("Data Exploration", tabName = "app", icon= icon("archive",verify_fa = FALSE))
                # menuItem("Modelling", tabName = "app", icon = icon("archive",verify_fa = FALSE)),
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
                  )
                )
              )
)


