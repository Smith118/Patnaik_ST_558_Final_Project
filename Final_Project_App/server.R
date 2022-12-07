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

# PRIOR WORKS , DATA DOWNLOAD AND FUNCTIONS

df1 <-read_delim("Daily_Demand_Forecasting_Orders.csv",delim=";",
                 show_col_types = FALSE)
names(df1) <- c('week', 'day', 'non-urgent', 'urgent', 'typeA', 'typeB', 
                'typeC ', 'fiscal', 'traffic', 'banking1', 'banking2',
                'banking3', 'target')

df1<-df1 %>% mutate(day1=as.factor(day))
df1<-df1 %>% mutate(week1=as.factor(week))
m<-mean(df1$target)
df1<-df1%>%mutate(target1=as.factor(ifelse(target<m,0,1)))


df_see_data<-function(df_data,Criteria,nums1=0,nums2=0)
{
  if (Criteria==1) {
    show_df<-df_data 
  }
  
  else if (Criteria==2 )
  {
    if(nums1<=nrow(df_data)) 
    {
      show_df<-df_data[1:nums1, ] 
    }
    else {
      print("Please select within range")
    }
  }
  else if (Criteria==3 ) 
  {
    if(nums2<=ncol(df_data)) 
    {
      show_df<-subset(df_data, select=1:nums2)
    }
    else {
      print("Error")
    }
  }
  
  else if (Criteria==4 ) 
  {
    if(nums1<=nrow(df_data) & nums2<=ncol(df_data)) 
    {
      show_df1<-df_data[1:nums1, ] 
      show_df<-subset(show_df1, select=1:nums2)
      
    }
    else {
      print("Error")
    }
  }
  
  return (show_df)
}


shinyServer =function(input, output) {
  output$tab<-renderDataTable({
    datatable(df_see_data(df1,input$Criteria,input$nums1,input$nums2),
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'Bfrtip',
                scrollX = T,
                #buttons = c('copy', 'csv', 'excel','pdf'),
                buttons=list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span>'), 
                             list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span>'))
              ),
              class = "display"
    )  
  })
}