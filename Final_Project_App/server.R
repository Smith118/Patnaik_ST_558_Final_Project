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
library(caTools)
library(shinyr)
# PRIOR WORKS , DATA DOWNLOAD AND FUNCTIONS

df1 <-read_delim("Daily_Demand_Forecasting_Orders.csv",delim=";",
                 show_col_types = FALSE)
names(df1) <- c('week', 'day', 'non_urgent', 'urgent', 'typeA', 'typeB', 
                'typeC', 'fiscal', 'traffic', 'banking1', 'banking2',
                'banking3', 'target')

df1<-df1 %>% mutate(day1=as.factor(day))
df1<-df1 %>% mutate(week1=as.factor(week))
m<-mean(df1$target)
df1<-df1%>%mutate(target1=as.factor(ifelse(target<m,0,1)))


para <- 1:ncol(df1)
names(para) <- names(df1)


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

all_plots1<-function(df1,ply,ins,outs,cat,catvar)
{
  if (ply==1) { #scatter
    { if (cat==1) 
      {  ggplot(df1,aes_string(x =ins,
                                    y = outs))+
          geom_point(alpha=0.5)+  geom_jitter()
      }
      else if(cat==2)
      {
          ggplot(df1,aes_string(x =ins,
                                 y = outs,color=catvar))+
          geom_point(alpha=0.5)+  geom_jitter() 
      }
    }
  
  }
  else if(ply==2){ #bar
   
   { if (cat==1) 
     {  
       ggplot(df1,aes_string(x =ins,
                             y = outs))+
       geom_bar(position='dodge',stat='identity')
   }
     else if(cat==2)
     {
       ggplot(df1,aes_string(x =ins,
                              y = outs,color=catvar))+
       geom_bar(position='dodge',stat='identity') 
     }
   }
   
  }
   
}

  



shinyServer =function(input,output,session) {
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
              class = "display")
  })
  
df2<-reactive({df1})
 
 #trainindex<-reactive ({
# sample <- sample.split(dfr()$target, SplitRatio = input$prop)
# dtrain  <- dfr()[sample == TRUE]
# dtest   <-dfr()[sample == FALSE]
# }) 
 
#inTrain <- reactive({caret::createDataPartition(df2()$target, p = input$prop, list = FALSE) 
#train <- df2()[inTrain()]
#test<- df2()[-inTrain()]
 #            })  
lm_model <- reactive({
      req(df2(),input$columns,'target')
      x <- as.numeric(df2()[[as.name(input$columns)]])
      y <- as.numeric(df2()$target)
      f <- paste0('target', " ~ ", paste0(input$columns, collapse = " + "))
     f <- as.formula(f)
      model <- lm(f, data = df2(), na.action=na.exclude)
      return(model)
})


                           

    output$lmsummary <- renderPrint({
      req(lm_model())
      summary(lm_model())
        })
        
     output$importance <- renderPlot({
     req(lm_model())
      par(mfrow = c(1,2))
      plot(lm_model())
      plot(lm_model())
      })
        
        
    treeFit <- reactive({
      req(df2(),'target')
      x <- as.numeric(df2()[[as.name(names(df2()))]])
      y <- as.numeric(df2()$target)
      f <- paste0('target', " ~ ", "." )
      f <- as.formula(f)
      train(f, data=df2(), method = "rpart",
                       trControl = trainControl(method = "repeatedcv", number = input$folds, repeats = input$rep),
                        preProcess = c("center", "scale"),
                        tuneGrid = data.frame(cp = seq(0, 0.1, 0.001)))
          
    return(treeFit)
})
  
output$rtreesummary <- renderPrint({
  req(treeFit())
  summary(treeFit())
}) 
  
    
}   

  
 
 
  
  
  
  
  

  
  
  
  