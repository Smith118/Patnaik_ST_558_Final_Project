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
library(rpart)
library(randomForest)
library(Metrics)
library(GGally)
# PRIOR WORKS , DATA DOWNLOAD AND FUNCTIONS

df1 <-read_delim("Daily_Demand_Forecasting_Orders.csv",delim=";",
                 show_col_types = FALSE)
names(df1) <- c('week', 'day', 'non_urgent', 'urgent', 'typeA', 'typeB', 
                'typeC', 'fiscal', 'traffic', 'banking1', 'banking2',
                'banking3', 'target')






shinyServer <-function(input,output,session) {
  
  output$pic <- renderImage({
    return(list(src = "pics.PNG",contentType = "image/png",alt = "pic"))
  }, deleteFile = FALSE) 
  
  
  
  
  
  
  dff <- reactive({
    df1})
  
  output$tab<-renderDataTable ({
    ss<-datatable(
      if (input$Criteria==1) {
        df1
      }
      
      else if (input$Criteria==2 )
      {
        
        df1[1:input$nums1, ] 
      }
      else if (input$Criteria==3 ) 
      {
        
        df1%>%subset(select=1:input$nums2)
      }
      else if (input$Criteria==4 ) 
      {
        f<-df1[1:input$nums1, ] 
        f%>%subset(select=1:input$nums2)
      },
      extensions = 'Buttons',
      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = FALSE,
        ordering = TRUE,
        dom = 'Bfrtip',
        scrollX = T,
        scrollY = T, 
        #buttons = c('copy', 'csv', 'excel','pdf'),
        buttons=list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span>'), 
                     list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span>'))
      ),
      class = "display"
    )
    return(ss)  
  }) 
  
  
  output$summ<-renderDataTable ({
    sc<-datatable( 
      if (input$act==1) {
        if(input$filter==1) 
        { 
          df1%>% summarise_all(mean)
        }
        else if(input$filter==2)
        {
          df1%>% group_by(input$var1) %>% summarise_all(mean)
        }
      }
      else if (input$act==2) {
        if(input$filter==1) #totals
        {   
          df1%>%  summarise_all(sum)
        }
        else if(input$filter==2)
        {
          
          df1%>% group_by(input$var1) %>% summarise_all(sum)
          
        }}
      
      else if (input$act==3) {
        
        if(input$filter==1) #sd
        {   
          df1%>% summarise_all(sd)
        }
        else if(input$filter==2)
        {
          
          df1%>% group_by(input$var1) %>% summarise_all(sd)
          
        }}
      
      else if (input$act==4) {
        
        if(input$filter==1) #sd
        {   
          summary(df1)
        }
        else if(input$filter==2)
        {
          
          df1%>% group_by(input$var1) %>% summary(df1)
          
        } 
        
        
        
      }
      
      ,options = list(scrollY = '300px') 
    ) 
    return(sc) 
  })  
  
  output$plot1 <- renderPlot({ 
    
    #req(df1,input$ply,input$ins,input$outs,input$cat,input$catvar)
    g<- if (input$ply==1) 
      #scatter
    { 
      if (input$cat==1) 
      { 
        ggplot(df1,aes_string(x =df1[[input$ins]],
                              y = df1[[input$outs]]))+
          geom_point(alpha=0.5)+  geom_jitter()
      }
      else if(input$cat==2)
      {
        ggplot(df1,aes_string(x =df1[[input$ins]],
                              y = df1[[input$outs]],color=df1[[input$catvar]]))+
          geom_point(alpha=0.5)+  geom_jitter() 
      }
    }
    
    
    else if(input$ply==2) #bar
    { 
      if (input$cat==1) 
      {  
        ggplot(df1,aes_string(x =df1[[input$ins]],
                              y = df1[[input$outs]]))+
          geom_bar(position='dodge',stat='identity')
      }
      else if(input$cat==2)
      {
        ggplot(df1,aes_string(x =df1[[input$ins]],
                              y = df1[[input$outs]],color=df1[[input$catvar]]))+
          geom_bar(position='dodge',stat='identity') 
      }
    }
    else if(input$ply==3) #box
      
    { 
      if (input$cat==1) 
      {  
        ggplot(df1,aes_string(x =df1[[input$ins]],
                              y = df1[[input$outs]]))+
          geom_boxplot()
      }
      else if(input$cat==2)
      {
        ggplot(df1,aes_string(x =df1[[input$ins]],
                              y = df1[[input$outs]],color=df1[[input$catvar]]))+
          geom_boxplot() 
      }
    }
    
    else if(input$ply==4){ #hist
      ggpairs(df1)
    }
    
    g
    
  })
  
  
  
  ### REERESSION FITS
  
  
  df2<-reactive({df1})
  
  
  traind<-reactive({sample_frac(df2(), size=input$prop, replace=FALSE)
  })
  testd<-reactive({anti_join(df2(), traind())
  })
  
  
  
  lm_model <- reactive({
    output$value <- renderPrint({ input$columns })  
    req(traind(),input$columns,traind()$target)
    x <- as.numeric(traind()[[as.name(input$columns)]])
    y <- as.numeric(traind()$target)
    f <- paste0('target', " ~ ", paste0(input$columns, collapse = " + "))
    f <- as.formula(f)
    model <- lm(f, data = traind(), na.action=na.exclude)
    return(model)
  })
  
  
  
  
  output$lmsummary <- renderPrint({
    req(lm_model())
    summary(lm_model())
  })
  
  output$resultlm <- renderPlot({
    req(lm_model())
    par(mfrow = c(1,2))
    plot(lm_model(),main="Multiple Regression results")
  })
  
  
  treeFit <- reactive({
    req(traind(),input$columns,input$mins,input$dep,traind()$target)
    x <- as.numeric(traind()[[as.name(input$columns)]])
    y <- as.numeric(traind()$target)
    f <- paste0('target', " ~ ", paste0(input$columns, collapse = " + " ))
    f <- as.formula(f)
    treefit <- rpart(
      f, data=traind(), 
      control = list(minsplit = input$mins, maxdepth = input$dep, cp = input$cp)
    )
    
    return(treefit)
  })
  
  output$rtreesummary <- renderPrint({
    req(treeFit())
    summary(treeFit())
  }) 
  
  output$resulttree <- renderPlot({
    req(treeFit())
    plotcp(treeFit(),main="Tree Cp Results")
  })
  
  
  rforest <- reactive({
    req(traind(),input$columns,input$trym,input$cvn,input$tn,traind()$target)
    x <- as.numeric(traind()[[as.name(input$columns)]])
    y <- as.numeric(traind()$target)
    f <- paste0('target', " ~ ", paste0(input$columns, collapse = " + " ))
    f <- as.formula(f)
    control <- trainControl(method="cv", number=input$cvn,returnResamp="all")
    tunegrid <- expand.grid(.mtry=c(1:input$trym))
    rf <- train(f, data=traind(), method="rf", metric="RMSE", 
                tuneGrid=tunegrid, ntree = input$tn, trControl=control)
    
    return(rf)
    
  })
  
  output$rfsummary <- renderPrint({
    req(rforest())
    print(rforest())
  }) 
  
  output$resultrf <- renderPlot({
    req(rforest())
    plot(main="Random Forest fit Results",rforest())
    plot(rforest())
  })
  
  
  output$resultvimp <- renderText({
    req(rforest())
    varimp<-varImp(rforest(), scale = FALSE)
    varimp
  })
  
  lmfitd<-reactive({
    req(lm_model(),traind(),input$columns,'target')
    x <- as.numeric(traind()[[as.name(input$columns)]])
    y <- as.numeric(traind()$target)
    predict(lm_model(), newdata = testd())
    
  }) 
  
  treefitd<-reactive({
    req(treeFit(),traind(),input$columns,'target')
    x <- as.numeric(traind()[[as.name(input$columns)]])
    y <- as.numeric(traind()$target)
    predict(treeFit(), newdata = testd())
    
  }) 
  
  rffitd<-reactive({
    req(rforest(),traind(),input$columns,'target')
    x <- as.numeric(traind()[[as.name(input$columns)]])
    y <- as.numeric(traind()$target)
    predict(rforest(), newdata = testd())
    
  }) 
  
  output$infitrmse<-renderDataTable({
    req(lmfitd(),traind()$target,treefitd(),rffitd()) 
    datatable(data.frame("RMSE",
                         "Multiple Reg "=RMSE(lmfitd(), traind()$target),
                         "Regression Tree "=RMSE(treefitd(), traind()$target),
                         "Random Forest "=RMSE(rffitd(), traind()$target)
    ))
    
  })
  
  
  
  
  
  ## Prediction Part 
  lmpred<-reactive({
    req(lm_model(),testd(),input$columns,'target')
    x <- as.numeric(testd()[[as.name(input$columns)]])
    y <- as.numeric(testd()$target)
    predict(lm_model(), newdata = testd())
    
  }) 
  
  output$lmpredsummary <- renderPrint({
    req(lmpred())
    summary(lmpred())
  })
  
  treepred<-reactive({
    req(treeFit(),testd(),input$columns,'target')
    x <- as.numeric(testd()[[as.name(input$columns)]])
    y <- as.numeric(testd()$target)
    predict(treeFit(), newdata = testd())
    
  }) 
  
  output$treepredsummary <- renderPrint({
    req(treepred())
    summary(treepred())
  })
  
  
  rfpred<-reactive({
    req(rforest(),testd(),input$columns,'target')
    x <- as.numeric(testd()[[as.name(input$columns)]])
    y <- as.numeric(testd()$target)
    predict(rforest(), newdata = testd())
    
  }) 
  
  
  output$rfpredsummary <- renderPrint({
    req(rfpred())
    summary(rfpred())
  })
  
  output$fitrmse<-renderDataTable({
    req(lmpred(),treepred(),rfpred(),testd()$target) 
    datatable(data.frame("RMSE",
                         "Multiple Reg "=RMSE(lmpred(), testd()$target),
                         "Regression Tree "=RMSE(treepred(), testd()$target),
                         "Random Forest "=RMSE(rfpred(), testd()$target)))
    
  })
  
  
  
  output$databox<-renderDataTable({ 
    
    req(testd()$target,lmpred(),treepred(),rfpred())
    datgg<-datatable(data.frame( "Actual" = testd()$target,
                                 "Multiple Linear Regression" ,lmpred(),
                                 "Regression Tree",treepred(),
                                 "Random Forest", rfpred()),options=list(scrollX=T))
    
    
  })
  
  
  
  
  
  
}   














