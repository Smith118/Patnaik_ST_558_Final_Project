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

#df_see_data<-function(df1,
                #     Criteria,nums1=0,nums2=0)
#{
 
  
#}

all_tables<-function(df1,act,filter,varcount,var1)
{ 
  
  df_1<-df1%>%select(- c(target1,week1,day1))
  if (act==1) {
    if(filter==1) #mean
    {   
      df_summary <-sapply(df_1,mean, 2)
    }
    else if(filter==2)
    {
      df_summary<-df_1%>% group_by((get(var1))) %>% summarise_all(mean)
      
    }
  }
  
  if (act==2) {
    if(filter==1) #totals
    {   
      df_summary <-sapply(df_1,sum, 2)
    }
    else if(filter==2)
    {
      
      df_summary<-df_1%>% group_by((get(var1))) %>% summarise_all(sum)
      
    }}
  
  return(data.frame(df_summary))
}  

#all_plots1<-function(df1,ply,ins,outs,cat,catvar)
#{


#}


shinyServer <-function(input,output,session) {
  
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
                #buttons = c('copy', 'csv', 'excel','pdf'),
                buttons=list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span>'), 
                             list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span>'))
              ),
    class = "display"
    )
  return(ss)  
  }) 
  
  output$plot1 <- renderPlot({ 
    
    #req(df1,input$ply,input$ins,input$outs,input$cat,input$catvar)
    g<-  if (input$ply==1) { #scatter
      { if (input$cat==1) 
      { ggplot(df1,aes_string(x =df1[[input$ins]],
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
      
    }
    else if(input$ply==2){ #bar
      
      { if (input$cat==1) 
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
      
    }
    g})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #trainindex<-reactive ({
  # sample <- sample.split(dfr()$target, SplitRatio = input$prop)
  # dtrain  <- dfr()[sample == TRUE]
  # dtest   <-dfr()[sample == FALSE]
  # }) 
  
  #inTrain <- reactive({caret::createDataPartition(df2()$target, p = input$prop, list = FALSE) 
  #train <- df2()[inTrain()]
  #test<- df2()[-inTrain()]
  #            })  
  
  df2<-reactive({df1})   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  img(src='pics.png', align = "right")
  
  
  
  
  ### RGERESSION FITS
  lm_model <- reactive({
    output$value <- renderPrint({ input$columns })  
    req(df2(),input$columns,df2()$target)
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
  
  output$resultlm <- renderPlot({
    req(lm_model())
    par(mfrow = c(1,2))
    plot(lm_model(),main="Multiple Regression results")
  })
  
  
  treeFit <- reactive({
    req(df2(),input$columns,input$mins,input$dep,df2()$target)
    x <- as.numeric(df2()[[as.name(input$columns)]])
    y <- as.numeric(df2()$target)
    f <- paste0('target', " ~ ", paste0(input$columns, collapse = " + " ))
    f <- as.formula(f)
    treefit <- rpart(
      f, data=df2(), 
      control = list(minsplit = input$mins, maxdepth = input$dep, cp = input$cp)
    )
    #control <- trainControl(method="cv", number=input$cvt)
    # treefit<-train(
    # f, data=df2(),method = "rpart",
    #  metric = "RMSE", 
    #trControl = control,
    #   minsplit = input$mins, 
    # depth = input$dep, 
    # cp = input$cp
    #)
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
    req(df2(),input$columns,input$trym,input$cvn,input$tn,df2()$target)
    x <- as.numeric(df2()[[as.name(input$columns)]])
    y <- as.numeric(df2()$target)
    f <- paste0('target', " ~ ", paste0(input$columns, collapse = " + " ))
    f <- as.formula(f)
    control <- trainControl(method="cv", number=input$cvn,returnResamp="all")
    tunegrid <- expand.grid(.mtry=c(1:input$trym))
    rf <- train(f, data=df2(), method="rf", metric="RMSE", 
                tuneGrid=tunegrid, ntree = input$tn, trControl=control)
    # rf<-randomForest(f,
    #             data=df2(), 
    #            ntree=input$tn,
    ##           importance=TRUE,
    #           replace=TRUE)
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
  
  
  # output$resultvimp <- renderPlot({
  # req(rforest())
  #varImpPlot(rforest(),type=2,main="Random Forest Variable importance")
  #})
  
  ## Predicitin Part 
  lmpred<-reactive({
    req(df2(),input$columns,'target')
    x <- as.numeric(df2()[[as.name(input$columns)]])
    y <- as.numeric(df2()$target)
    predict(lm_model(), newdata = df2())
    
  }) 
  
  output$lmpredsummary <- renderPrint({
    req(lmpred())
    summary(lmpred())
  })
  
  treepred<-reactive({
    req(df2(),input$columns,'target')
    x <- as.numeric(df2()[[as.name(input$columns)]])
    y <- as.numeric(df2()$target)
    predict(treeFit(), newdata = df2())
    
  }) 
  
  output$treepredsummary <- renderPrint({
    req(treepred())
    summary(treepred())
  })
  
  
  rfpred<-reactive({
    req(df2(),input$columns,'target')
    x <- as.numeric(df2()[[as.name(input$columns)]])
    y <- as.numeric(df2()$target)
    predict(rforest(), newdata = df2())
    
  }) 
  
  
  output$rfpredsummary <- renderPrint({
    req(rfpred())
    summary(rfpred())
  })
  
  output$fitrmse<-renderDataTable({
    req(lmpred(),treepred(),rfpred(),df2()$target) 
    datatable(data.frame("Multiple Reg "=sqrt(sum((lmpred()- df2()$target)^2)/length(lmpred())),
                         "Regression Tree "=sqrt(sum((treepred()- df2()$target)^2)/length(lmpred())),
                         "Random Forest "=RMSE(rfpred(), df2()$target)))
    
  })
  
  
  
  
}   














