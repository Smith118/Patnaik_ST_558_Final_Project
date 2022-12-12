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

#df1<-df1 %>% mutate(day1=as.factor(day))
#df1<-df1 %>% mutate(week1=as.factor(week))
#m<-mean(df1$target)
#df1<-df1%>%mutate(target1=as.factor(ifelse(target<m,0,1)))


para <- 1:ncol(df1)
names(para) <- names(df1)

#df_see_data<-function(df1,
                #     Criteria,nums1=0,nums2=0)
#{
 
  
#}

#all_tables<-function(df1,act,filter,varcount,var1)
#{ 
  
  
 # return(data.frame(df_summary))
#}  

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
  
  
#output$summ<-renderDataTable ({
#  sc<-datatable( 
#  if (input$act==1) {
 #   if(input$filter==1) 
 #   {   
 #   }
 #   else if(input$filter==2)
 #   {
 #     v<-as.names(input$var1)
 #   df1<-df1%>% group_by(v) %>% summarise_all(mean)
  #  }
 # }
 # else if (input$act==2) {
 #   if(input$filter==1) #totals
 #   {   
 #     summary(df1)
 #   }
  #  else if(input$filter==2)
 #   {
 #     v<-!as.names(input$var1)
  #    df1<-df1%>% group_by(v) %>% summarise_all(sum)
      
 #   }}
  
#  else if (input$act==3) {
    
    
 #   if(input$filter==1) #totals
 #   {   
  #    summary(df1)
 #   }
 #   else if(input$filter==2)
  #  {
 #      v<-as.names(input$var1)
 #     df1<-df1%>% group_by(v) %>% summarise_all(sd)
      
  #   }}
 #   )  
# 3return(sc) 
#})  
  
  
  
  
   
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
  
  
  
  
  
  df2<-reactive({df1})
  
  
  traind<-reactive({sample_frac(df2(), size=input$prop, replace=FALSE)
    })
  testd<-reactive({anti_join(df2(), traind())
    })
 
  
  
  
  
  
  #trainindex<-reactive ({
  # sample <- sample.split(dfr()$target, SplitRatio = input$prop)
  # dtrain  <- dfr()[sample == TRUE]
  # dtest   <-dfr()[sample == FALSE]
  # }) 
  
  #inTrain <- reactive({caret::createDataPartition(df2()$target, p = input$prop, list = FALSE) 
  #train <- df2()[inTrain()]
  #test<- df2()[-inTrain()]
  #            })  
  

  
  
  
  
  
  
  
  
  
  
  
  
  img(src='pics.png', align = "right")
  
  
  
  
  ### RGERESSION FITS
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
    req(traind(),input$columns,input$trym,input$cvn,input$tn,traind()$target)
    x <- as.numeric(traind()[[as.name(input$columns)]])
    y <- as.numeric(traind()$target)
    f <- paste0('target', " ~ ", paste0(input$columns, collapse = " + " ))
    f <- as.formula(f)
    control <- trainControl(method="cv", number=input$cvn,returnResamp="all")
    tunegrid <- expand.grid(.mtry=c(1:input$trym))
    rf <- train(f, data=traind(), method="rf", metric="RMSE", 
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
    req(testd(),input$columns,'target')
    x <- as.numeric(testd()[[as.name(input$columns)]])
    y <- as.numeric(testd()$target)
    predict(lm_model(), newdata = testd())
    
  }) 
  
  output$lmpredsummary <- renderPrint({
    req(lmpred())
    summary(lmpred())
  })
  
  treepred<-reactive({
    req(testd(),input$columns,'target')
    x <- as.numeric(testd()[[as.name(input$columns)]])
    y <- as.numeric(testd()$target)
    predict(treeFit(), newdata = testd())
    
  }) 
  
  output$treepredsummary <- renderPrint({
    req(treepred())
    summary(treepred())
  })
  
  
  rfpred<-reactive({
    req(testd(),input$columns,'target')
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
    datatable(data.frame("Multiple Reg "=RMSE(lmpred(), testd()$target),
                         "Regression Tree "=RMSE(treepred(), testd()$target),
                         "Random Forest "=RMSE(rfpred(), testd()$target)))
    
  })
  
  
  
  
}   














