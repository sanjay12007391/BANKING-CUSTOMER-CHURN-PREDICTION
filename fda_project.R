library(shiny)
library(shinythemes)
library(ISLR)
library(DataExplorer)
library(ggplot2)
require(dplyr)
library(ggcorrplot)
library(tidyr)
library(purrr)
library(printr)
library(pROC) 
library(ROCR) 
library(caret)
library(car)
library(rpart)
library(rpart.plot)
library(e1071) 
library(ISLR) 
library(markdown)
library(randomForest)
library(MLmetrics)


ui <- fluidPage(
  navbarPage("Churn!",
             tabPanel("Table",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          tags$hr(),
                          checkboxInput("header", "Header", TRUE)
                        ),
                        mainPanel(
                          tableOutput("contents")
                        )
                      )
             ),
             tabPanel("Summary",
                      verbatimTextOutput("summary")
             ),
             tabPanel("Missing Values",
                      verbatimTextOutput("missing")
             ),
             navbarMenu("EDA",
                        tabPanel("Histogram"
                                 ,mainPanel("Plotting Histograms to understand the distributions",
                                            fluidRow(
                                              splitLayout(cellWidths = c("32%", "30%","30%","30%","30%"), 
                                                          plotOutput("Hist1"), plotOutput("Hist2"),plotOutput("Hist3"), plotOutput("Hist4"),plotOutput("Hist5"))
                                            )
                                 )
                        ),
                        tabPanel("Categorical",
                                 mainPanel("Plotting Bar Charts to understand the Categorical Variables",
                                           fluidRow(
                                             splitLayout(cellWidths = c("30%", "30%","30%","30%","30%"), 
                                                         plotOutput("Bar1"),plotOutput("Bar2"),
                                                         #plotOutput("Bar3"),
                                                         plotOutput("Bar4"),plotOutput("Bar5"))
                                           )
                                 )
                        ),
                        tabPanel("Customer Churn",
                                 mainPanel("Customer Churn",
                                           fluidRow(
                                             splitLayout(cellWidths = c("73%", "73%"), 
                                                         plotOutput("Churn1"),plotOutput("Churn2"))
                                           )
                                 )
                                 
                        ),
                        tabPanel("Density graph",
                                 mainPanel("Density graph",
                                           fluidRow(
                                             splitLayout(cellWidths = c("73%", "73%"), 
                                                         plotOutput("den1"),plotOutput("den2"))
                                           )
                                 )
                                 
                        )
                        
             ),
             navbarMenu("Models",
                        tabPanel("Logistic Regression",
                                 mainPanel("Logistic Regression",
                                           verbatimTextOutput("logi_reg")
                                 )
                                 
                        ),
                        tabPanel("Decision Tree",
                                 mainPanel("Decision Tree",
                                          # plotOutput("dt1"),
                                           verbatimTextOutput("dt2")
                                           
                                 )
                                 
                        ),
                        tabPanel("Support Vector Machine",
                                 mainPanel("Support Vector Machine",
                                           verbatimTextOutput("svm")
                                 )
                                 
                        ),
                        tabPanel("Random Forest",
                                 mainPanel("Random Forest",
                                           verbatimTextOutput("ran_for")
                                 )
                                 
                        )
                        
                                    
             ),
             tabPanel("About Us",
                      fluidRow(
                           includeMarkdown("about.md")
                        )
             )
             
  )
  
)


server <- function(input, output) {
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  })
  output$summary <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    summary(read.csv(inFile$datapath, header = input$header))
  })
  output$missing <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    knitr::kable(sapply(tab, function(x) sum(is.na(x))), col.names = c("Missing Value Count"))
  })
  output$Hist1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$CreditScore)
  }) 
  output$Hist2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$Age)
    
  }) 
  output$Hist3 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$Tenure, breaks = 5)
    
  }) 
  output$Hist4 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$Balance, breaks = 10)
    
  }) 
  output$Hist5 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
     hist(tab$EstimatedSalary)
    
  }) 
  output$Bar1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(Gender))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Bar2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(Geography))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Bar3 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(NumOfProducts))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Bar4 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(HasCrCard))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Bar5 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = Exited)) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")+
      labs(title = "Exited vs Stayed", x = "Exit Status")
  }) 
  output$Churn1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df=read.csv(inFile$datapath, header = input$header)
    table1 <- table(df$Exited, df$Geography, dnn=c("Exit Count", "Geography")) 
    barplot(table1, ylab="Frequency", xlab="Geography", main="Churn by Geography", 
            col=c("turquoise4", "turquoise2" ), beside=TRUE, width=.2)
    
    legend("right", title="Exited", legend= sort(unique(df$Exited)),
           fill =c("turquoise4", "turquoise2" ), box.lty=0)
  
    
  }) 
  output$Churn2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(Geography))) +
      geom_bar(aes(fill = Gender),position="dodge") +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
      scale_fill_manual(values=c('#999999','#E69F00'))
    
  })
  output$den1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df=read.csv(inFile$datapath, header = input$header)
    df$Exited = as.character(df$Exited)
    ggplot(df, aes(x=Age, color=Exited)) + 
      geom_density()+
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      labs(title = "Density Plot: Age")
    
  }) 
  output$den2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df=read.csv(inFile$datapath, header = input$header)
    df$Exited = as.character(df$Exited)
    ggplot(df, aes(x=NumOfProducts, color=Exited)) + 
      geom_density()+
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      labs(title = "Density Plot: #Products")
    
  })
  output$logi_reg <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    data=read.csv(inFile$datapath, header = input$header)
    data = data[, !names(data) %in% c('RowNumber', 'CustomerId', 'Surname')]
    
    # data encoding
    data$Geography = factor(data$Geography, labels=c(0, 1, 2))
    data$Gender = factor(data$Gender, labels=c(0, 1))
    
    # data transformation
    data$Age = log(data$Age)
    data$CreditScore = log(data$CreditScore)
    data$Balance = log(data$Balance)
    data[data$Balance == -Inf, 'Balance'] <- 0
    
    # scaling
    fun_scale_0to1 <- function(x) {                           
      (x - min(x)) / (max(x) - min(x))
    }
    data$Age = fun_scale_0to1(data$Age)
    data$CreditScore = fun_scale_0to1(data$CreditScore)
    data$Balance = fun_scale_0to1(data$Balance)
    data$EstimatedSalary = fun_scale_0to1(data$EstimatedSalary)
    
    set.seed(1000)
    trainIndex <- createDataPartition(data$Exited, p = 0.8, list = FALSE, times = 1)
    training_data <- data[ trainIndex,]
    testing_data  <- data[-trainIndex,]
    LR_model = glm(Exited ~ CreditScore + Geography + Gender + Age +Tenure+ Balance + NumOfProducts + IsActiveMember, data = training_data, family = "binomial")
    pred2 <- predict(LR_model,testing_data,type="response")
    cutoff_churn <- ifelse(pred2>=0.50, 1,0)
    cm <- confusionMatrix(as.factor(testing_data$Exited),as.factor(cutoff_churn),mode = "everything",positive ='1')
    cm
  })

  output$dt1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data=read.csv(inFile$datapath, header = input$header)
    data = data[, !names(data) %in% c('RowNumber', 'CustomerId', 'Surname')]
    
    # data encoding
    data$Geography = factor(data$Geography, labels=c(0, 1, 2))
    data$Gender = factor(data$Gender, labels=c(0, 1))
    
    # data transformation
    data$Age = log(data$Age)
    data$CreditScore = log(data$CreditScore)
    data$Balance = log(data$Balance)
    data[data$Balance == -Inf, 'Balance'] <- 0
    
    # scaling
    fun_scale_0to1 <- function(x) {                           
      (x - min(x)) / (max(x) - min(x))
    }
    data$Age = fun_scale_0to1(data$Age)
    data$CreditScore = fun_scale_0to1(data$CreditScore)
    data$Balance = fun_scale_0to1(data$Balance)
    data$EstimatedSalary = fun_scale_0to1(data$EstimatedSalary)
    
    set.seed(1000)
    trainIndex <- createDataPartition(data$Exited, p = 0.8, list = FALSE, times = 1)
    training_data <- data[ trainIndex,]
    testing_data  <- data[-trainIndex,]
    Dtree = rpart(Exited ~., data = training_data, method = "class")
    set.seed(12345)
    cv.ct <- rpart(Exited ~., data = training_data, method = "class", 
                   cp = 0.00001, minsplit = 5, xval = 5)
    prune_dt <- prune(cv.ct,cp=cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
    predict_dt <- predict(prune_dt, testing_data,type="class") 
    prp(prune_dt, type = 1, extra = 1, split.font = 1, varlen = -10)
  })
  
  
  output$dt2 <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data=read.csv(inFile$datapath, header = input$header)
    data = data[, !names(data) %in% c('RowNumber', 'CustomerId', 'Surname')]
    
    # data encoding
    data$Geography = factor(data$Geography, labels=c(0, 1, 2))
    data$Gender = factor(data$Gender, labels=c(0, 1))
    
    # data transformation
    data$Age = log(data$Age)
    data$CreditScore = log(data$CreditScore)
    data$Balance = log(data$Balance)
    data[data$Balance == -Inf, 'Balance'] <- 0
    
    # scaling
    fun_scale_0to1 <- function(x) {                           
      (x - min(x)) / (max(x) - min(x))
    }
    data$Age = fun_scale_0to1(data$Age)
    data$CreditScore = fun_scale_0to1(data$CreditScore)
    data$Balance = fun_scale_0to1(data$Balance)
    data$EstimatedSalary = fun_scale_0to1(data$EstimatedSalary)
    
    set.seed(1000)
    trainIndex <- createDataPartition(data$Exited, p = 0.8, list = FALSE, times = 1)
    training_data <- data[ trainIndex,]
    testing_data  <- data[-trainIndex,]
    Dtree = rpart(Exited ~., data = training_data, method = "class")
    set.seed(12345)
    cv.ct <- rpart(Exited ~., data = training_data, method = "class", 
                   cp = 0.00001, minsplit = 5, xval = 5)
    prune_dt <- prune(cv.ct,cp=cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
    predict_dt <- predict(prune_dt, testing_data,type="class") 
    cm_dt <- confusionMatrix(as.factor(testing_data$Exited),as.factor(predict_dt),mode = "everything",positive='1')
    cm_dt
  })
  
  output$svm <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data=read.csv(inFile$datapath, header = input$header)
    data = data[, !names(data) %in% c('RowNumber', 'CustomerId', 'Surname')]
    
    # data encoding
    data$Geography = factor(data$Geography, labels=c(0, 1, 2))
    data$Gender = factor(data$Gender, labels=c(0, 1))
    
    # data transformation
    data$Age = log(data$Age)
    data$CreditScore = log(data$CreditScore)
    data$Balance = log(data$Balance)
    data[data$Balance == -Inf, 'Balance'] <- 0
    
    # scaling
    fun_scale_0to1 <- function(x) {                           
      (x - min(x)) / (max(x) - min(x))
    }
    data$Age = fun_scale_0to1(data$Age)
    data$CreditScore = fun_scale_0to1(data$CreditScore)
    data$Balance = fun_scale_0to1(data$Balance)
    data$EstimatedSalary = fun_scale_0to1(data$EstimatedSalary)
    
    set.seed(1000)
    trainIndex <- createDataPartition(data$Exited, p = 0.8, list = FALSE, times = 1)
    training_data <- data[ trainIndex,]
    testing_data  <- data[-trainIndex,]
    learn_svm <- svm(factor(Exited)~.,data=training_data) 
    predict_svm <- predict(learn_svm, testing_data,type ="response") 
    cm_svm <- confusionMatrix(as.factor(testing_data$Exited),as.factor(predict_svm),mode = "everything",positive='1')
    cm_svm
   
  })
  output$ran_for <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data=read.csv(inFile$datapath, header = input$header)
    data = data[, !names(data) %in% c('RowNumber', 'CustomerId', 'Surname')]
    
    # data encoding
    data$Geography = factor(data$Geography, labels=c(0, 1, 2))
    data$Gender = factor(data$Gender, labels=c(0, 1))
    
    # data transformation
    data$Age = log(data$Age)
    data$CreditScore = log(data$CreditScore)
    data$Balance = log(data$Balance)
    data[data$Balance == -Inf, 'Balance'] <- 0
    
    # scaling
    fun_scale_0to1 <- function(x) {                           
      (x - min(x)) / (max(x) - min(x))
    }
    data$Age = fun_scale_0to1(data$Age)
    data$CreditScore = fun_scale_0to1(data$CreditScore)
    data$Balance = fun_scale_0to1(data$Balance)
    data$EstimatedSalary = fun_scale_0to1(data$EstimatedSalary)
    
    set.seed(1000)
    trainIndex <- createDataPartition(data$Exited, p = 0.8, list = FALSE, times = 1)
    training_data <- data[ trainIndex,]
    testing_data  <- data[-trainIndex,]
    
    answer <- testing_data$Exited
    rf <- randomForest(factor(Exited)~., data = training_data)
    pred.rf <- predict(rf, testing_data,type ="response")
    cm_rf <- confusionMatrix(as.factor(testing_data$Exited),as.factor(pred.rf),mode = "everything",positive='1')
    cm_rf
    
  })
 
}

shinyApp(ui, server)