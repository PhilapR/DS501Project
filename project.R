library(shiny)
library(dplyr)
library(onehot)
library(ggplot2)
library(readr)


url = "https://raw.githubusercontent.com/PhilapR/DS501Project/main/train.csv"
train = read.csv(url)
varnames = setdiff(names(train), c("Id","SalePrice"))

shinyApp(
  
  ui = fluidPage(
    mainPanel(
      titlePanel("Phlip Rago's 501 Final Project"),
      tabsetPanel(
        tabPanel("Visual Exploration",
                 sidebarPanel(p("Select variable to model against Sale Price:"),
                              selectInput("xaxis", "Select X axis:", varnames),
                              br(),
                              verbatimTextOutput('summary2'),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              p("Select variables to model against each other:"),
                              selectInput("xaxis2", "Select X axis:", varnames),
                              verbatimTextOutput('summary3'),
                              selectInput("yaxis", "Select y axis:", varnames),
                              verbatimTextOutput('summary4')),
                 mainPanel(
                   plotOutput('visual'),
                   br(),
                   plotOutput('visual2')
                 )
        ),  
        tabPanel("Variable Selection",
                 checkboxGroupInput("variables", "Select Variables to consider. Catagorical variables will be transformed to become usable.", choices = varnames , selected = c('LotArea','YearBuilt','BedroomAbvGr','FullBath','Functional'),inline = TRUE),
                 tableOutput("data")
        ),
        tabPanel("Plot of PCA",
                 plotOutput('cumpca'),
                 br(),
                 p("Select the PCs to plot"),
                 br(),
                 uiOutput("pcX"),
                 uiOutput("pcY"),
                 br(),
                 plotOutput('pca_plot')
                 
        ),
        tabPanel("Linear Regression",
                 uiOutput('regressor'),
                 p("Using the variables you have selected a basic linear regression using the pca results in a RMSE of:"),
                 verbatimTextOutput('model'),
                 br(),
                 br(),
                 p("To give some context here is a basic summary of the Sale Prices of the homes."),
                 br(),
                 verbatimTextOutput('summary')
        ),
        tabPanel("About the Project",
                 p('Data Selection'),
                 p("I explored several different data sets before settling on this one. I had no area that I was particually interested and this was a well reviewed set on kaggle. That being said the dimentionality of the data made it a little difficult to work with. Included in the data was 80 predictors and 1 Sale Price. Roughly half of the variables were catagorical. Varibles represented various charateristics of the homes sold, it is significant that the only locational data is one neighborhood variable."),
                 p("Analysis"),
                 p('My goal was to maximize the control of the user take full advantage of the Shiny interface. So I created pca analysis that allows for the user to select the variables and explore how it effects the output. From there a simple linear regression is done using the pca results. Again I made it cusomizable how many PCs were considered.'),
                 p('I also Inlcuded a tab for data exploreation. I figured that taking a look at the data graphicly woul help choose what variables to inlcude. With a dataset like this there are countless combinations of vaiables, but human instinct can inutively determine some of the most impactful charateristics. Seeing them graphicly emphisizes that intuition.'),
                 p('The regression is quite simple, but that is by design. Because the PCA has mean centered and scaled all of the data, naturally the regression performs well without more complication. PCA is benefical for this problem for two main reasons. 1. There are a lot of variables, component anaylsis help us reduce our dementionality signifiganly. 2. Catagoric variables make up alot of the data, again pca can represent these well all while reducing dimentionality.' )
                 
        )
      )
    )
  ),
  
  
  server = function(input, output, session) {
    
    
    selectedData <- reactive({
      train[,input$variables]
    })
    
    value <- reactive({
      train[,'SalePrice']
    })
    
    output$data <- renderTable({
      selectedData()
    })
    
    
    clean <- reactive({
      data <- selectedData()
      
      numericvar <- data%>%
        select_if(is.numeric) #this solution doesnt work for the other half of the data for some reason, spent so many hours on it
      
      catagoricvar <- data[, sapply(data, class) %in% c('character', 'factor')] #some how this solution doesnt work for the other half 
      
      catadf <- data.frame(catagoricvar)
      
      encode <- onehot(catadf,  addNA = TRUE, stringsAsFactors = TRUE, max_levels = 30)
      encoded <- predict(encode, catadf)
      asdf <- data.frame(encoded)
      
      df1 <- append(numericvar, asdf)
      na<- na.omit(df1) # done because I know there is one column left the could have N/A
      
      df <- data.frame(na)
      
      return(df)
      
    })
    
    output$summary <- renderPrint({
      summary(value())
    })
    
    output$summary2 <- renderPrint({
      summary(train[,input$xaxis])
    })
    
    output$summary3 <- renderPrint({
      summary(train[,input$xaxis2])
    })
    
    output$summary4 <- renderPrint({
      summary(train[,input$yaxis])
    })
    
    output$regressor <- renderUI({
      numericInput('regressors', 'How many PCs should be included?', value = 3, min= 2, max = length(colnames(compute_pca()$x)))
    })
    output$model <- renderText({
      RMSE = function(m, o){
        sqrt(mean((m - o)^2))
      }
      
      data <- clean()
      reg <- data[,1:input$regressors]
      model1<- lm(value() ~.,reg)
      prediction<-predict(model1,data)
      RMSE <- RMSE(value(),prediction)
      
      paste(RMSE)
    })
    
    
    
    output$visual <- renderPlot(
      
      ggplot(train, aes(train[,input$xaxis], SalePrice))+
        geom_point()+
        xlab(paste0(input$xaxis))+
        geom_smooth(method = 'lm')
    )
    
    output$visual2 <- renderPlot(
      
      ggplot(train, aes(train[,input$xaxis2], train[,input$yaxis], color = SalePrice))+
        geom_point()+
        xlab(paste0(input$xaxis2))+
        ylab(paste0(input$yaxis))+
        geom_smooth(method = 'lm')
    )
    
    
    
    compute_pca <- reactive({
      data <- clean()
      data0var <- data[, colSums(data != 0) > 0]
      df.pca <- prcomp(data0var, center = TRUE, scale. = TRUE)
      return(list(x = df.pca$x, pca = df.pca))
    })
    
    
    
    output$pcX <- renderUI({
      pca <- compute_pca()$x
      selectInput('selectX', label = 'X axis:',colnames(pca), selected = 'PC1')
    })
    output$pcY <- renderUI({
      pca <- compute_pca()$x
      selectInput('selectY', label = 'Y axis:',colnames(pca), selected = 'PC2')
    })
    
    output$pca_plot <- renderPlot({
      data<- compute_pca()$x
      df <- as.data.frame(data)
      x<- input$selectX
      y<- input$selectY
      color <- value()
      
      df%>%
        ggplot(aes(df[,x], df[,y], color = color))+
        geom_point()+
        xlab(paste0(x))+
        ylab(paste0(y))
      
      
    })
    
    output$cumpca <- renderPlot({
      pca_output <- compute_pca()$pca
      
      var_explained_df <- data.frame(PC = colnames(pca_output$x),var_explained=(pca_output$sdev)^2/sum((pca_output$sdev)^2))
      
      var_explained_df$ordered <- factor(var_explained_df$PC, levels = var_explained_df$PC)
      var_explained_df %>%
        ggplot(aes(x=ordered,y=var_explained,group = 1))+
        geom_point(size=4)+
        geom_line()+
        labs(title="Scree plot: PCA on scaled data")
      
    })
    
  }
  
)
