library(shiny)
library(dplyr)
library(onehot)
library(ggplot2)


varnames = setdiff(names(train), c("Id","SalePrice"))
min = 2

shinyApp(
  
  ui = fluidPage(
    mainPanel(
    titlePanel("Phlip Rago's 501 Final Project"),
    tabsetPanel(
    tabPanel("Variable Selection",
      checkboxGroupInput("variables", "Select Variables to consider. Catagorical variables will be transformed to become usable.", choices = varnames , selected = c('LotArea','YearBuilt','BedroomAbvGr','FullBath','Functional'),inline = TRUE),
      tableOutput("data")
    ),
    tabPanel("Plot of PCA",
             p("Select the PCs to plot"),
             uiOutput("pcX"),
             uiOutput("pcY"),
             plotOutput('pca_plot')
              
             ),
    tabPanel("Linear Regression",
             selectInput("xaxis", "Select X axis:", varnames),
             plotOutput('model')),
    tabPanel("Calculator"),
    tabPanel("Association  Matrix"),
    tabPanel("About the Project",
             uiOutput('cleantbl'))
    )  
  )),
  
  
  server = function(input, output, session) {
    
    
    selectedData <- reactive({
      train[,input$variables]
      })
    
    output$data <- renderTable(selectedData())
    
    observe({
      if(length(input$variables) < min){
        updateCheckboxGroupInput(session, "variables", selected= tail(input$variables,min))
      }
    })
    
    
    
    clean <- reactive({
      data <- selectedData()
      
      numericvar <- data %>%
        select_if(is.numeric)
      
      catagoricvar <-data %>%
        select_if(is.character)
      encode <- onehot(catagoricvar,  addNA = TRUE, stringsAsFactors = TRUE, max_levels = 30)
      
      encoded <- predict(encode, catagoricvar)
      asdf <- data.frame(encoded)
      
      df1 <- append(numericvar, asdf)
      df1[is.na(df1)] <- 0 # done because I know there is one column left the could have N/A
      
      return(df1)
      
    })
    
    
    
    model <- reactive({
      data <- clean()
      columns <- names(data)
      
      model1<- lm(train$SalePrice ~ ., train)
      
      return(model1)
    })
    
    

    output$model <- renderPlot(
    
      ggplot(train, aes(train[input$xaxis], SalePrice)) 
    )
    
    
    
    
    compute_pca <- reactive({
      data <- as.numeric(clean())
      df.pca <- prcomp(data, center = TRUE)
      
      return(df.pca)
    })
    
    
    pcs <- reactive({
      data <- compute_pca()
      
      return(names(data$x))
    })
    
    output$cleantbl <- renderUI({
      compute_pca()
      
    })
    
    output$pcX <- renderUI({
      x <- compute_pca()
      varSelectInput('pcX', label = 'X axis:',x$x)
    })
    output$pcY <- renderUI({
      y <- compute_pca()
      varSelectInput('pcY', label = 'Y axis:',y$x, selected = 'PC2')
    })
    
    output$pca_plot <- renderPlot({
      data <- compute_pca()
      
      x <- input$pcX
      y <- input$pcY
      
      plot(x,y)

                   
      })
    
    
      
    
    
    
    }
  
)



