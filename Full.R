# Load library
library(shiny)
library(shinythemes)
library(DT)

# Data
data <- data.frame(
  Month = c("January", "February", "March", "April","Mei", "June", "July", "August", 
            "September", "October", "November", "December"),
  x1 = c(150000,160000,170000,180000,190000,200000,210000,220000,230000,
         240000,250000,260000),
  x2 = c(8000,9500,10000,10500,11000,9000,11500,12000,12500,13000,14000,15000),
  x3 = c(5,4.5,4.8,4.6,5.1,4.7,4.9,5.0,5.2,5.3,5.4,5.5),
  x4 = c(8.5,8.2,8.4,8.5,8.6,8.7,8.8,8.9,8.7,8.8,8.9,9.0),
  x5 = c(20000,22000,25000,23000,30000,28000,27000,35000,40000,45000,50000,60000),
  y = c(120,150,160,165,180,170,190,210,230,250,300,350)
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(
      HTML("
        .title-panel {
          text-align: center;
        }
        .sidebar-panel {
          background-color: #F4F4F4;
          padding: 10px;
        }
      ")
    )
  ),
  div(class = "title-panel", titlePanel("Regression Application for E-Commerce")),
  sidebarLayout(
    sidebarPanel(
      selectInput("vardipen", label = h3("Variabel Dependent"), choices = list("y" = "y"), selected = 1),
      selectInput("varindepen", label = h3("Variabel Independent"), choices = list("x" = "x1","x2","x3","x4","x5"), selected = 1),
      class = "sidebar-panel"
    ),
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("About", 
                           fluidPage(
                             titlePanel("Tentang Data"),
                             tags$p("Data merupakan faktor-faktor yang diduga mempengaruhi volume penjualan bulan. Data yang dikumpulkan mencakup variabel-variabel berikut:"),
                             tags$p("y = volume penjualan bulanan (dalam ribuan USD),"),
                             tags$p("x1 = jumlah pengunjung situs web per bulan,"),
                             tags$p("x2 = jumlah transaksi per bulan,"),
                             tags$p("x3 = rata-rata jumlah item per transaksi,"),
                             tags$p("x4 = rating kepuasan pelanggan (skala 1-10),"),
                             tags$p("x5 = jumlah iklan online yang dijalankan per bulan."),
                             DT::dataTableOutput('about_tbl')
                           )
                  ),
                  tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                  tabPanel("Summary Model", verbatimTextOutput("summary")), # output Regresi
                  tabPanel("Residual Plot", verbatimTextOutput("residualPlot")),
                  tabPanel("Heteroscedasticity Plot", verbatimTextOutput("heteroPlot")),
                  tabPanel("Multicollinearity", verbatimTextOutput("multicollinearity")),
                  tabPanel("Autocorrelation", verbatimTextOutput("autokorelasi")),
                  tabPanel("Data", DT::dataTableOutput('tbl')), # Data dalam tabel
                  tabPanel("Prediction",
                           fluidPage(
                             titlePanel(title = div("Monthly Sales Volume", style = "color: #333333; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
                             sidebarLayout(
                               sidebarPanel(
                                 numericInput("x1.input", "x1 Value:", value = 150000),
                                 numericInput("x2.input", "x2 Value:", value = 8000),
                                 numericInput("x3.input", "x3 Value:", value = 5),
                                 numericInput("x4.input", "x4 Value:", value = 8.5),
                                 numericInput("x5.input", "x5 Value:", value = 20000),
                                 actionButton("predict_button", "Generate Predict", class = "btn-primary")
                               ),
                               
                               mainPanel(
                                 plotOutput("Monthly_Sales_Volume"),
                                 verbatimTextOutput("prediction_output")
                               )
                             )
                           )
                  )
                  
      )
      
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # Output Regresi
  fit <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
  names(fit$coefficients) 
  summary_output <- summary(fit)
  residual_plot <- ad.test(fit$residuals)
  autokorelasi_plot <- dwtest(fit)
  hetero_plot <- bptest(fit, studentize = F, data= data)
  multicollinearity_output <- vif(fit)
  
  output$summary <- renderPrint({
    summary_output
  })
  
  output$residualPlot <- renderPrint({
    residual_plot
  })
  
  output$heteroPlot <- renderPrint({
    print(hetero_plot)
  })
  
  output$multicollinearity <- renderPrint({
    multicollinearity_output
  })
  
  output$autokorelasi <- renderPrint({
    autokorelasi_plot
  })
  
  # Output Data
  output$tbl = DT::renderDataTable({
    DT::datatable(data, options = list(lengthChange = FALSE))
  })
  
  # Output Data di tab "About"
  output$about_tbl = DT::renderDataTable({
    DT::datatable(data, options = list(lengthChange = FALSE))
  })
  
  # Output Scatterplot
  output$scatterplot <- renderPlot({
    plot(data[,input$varindepen], data[,input$vardipen], main="Scatterplot",
         xlab=input$varindepen, ylab=input$vardipen, pch=19)
    abline(lm(data[,input$vardipen] ~ data[,input$varindepen]), col="red")
    lines(lowess(data[,input$varindepen],data[,input$vardipen]), col="blue")
  }, height=400)
  
  # Prediction 
  observeEvent(input$predict_button, {
    new_data <- data.frame(
      x1 = input$x1.input,
      x2 = input$x2.input,
      x3 = input$x3.input,
      x4 = input$x4.input,
      x5 = input$x5.input
    )
    
    # Predicted predicted_Monthly_Sales_Volume
    predicted_Monthly_Sales_Volume <- predict(fit, newdata = new_data)
    predicted_Monthly_Sales_Volume <- abs(predicted_Monthly_Sales_Volume)
    predicted_Monthly_Sales_Volume <- round(predicted_Monthly_Sales_Volume)
    
    output$Monthly_Sales_Volume <- renderPlot({
      plot(data$x1, data$y, col = "#3498DB", xlab = "number of website visitors per month", ylab = "Sales", main = "Scatterplot of Predicted Monthly Sales Volume")
      points(new_data$x1, predicted_Monthly_Sales_Volume, col = "red", pch = 16)
      legend("topright", legend = c("Actual Data", "Predicted Data"), col = c("#3498DB", "red"), pch = c(1, 16))
    })
    
    output$prediction_output <- renderPrint({
      cat("Prediction of Monthly Sales Volume: ", predicted_Monthly_Sales_Volume, "\n")
    })
  })
}

# Deploy Shiny App
shinyApp(ui = ui, server = server)
