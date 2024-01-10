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


