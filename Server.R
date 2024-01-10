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