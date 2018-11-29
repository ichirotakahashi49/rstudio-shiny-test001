shinyServer(function(input, output) {
   
  observeEvent(input$file,{
    
    csv_file <- reactive(read.csv(input$file$datapath))
    
    input_data <- csv_file()
    
    output$table <- DT::renderDataTable(input_data,
                                        options=list(scrollY="400px",
                                                     scrollCollapse=TRUE,
                                                     scrollX=TRUE
                                                     )
                                        )
    
  })
  
  observeEvent(input$button,{
    
    lof_result <- lof_eval(input_data,input$learn_data_start,input$learn_data_end,input$target_start,input$target_end)
    
    output$model_summary <- renderPrint(lof_result$model_summary)
    
    output$model_eval <- renderPrint(lof_result$model_eval)
    
    output$target <- DT::renderDataTable(lof_result$target,
                                         extensions=c('Buttons'),
                                         options=list(scrollY="400px",
                                                      crollCollapse=TRUE,
                                                      scrollX=TRUE,
                                                      dom='Blfrtip',
                                                      buttons=c('csv')
                                                      )
                                         )
    
    output$learn <- DT::renderDataTable(lof_result$learn,
                                        extensions=c('Buttons'),
                                        options=list(scrollY="400px",
                                                     scrollCollapse=TRUE,
                                                     scrollX=TRUE,
                                                     dom='Blfrtip',
                                                     buttons=c('csv')
                                                     )
                                        )
    
    output$plot_regression <- renderPlot({
      
      plot(lof_result$learn[,1],
           col="blue",
           type="l",
           ylim=c(0,10000),
           xlab = "Time",
           ylab = "Sales",
           xaxt="n")
      axis(1,at=1:nrow(lof_result$learn),labels=rownames(lof_result$learn))
      lines(lof_result$learn[,2],
            col="red",
            type="l",
            lty=3)
      labels <- colnames(lof_result$learn[,1:2])
      legend("topleft", legend = labels, col = c("blue","red"), lty = c(1,3))
      
    })
    
    output$plot_lof <- renderPlot({
      
      barplot(lof_result$target[,5])
      
    })
    
    output$plot_Pearson <- renderPlot({    
      
      barplot(lof_result$target[,4],ylim=c(-3,3))
      
    })
      
    output$plot_predict <- renderPlot({
      
      plot(lof_result$target[,1],
           col="blue",
           type="l",
           ylim=c(0,10000),
           xlab = "Time",
           ylab = "Sales",
           xaxt="n")
      axis(1,at=1:nrow(lof_result$target),labels=rownames(lof_result$target))
      lines(lof_result$target[,2],
            col="red",
            type="l",
            lty=3)
      labels <- colnames(lof_result$target[,1:2])
      legend("topleft", legend = labels, col = c("blue","red"), lty = c(1,3))
      
    })
      
    output$plot_gap <- renderPlot({
      
      barplot(lof_result$target[,3],ylim=c(-5000,5000))
      
    })
    
  })
  
})
