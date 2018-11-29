shinyUI(navbarPage("異常検知（1変量時系列データ）",
  
  tabPanel("Input",
                   
    sidebarLayout(
      sidebarPanel(
         fileInput("file",
                   "CSV file",
                   accept=c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
                   ),
         numericInput("learn_data_start","学習データの開始行の設定",min=1,value=1),
         numericInput("learn_data_end","学習データの終了行の設定",min=1,value=36),
         numericInput("target_start","評価対象データの開始行の設定",min=1,value=37),
         numericInput("target_end","評価対象データの終了行の設定",min=1,value=42),
         actionButton("button","実行")
      ),
      mainPanel(
         DT::dataTableOutput("table")
      )
    )
  
  ),
  
  tabPanel("Output-Model",
           tabsetPanel(type="tabs",
                       tabPanel("Summary",
                                plotOutput('plot_regression'),
                                verbatimTextOutput('model_summary'),
                                verbatimTextOutput('model_eval')
                                ),
                       tabPanel("Data",
                                DT::dataTableOutput('learn')
                                )
                       )
           ),
  
  tabPanel("Output-Target",
           tabsetPanel(type="tabs",
                       tabPanel("Predict",
                                h3("Predict"),
                                plotOutput('plot_predict'),
                                h3("GAP(Residuals)"),
                                plotOutput('plot_gap')
                                ),
                       tabPanel("LOF",
                                h3("LOF"),
                                plotOutput('plot_lof'),
                                h3("Pearson Residuals"),
                                plotOutput('plot_Pearson')
                       ),
                       tabPanel("Data",
                                DT::dataTableOutput('target')
                                )
                       )
           )
           
  
))
