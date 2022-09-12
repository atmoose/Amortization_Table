ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "solar"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of loans", value = 1, min = 1, max = 8),
      selectInput("method", "Method", choices = c("Avalanche", "Snowball")),
      radioButtons("know", "Do you know your monthly payments?", c("Yes", "No")),
      numericInput("amt", "Total payment to be split among loans", value = 50, min = 1),
      actionButton("calc", "Calculate", class = "btn-block btn-success"), width = 3
    ),
    mainPanel(
      
      # Put everything in tabs
      tabsetPanel(type = "tabs",
                  
                  tabPanel(title = "Information",
                           ### General information about the app
                           h2("Disclaimer"),
                           p("This app only works for loans with monthly payments. If you don't know your monthly
                             payments for each loan, then the app's calculated values might differ slightly from
                             your actual values. Your loan holders might do intermediate rounding when calcuating
                             your loan payments which this app does not do. If you've already paid off some of a
                             loan and don't know your monthly payment, you can calculate the monthly payment using
                             the initial values for the loan. Then you can use the calculated payment with the
                             current loan balance."),
                           h3("Inputs"),
                           p("Select the number of loans you want to include and how much each month you can
                             afford to pay towards those loans. Fill in the generated table on the next
                             tab with your loan information."),
                           p("Select which repayment method you want to use. The avalanche method
                           will repay the highest interest rate loan first. This will
                           end up costing less over time, but takes longer to payoff a loan. The 
                           snowball method will repay the lowest balance loan first. This will payoff a 
                             loan quicker, but will pay more in interest. Click calculate
                             to create the amortization table of loan repayments."),
                           h3("Table and Plots"),
                           p("You can download a copy of the amortization table. The Payment Plot
                             shows you which loans you are paying off and in which order. The
                             Interest Plot shows you the monthly, cumulative interest payed 
                             on your loans and the total cumulative interest paid.")),
                  
                  tabPanel("Inputs & Table", uiOutput("table"),
                           dataTableOutput("pay.table"),
                           downloadButton("downloadTab", "Download .tsv")),
                  tabPanel("Payment Plot", plotOutput("pay.graph")),
                  tabPanel("Interest Plot", plotOutput("interest.graph"))
      ),
      width = 9
    )
  )
)