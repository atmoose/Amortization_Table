library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(shinyWidgets, warn.conflicts = FALSE)
library(shinyjs)

## Set working directory with files
# setwd("......")

## Call the helper functions used in the app.
source('helper_functions.R')

## Separate UI & Server from app. Calling it into the workspace
source('ui.R')
source('server.R')


shinyApp(ui = ui, server = server)

################################################################################
################################################################################

### Everything was copied into separate .R files to make it easier to change
### specific parts. Commented them out in this file.

################################################################################
##### Helper Functions
# 
# # Minimum monthly payment
# monthly.pay = function(l, r, t){
#   # Interest rate is in percent. Change to decimal
#   i = (1+r/100)^(1/12) - 1  # monthly effective interest rate
#   y = l * i / (1 - (1 + i)^-t)
#   return(round(y, 2))
# }
# 
# # Balance in amortization table
# balance = function(b, mp = monthly.pay){
#   y = b - mp
#   return(round(y, 2)
# }
# 
# # Interest in amortization table
# interest.pay = function(b, r){
#   i = (1+r/100)^(1/12) - 1
#   y = round(b * i, 2)
#   return(round(y, 2))
# }
# 
# # Principal being paid
# principal.pay = function(mp, inter){
#   y = mp - inter
#   return(round(y, 2))
# }
# ################################################################################
# 
# 
# ################################################################################
# ################################################################################
# ##### AVALANCHE METHOD code
# avalanche.amort.table = function(df, apr,
#                                  R = MIN.PAY,
#                                  amt=sum(MIN.PAY)){
#   
#   num.l = max(as.numeric(df[,6]))
#   
#   # Checking to make sure you are paying enough each month
#   #if(amt < sum(monthly.pay(df[1:4,7], df[1:4,8], df[1:4,9], df[1:4,10]))){
#   if(amt < sum(R)){
#     stop('Amount not meeting the minimum monthly payments')
#   }
#   
#   # Running amount left of your payment
#   run.amt = amt
#   
#   # Increase to next month
#   time.mnth = df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),1] + 1
#   
#   # # Calculate minimum monthly payment
#   # R = monthly.pay(df[(max(df[,1])*4+1):(max(df[,1]+1)*4),7],
#   #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),8], 
#   #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),9], 
#   #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),10])
#   
#   
#   # Calculate interest off remaining balance
#   I = interest.pay(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], apr)
#   
#   # Find loan with the highest interest rate, and balance is non-zero
#   non.zero.bal = which(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5] != 0) # Non-zero balance
#   
#   # Set any zero balance loans to 0 monthly payment.
#   R[-non.zero.bal] = 0
#   
#   # Don't need, but makes sure not paying interest on zero balance loans.
#   I[-non.zero.bal] = 0
#   
#   # Find which balances are non-zero and return indices, then order them based
#   # on decreasing interest rates. This should make the loan that still has a 
#   # balance and the highest interest rate be listed first.
#   #sort.interest = which(df[(max(df[,1])*4+1):(max(df[,1]+1)*4) ,5] != 0, arr.ind = T)[order(df[non.zero.bal, 8], decreasing = TRUE)]
#   
#   # If multiple loans have the same interest rate, then order by highest balance.
#   # Idea is to pay off the loan accruing the most interest per period.
#   sort.interest = with(df, order(-apr, df[[5]][1:num.l]))
#   
#   ## Not needed. Max interest can change depending on which loans are paid off.
#   #max.interest.loc = sort.interest[1]
#   
#   # Running counter that takes care of two issues:
#   ## - Makes sure you are starting at the top of the sorted interest rates. 
#   ## - If you reach the end of this whole process and still have money left over
#   ##   in the running payment amount, this means you paid off everything and
#   ##   escape the loop.
#   loc = 1
#   
#   # While you still have money that you can allocate to your loans, do the following
#   while(abs(run.amt - 0) > 10^(-10)){
#     # If you cannot pay off your remaining balance of the highest interest rate
#     # loan with the running amount
#     if(df[max(df[,1]*num.l)+sort.interest[loc], 5] > run.amt-sum(R[-sort.interest[loc]])){
#       # Pay as much as you can
#       R[sort.interest[loc]] = run.amt - sum(R[-sort.interest[1:loc]])
#       run.amt = amt - sum(R)
#     } else { # Otherwise
#       # Change the payment of the highest interest rate loan to be the balance
#       R[sort.interest[loc]] = df[max(df[,1]*num.l)+sort.interest[loc], 5] + I[sort.interest[loc]]
#       # Take that payment from the running amount
#       run.amt = run.amt - R[sort.interest[loc]]
#     }
#     # If you've reached the end of the highest interest rates, break the loop
#     if(loc == length(sort.interest)) {break}
#     
#     # Increase to the next highest interest rate. This only comes into play when
#     # you can pay off multiple loans in one monthly payment.
#     loc = loc + 1
#   }
#   
#   # Calculate remaining principal
#   P = principal.pay(R, I)
#   
#   # Update balance off monthly payment
#   B = balance(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], P)
#   
#   # Construct this month's amortization table
#   dat.f = data.frame(month = time.mnth, 
#                      payment = R, 
#                      interest = I, 
#                      principal = P,
#                      balance = B, 
#                      loan.number = factor(1:num.l))
#   
#   # Make sure the names are the same when concatenating the data frames.
#   colnames(dat.f) = colnames(df)
#   return(dat.f)
# }
# ################################################################################
# ################################################################################
# 
# ################################################################################
# ################################################################################
# ##### AVALANCHE METHOD code
# snowball.amort.table = function(df, apr,
#                                 R = MIN.PAY,
#                                 amt=sum(MIN.PAY)){
#   # Checking to make sure you are paying enough each month
#   if(amt < sum(R)){
#     stop('Amount not meeting the minimum monthly payments')
#   }
#   
#   num.l = max(as.numeric(df[,6]))
#   
#   # Running amount left of your payment
#   run.amt = amt
#   
#   # Increase to next month
#   time.mnth = df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),1] + 1
#   
#   # # Calculate minimum monthly payment
#   # R = monthly.pay(df[(max(df[,1])*4+1):(max(df[,1]+1)*4),7],
#   #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),8], 
#   #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),9], 
#   #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),10])
#   
#   
#   # Calculate interest off remaining balance
#   I = interest.pay(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], 
#                    apr)
#   
#   # Find loan with the lowest balance, and balance is non-zero
#   non.zero.bal = which(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5] != 0) # Non-zero balance
#   
#   # Set any zero balance loans to 0 monthly payment.
#   R[-non.zero.bal] = 0
#   
#   # Don't need, but makes sure not paying interest on zero balance loans.
#   I[-non.zero.bal] = 0
#   
#   # Find which balances are non-zero and return indices, then order them based
#   # on decreasing interest rates. This should make the loan that still has a 
#   # balance and the highest interest rate be listed first.
#   #sort.balance = which(df[(max(df[,1])*4+1):(max(df[,1]+1)*4) ,5] != 0, arr.ind = T)[order(df[non.zero.bal, 5], decreasing = FALSE)]
#   # If balances are the same, orders by lowest interest rate. Idea is that since
#   # less interest is accruing, you can pay that off quicker.
#   sort.balance = with(df, order(df[[5]][1:num.l], apr))
#   
#   ## Not needed. Max interest can change depending on which loans are paid off.
#   #max.interest.loc = sort.interest[1]
#   
#   # Running counter that takes care of two issues:
#   ## - Makes sure you are starting at the top of the sorted interest rates. 
#   ## - If you reach the end of this whole process and still have money left over
#   ##   in the running payment amount, this means you paid off everything and
#   ##   escape the loop.
#   loc = 1
#   
#   # While you still have money that you can allocate to your loans, do the following
#   while(abs(run.amt - 0) > 10^(-10)){
#     # If you cannot pay off your remaining balance of the highest interest rate
#     # loan with the running amount
#     if(df[max(df[,1]*num.l)+sort.balance[loc], 5] > run.amt-sum(R[-sort.balance[loc]])){
#       # Pay as much as you can
#       R[sort.balance[loc]] = run.amt - sum(R[-sort.balance[1:loc]])
#       run.amt = amt - sum(R)
#     } else { # Otherwise
#       # Change the payment of the highest interest rate loan to be the balance
#       R[sort.balance[loc]] = df[max(df[,1]*num.l)+sort.balance[loc], 5] + I[sort.balance[loc]]
#       # Take that payment from the running amount
#       run.amt = run.amt - R[sort.balance[loc]]
#     }
#     # If you've reached the end of the highest interest rates, break the loop
#     if(loc == length(sort.balance)) {break}
#     
#     # Increase to the next highest interest rate. This only comes into play when
#     # you can pay off multiple loans in one monthly payment.
#     loc = loc + 1
#   }
#   
#   # Calculate remaining principal
#   P = principal.pay(R, I)
#   
#   # Update balance off monthly payment
#   B = balance(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], P)
#   
#   # Construct this month's amortization table
#   dat.f = data.frame(month = time.mnth, 
#                      payment = R, 
#                      interest = I, 
#                      principal = P,
#                      balance = B, 
#                      loan.number = factor(1:num.l))
#   colnames(dat.f) = colnames(df)
#   return(dat.f)
# }




################################################################################
################################################################################
################################################################################
################################################################################


################################################################################
# ui <- fluidPage(
#   shinyFeedback::useShinyFeedback(),
#   shinyjs::useShinyjs(),
#   theme = bslib::bs_theme(version = 5, bootswatch = "solar"),
#   sidebarLayout(
#     sidebarPanel(
#       numericInput("n", "Number of loans", value = 1, min = 1),
#       selectInput("method", "Method", choices = c("Avalanche", "Snowball")),
#       radioButtons("know", "Do you know your monthly payments?", c("Yes", "No")),
#       numericInput("amt", "Total payment to be split among loans", value = 50, min = 1),
#       actionButton("calc", "Calculate", class = "btn-block btn-success"), width = 3
#     ),
#     mainPanel(
#       
#       # Put everything in tabs
#       tabsetPanel(type = "tabs",
#                   
#                   tabPanel(title = "Information",
#                   ## General information about the app
#                     # h2("Disclaimer"),
#                     # p("This app only works for loans with monthly payments."),
#                     # h3("Inputs"),
#                     # p("Select the number of loans you want to include and how much each monthly you can
#                     #   afford to pay towards those loans. Fill in the generate table the loan information."),
#                     # br(),
#                     p("Select which repayment method you want to use. The avalanche method will repay the
#                       highest interest rate loan first. This will end up costing less over time, but takes
#                       longer to payoff a loan. The snowball method will repay the lowest balance loan first.
#                       This will payoff a loan quicker, but will pay more in interest.")),
#                   
#                   tabPanel("Inputs & Table", uiOutput("table"),
#                             dataTableOutput("pay.table"),
#                            downloadButton("downloadTab", "Download .tsv")),
#                   tabPanel("Payment Plot", plotOutput("pay.graph")),
#                   tabPanel("Interest Plot", plotOutput("interest.graph"))
#       ),
#       width = 9
#     )
#   )
# )

# server <- function(input, output, session) {
#   
# 
# 
#   # Create names based off the number of loans
#   loan_names = reactive(paste0("loan", seq_len(input$n)))
#   apr_names = reactive(paste0("apr", seq_len(input$n)))
#   monthly_names = reactive(paste0("mth.pay", seq_len(input$n)))
#   term_names = reactive(paste0("term", seq_len(input$n)))
#   
#   # Used this when creating the UI. Didn't know how to use this reactive. Switches
#   # the last column of UI if you know your monthly payments.
#   know = reactive(input$know)
#   
#   output$table = renderUI({
#     fluidRow(
#       # Add the dollar sign icon to the left
#       column(3, map(loan_names(), ~numericInputIcon(.x, "Loans", value = isolate(input[[.x]]),
#                                                     min = 1, icon = icon("dollar-sign")))),
#       # Add the percent icon to the right of the input box
#       column(3, map(apr_names(), 
#                     ~shinyWidgets::numericInputIcon(.x, "APR", value = isolate(input[[.x]]), 
#                                                     min = 0, icon = list(NULL, icon("percent"))))),
#       # Switch last column if you know your monthly payments.
#       if(know() == "Yes"){
#         # Add the dollar sign icon to the left
#         column(4, map(monthly_names(), ~numericInputIcon(.x, "Monthly Payment", value = isolate(input[[.x]]),
#                                                          min = 1, icon = icon("dollar-sign"))))
#       } else {
#         column(4, map(term_names(), ~numericInput(.x, "Total Number of Payments", value = isolate(input[[.x]]),
#                                                   min = 1)))
#       }
#     )
#   })
#   
#   ## Grabbing input values
#   loan_val = reactive(map_dbl(loan_names(), ~input[[.x]] %||% ""))
#   apr_val = reactive(map_dbl(apr_names(), ~input[[.x]] %||% ""))
#   monthly_val = reactive(map_dbl(monthly_names(), ~input[[.x]] %||% ""))
#   term_val = reactive(map_dbl(term_names(), ~input[[.x]] %||% ""))
#   
#   # Calculate the minimum monthly payment if user doesn't know them.
#   MIN.PAY = reactive({
#     if(know() == "Yes"){
#       req(monthly_val())
#       
#       monthly_val()
#     } else {
#       req(loan_val(), apr_val(), term_val())
#       
#       monthly.pay(loan_val(), apr_val(), term_val())
#     }
#   })
#   
#   # Create data frame in reactive value
#   dat = reactiveValues()
#   
#   
#   # Will create the output only after the Calculate button is pressed.
#   observeEvent(input$calc, {
#     req(MIN.PAY())
#     
#     ### Trying to give a warning if the amount your are willing to pay
#     ### each month is less than the monthly payments. It works when you try to
#     ### calculate the amortization table.
#     amt.check = sum(MIN.PAY()) < input$amt
#     shinyFeedback::feedbackDanger("amt", !amt.check, 
#                                   paste0("Your payment amount does not meet the sum of the minimum monthly payments of ", 
#                                          sum(MIN.PAY())))
#     req(amt.check, cancelOutput = TRUE)
#     
#     
#     # Need to create the starting data frame of values
#     dat$df = data.frame(
#       month = rep(0,input$n),
#       payment = rep(0,input$n),
#       interest = rep(0,input$n),
#       principal = rep(0,input$n),
#       balance = loan_val(),
#       loan.number = factor(1:input$n)
#     )
#     
#     # Switching which function  to use based on repayment method
#     funct = switch(input$method,
#                    Avalanche = avalanche.amort.table,
#                    Snowball = snowball.amort.table
#     )
#     
#     # Keeps track of the number of loans
#     num.l = nrow(dat$df)
#     
#     # Construct the amortization table iteratively
#     while(sum(dat$df$balance[(max(dat$df[,1])*num.l+1):(max(dat$df[,1]+1)*num.l)])>0){
#       dat$df = rbind(dat$df, funct(dat$df, apr_val(), MIN.PAY(), input$amt))
#     }
#     
#   }) # Ends the observe event from clicking Calculate button
#   
#   
#   ## Output table of payments
#   output$pay.table = renderDataTable(dat$df, 
#                                      options= list(#pageLength = 8,
#                                        dom = '<i<t>>',
#                                        #filter = list(position = "top"),
#                                        #escape = FALSE,
#                                        scrollY = "300px",
#                                        #searching = FALSE,          # Removes overall search feature
#                                        paging = FALSE        # Removes the pages
#                                      ))
#   
#   ## Enable/Disable download button once you calculate the repayment schedule
#   observe({
#     if(is.null(dat$df)){
#       shinyjs::disable("downloadTab")
#     } else {
#       shinyjs::enable("downloadTab")
#     }
#   })
#   
#   output$downloadTab = downloadHandler(
#     filename = function() {
#       paste0(input$method, "_Repayment.tsv")
#     },
#     content = function(file){
#       vroom::vroom_write(dat$df, file)
#     }
#   )
#   
#   
#   ## Build the loan balance graph
#   graph1 = reactive({
#     req(dat$df)
#     dat$df %>%
#           ggplot(aes(month, balance, color = loan.number)) +
#           #  facet_wrap(~loan.number) + # Breaks into a number of graphs equal to loans
#           geom_line(size = 1.2) +
#           labs(
#             title = 'Length of time to pay off loans under the chosen repayment method',
#             x = 'Month',
#             y = 'Loan balance'
#           )
#   })
#   
#   ## Build the cumulative interest graph
#   graph2 = reactive({
#     req(dat$df)
#     dat$df %>%  # Needed to sum the interest per month on each loan
#           group_by(month) %>%
#           summarise(cs.interest = sum(interest)) %>%
#           ggplot(aes(x = month, y = cumsum(cs.interest))) +  # Cumulative sum of the interest
#           geom_line(size = 1.2) +
#           labs(
#             title = 'Cumulative sum of interest under chosen repayment method',
#             x = 'Month',
#             y = 'Cumulative Interest'
#           ) + # Only labeling the total interest rate
#           geom_text(aes(label=ifelse(month == max(month),as.character(cumsum(cs.interest)),'')),hjust=.75,vjust=2)
#   })
#   
#   # Output the graphs
#   output$pay.graph = renderPlot(graph1(), res = 96)
#   output$interest.graph = renderPlot(graph2(), res = 96)
#   
# }
