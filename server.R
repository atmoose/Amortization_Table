server <- function(input, output, session) {
  
  # Create names based off the number of loans
  loan_names = reactive(paste0("loan", seq_len(input$n)))
  apr_names = reactive(paste0("apr", seq_len(input$n)))
  monthly_names = reactive(paste0("mth.pay", seq_len(input$n)))
  term_names = reactive(paste0("term", seq_len(input$n)))
  
  # Used this when creating the UI. Didn't know how to use this reactive. Switches
  # the last column of UI if you know your monthly payments.
  know = reactive(input$know)
  
  output$table = renderUI({
    fluidRow(
      # Add the dollar sign icon to the left
      column(3, map(loan_names(), ~numericInputIcon(.x, "Loans", value = isolate(input[[.x]]),
                                                    min = 1, icon = icon("dollar-sign")))),
      # Add the percent icon to the right of the input box
      column(3, map(apr_names(), 
                    ~shinyWidgets::numericInputIcon(.x, "APR", value = isolate(input[[.x]]), 
                                                    min = 0, icon = list(NULL, icon("percent"))))),
      # Switch last column if you know your monthly payments.
      if(know() == "Yes"){
        # Add the dollar sign icon to the left
        column(4, map(monthly_names(), ~numericInputIcon(.x, "Monthly Payment", value = isolate(input[[.x]]),
                                                         min = 1, icon = icon("dollar-sign"))))
      } else {
        column(4, map(term_names(), ~numericInput(.x, "Total Number of Payments", value = isolate(input[[.x]]),
                                                  min = 1)))
      }
    )
  })
  
  ## Grabbing input values
  loan_val = reactive(map_dbl(loan_names(), ~input[[.x]] %||% ""))
  apr_val = reactive(map_dbl(apr_names(), ~input[[.x]] %||% ""))
  monthly_val = reactive(map_dbl(monthly_names(), ~input[[.x]] %||% ""))
  term_val = reactive(map_dbl(term_names(), ~input[[.x]] %||% ""))
  
  # Calculate the minimum monthly payment if user doesn't know them.
  MIN.PAY = reactive({
    if(know() == "Yes"){
      req(monthly_val())
      
      monthly_val()
    } else {
      req(loan_val(), apr_val(), term_val())
      
      monthly.pay(loan_val(), apr_val(), term_val())
    }
  })
  
  # Create data frame in reactive value
  dat = reactiveValues()
  
  
  # Will create the output only after the Calculate button is pressed.
  observeEvent(input$calc, {
    req(MIN.PAY())
    
    ### Trying to give a warning if the amount your are willing to pay
    ### each month is less than the monthly payments. It works when you try to
    ### calculate the amortization table.
    amt.check = sum(MIN.PAY()) < input$amt
    shinyFeedback::feedbackDanger("amt", !amt.check, 
                                  paste0("Your payment amount does not meet the sum of the minimum monthly payments of ", 
                                         sum(MIN.PAY())))
    req(amt.check, cancelOutput = TRUE)
    
    
    # Need to create the starting data frame of values
    dat$df = data.frame(
      month = rep(0,input$n),
      payment = rep(0,input$n),
      interest = rep(0,input$n),
      principal = rep(0,input$n),
      balance = loan_val(),
      loan.number = factor(1:input$n)
    )
    
    # Switching which function  to use based on repayment method
    funct = switch(input$method,
                   Avalanche = avalanche.amort.table,
                   Snowball = snowball.amort.table
    )
    
    # Keeps track of the number of loans
    num.l = nrow(dat$df)
    
    # Construct the amortization table iteratively
    while(sum(dat$df$balance[(max(dat$df[,1])*num.l+1):(max(dat$df[,1]+1)*num.l)])>0){
      dat$df = rbind(dat$df, funct(dat$df, apr_val(), MIN.PAY(), input$amt))
    }
    
  }) # Ends the observe event from clicking Calculate button
  
  
  ## Output table of payments
  output$pay.table = renderDataTable(dat$df, 
                                     options= list(#pageLength = 8,
                                       dom = '<i<t>>',
                                       #filter = list(position = "top"),
                                       #escape = FALSE,
                                       scrollY = "350px",
                                       #searching = FALSE,          # Removes overall search feature
                                       paging = FALSE        # Removes the pages
                                     ))
  
  ## Enable/Disable download button once you calculate the repayment schedule
  observe({
    if(is.null(dat$df)){
      shinyjs::disable("downloadTab")
    } else {
      shinyjs::enable("downloadTab")
    }
  })
  
  output$downloadTab = downloadHandler(
    filename = function() {
      paste0(input$method, "_Repayment.tsv")
    },
    content = function(file){
      vroom::vroom_write(dat$df, file)
    }
  )
  
  
  ## Build the loan balance graph
  graph1 = reactive({
    req(dat$df)
    dat$df %>%
      ggplot(aes(month, balance, color = loan.number)) +
      #  facet_wrap(~loan.number) + # Breaks into a number of graphs equal to loans
      geom_line(size = 1.2) +
      labs(
        title = 'Length of time to pay off loans under the chosen repayment method',
        x = 'Month',
        y = 'Loan balance'
      )
  })
  
  ## Build the cumulative interest graph
  graph2 = reactive({
    req(dat$df)
    dat$df %>%  # Needed to sum the interest per month on each loan
      group_by(month) %>%
      summarise(cs.interest = sum(interest)) %>%
      ggplot(aes(x = month, y = cumsum(cs.interest))) +  # Cumulative sum of the interest
      geom_line(size = 1.2) +
      labs(
        title = 'Cumulative sum of interest under chosen repayment method',
        x = 'Month',
        y = 'Cumulative Interest'
      ) + # Only labeling the total interest rate
      geom_text(aes(label=ifelse(month == max(month),as.character(cumsum(cs.interest)),'')),hjust=.75,vjust=2)
  })
  
  # Output the graphs
  output$pay.graph = renderPlot(graph1(), res = 96)
  output$interest.graph = renderPlot(graph2(), res = 96)
  
}