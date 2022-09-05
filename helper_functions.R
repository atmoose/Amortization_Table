################################################################################
##### Helper Functions

# Minimum monthly payment
monthly.pay = function(l, r, t){
  # Interest rate is in percent. Change to decimal
  i = (1+r/100)^(1/12) - 1  # monthly effective interest rate
  y = l * i / (1 - (1 + i)^-t)
  return(round(y, 2))
}

# Balance in amortization table
balance = function(b, mp = monthly.pay){
  y = round(b - mp, 2)
  return(y)
}

# Interest in amortization table
interest.pay = function(b, r){
  i = (1+r/100)^(1/12) - 1
  y = round(b * i, 2)
  return(y)
}

# Principal being paid
principal.pay = function(mp, inter){
  y = mp - inter
  return(round(y, 2))
}
################################################################################


################################################################################
################################################################################
##### AVALANCHE METHOD code
avalanche.amort.table = function(df, apr,
                                 R = MIN.PAY,
                                 amt=sum(MIN.PAY)){
  
  num.l = max(as.numeric(df[,6]))
  
  # Running amount left of your payment
  run.amt = amt
  
  # Increase to next month
  time.mnth = df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),1] + 1
  
  # # Calculate minimum monthly payment
  # R = monthly.pay(df[(max(df[,1])*4+1):(max(df[,1]+1)*4),7],
  #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),8], 
  #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),9], 
  #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),10])
  
  
  # Calculate interest off remaining balance
  I = interest.pay(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], apr)
  
  # Find loan with the highest interest rate, and balance is non-zero
  non.zero.bal = which(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5] != 0) # Non-zero balance
  
  # Set any zero balance loans to 0 monthly payment.
  R[-non.zero.bal] = 0
  
  # Don't need, but makes sure not paying interest on zero balance loans.
  I[-non.zero.bal] = 0
  
  # Find which balances are non-zero and return indices, then order them based
  # on decreasing interest rates. This should make the loan that still has a 
  # balance and the highest interest rate be listed first.
  #sort.interest = which(df[(max(df[,1])*4+1):(max(df[,1]+1)*4) ,5] != 0, arr.ind = T)[order(df[non.zero.bal, 8], decreasing = TRUE)]
  
  # If multiple loans have the same interest rate, then order by highest balance.
  # Idea is to pay off the loan accruing the most interest per period.
  sort.interest = with(df, order(-apr, df[[5]][1:num.l]))
  
  ## Not needed. Max interest can change depending on which loans are paid off.
  #max.interest.loc = sort.interest[1]
  
  # Running counter that takes care of two issues:
  ## - Makes sure you are starting at the top of the sorted interest rates. 
  ## - If you reach the end of this whole process and still have money left over
  ##   in the running payment amount, this means you paid off everything and
  ##   escape the loop.
  loc = 1
  
  # While you still have money that you can allocate to your loans, do the following
  while(abs(run.amt - 0) > 10^(-10)){
    # If you cannot pay off your remaining balance of the highest interest rate
    # loan with the running amount
    if(df[max(df[,1]*num.l)+sort.interest[loc], 5] > run.amt-sum(R[-sort.interest[loc]])){
      # Pay as much as you can
      R[sort.interest[loc]] = run.amt - sum(R[-sort.interest[1:loc]])
      run.amt = amt - sum(R)
    } else { # Otherwise
      # Change the payment of the highest interest rate loan to be the balance
      R[sort.interest[loc]] = df[max(df[,1]*num.l)+sort.interest[loc], 5] + I[sort.interest[loc]]
      # Take that payment from the running amount
      run.amt = run.amt - R[sort.interest[loc]]
    }
    # If you've reached the end of the highest interest rates, break the loop
    if(loc == length(sort.interest)) {break}
    
    # Increase to the next highest interest rate. This only comes into play when
    # you can pay off multiple loans in one monthly payment.
    loc = loc + 1
  }
  
  # Calculate remaining principal
  P = principal.pay(R, I)
  
  # Update balance off monthly payment
  B = balance(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], P)
  
  # Construct this month's amortization table
  dat.f = data.frame(month = time.mnth, 
                     payment = R, 
                     interest = I, 
                     principal = P,
                     balance = B, 
                     loan.number = factor(1:num.l))
  
  # Make sure the names are the same when concatenating the data frames.
  colnames(dat.f) = colnames(df)
  return(dat.f)
}
################################################################################
################################################################################

################################################################################
################################################################################
##### AVALANCHE METHOD code
snowball.amort.table = function(df, apr,
                                R = MIN.PAY,
                                amt=sum(MIN.PAY)){
  # Checking to make sure you are paying enough each month
  
  num.l = max(as.numeric(df[,6]))
  
  # Running amount left of your payment
  run.amt = amt
  
  # Increase to next month
  time.mnth = df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),1] + 1
  
  # # Calculate minimum monthly payment
  # R = monthly.pay(df[(max(df[,1])*4+1):(max(df[,1]+1)*4),7],
  #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),8], 
  #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),9], 
  #                 df[(max(df[,1])*4+1):(max(df[,1]+1)*4),10])
  
  
  # Calculate interest off remaining balance
  I = interest.pay(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], 
                   apr)
  
  # Find loan with the lowest balance, and balance is non-zero
  non.zero.bal = which(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5] != 0) # Non-zero balance
  
  # Set any zero balance loans to 0 monthly payment.
  R[-non.zero.bal] = 0
  
  # Don't need, but makes sure not paying interest on zero balance loans.
  I[-non.zero.bal] = 0
  
  # Find which balances are non-zero and return indices, then order them based
  # on decreasing interest rates. This should make the loan that still has a 
  # balance and the highest interest rate be listed first.
  #sort.balance = which(df[(max(df[,1])*4+1):(max(df[,1]+1)*4) ,5] != 0, arr.ind = T)[order(df[non.zero.bal, 5], decreasing = FALSE)]
  # If balances are the same, orders by lowest interest rate. Idea is that since
  # less interest is accruing, you can pay that off quicker.
  sort.balance = with(df, order(df[[5]][1:num.l], apr))
  
  ## Not needed. Max interest can change depending on which loans are paid off.
  #max.interest.loc = sort.interest[1]
  
  # Running counter that takes care of two issues:
  ## - Makes sure you are starting at the top of the sorted interest rates. 
  ## - If you reach the end of this whole process and still have money left over
  ##   in the running payment amount, this means you paid off everything and
  ##   escape the loop.
  loc = 1
  
  # While you still have money that you can allocate to your loans, do the following
  while(abs(run.amt - 0) > 10^(-10)){
    # If you cannot pay off your remaining balance of the highest interest rate
    # loan with the running amount
    if(df[max(df[,1]*num.l)+sort.balance[loc], 5] > run.amt-sum(R[-sort.balance[loc]])){
      # Pay as much as you can
      R[sort.balance[loc]] = run.amt - sum(R[-sort.balance[1:loc]])
      run.amt = amt - sum(R)
    } else { # Otherwise
      # Change the payment of the highest interest rate loan to be the balance
      R[sort.balance[loc]] = df[max(df[,1]*num.l)+sort.balance[loc], 5] + I[sort.balance[loc]]
      # Take that payment from the running amount
      run.amt = run.amt - R[sort.balance[loc]]
    }
    # If you've reached the end of the highest interest rates, break the loop
    if(loc == length(sort.balance)) {break}
    
    # Increase to the next highest interest rate. This only comes into play when
    # you can pay off multiple loans in one monthly payment.
    loc = loc + 1
  }
  
  # Calculate remaining principal
  P = principal.pay(R, I)
  
  # Update balance off monthly payment
  B = balance(df[(max(df[,1])*num.l+1):(max(df[,1]+1)*num.l),5], P)
  
  # Construct this month's amortization table
  dat.f = data.frame(month = time.mnth, 
                     payment = R, 
                     interest = I, 
                     principal = P,
                     balance = B, 
                     loan.number = factor(1:num.l))
  colnames(dat.f) = colnames(df)
  return(dat.f)
}