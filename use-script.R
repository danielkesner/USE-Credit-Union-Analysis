# Read data
setwd("C:/Users/Daniel/Documents/USE-Data")
mydata <- read.csv("august.csv")

# Clean me up daddy
mydata[is.na(mydata)] <- 0

# Add a boolean for whether entry was lunch expense
for (i in 1:length(mydata$Memo)) {
  if ((substr(mydata$Memo[i], 0, 8)) == 'CULINART') {
     mydata$IntuitLunch[i] = 1 
  }
  else {
     mydata$IntuitLunch[i] = 0 
  }
}

# Calculate income (includes Venmo + paychecks)
# If entry starts w/ 'VENMO' and contains 'CASHOUT' (i.e. cash coming from venmo -> my bank account), consider it 'income'
venmo_deposit <- 'CASHOUT'
venmo_withdrawal <- 'PAYMENT'

venmo_income <- 0

for (i in 1:length(mydata$Amount.Credit)) {
  if (substr(mydata$Memo[i], 0, 5) == 'VENMO' && grepl(venmo_deposit, mydata$Memo[i], fixed = TRUE) 
      && mydata$Amount.Credit[i] > 0) {
    venmo_income <- venmo_income + mydata$Amount.Credit[i]
  }
}

# Calculate any Venmo payments
# If entry starts w/ 'VENMO' and contains 'PAYMENT' (i.e. cash going from my bank -> someone else), consider it 'expenses'
venmo_payments <- 0
for (i in 1:length(mydata$Amount.Debit)) {
  if (substr(mydata$Memo[i], 0, 5) == 'VENMO' && grepl(venmo_withdrawal, mydata$Memo[i], fixed=TRUE)
      && mydata$Amount.Debit > 0) {
    venmo_payments <- venmo_payments + mydata$Amount.Debit[i]
  }
}

# Calculate income (paychecks)
paycheck <- 0
for (i in 1:length(mydata$Amount.Credit)) {
  if (substr(mydata$Memo[i], 0, 5) == 'CYDIO') {
    paycheck <- paycheck + mydata$Amount.Credit[i]
  }
}

# Calculate sums for total income, lunch/other/total expenses, and net income after expenses
lunch.expenses <- abs(sum(mydata[which (mydata$IntuitLunch == 1) ,]$Amount.Debit))
other.expenses <- abs(sum(mydata[which (mydata$IntuitLunch == 0) ,]$Amount.Debit)) + venmo_payments
total.expenses <- lunch.expenses + other.expenses

net.income <- paycheck + venmo_income

all.expenses <- c(lunch.expenses, other.expenses, total.expenses)
net.income.after.expenses <- net.income - total.expenses
total.cash.flow <- c(net.income, all.expenses, net.income.after.expenses)

# Plot me daddy
plot <- barplot(total.cash.flow, main = "Income/Expenses for August", ylab = "Amount ($USD)", names.arg = c("Income", "Lunch expenses", "Other Expenses", "Total Expenses", "Net Income"), 
        col = c("darkblue", "red", "goldenrod3", "tan4", "springgreen"), ylim = c(0, 5000))
text(x = plot, y = total.cash.flow, label = total.cash.flow, pos = 3, cex = 0.8, col ="red")

# Clean up workspace
rm(i)

# Print values to console
c("Total income", "Lunch Expenses", "Other Expenses", "Total Expenses")
total.cash.flow