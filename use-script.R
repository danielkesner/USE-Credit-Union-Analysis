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
income <- 0
for (i in 1:length(mydata$Amount.Credit)) {
  if (mydata$Amount.Credit[i] > 0) {
    income <- income + mydata$Amount.Credit[i]
  }
}

# Calculate sums for lunch/other/total expenses
lunch.expenses <- abs(sum(mydata[which (mydata$IntuitLunch == 1) ,]$Amount.Debit))
other.expenses <- abs(sum(mydata[which (mydata$IntuitLunch == 0) ,]$Amount.Debit))
total.expenses <- abs(sum(mydata$Amount.Debit))
all.expenses <- c(lunch.expenses, other.expenses, total.expenses)
total.cash.flow <- c(income, all.expenses)

# Plot me daddy
barplot(total.cash.flow, main = "Income/Expenses for August", ylab = "Amount ($USD)", names.arg = c("Income", "Lunch expenses", "Other Expenses", "Total Expenses"), 
        col = c("darkblue", "red", "goldenrod3"), ylim = c(0, 5000))

# Clean up workspace
rm(i)