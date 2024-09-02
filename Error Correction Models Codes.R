# Loading the data
detach(project_data)
project_data <-read.csv("D:/PROJECT/Data_4.csv", header = T, sep = ",")
L_TradeDeficit <- log(project_data$TradeDeficit)
project_data <- cbind(project_data, L_TradeDeficit)
project_data$trade_deficits <- ts(project_data$L_TradeDeficit, start = 2005, frequency = 12)
project_data$public_debt <- ts(project_data$PublicDebt, start = 2005, frequency = 12)
project_data$government_revenue <- ts(project_data$GovernmentRevenue, start = 2005, frequency = 12)
project_data$inflation_rates <- ts(project_data$InflationRates, start = 2005, frequency = 12)

attach(project_data)
head(project_data)

# Loading necessary packages 
library(vars)
library(dynamic)
library(forecast)
library(tidyverse)
library(tseries)
library(urca)
library(TSstudio)
library(psych)
library(pastecs)
library(dLagM)

# Obtaining their Time Series plots
ts.plot(TradeDeficit, ylab = "Trade Deficits", xlab = "Months of the Year")
ts.plot(public_debt, ylab = "Total Public Debts", xlab = "Months of the Year")
ts.plot(government_revenue, ylab = "Government's Total Revenue", xlab = "Months of the Year")
ts.plot(inflation_rates, ylab = "Inflation Rates", xlab = "Months of the Year")

# Obtaining univariate descriptive statistics of the variables
describe(TradeDeficit)
describe(public_debt)
describe(government_revenue)
describe(inflation_rates)

# Scatter plot and correlation coefficients
plot(public_debt, TradeDeficit, main="Scatterplot of Trade Deficits by Public Debt",
     xlab="Total Public Debt", ylab="Trade Deficit", pch=19) 
abline(lm(TradeDeficit ~ public_debt), col="blue") 

cor.test(TradeDeficit, public_debt, method = "pearson")
cor.test(TradeDeficit, government_revenue, method = "pearson")
cor.test(TradeDeficit, inflation_rates, method = "pearson")
cor.test(public_debt, government_revenue, method = "pearson")
cor.test(public_debt, inflation_rates, method = "pearson")
cor.test(government_revenue, inflation_rates, method = "pearson")

# Paired Samples t-test
t.test(Trade_Deficit, PublicDebt, mu = 0, paired = TRUE, var.equal = TRUE, conf.level = 0.95)

# Assessing stationarity and integration of the variables
adf.test(trade_deficits)
adf.test(public_debt)
adf.test(government_revenue)
adf.test(inflation_rates, k = 1)

d_trade_deficits <- diff(trade_deficits)
d_public_debt <- diff(public_debt)
d_government_revenue <- diff(government_revenue)
d_inflation_rates <- diff(inflation_rates)

adf.test(d_trade_deficits)
adf.test(d_public_debt)
adf.test(d_government_revenue)
adf.test(d_inflation_rates, k = 1)

# Identifying optimal lags for the ARDL model
VARselect(trade_deficits)
VARselect(public_debt)
VARselect(government_revenue)
VARselect(inflation_rates)

# Estimation of the ARDL model
rem.p = list(government_revenue = c(2), public_debt = c(2))
remove = list(p = rem.p)
remove
model2  = ardlDlm(trade_deficits ~ public_debt + government_revenue + inflation_rates, 
                         data = project_data , p = 2 , q = 4, remove = remove)
summary(model2)

exp(coef(model2))

rem.p1 = list(d_government_revenue = c(2), d_public_debt = c(2))
remove1 = list(p = rem.p1)
remove1
model3  = ardlDlm(d_trade_deficits ~ d_public_debt + d_government_revenue + d_inflation_rates, 
                  data = project_data , p = 2 , q = 4, remove = remove1)
summary(model3)

exp(coef(model3))

ardlBound(data = project_data, L_TradeDeficit ~ PublicDebt + GovernmentRevenue + InflationRates, 
          max.p = 2, max.q = 4, ECM = T)



