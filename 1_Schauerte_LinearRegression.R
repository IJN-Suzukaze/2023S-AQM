#### loading data ####
install.packages("readxl")
library(readxl)
store24 <- read_excel("/Users/letchkov/Dropbox/01_Letchkov/05 Arbeit/2020-heute_VU Amsterdam/2_Lehre/8_Amsterdam Summer School (VU)/2_Linear Regression/Tutorial Linear Regression_Store24/Store 24/1_Tutorial Linear Regression_Data.xlsx")

#store24 <- read.table("S:/3_Exercise/1_Exercise 1_Store24/1_Store24A/Store24(A)_Data.csv",
#                      sep=";", dec=".", header=T)
#store24 <- read.table("S:/3_Exercise/1_Exercise 1_Store24/1_Store24A/Store24(A)_Data.txt",
#                      sep="", dec=",", header=T)



#### Question 0. Summary descriptive statistics ++++++++++++++++++++++++++++++++++++++++++++++++++++++
summary(store24)
round(cor(store24),2)

hist(store24$Profit)
hist(store24$Mtenure)
hist(store24$Ctenure)

plot(store24$Ctenure, store24$Profit, xlab="Crew Tenure", ylab="Profit", main="Profits vs. Crew Tenure", frame.plot=FALSE , col="red")
plot(store24$Mtenure, store24$Profit, xlab="Manager Tenure [months]", ylab="Profit", main="Profits vs. Manager Tenure", frame.plot=FALSE , col="red")














#### Question1: Simple and multiple regressions ++++++++++++++++++++++++++++++++++++++++++++++++++++
question1a <- lm(store24$Profit ~ store24$Ctenure)
summary(question1a)

question1b <- lm(store24$Profit ~ store24$Mtenure)
summary(question1b)

question1c <- lm(store24$Profit ~ store24$Ctenure + store24$Mtenure)
summary(question1c)

question1d <- lm(Profit ~ Ctenure + Mtenure + Pop + Comp
                 + Visible + PedCount + Res  + Hours24, data=store24)
summary(question1d)






















#### Question2: multiple regression and standardized coefficients ++++++++++++++++++++++++++++++++++
question2a <- lm(store24$Profit ~ store24$Ctenure + store24$Mtenure + store24$Pop + store24$Comp
                + store24$Visible + store24$PedCount + store24$Res  + store24$Hours24)
summary(question2a)

question2b <- lm(scale(Profit) ~ scale(Ctenure) + scale(Mtenure) + scale(Pop) + scale(Comp)
                 + scale(Visible) + scale(PedCount) + Res + Hours24, data=store24)
summary(question2b)




























#### Question3: Non-linear relationships +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### log
question3a.log <- lm(Profit ~ log(Ctenure) + Mtenure + Mtenure2 + Pop + Comp
                     + Visible + PedCount + Res  + Hours24, data=store24)
summary(question3a.log)

question3b.log <- lm(Profit ~ Ctenure + log(Mtenure+1) + Mtenure2 + Pop + Comp
                     + Visible + PedCount + Res  + Hours24, data=store24)
summary(question3b.log)

question3c.log <- lm(Profit ~ log(Ctenure) + log(Mtenure+1) + Mtenure2 + Pop + Comp
                     + Visible + PedCount + Res  + Hours24, data=store24)
summary(question3c.log)

### squared
Mtenure2 = store24$Mtenure * store24$Mtenure
Ctenure2 = store24$Ctenure * store24$Ctenure

question3a <- lm(Profit ~ Ctenure + Mtenure + Mtenure2 + Pop + Comp
                 + Visible + PedCount + Res  + Hours24, data=store24)
summary(question3a)

question3b <- lm(Profit ~ Ctenure + Ctenure2 + Mtenure +  Pop + Comp
                 + Visible + PedCount + Res  + Hours24, data=store24)
summary(question3b)

question3c <- lm(Profit ~ Ctenure + Ctenure2 + Mtenure + Mtenure2 + Pop + Comp
                 + Visible + PedCount + Res  + Hours24, data=store24)
summary(question3c)

# calcluating turning points
question3c$coefficients[2] / (-2*question3c$coefficients[3]) 
question3c$coefficients[2] / (-2*question3c$coefficients[3]) /12

question3c$coefficients[4] / (-2*question3c$coefficients[5]) 
question3c$coefficients[4] / (-2*question3c$coefficients[5]) /12



















#### Question 4: Final Model
question4 <- lm(Profit ~ Ctenure + Ctenure2 + Mtenure + Mtenure2 + Pop + Comp
                 + Visible + PedCount + Res  + Hours24, data=store24)
summary(question4)




#### Question 5: Check whether your model is correct

### Assumption 1 -> well-specified
## Nothing you usually test and there could always be misspecification error

### Assumption 2 -> exogenous errors
## this cannot be tested specifically but only considered theoretically

### Assumption 3 -> homoskedasticity
## Visual inspection of residuals
plot(question4$residuals) #looks pretty random
## homoskedasticity test
#install.packages("car")
library(car)
ncvTest(question4) # looks fine

# Assumption 4 -> no autocorrelation among residuals
plot(question4$residuals)
acf(question4$residuals)
durbinWatsonTest(question4)

# Assumption 5 -> no multicollinearity
cor(store24)
vif(question4)
vif(question2a)

# Assumption 6 -> normally distributed residuals 
hist(question4$residuals)
plot(question4) #q-q plot should be roughly a straight line



#### Excursion ####
# Plotting fitted values
plot(question4$model$Mtenure [order(question4$model$Mtenure, decreasing=F)], 
     question4$fitted.values [order(question4$fitted.values, decreasing=F)],
     xlab="Mtenure", ylab="Predicted Profits")


