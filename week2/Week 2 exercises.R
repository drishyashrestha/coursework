library(tidyverse)
library(lubridate)

magnets <- read.csv('magnets.csv')







#Machine learning Lab: Linear Regression

library(MASS)
install.packages("ISLR2")
library(ISLR2)
head(Boston)
str(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv~lstat)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat = (c(5,10,15))),
    interval = "confidence")

plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(1,1))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


#Multiple Linear regression
lm.fit <- lm(medv~lstat + age, data = Boston)
summary(lm.fit)

#USE all variables to perform regression line
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
