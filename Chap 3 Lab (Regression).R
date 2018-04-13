#ISLR Chap. 3 Lab: Linear Regression (4/13/18)

library(MASS)
library(ISLR)

#3.6.2 Simple Linear Regression:

#Boston Housing Data (Median house values for 506 neighborhoods in Boston. Will use 13 predictors to predict median housing value)

fix(Boston)
View(Boston)

#Columns in Boston data set
names(Boston)

#Information on Boston data set
?Boston

#Model of medv(median house value) by only lstat (precent of households with low socioeconomic status)
lm.fit <- lm(medv~lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv~lstat)

#Look at basic information of lm.fit
lm.fit
summary(lm.fit) #lstat looks significant and the F test p value is significant too

#Use names() function to see what pieces of information is stored in lm.fit
names(lm.fit)

#To get the confidence interval for coefficient estimates:
confint(lm.fit)

#Use predict() to produce confidence intervals and prediction intervals for the prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

#Plot
plot(lstat,medv) #data doesn't look linear
abline(lm.fit) 

#Other plots/line options
abline (lm.fit ,lwd =3)
abline (lm.fit ,lwd=3,col ="red")
plot(lstat ,medv ,col="red")
plot(lstat ,medv ,pch =20)
plot(lstat ,medv ,pch ="+")
plot(1:20,1:20,pch =1:20)

#How to show more than one plot at a time
par(mfrow = c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#To find/look for leverage statistics
plot(hatvalues(lm.fit))

#which.max() identifies the index of the largest element of a vector. Here it tells which observation has the largest leverage statistic
which.max(hatvalues(lm.fit)) 


#3.6.3 Multiple Linear Regression:
lm.fit <- lm(medv ~ lstat + age)
summary(lm.fit)

#Include all the variables model
lm.fit <- lm(medv~., data = Boston) 
summary(lm.fit)

#To look at different elements from the model
summary(lm.fit)$r.sq #R squared
summary(lm.fit)$sigma #RSS

#Look at variance inflation factors. Rule of thumb: If VIF exceeds 5 or 10 there is a problem with collinearity
library(car)
vif(lm.fit)

#How to make a model with all variables except for one
lm.fit1 <- lm(medv ~.-age, data = Boston)
summary(lm.fit1)

#Do the samething but with update()
lm.fit1 <- update(lm.fit, ~.-age)
summary(lm.fit1)


#3.6.4 Interaction Terms:
summary(lm(medv~ lstat*age, data = Boston))


#3.6.5 Non-linear Transformations of Predictors
lm.fit2 <- lm(medv~lstat + I(lstat^2))
summary(lm.fit2)

#Use anova to compare models
lm.fit <- lm(medv~lstat)
anova(lm.fit, lm.fit2) #lm.fit2 does significantly better than lm.fit

par(mfrow = c(2,2))
plot(lm.fit)


#Faster way to create polyminal models
lm.fit5 <- lm(medv~poly(lstat,5))
summary(lm.fit5) #lstat to the 5th order are all significant factors

#Let's try log transformation
summary(lm(medv~ log(rm), data = Boston))


#3.6.6 Qualitative Predictors
fix(Carseats)
names(Carseats)

#Use dummy varariables to for categorical variables and then also put in some interaction terms too
lm.fit <- lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)


#Use contrasts() to returen the coding that R uses for the dummy variables
attach(Carseats)
contrasts(ShelveLoc)

#3.6.7 Creating functions
Loadlibraries = function() {
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded")
}

Loadlibraries()
