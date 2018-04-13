#ISLR Chap. 5 Lab: Resampling Methods (4/13/18)

#5.3.1
library(ISLR)
set.seed(1)
train = sample(392,196) #Selects 196 observations from 392

#Need to create a model first
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
summary(lm.fit)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2) #26.14

#Use poly() to get MSE for polynomical and cubic regression
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2) #19.822

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) #19.78252

#set.seed(2) to get a different training set and see how results differ
set.seed(2)
train<- sample(392,196) 
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2) #23.29559

lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2) #18.8792

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) #19.2574

#Shows that quadtraic function is better than linear model since it has a lower MSE. Also, shows that cubic is probably not needed, since its results dont' really differ.



#5.3.2 Leave-One-Out Cross-Validation
glm.fit <- glm(mpg~horsepower, data = Auto) #No family for glm performs linear modeling
coef(glm.fit)

lm.fit<- lm(mpg~horsepower, data = Auto)
coef(lm.fit) #Same as above

#We will use glm() because later we will use it to do cv.glm()

library(boot)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

#Repeat the process for more complex polynomial functions 1 through 5
cv.error <- rep(0,5) #A vector of 5 zeros
for(i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower,i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error #we see a large drop of MSE between linear and quadtraic but not much after that

#5.3.3 K-Fold Cross Validation
set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower,i),data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K =10)$delta[1]
}
cv.error.10


#5.3.4 Bootstrap

##Estimating the Accuracy of a statistic of interest
alpha.fn = function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) -cov(X,Y))/(var(X)+var(Y) - 2*cov(X,Y)))
} #This function returns/outputs estimate for a

#This estimates a using 100 observations
alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace =T))

#Produce R=1,000 bootstraps for a
boot(Portfolio, alpha.fn, R = 1000)

#The final output shows that using the original data, a = .5758, and that the bootstrap estimate SE(a) is .0886


##Estimating the Accuracy of a Linear Regression Model
boot.fn = function(data,index){
  return(coef(lm(mpg~horsepower, data = data, subset = index)))
}

boot.fn(Auto,1:392) #gives estimated B0 and B1

#Use random sampling
set.seed(1)
boot.fn(Auto,sample(392,392,replace = T))


#We compute teh standard errors of 1,000 bootstrap estimates for the intercept and the slope terms
boot(Auto,boot.fn, 1000)
#This indicates that the bootstrap estimate for SE(B0) is .86 and the SE(B1) is .00074. 
summary(lm(mpg~horsepower, data = Auto))$coef #Different from bootstrap's SE, but that's okay since data looked a bit none linear.
#So the bootstrap coefficients gives a more accurate estimate. 

#Using quadratic
boot.fn=function (data ,index){
  coefficients(lm(mpg~horsepower +I(horsepower ^2),data=data ,subset=index))}
set.seed(1)
boot(Auto ,boot.fn ,1000)

summary (lm(mpg~horsepower +I(horsepower ^2),data=Auto))$coef #the SE are closer here
