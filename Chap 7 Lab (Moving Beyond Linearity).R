#ISLR Chap. 7 Lab: Moving Beyond Linearity (4/13/18)

library(ISLR)
attach(Wage)

#7.8.1 Polynomial Regression and Step Functions

fit= lm(wage~poly(age,4),data = Wage) #EAch column is a linear combination of variables age, age^2, etc..
coef(summary(fit))

#can get those values directly too
fit2= lm(wage~poly(age,4, raw = T),data = Wage)
coef(summary(fit2))


#create a gird of values of age at whcih we want predications, and then use predict() and and specify we want SE
agelims = range(age)
age.grid <- seq(from=agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age =age.grid), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit -2*preds$se.fit)

#Finally plot the data and add the fit from the degree 4 polynomial
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title(" Degree -4 Polynomial ",outer=T)
lines(age.grid ,preds$fit ,lwd=2,col="blue")
matlines(age.grid ,se.bands ,lwd=1, col=" blue",lty=3)


# In performing a polynomial regression we must decide on the degree of the polynomial to use. One way do this is by hypothesis tests.
#We now fit models ranging from linear to degree 5 and seek to determine the simplest model which is sufficient to explain teh relationship
#Use anova()

fit.1=lm(wage~age ,data=Wage)
fit.2=lm(wage~poly(age ,2),data=Wage)
fit.3=lm(wage~poly(age ,3),data=Wage)
fit.4=lm(wage~poly(age ,4),data=Wage)
fit.5=lm(wage~poly(age ,5),data=Wage)
anova(fit.1,fit.2, fit.3, fit.4, fit.5) #Looks like either a cubic or quartic poly model is good

#Choose poly degree using CV
fit=glm(I(wage >250)~poly(age ,4),data=Wage , family=binomial)
#make predictions using predict()
preds=predict (fit ,newdata =list(age=age.grid),se=T)


#In order to create confidence intervals we need to do some transformation
pfit <- exp(preds$fit)/ (1+exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit -2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

#Note we can directly compute the probablities by selecting the type = "response" in predict()
preds <- predict(fit, newdata = list(age = age.grid), type = "response", se = T)


#Finally create plot
plot(age ,I(wage >250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage >250)/5),cex=.5,pch ="|",col="darkgrey ")
lines(age.grid ,pfit ,lwd=2, col ="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)
#Use jitter() so that observations with the same age are not covering each other up

#To fit a step function
table(cut(age,4))
fit <- lm(wage~cut(age,4), data = Wage) #Auto picked the breaks, we coudl ahve done it manually with breaks = 
coef(summary(fit))


#7.8.2 Splines
library(splines)

fit = lm(wage~bs(age,knots = c(25,40,60)), data = Wage)
pred <- predict(fit, newdata= list(age = age.grid), se = T)
plot(age ,wage ,col="gray")
lines(age.grid ,pred$fit ,lwd=2)
lines(age.grid ,pred$fit +2*pred$se ,lty="dashed")
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")
#Produces a spline with 6 basis functions (cubic spline with 3 knots has 7df; these are used with 1 by the intercept and 6 basis functions)
dim(bs(age,knots = c(25,40,60)))
dim(bs(age ,df=6))
attr(bs(age ,df=6) ,"knots") #R has chosen the knots at 33.75, 42, and 51 using the bs() function
#bs() can also change the degree agrument to something else. default is 3 (cubic spline)

#Natural Spline
fit2=lm(wage~ns(age ,df=4),data=Wage)
pred2=predict (fit2 ,newdata=list(age=age.grid),se=T)
lines(age.grid , pred2$fit ,col="red",lwd=2)


#Smooth Spline
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title("Smoothing Spline ")
fit=smooth.spline(age ,wage ,df=16)
fit2=smooth.spline(age ,wage ,cv=TRUE)
fit2$df
lines(fit ,col="red",lwd =2)
lines(fit2 ,col="blue",lwd=2)
legend ("topright",legend=c("16 DF" ,"6.8 DF"),col=c("red","blue"),lty=1,lwd=2, cex =.8)


#Local Regression use loess() function
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title("Local Regression ")
fit=loess(wage~age ,span=.2,data=Wage)
fit2=loess(wage~age ,span=.5,data=Wage)
lines(age.grid ,predict (fit ,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),col="blue",lwd=2)
legend ("topright",legend=c("Span=0.2"," Span=0.5"),col=c("red","blue"),lty=1,lwd=2, cex =.8)
#Span is .5 and .2. That is each neighborhood consists of 20% to 50% of the observations. The larger the span, the smoother the fit. 

#7.8.3 GAMs
#Using natural splines can use lm since it's just a big linear regression model
gam1 <- lm(wage~ns(year,4) + ns(age,5) + education, data = Wage)

#To fit other splines like smooth we need gam()
library(gam)
gam.m3 <- gam(wage~s(year,4) + s(age,5) + education, data = Wage)
par(mfrow = c(1,3))
plot(gam.m3, se = T, col = "blue")

#Can compare models with anova()
#M1 is GAM the excludes year
#M2 is GAM using linear function of year
#M3 is GAM using spline function of year
gam.m1=gam(wage~s(age,5)+education ,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education ,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F") #Linear is better than no year, and spline is not better than linear year
summary(gam.m3) #big p value for year under nonparametric effect says we don't need a nonlinear relationship. Can't say the same for age

#Make predictions using GAM
preds <- predict(gam.m2, newdata = Wage)

#We can use local regression fits as building blocks in GAM, using lo() function
gam.lo <- gam(wage~s(year, df = 4) + lo(age,span =.7) + education, data = Wage)
plot.gam(gam.lo, se = T, col = "green")

#Here we have a local regression for age term with a span of .7. We can also use the lo() function to create interacionts before calling the gam function
gam.lo.i <- gam(wage~lo(year, age, span = .5) + education, data = Wage)
#fits a 2 term model in which the first term is an intercation between year and age, fit by local regression surface. 

#We plot the resulting 2D surface if we install akima package
library(akima)
plot(gam.lo.i)



#Logistic GAM
gam.lr=gam(I(wage >250)~year+s(age ,df=5)+education ,family=binomial ,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")


#It is easy to see that there are no high earners in the <HS category
table(education ,I(wage >250))

#hence we see that fitting a logistic regression GAM without this category is a better result
gam.lr=gam(I(wage >250)~year+s(age ,df=5)+education ,family=binomial ,data=Wage, subset = (education!="1. <HS Grad"))
plot(gam.lr, se =T, col = "Green")




