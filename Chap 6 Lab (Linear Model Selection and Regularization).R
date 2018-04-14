#ISLR Chap. 6 Lab: Linear Model Selection and Regularization (4/13/18)

#6.5.1 Best Subset Selection

library(ISLR)
fix(Hitters)
names(Hitters)

#Take out NA's
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


#Use regsubsets() function to perform best subset selection, by identifiying the best model that contains a given number of predictors, where "best"
#quantified using RSS

library(leaps)
regfit.full <- regsubsets(Salary ~., Hitters)
summary(regfit.full)

#An asterisk indicates that a given variable is included in the corresponding model. 
#EX: Best 2 variable model is Hits with CRBI.
#devault setting is that regsubsets only reports up to the best 8 variables
#That can be changed to up to 19 variable

regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
#The summary() function also returns R^2, RSS, adjusted R^2, Cp, and BIC
names(reg.summary)

#R squared
reg.summary$rsq
#Shows r squared increased from .32 to .546 when all the variables were included


#Plotting the R squared, Cp, and BIC will help us decide which  model to select
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type ="l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R Squared", type ="l")


#which.max() can be used to identify the location of max point of a vector. We will now plot a red dot to indicate the model with the largest adj. R Squared.
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col = "red", cex =2, pch = 20)

#We can do the same for Cp and BIC by using which.min()
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6,reg.summary$bic[6], col = "red", cex = 2, pch = 20)


#regsubsets() has built in plot commands
plot(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")


#6.5.2 Forward and Backward Stepwise Selection
#Can use regsubsets() to perform forward or backwards stepwise selection.
regfit.fwd=regsubsets(Salary~.,data=Hitters , nvmax=19,method ="forward")
summary (regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters , nvmax=19, method ="backward")
summary (regfit.bwd)

#best subset, forward, and backward get different models after 7 variables included
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)



#6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation
set.seed(1)
train <- sample(c(T,F), nrow(Hitters), rep = T)
train #random vector,train, of elements qual to T is in the corresponding training set and F o.w.
test = (!train) #test vector is the opposite of train

#apply regsubsets() to do best subset with train data set
set.seed(1)
regfit.best <- regsubsets(Salary~., data = Hitters[train,], nvmax = 19)

#To compute the validation set error for the best model of each model size. We first make a model matrix from the test data
test.mat <- model.matrix(Salary~., data = Hitters[test,])

#model.matrix() is used in many regression packages for building "X" matrix from the data. 
#Now we run a loop, and for each size i, we extract the coef. from regfit.best for the best model of that size, 
#muliply them into the appropriate columns of thes test model matrix to form the predicitons, and compute the test MSE

#%*% is matrix multiplication
val.error = rep(NA,19)
for(i in 1:19){
  coefi = coef(regfit.best,id =i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.error[i] <- mean((Hitters$Salary[test]-pred)^2)
}

val.error
which.min(val.error)
#model with 10 variables is the best

coef(regfit.best,10)


#Since we will be using this again, let's make function for it
predict.regsubsets <-function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Best 10 variables subset on full variables becuase coeficients for full data set is different from just the ones in the train set
regfit.best <- regsubsets(Salary~.,data = Hitters, nvmax = 19)
coef(regfit.best,10)


#10 fold cross validation
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA,k,19, dimnames = list(NULL, paste(1:19)))

#Now we write the for loop that will perform the cross validation
for(j in 1:k){
  best.fit=regsubsets (Salary~.,data=Hitters [folds!=j,],nvmax=19)
  for(i in 1:19){
     pred=predict (best.fit ,Hitters [folds ==j,],id=i)
     cv.errors[j,i]= mean( ( Hitters$Salary[ folds==j]-pred)^2)
     }
}

#use apply() to average over teh columns of this matrix in order to obtain a vector for which jth element is the cross-validation error for the j-variable model
mean.cv.errors<-apply(cv.errors,2,mean)
mean.cv.errors
plot(mean.cv.errors, type ="b")
cv.errors
#We see that the model with 11 variables is the best

#Now perform best subset to obtain the best 11 variable model
reg.best <- regsubsets(Salary~., data =Hitters, nvmax = 19)
coef(reg.best, 11)



#6.6 Lab2: Ridge Regression and Lasso Regression
x = model.matrix(Salary~.,Hitters)[,-1] #model.matrix() created dummy variables for qualative variables too
y = Hitters$Salary
#glmnet() has alpha agrument. if alpha = 0 then ridge regression, if alpha = 1 then lasso regression
#Needs X matrix and y vector
library(glmnet)
grid = 10^seq(10,-2, length =100)
ridge.mod = glmnet(x,y,alpha =0, lambda = grid)
#glmnet() performs ridge regression for an automatically selected range of lambda values
#Here we have chosen to implement the function over a grid of values ranging from 10^10 to 10^-2. 
#This essentially covers  the fully range of scenarios from the null model containing only the intercept, to the least squares fit.

#glmnet() default standarized the variables
dim(coef(ridge.mod)) #20X100 matrix, with 20 rows (one for each predictor, plut an intercept) and 100 columns (one for each value of lambda)

#We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value of lambda is used, as compared to when a small value of lamda is used
#These are the coefficients when lambda = 11,498 along with their l2norm:
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))


#In contrast here are the coefficients when lambda = 705, along with their l2 norm. Note the much larger l2 norm of the coefficients associated with the smaller value of lambda
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

#We can use the predict() function for a number for purposes like obtain the ridge regression coefficients for anew value of lambda say, 50
predict(ridge.mod, s = 50, type ="coefficients")[1:20,]



#Now create a test and train set to get the test error
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

#Next we fit a ridge regression model on the train set, and evaluate its mse on the test set, using lambda = 4
#Note: predict() again, this time we get predicgtions for test set, by replacing type="coeficients" with newx agrument
ridge.mod <- glmnet(x[train,], y[train], alpha =0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s =4, newx=x[test,])
mean((ridge.pred-y.test)^2)
#The MSE is 101037. Note that if we had instead simply fit the model with just an intercept, we would have predicted each test obs using the mean of the training observations.
#In that case, we could compute the test set MSE liek this:
mean((mean(y[train])-y.test)^2)

#We could also get the same result by fitting a ridge regression model with a very large value of lambda. Not that 1e10 mean 10^10
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred-y.test)^2)
#So fitting a ridge regression with lambda =4 leads to a much lower test mse than fitting a model with just and intercept.
#We now check whether there is any benefit to peforming ridge with lambda = 4 instead of performing least squares regression. 
#Recall that least squares is simply ridge regression with lambda =0^5.

ridge.pred=predict(ridge.mod ,s=0, newx=x[test ,])
mean((ridge.pred -y.test)^2)
#Mse is 114723.6

lm(y~x, subset = train)
predict(ridge.mod, s=0,  type = "coefficients")[1:20,]

#Let's use cross validation to choose our lambda instead of just picking lambda = 4
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
#We can wee that the value of lambda that results in the smallest cross validation error is 212. 
#What is the MSE with this value for lambda?
ridge.pred <- predict(ridge.mod, s =bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)#this MSE is better than lambda = 4

#Finally we refit the ridge regression model on the full data set usnig the bestlam to get our coefficient estimates
out<-glmnet(x,y,alpha =0)
predict(out,type = "coefficients", s = bestlam)[1:20,]
#none of the coefficients are zero, but that will be different in Lasso

#6.6.2 Lasso
lasso.mod=glmnet(x[train ,],y[ train],alpha=1, lambda =grid)
plot(lasso.mod)

#Now use cross validation to find best lambda
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha =1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam #Best is when lambda = 16.78
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred -y.test)^2) #It is lower than the mse of null or least squares and similar to that of the ridge regression cv
#However the lasso has a substantial advantage over the ridge regression in that the resulint coefficients are sparse.
#Here we see that 12 of teh coefficients are 0. So this model only has 7 variables
out= glmnet(x,y, alpha = 1, lambda = grid)
lasso.coef<- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef



#6.7 Lab 3 PCR and PLS Regression
#6.7.1 Principal Components Regression
library(pls)
set.seed(2)
pcr.fit<- pcr(Salary~., data = Hitters, scale =T, validation = "CV") #CV is the 10 fold
summary(pcr.fit) #need to square 352.8 root mean squared error to get mSE
#val.type causes the MSE to be plotted
validationplot(pcr.fit ,val.type ="MSEP") #smallest CV error occurs at M =16 components, which is barely fewer than M=19, which amounts to a simple least squares.
#Using just 1 M might be good too, but not the best
#summary() also provides the percentage of variance explained.
#EX: M = 1 only captures 38.31% of the variance or information
#M = 6 increases the value to 88.63%.

#Now we perform PCR on the training data and evaluate its test set performance
set.seed(1)
pcr.fit= pcr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
validationplot(pcr.fit,val.type = "MSEP") #Look like around 7=M has the lowest cross validation error

#Let's compute the test MSE
pcr.pred<- predict(pcr.fit,x[test,], ncomp = 7)
mean((pcr.pred -y.test)^2) #This MSE is competitive with those we got from ridge regression
#However pcr is harder to interpret

#Finally, we fit PCR ont the full data set
pcr.fit=pcr(y~x, scale =T, ncomp =7)
summary(pcr.fit)



#6.7.2 Partial Least Squares
library(pls)
set.seed(1)
pls.fit <- plsr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls.fit)
#The lowest CV error occurs at M=2.
#We now evaluate the corresponding test set MSE
pls.pred = predict(pls.fit, x[test,], ncomp =2)
mean((pls.pred-y.test)^2) #MSE is comparable but not as good as using lasso, ridge, or PCR

#Finally perform PLS on using the full data set
plst.fit <- plsr(Salary~., data = Hitters, scale = T, ncomp=2)
summary(plst.fit) #See that the M=2 here explains almost as much as the PCR when M=7
#This is because PCR only attempts to maximize the amount the variance explained in the predictors, while PLS searches for directions that explain variance in both the preidictor and the response.
