#ISLR Chap. 9 Lab: Support Vector Machines (4/13/18)

#9.6.1 Support Vector Classifer
library(e1071)
par(mfrow = c(1,1))
set.seed(1)
x= matrix(rnorm(20*2), ncol =2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,] +1
plot(x,col = (3-y))


#Fit SVM. Response variable must be a factor
dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 10, scale =F)
plot(svmfit, dat) #support vectors are plotted with "x"

#Identify the support vectors:
svmfit$index

#Get some basic information about the support vector classier fit
summary(svmfit)

#What if we instead used a smaller value of cost parameter?
svmfit = svm(y~., data = dat, kernal = "linear", cost = .1, scale = F)
plot(svmfit, dat)
svmfit$index #We get more support vectors than before becuase the margin is wider now.

#Use tune() to get the 10 fold CV
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernal = "linear", ranges = list(cost =c(.001, .01, .1, 1, 5, 10,100)))

#Let's assess the CV error for each model using summary()
summary(tune.out) #We see that cost = .1 results in the lowest CV error rate. 

#To get best model
bestmod = tune.out$best.model
summary(bestmod)

#Use predict() to predict class level on a set of test observations
set.seed(1)
xtest <- matrix(rnorm(20*2), ncol =2)
ytest <- sample(c(-1,1), 20, rep = T)
xtest[ytest==1,] = xtest[ytest==1,] +1
testdat = data.frame(x=xtest, y = as.factor(ytest))

#Now predict class labels of these test observations
ypred=predict (bestmod ,testdat)
table(predict = ypred, truth = testdat$y) #18 of the observations are correct with cost = .1

#See what happens if we use c = .01 instead
set.seed(1)
svmfit=svm(y~., data=dat , kernel ="linear", cost =.01,scale=FALSE)
ypred=predict (svmfit ,testdat )
table(predict =ypred , truth=testdat$y ) #Only 15 classfied correctly

#Consider a situation where the 2 classes are linearly separable. Then we can find a separating hyperplane using svm().
#We first further separate the 2 classes in our simulated data so that they are linearly separable:
x[y==1,] = x[y==1,] + .5
plot(x, col = (y+5)/2, pch = 19)

#Now the observations are barely separable. We fit the vector classifer and plot the resulint hyper plan using a very large value of cost so no observations get misclassified
set.seed(1)
dat= data.frame(x=x, y = as.factor(y))
svmfit = svm(y~., data =dat, kernel = "linear", cost = 1e5)
summary(svmfit) #No training errors were made and only 3 support vecotrs

#We can see from the figure that the margin is very narrow(because the observations that are not support vectors are indicated as circles are very close to the decision boundary)

svmfit <- svm(y~., data = dat, kernel = "linear", cost =1)
summary(svmfit)
plot(svmfit,dat) #We misclassify a observation, but we hae a wider margin now. So probably better on test data


#9.6.2 Support Vector Machine (Non linear kernels)
set.seed(1)
x=matrix(rnorm (200*2) , ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150 ,]=x[101:150,]-2
y=c(rep(1,150) ,rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

plot(x, col = y) #This is definitely non linear

#Create test and train groups
train = sample(200,100)
svmfit <- svm(y~., data =dat[train,], kernel = "radial", gamma =1, cost = 1)
#gamma is to specify a value of gamma for the radial basis kernel
plot(svmfit, dat[train,]) # some training errors

#use summary()  to get more information
summary(svmfit)

#we can reduce the number of training errors, however this comes at the price of more irregular decision boundary that has the risk of overfitting
svmfit=svm(y~., data=dat[train ,], kernel ="radial",gamma=1, cost=1e5)
plot(svmfit ,dat[train ,])

#We can use CV using tune() to select bset choice of gamma and cost for SVM with radial kernel
set.seed(1)
tune.out=tune(svm, y~., data = dat[train,], kernel = "radial", ranges = list(cost =c(.1,1,10,100,1000), gamma =c(.5,1,2,3,4)))
summary(tune.out) #best 


#View test set predictions for this model by applying predict() function to data
table(true = dat[-train,"y"], pred = predict(tune.out$best.model, newdata = dat[-train,]))
#10% of the observations are misclassified
68/(68+21) #.764 correct

#9.6.3 ROC Curves
library(ROCR)
#Create function that will plot ROC curve given a vector containing a numerical score for each observation, pred, and a vector containing the class label for each observation, truth
rocplot =function (pred , truth , ...) {
  predob = prediction (pred , truth)
  perf = performance (predob , "tpr", "fpr")
  plot(perf ,...)
  }

#In order to obtain the fitted values for a given SVM model fit, we use decision.values = T
svmfit.opt=svm(y~., data=dat[train ,], kernel ="radial", gamma=2, cost=1, decision.values =T)
fitted =attributes(predict (svmfit.opt ,dat[train ,], decision.values=TRUE))$decision.values


#Now produce a ROC plot
par(mfrow =c(1,2))
rocplot(fitted,dat[train,"y"], main = "Training Data")


#By increasing gamma we can produce a more flexible fit and gernerate futher improvements on accuracy
svmfit.flex=svm(y~., data=dat[train ,], kernel ="radial",gamma=50, cost=1, decision.values =T)
fitted=attributes (predict (svmfit.flex ,dat[train ,], decision.values=T))$decision.values
rocplot(fitted ,dat[train ,"y"],add=T,col="red ") #However these ROC curves are on the train data

#Try on test set
fitted =attributes(predict(svmfit.opt ,dat[-train ,], decision.values=T))$decision.values
rocplot (fitted ,dat[-train ,"y"], main="Test Data")
fitted=attributes(predict (svmfit.flex ,dat[- train ,], decision.values=T))$decision.values
rocplot(fitted ,dat[-train ,"y"],add=T,col="red")


#9.6.4 SVM with muliple Classes
#if the response is a factor with more than 2 levels, then svm() function will perform multi-class classification using the one versus one approach
#We explore that here by generating a 3rd class of observations
set.seed(1)
x=rbind(x, matrix(rnorm (50*2) , ncol=2))
y=c(y, rep(0,50))
x[y==0,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))


#Now fit an SVM to the data:
svmfit <- svm(y~., data =dat, kernel ="radial", cost = 10, gamma =1)
plot(svmfit,dat)


#9.6.5 Application to Gene Expression Data
#We examine Khan data set, which contsists of a number of tissue samples corresponding to 4 distinct types of small glue cell tumors.
#For each tissue sample, gene expression measurements are available. The data set consists of training data, xtrain and ytrain, and test data xtest and ytest
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
#The data set consist of expression measurements of 2308 genes. The training and test sets consist of 63 and 20 obsverations
table(Khan$ytrain)
table(Khan$ytest)


#We will use a support vector approach to predict cancer subtype using gene expression measurements. 
#In this data set there are a large number of features relative to the number of observations.
#This suggest that we should use linear kernel, becuase the additional flexibility that will result using polynomial or radial kernel is unnessarcy

dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y~., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)
#We see no training errors. This is not suprising because the large number of variables relative to the number of observations implies that 
#it is easy to find hyperplaanes that fully separate the classes. We are most interested no in the support vecotr classifer's performance on the training observations
#but on the test observations
dat.te <- data.frame(x= Khan$xtest, y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y) # We see that cost = 10 yields two test set errors on this data


