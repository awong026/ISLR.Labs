#ISLR Chap. 8 Lab: Tree Base Methods (4/13/18)

#8.3.1 Fitting Classification Trees
library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales <=8, "No", "Yes")
#use data.frame() to merge High column with the other Carseat data
Carseats <- data.frame(Carseats, High)
Carseats

#Fit a classification tree
tree.carseats <- tree(High~.-Sales, Carseats)
summary(tree.carseats)
#Lists: variables that are used as internal nodes, number of terminal nodes, and training error rate
#Error rate is 9%
#the Residual mean deviance is n - T0 whcih is 400-27 = 373
par(mfrow = c(1,1))
plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats #Tells you split criterion, number of observations, deviance, overall prediction of the branch (yes or no). 
#Terminal nodes are labeled with an astricks


#Let's evaluate this tree model
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats <- tree(High~.-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test)

#Try pruning the tree using CV
#We will use FUN = prune.misclass in order to indicate that we want the classification error to guide the CV and pruning process, rather than deviance.
set.seed(3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass)
names(cv.carseats )
cv.carseats

#Note that despite teh name, dev corresponds to the CV error rate at each instance. 
#The tree with 9 terminal nodes results in teh lowest CV error rate, with 50 CV errors. 

#Plot the error rate as a function of both size and k
par(mfrow = c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

#Now prune the tree to obtain the nine-node tree
prune.carseats <-prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)


#How does this pruned tree perform on the test set?
tree.pred<- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test) #Accuracy is 75% of the time. 

#If we increase the vlaue of best, we obtain a larger pruned tree with lower classification accuracy
prune.carseats <- prune.misclass(tree.carseats, best = 15)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test) #Not as good, but with more stuff in it. 

#8.3.2 Fitting Regression Trees
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)

#In the context of a regression tree, the deviance is the sum of squard errors of the tree (SSE)
#plot the tree
par(mfrow = c(1,1))
plot(tree.boston)
text(tree.boston, pretty = 0)

#CV to to prune the tree
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")


#Use unpruned tree to make predictions on test set
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat -boston.test)^2) #25 is the test MSE for the regression tree
#The square root is 5.005 which indicates that this model leads to test predictions that are around $5,005 of the off the True median home value in the suburb

#8.3.3 Bagging and Random Forests
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = T)
bag.boston
#mtry =13 indicates that all 13 predictors should be considered for each split of the tree. In other words bagging is done.

#How does bagging do on the test set?
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) #test MSE is only 13.5 Now.

#We cnage the number of trees grown by randomForest() using the ntree agrumanet
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2)

#Random Forest model: by default randomForest() uses p/3 variables when building a regression tree and sqrt(p) when doing classification trees
#Let's build a random forest using mtry = 6
set.seed(1)
rf.boston <- randomForest(medv~., data = Boston, subset = train, mtry = 6, importance = T)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2) #test MSE is now only 11.66

#Use importance() to see the importance of each variable
importance(rf.boston) #Impurity is trained by RSS in regression and deviance in classfication

#Plots of these importance measures
varImpPlot(rf.boston)
#results indicate that across all the trees considered in the random forest, wealth level of community (lstat) and house size(rm) are by far the 2 most important variables


#8.3.4 Boosting
library(gbm)
set.seed(1)
boost.boston = gbm(medv~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
#distribution = "gaussian" for regression and distribution = bernoulli for classification
#n.trees = 5000 indicates that we want 5000 trees
#interaction.depth = 4 limits the depth of tree
summary(boost.boston) #we see that lstat and rm are the most important variables

#We can produce partial dependence plots for these two variables. These plots illustrate the marginal effect of the selected variables on the response
#after integrating out the other variables. In this case, as we might expect, median house prices are increasing with rm and decrease with lstat
par(mfrow = c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

#Use boosted model to predict medv on the test set
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2) #Mse is 11.84434. 

#boost with different levels of shrinage. default is .0001. Let's try lambda =.2
boost.boston <- gbm(medv~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth =4, shrinkage =.2, verbose = F)
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2) #lower MSE 10.89














