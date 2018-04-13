#ISLR Chap. 4 Lab: Classification (4/13/18)

#4.6.1 The Stock Market Data
library(ISLR)
names(Smarket)

#Check correlation
cor(Smarket) #error since Direction is a qualitative factor
cor(Smarket[,-9]) #Appears to be little correlation between the predictors except for year and volume
#This means that the avg. number of shares traded increased from 2001 to 2005
attach(Smarket)
plot(Volume)

#4.6.2 Logistic Regression
glm.fit <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)
#Lowest p value was Lag1, even though it wasn't significant. The negative cofficient suggests that if the market had a positive return yesterday, then it is less likely to go up today.

#Just look at the coefficients with coef()
coef(glm.fit)
#or
summary(glm.fit)$coef #This gives the estimates with the standard error and the etc.. like p value

#Use the predict() function to predict the probability that the market will go up. The type = "response" option tells R to output probablities of the form P(y = 1| X), as opposed to other information such as the logit. 
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]

#To see how Direction was coded
contrasts(Direction)

#To create prediction on if the market will go up or down on a particular day, we must convert these predicted probabliies into class labels, Up or Down. 
#The following commands create a vector of class predictions based on whether the predicted probablity of a market increase is greater than or less than .5
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs>.5] <- "Up"

#Look at the result (Confustion Matrix)
table(glm.pred, Direction)
(507+145) /1250 #Number of correct classifications (Training set)
#or this code gives the same thing
mean(glm.pred==Direction )

#training error rate
1-.5216 #.4784

#So in order to really see if our model works let's create a train and test set
train <- (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

#Now let's fit a logistic regression on train set
glm.fit <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs >.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) #We only got .48 correct
mean(glm.pred != Direction.2005) #Our test error rate is .519. This is worse than random guessing

#Let's try refitting the model with only the top 2 most significant predictors (Lag1 and Lag2)
glm.fit <- glm(Direction~Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs >.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) #We only got .559 correct
mean(glm.pred != Direction.2005) #OUr test error rate this time is .44

#Suppose we want to predict the returns associated with particular values of Lag1 and Lag2.
#We want to predict Direction on a day when Lag1 and Lag2 eqal 1.2 and 1.1, and on a day when they equal 1.5 and -.8.
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -.8)), type = "response")


#4.6.3 Linear Discrminiant Analysis
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit #Shows that pi1 = .492 and pi2 = .508. This means 49.2% of th train observations correspond to days during which the market went down. 
#Gives group means: These are the average of each predictor within each class, and are used by LDA as estimates of uk. 
#These show that there is a tendency for the previous 2 days returns to be neg. on days when the market increases, and tendency for the previous return days' returns
#to be pos. on days when the market declines. 
#The coefficients of linear discriminants output provides the linear combination of Lag1 and Lag2 that are used to form the LDA decision rule.
#In other words, theses are the multipliers of the elements of X=x.
#If ???0.642*Lag1???0.514*Lag2 is large, then the LDA classifier will predict market increase, and if small then LDA predict market decline. 

#plot() produces plots of the linear discriminants obtained by computing -0.642*Lag1???0.514*Lag2 for each training observation
plot(lda.fit)
#lda predict returns 3 elements
#1. class contains the LDA's predictions about the movement of the market
#2. posterier is a matrix whose kth column contains the posterier probablity that the corresponding observation belongs to the kth class.
#3. x contains the linear discriminants
lda.pred <- predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005) #Identical to logistic regression
mean(lda.class == Direction.2005) #accuracy is .56

#Applying 50% threshold to the posterior probablities allows us to recreate the predictions in lda.pred$class
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

#Notice that the posterior proability output by the model corresponds to the probability that the market will decrease:
lda.pred$posterior [1:20,1]
lda.class [1:20]

#Threshold at 90% instead
sum(lda.pred$posterior[,1]>.9)


#4.6.4 Quadratic Discriminant Analysis

qda.fit <- qda(Direction ~Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

#Use predict() to see how this model does on the test set
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) #.599 accuaracy on test data

#4.6.5 K Nearst Neighbors
#knn() fucntion requires 4 inputs
#1.  A matrix containing the predictors associated with the training data, labeled train.X below.
#2.  A matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X below.
#3.  A vector containing the class labels for the training observations, labeled train.Direction below.
#4.  A value for K, the number of nearest neighbors to be used by the classifier

library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

#Create model
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)

#See how Knn pred does
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) #The result is not very good since the accuracy is only .5.

#Let's try with k =3
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)

#See how k = 3 model predicts
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) #Up to .53 accuracy


#4.6.6 An Application to Caravan Insurance Data
#Applying KNN approach to Caravan data set: Determine if someone buys a caravan insurance policy
library(ISLR)
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

#Need to standarize the predictors
standarized.X = scale(Caravan[,-86]) #Excluded column 86 since it's the qualative response (Purchase)
var(Caravan[,1])
var(Caravan[,2])

#Now let's see what the variance of the standarized versions are. Should be 1, since N(0,1) is standarized normal dist.
var(standarized.X[,1])
var(standarized.X[,2])

#Now split the data into train and test sets
test <- 1:1000
train.X = standarized.X[-test,]
test.X = standarized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

#Create model with k = 1
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k =1)
mean(test.Y !=knn.pred) #Not accuracy rate, but the error rate. That's pretty low, which is good
mean(test.Y !="No") #This number is the % of people who bought insurance. So model is not good as if we just predicted that everyone was a no. Since that error rate is 6%

##Want ot know who would buy
table(knn.pred, test.Y)
9/(68+9) #.11. This is the % of people that model got correct for people who would buy insurance



#Try with k = 3
knn.pred <- knn(train.X, test.X, train.Y, k =3)
table(knn.pred, test.Y)
5/(21+5) #.19


#Try with k = 5
knn.pred <- knn(train.X, test.X, train.Y, k =5)
table(knn.pred, test.Y)
4/(11+4) #.266

##Try using a logistic regression
glm.fit <- glm(Purchase~., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fit, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs>.5] <- "Yes"
table(glm.pred,test.Y) #Not good. Got zero number of yes predictions correct for yes responses

#Let's soften our threshold for glm.probs>.5 to glm.probs>.25 to be yes
glm.pred <- rep("No", 1000)
glm.pred[glm.probs>.25] <- "Yes"
table(glm.pred,test.Y)
11/(11+22) #Now we predict that 33 people will buy insurance, and we get 33% of these people correct. This is 5 times better than random guess, which was 6%

