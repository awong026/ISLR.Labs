#ISLR Chap. 10 Lab: Unsupervised Learning (4/13/18)

#10.4 Principal Components Analysis (PCA)

states = row.names(USArrests)
states

names(USArrests)

#Examine data
apply(USArrests, 2, mean) #mean of each column

apply(USArrests, 2, var) #vastly different from each other

#Need to standarize for PCA
#Create model
pr.out = prcomp(USArrests, scale = T) #prcomp() by default centers variables to have mean zero

names(pr.out)


#The center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA
pr.out$center
pr.out$scale

#Rotation matrix provides the principal components loadings; each column contains the corresponding principal component loading vector
pr.out$rotation #there are 4 principal components. 
#This is becuse there are general min(n-1,p) informative principal components in a data set with n observations and p variables

#Using prcomp() we don't need to multiply the data by the principal compoent loading vectors in order to obtain the principal component score vectors.
#Rather the 50*4 matrix "x" has as its columns the princpal component score vectors. That is, the kth column is the kth principal component score vector

dim(pr.out$x)
pr.out$x


#We can plot the first 2 principal components as follows:
biplot(pr.out, scale = 0)
#The scale = 0 ensures that the arrows are scaled to represent the loadings; other alues for scale give slightly different biplots with different interpretations

#Recall thata the principal components are only unique up to a sign change, so we can reproduce Figure 10.1 by making a few changes
#(flip the plot)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

#Standard deviation of each principal component.
pr.out$sdev

#The variance explaiend by each principal component is obtained by:
pr.var <- pr.out$sdev^2
pr.var


#Proportion of variance explained by each principal component
#Divide variance explained by each pc by the total variance explained by all four pc
pve <- pr.var/sum(pr.var)
pve #first princpal explains 62% of variance in data, then etc..

#Plot PVE and cumlative PVE
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Compnent" , ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = "b")

#Use cumsum() to compute the cumlative sum of the elements of a numeric vector
a = c(1,2,8,-3)
cumsum(a)



#10.5 Clustering
#10.5.1 K means Clustering
set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] = x[1:25,1] +3
x[1:25, 2] = x[1:25,2] -4


#Perform k mean clustering with k = 2
km.out <- kmeans(x,2, nstart = 20)

#The cluter assignments of the 50 obs are contained in km.out$cluter
km.out$cluster

#kmean() separted the observations into 2 clusters even though we didn't give it any information
#plot
plot(x, col = (km.out$cluster+1), main = "k mean clustering results with k = 2", xlab = "", ylab = "", pch = 20, cex = 20)


#Here the observations can be easily plotted because they are 2D. If there were more than 2 variables then we could perform PCA and plot the first 2 PC score vectors

#k=3 model
set.seed(4)
km.out <- kmeans(x,3,nstart =20) #When k = 3, the k means clustering splits up the two clusters
km.out

#To run the kmeans() function in R with multiple initial cluster assignments, we use the nstart agrument. If a value of nstart greater than one is used,
#then k means clustering will be performed using multiple random assignments, and only the best result will be shown.

#Let's compare nstart = 1 to nstart = 20
set.seed(3)
km.out=kmeans (x,3, nstart =1)
km.out$tot.withinss
km.out=kmeans (x,3, nstart =20)
km.out$tot.withinss
#Note that the km.out$withinss is the total within cluster sum of squares, which we seek to minimize by performing k means clustering
##Always run k mean clustering with a large nstart value like 20 or 50, otherwise an undesirable local optimum may be obtained

#10.5.2 Hierarchial Clustering

#model with method = "complete"
hc.complete = hclust(dist(x), method = "complete")
#model with method = avg and single
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

#Plot the denderograms obtained by using plot(). The numbers at the bottom of the plot id each observation
par(mfrow = c(1,3))
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="", cex=.9)
plot(hc.average , main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single , main="Single Linkage ", xlab="", sub="",cex=.9)

#to determine the cluster labels for each obseration associated with given cut of dendrogram, we use cutree()
cutree(hc.complete , 2)
cutree(hc.average , 2)
cutree(hc.single , 2) #Only 1 cluster here let's try to fix it
cutree(hc.single , 4)

#To scale variables before performing hierarchial clustering of observations use scale()
xsc = scale(x)
plot(hclust(dist(xsc), method ="complete "), main=" Hierarchical Clustering with Scaled Features ")

#Correlation-based distance can be computed using as.dist()
#This only makes sense for data with at least 3 features since the absolute correlation between any two observations for a 2 features is always 1.
x=matrix(rnorm (30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method ="complete"), main=" Complete Linkage with Correlation -Based Distance ", xlab="", sub ="")

par(mfrow =c(1,1))

#10.6 NCI60 Example: 6830 gene expression on 64 cancer cell lines
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)

#Cancer types
nci.labs[1:4]
table(nci.labs)


#10.6.1 PCA on NCI60 Data
pr.out = prcomp(nci.data, scale = T)

#Plot first few Pc score vectors. 
#Create cols function: Assigns a distinct color to each element of a numeric vector
#function will be used to assign a color to each of the 64 cell lines based on cancer type it corresponds to
Cols=function (vec){
   cols=rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))])
  }
#rainbow() used to get colors 
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z3")
#On the whole, cell lines corresponding to a single cancer type do tend to have similar values on the first few pc score vecotrs.
#This indicates the cell lines from the same cancer type tend to have pretty similiar gene expression levels

#Get summary of proportion of variance explained (PVE) of the first few pc using summary()
summary(pr.out)

#Use plot() to plot the variance explained by the first few pcs
par(mfrow =c(1,1))
plot(pr.out)

#More helpful to plot PVE of each principal component (scree plot) and the cumlative PVE of each pc.
pve =100*pr.out$sdev ^2/sum(pr.out$sdev ^2)
par(mfrow=c(1,2))
plot(pve , type="o", ylab="PVE", xlab=" Principal Component ", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab= "Principal Component ", col="brown3")
par(mfrow =c(1,1))

#10.6.2 Clustering the Observations of NCI60 Data

#need to scale data
sd.data=scale(nci.data)

#Hierarchial clustering of observations using single, avg., and complete linage. 
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs , main="Complete Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="average "), labels=nci.labs , main="Average Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="single"), labels=nci.labs , main="Single Linkage ", xlab="", sub="",ylab="")
par(mfrow =c(1,1))


#Typically, single linkage will tend to yield trailing clusters: very large clusters onto which individual observations attach one-by-one.
#On the other hand, complete and average linkage tend to yield more balanced, attractive clusters. For this reason, complete
#and average linkage are generally preferred to single linkage.


#Use complete linkage going forward
#Can cut dendrogram at the height that will yield a particular number of clusters, say four:
hc.out=hclust(dist(sd.data))
hc.clusters =cutree(hc.out ,4)
table(hc.clusters ,nci.labs)
#There are some pattersn like all the leukemia cell lines fall in cluster 3, while the breast cancer cells lines are spread over 3 diff. clusters

#Cut on the dendrogram that produces these four clusters:
par(mfrow=c(1,1))
plot(hc.out , labels =nci.labs)
abline(h=139, col="red")

#Brief summary of the object:
hc.out


#how do these hieracharl models compare to the k means clustering earlier at k = 4?
set.seed(2)
km.out=kmeans(sd.data , 4, nstart =20)
km.clusters =km.out$cluster
table(km.clusters ,hc.clusters )
#We see that the four clusters obtained using hieracharl clustering and kmean clustering somewhat differ. 
#Cluster 2 in kmeans clustering is identical to  cluster 3 in hierarch clustering. 
#However, the other clusters differ: for instance, cluster 4 in K-means clustering contains a portion of
#the observations assigned to cluster 1 by hierarchical clustering, as well as
#all of the observations assigned to cluster 2 by hierarchical clustering

#Instead of preforming hiearcharl clustering on the entire data matrix, we perform hierarchial clustering on the first few principal component score vectors
hc.out=hclust(dist(pr.out$x [,1:5]) )
plot(hc.out , labels =nci.labs , main="Hier. Clust. on First Five Score Vectors ")
table(cutree (hc.out ,4), nci.labs)

#Not surprisingly, these results are different from the ones that we obtained
#when we performed hierarchical clustering on the full data set. Sometimes
#performing clustering on the first few principal component score vectors
#can give better results than performing clustering on the full data. In this
#situation, we might view the principal component step as one of denoising
#the data. We could also perform K-means clustering on the first few
#principal component score vectors rather than the full data set



