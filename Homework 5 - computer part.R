##Homework 5
load("hw3.RData")

##Part a
##Center training and test data according to training mean. Also make the ytrain and ytest columns.

xtrain<- rbind(train2, train3, train8)
##Center around means but don't scale variance
xtrain.standardized <- scale(xtrain, scale = FALSE)

#Make ytrain output variables
y2train <- cbind(rep(0,731))
y3train <- cbind(rep(1,658))
y8train <- cbind(rep(2,542))
ytrain<-rbind(y2train,y3train,y8train)

xtest <- rbind(test2,test3,test8)
##Standardize xtest according to column means of xtrain
xtest.standardized <- xtest - rep(colMeans(xtrain), nrow(xtest), ncol(xtest))

#Make ytest output variables
y2test <- cbind(rep(0,198))
y3test <- cbind(rep(1,166))
y8test <- cbind(rep(2,166))
ytest<-rbind(y2test,y3test,y8test)

##Kernal PCA
install.packages("kernlab")
library(kernlab)
par(mfrow=c(3,3))
sigma.list <- c(.01, .0125, .02, .025, .03, .035, .04, .045, .05)
for(sigma in sigma.list){
  kpc <- kpca(xtrain.standardized,kernel="rbfdot", kpar=list(sigma=sigma),features=2)
  plot(rotated(kpc),col = rep(2:4,rep(c(731,658,542))), xlab="1st Principal Component",ylab="2nd Principal Component",main=paste("sigma =", sigma))
}
par(mfrow=c(1,1))
kpc <- kpca(xtrain.standardized,kernel="rbfdot", kpar=list(sigma=.025),features=2)
plot(rotated(kpc),col = rep(2:4,rep(c(731,658,542))), xlab="1st Principal Component",ylab="2nd Principal Component",main=paste("sigma =", .025))
pca.matrix <- kpc@pcv
pca1 <- pca.matrix[,1]
pca2 <- pca.matrix[,2]


##Kernel FDA
##Compute the kernel matrix
K <- matrix(NA, 1931,1931)
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
sigma <- 2.5

for(i in seq(1:1931)){
  for(j in seq(1:1931)){
    kernel <- (exp(-(euc.dist(xtrain[i,],xtrain[j,])^2/(2*sigma^2))))
    K[i,j] <- kernel
  }
}


#Get u and u.k vectors
u <- as.matrix(apply(K, 1, sum) * 1/1931)
u.1 <- as.matrix(apply(K[,1:731], 1, sum) * 1/731)
u.2 <-as.matrix(apply(K[,732:1389], 1, sum) * 1/658)
u.3 <-as.matrix(apply(K[,1390:1931], 1, sum) * 1/542)


#Compute the K.w and K.b matrices

K.w <- (1/(1931 - 3)) * ((K%*%K) - ((731*(u.1%*%t(u.1))) + 
                                      (658*(u.2%*%t(u.2))) + 
                                      (542*(u.3%*%t(u.3)))))

K.b <- (1/(1931 - 1)) * (((731*(u.1%*%t(u.1))) + 
                        (658*(u.2%*%t(u.2))) +
                        (542*(u.3%*%t(u.3)))) - 
                        (1931*(u%*%t(u))))  
                            
#Get eigenvectors and values

K.S <- solve(K.w, tol = 5e-17) %*% K.b
K.S.eig <- eigen(K.S)


scores <- Re(K.S.eig$vectors) %*% K
plot(scores[,1], scores[,2], col = rep(2:4,rep(c(731,658,542))), main = "Sigma 2.5")

#Get test projection



#LDA with Kernel PCA Scores
library(MASS)
pca.train <- data.frame(x=pca.matrix, y = ytrain)
pca.lda <- lda(y~., data = pca.train)

#Get training errors
pca.train.preds <- predict(pca.lda, data.frame(x=pca.matrix))$class
pca.train.error <- mean(pca.train.preds != ytrain)
pca.train.error

#Get PCA for test matrix
pca.test.matrix <- predict(kpc, xtest.standardized)
pca.test <- data.frame(x = pca.test.matrix, y = ytest)
pca.test.lda <- lda(y~., data = pca.test)

#Get test errors
pca.test.preds <- predict(pca.test.lda, data.frame(x=pca.test.matrix))$class
pca.test.error <- mean(pca.test.preds != ytest)
pca.test.error

#LDA with Kernel FDA scores - 

fda.train <- data.frame(x = scores[,1:2], y = ytrain)
fda.lda <- lda(y~., data = fda.train)

#Get training errors
fda.train.preds <- predict(fda.lda, data.frame(x=scores[,1:2]))$class
fda.train.error <- mean(fda.train.preds != ytrain)
fda.train.error

#Get test errors -??




#SVM of Zipcode Data

#SVM with Polynomial Kernel
library(e1071)
fulltrain <- data.frame(x=xtrain, y=as.factor(ytrain))
svm.polynomial <- tune(svm, y~., data = fulltrain, kernel = "polynomial", ranges = list(cost=c(0.1,1,10,100,1000), coef0 = c(1,2,3,4)))
summary(svm.polynomial)
svm.poly.bestmod <- svm.polynomial$best.model

#Get training errors
train.preds <- predict(svm.poly.bestmod, data.frame(x=xtrain))
svm.poly.train.error <- mean(train.preds != as.factor(ytrain))
svm.poly.train.error

#Get test errors
test.preds <- predict(svm.poly.bestmod, data.frame(x=xtest))
svm.poly.test.error <- mean(test.preds != as.factor(ytest))
svm.poly.test.error

#SVM with radial kernel
svm.radial <- tune(svm, y~., data = fulltrain, kernel = "radial", ranges = list(cost=c(0.1,1,10,100,1000), gamma = c(.5,1,2,3,4)))
summary(svm.radial)
svm.radial.bestmod <- svm.radial$best.model

#Get training errors
radial.train.preds <- predict(svm.radial.bestmod, data.frame(x=xtrain))
svm.radial.train.error <- mean(radial.train.preds != as.factor(ytrain))
svm.radial.train.error

#Get test errors
radial.test.preds <- predict(svm.radial.bestmod, data.frame(x=xtest))
svm.radial.test.error <- mean(radial.test.preds != as.factor(ytest))
svm.radial.test.error


##Spectral Clustering
rm(list=ls(all=TRUE)) 
set.seed(1) 
n <- 300 
x <- matrix(rnorm(n*2),ncol=2) 
x <- t(apply(x, 1, function(x){x/sqrt(sum(x^2))})) 
x <- diag(c(rep(1,n/3),rep(2,n/3),rep(4,n/3))) %*% x 
x <- x + matrix(rnorm(n*2),ncol=2)*.1 
y <- c(rep(1,n/3),rep(2,n/3),rep(3,n/3)) 
plot(x,col=y+1)

#Kmeans with k =2

kmeansfit1 <- kmeans(x, center = 2)
plot(x, col = kmeansfit1$cluster)

kmeansfit2 <- kmeans(x, center = 3)
plot(x, col = kmeansfit2$cluster)

#Spectral Clustering
#Create the Similarity Matrix
#Same as Edge matrix since we are making it fully connected

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

sigma.list <- c(.1,.15,.20,.25,.30,.35,.40,.45,.5,.6,.7,.8,.9,1,2,3,4,5)
par(mfrow=c(6,3))
for(sigma in sigma.list){
  S <- matrix(rep(NA,300^2), 300, 300)
  sigma <- sigma
  for(i in 1:300){
    for(j in 1:300){
      S[i,j] <- (exp(-(euc.dist(x[i,],x[j,])^2/(2*sigma^2))))
    }
  }
  
  #This is spectral decomposition with fully connected matrix
  #Get D matrix
  D <- diag(apply(S,1,sum))
  #Get Laplace random walk matrix
  I <- diag(1,300)
  L.rw <- I - (solve(D)%*%S)
  L.eig <- eigen(L.rw, symmetric = TRUE)
  Z <- L.eig$vectors[,298:300]
  spectral <- kmeans(Z, center = 3)
  plot(x, col = spectral$cluster, main = sigma)
}







