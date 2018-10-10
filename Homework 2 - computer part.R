##Computer Homework Part
load("hw1.RData")

#Convert to dataframes for lm function
train2.df <- as.data.frame(train2)
train3.df <- as.data.frame(train3)
test2.df <- as.data.frame(test2)
test3.df <- as.data.frame(test3)

#Make xtrain mattrix
xtrain <- rbind(train2.df,train3.df)

#Make xtest matrix
xtest <- rbind(test2.df, test3.df)

#Make ytrain vector
y2 <- cbind(rep(0,731))
y3 <- cbind(rep(1,658))
ytrain <- rbind(y2,y3)
ytrain.vec <- as.vector(rbind(y2,y3))



#Make ytest vector
y2.test <- cbind(rep(0,198))
y3.test <- cbind(rep(1,166))
ytest <- rbind(y2.test,y3.test)
ytest.vec <- as.vector(ytest)


#Make fulltrain and fulltrest matrix 
fulltrain <- cbind(xtrain,ytrain)
fulltest <- cbind(xtest,ytest)

##Question 6

##Part a
pr.three <- prcomp(train3, scale=FALSE)
dim(pr.three$x)

x.mean <- apply(train3,2,mean)
x.sd <- apply(train3,2,sd)
x.svd <- svd(train3)
x.score1 <- train3 %*% x.svd$v
x.score2 <- x.svd$u %*% diag(x.svd$d)
plot(x.score1[,1],x.score1[,2])

##Part b
pr.two <- prcomp(train2, scale=FALSE)
x.mean <- apply(train2,2,mean)
x.sd <- apply(train2,2,sd)s
x.svd <- svd(train2)
x.score1 <- train2 %*% x.svd$v
x.score2 <- x.svd$u %*% diag(x.svd$d)
plot(x.score1[,1],x.score1[,2])

##Part c
pr.train <- prcomp(xtrain, scale=FALSE)
x.mean <- apply(xtrain,2,mean)
x.sd <- apply(xtrain,2,sd)
x.svd <- svd(xtrain)
x.score1 <- xtrain %*% x.svd$v
x.score2 <- x.svd$u %*% diag(x.svd$d)
plot(x.score1[,1],x.score1[,2])






##Question 7
#Part a

library(glmnet)
library(boot)
set.seed(1)
fulltrain.df
ols.mod <- glm(ytrain~., data = fulltrain)
cv.ols.error <- cv.glm(fulltrain, ols.mod, K=5)$delta[1]
cv.ols.error

#Part b
set.seed(1)
ols.mod <- glm(ytrain~., data = fulltrain)
cv.ols.error <- cv.glm(fulltrain, ols.mod, 
                       cost = function(y,y.hat) mean((y.hat>.5)!=y), 
                       K=5)$delta[1]
cv.ols.error

##Question 8
##Part a
##Get KNN functions from Code 4 in lecture notes

knn <- function(klist,x.train,y.train,x.test) {
  # k-nearest neighbors classification
  #
  # klist is a list of values of k to be tested
  # x.train, y.train: the training set
  # x.test: the test set
  # Output: a matrix of predictions for the test set (one column for each k in klist)
  # Number of training and test examples
  n.train <- nrow(x.train)
  n.test <- nrow(x.test)
  
  # Matrix to store predictions
  p.test <- matrix(NA, n.test, length(klist))
  
  # Vector to store the distances of a point to the training points
  dsq <- numeric(n.train)
  
  # Loop on the test instances
  for (tst in 1:n.test)
  {
    # Compute distances to training instances
    for (trn in 1:n.train)
    {
      dsq[trn] <- sum((x.train[trn,] - x.test[tst,])^2)
    }
    
    # Sort distances from smallest to largest
    ord <- order(dsq)
    
    # Make prediction by averaging the k nearest neighbors
    for (ik in 1:length(klist)) {
      p.test[tst,ik] <- mean(y.train[ord[1:klist[ik]]])
    }
  }
  
  # Return the matrix of predictions
  invisible(p.test)
}

knn.cv <- function(klist,x.train,y.train,nfolds) {
  # Cross-validation for kNN
  #
  # Perform nfolds-cross validation of kNN, for the values of k in klist
  
  # Number of instances
  n.train <- nrow(x.train)
  
  # Matrix to store predictions
  p.cv <- matrix(NA, n.train, length(klist))
  
  # Prepare the folds
  s <- split(sample(n.train),rep(1:nfolds,length=n.train))
  
  # Cross-validation
  for (i in seq(nfolds)) {
    p.cv[s[[i]],] <- knn(klist,x.train[-s[[i]],], y.train[-s[[i]]], x.train[s[[i]],])
  }
  
  # Return matrix of CV predictions
  invisible(p.cv)
}
klist <- seq(1,21, by=2)
nfolds <- 5
y.pred.train <- knn(klist,xtrain,ytrain,xtrain)
y.pred.test <- knn(klist,xtrain,ytrain,xtest)
set.seed(1)
y.pred.cv <- knn.cv(klist,xtrain,ytrain,nfolds)

y.pred.train

#Get Misclassification Errors
loss.train <- apply(((y.pred.train>.5)!=ytrain.vec) , 2, mean)
loss.test <- apply(((y.pred.test>.5)!=ytest.vec) , 2, mean)
loss.cv <- apply(((y.pred.cv>.5)!=ytrain.vec) , 2, mean)

#Plot errors as function of K-neighbors
plot(loss.train, type='l', ylim=c(0,.05), xlab='k', ylab='Misclassification Rate', col=1,lwd=2)
lines(loss.test, col=2, lwd=2)
lines(loss.cv, col=3, lwd=2)
legend("bottomright", legend = c('Train', 'Test', 'CV'), text.col=seq(3), lty=1, col=seq(3))

loss.cv[1]
loss.test[1]



#Question 9
##Part a
library(glmnet)
set.seed(1)
ridge.mod <- glmnet(xtrain,ytrain,alpha=0)

cv.ridge.out <- cv.glmnet(xtrain,ytrain,alpha=0,nfolds = 5)

plot(cv.ridge.out)
bestlam <- cv.ridge.out$lambda.min
bestlam
bestlam.se <- cv.ridge.out$lambda.1se
bestlam.se

##Get Test predictions 
ridge.pred <- predict(ridge.mod, s = bestlam, newx=xtest)
##Get Misclassification errors with bestlam
ridge.bestlam.test <- mean((ridge.pred>.5)!=ytest)
ridge.bestlam.test
##Get Misclassification errors with bestlam.se
ridge.pred.se <- predict(ridge.mod, s = bestlam.se, newx=xtest)
ridge.bestlam.se.test <- mean((ridge.pred.se>.5)!=ytest)
ridge.bestlam.se.test


##Part b - ask professor for help. Why does it keep saying my lambdas are negative

#List of lambdas to use in CV function
lambdas <- as.vector(cv.ridge.out$lambda)
typeof(lambdas)

##CV Function for Ridge and Lasso
ridge.cv <-function(xtrain, ytrain, nfolds, lambdalist, alpha){
  
  #Number of instances
  n.train <- nrow(xtrain)
  
  #Matrix to store predictions
  cv.errors <- matrix(NA, nfolds, length(lambdalist))
  
  #Prepare the folds
  s <- split(sample(n.train),rep(1:nfolds,length=n.train))
  
  #Cross-validation
  for(i in seq(nfolds)){
    for(j in 1:length(lambdalist)){
      mod <- glmnet(xtrain[-s[[i]],], ytrain[-s[[i]]], alpha = alpha, lambda=lambdalist[j] )
      preds <- predict(mod, s=lambdalist[j], newx = xtrain[s[[i]],])
      cv.errors[i,j] <- mean((preds>.5)!=ytrain[s[[i]]])
    }
  }
  #CV Error matrix it returns
  cv.errors
}


cv.errors <- ridge.cv(xtrain=xtrain,ytrain=ytrain,nfolds=5,lambdalist=lambdas,alpha=0)
mean.cv.errors <- apply(cv.errors,2,mean)
plot(log(lambdas), mean.cv.errors)
which.min(mean.cv.errors)
lambdas[67]
mean.cv.errors[67]

ridge.pred <- predict(ridge.mod, s = .01367945, newx=xtest)
ridge.bestlam.test <- mean((ridge.pred>.5)!=ytest)
ridge.bestlam.test


##Question 10
##Part a

set.seed(1)
lasso.mod <- glmnet(xtrain,ytrain,alpha=1)
cv.lasso.out <- cv.glmnet(xtrain,ytrain,alpha=1,nfolds = 5)
plot(cv.lasso.out)

plot(cv.ridge.out)
bestlam.lasso <- cv.lasso.out$lambda.min
bestlam.lasso
bestlam.lasso.se <- cv.lasso.out$lambda.1se
bestlam.lasso.se

##Get Test predictions 
lasso.pred <- predict(lasso.mod, s = bestlam.lasso, newx=xtest)
##Get Misclassification errors with bestlam
lasso.bestlam.test <- mean((lasso.pred>.5)!=ytest)
lasso.bestlam.test
##Get Misclassification errors with bestlam.se
lasso.pred.se <- predict(lasso.mod, s = bestlam.lasso.se, newx=xtest)
lasso.bestlam.se.test <- mean((lasso.pred.se>.5)!=ytest)
lasso.bestlam.se.test

##Part b - Fix code from part b question 9 and re-use
lambdas.lasso <- as.vector(cv.lasso.out$lambda)
lasso.cv.errors <- ridge.cv(xtrain=xtrain,ytrain=ytrain,nfolds=5,lambdalist=lambdas.lasso,alpha=1)
mean.lasso.cv.errors <-apply(lasso.cv.errors,2,mean)
plot(log(lambdas.lasso), mean.lasso.cv.errors)

which.min(mean.lasso.cv.errors)
lambdas.lasso[41]
mean.lasso.cv.errors[41]
lasso.pred <- predict(lasso.mod, s = .00858805, newx=xtest)
lasso.bestlam.test <- mean((lasso.pred>.5)!=ytest)
lasso.bestlam.test

#Question 11

install.packages("pls")
library(pls)
fulltrain.df <- as.data.frame(fulltrain)

#Make PCR model
pcr.fit <- pcr(ytrain~xtrain, scale=FALSE, validation="none")

#Make empty vectors to hold the training/test errors for PC=1..256
pcr.train.errors <- rep(NA,256)
pcr.test.errors <- rep(NA,256)

for(i in 1:256){
  train.pred <- predict(pcr.fit, xtrain, ncomp = i)
  pcr.train.errors[i] <- mean((train.pred>.5)!=ytrain.vec)
  test.pred <- predict(pcr.fit, xtest, ncomp = i)
  pcr.test.errors[i] <- mean((test.pred>.5)!=ytest.vec)
}


##Cross Validation function for PCR
pcr.cv <- function(xtrain, ytrain, nfolds){
  
  #Number of instances
  n.train <- nrow(xtrain)
  
  #Max number of components
  components <-ncol(xtrain)
  
  #Matrix to store predictions
  pcr.errors <- matrix(NA, nfolds, components)
  
  #Prepare the folds
  s <- split(sample(n.train),rep(1:nfolds,length=n.train))
  
  #Cross-validation
  for(i in seq(nfolds)){
    mod <- pcr(ytrain[-s[[i]]] ~ xtrain[-s[[i]],], scale = FALSE, validation = "none")
    for(j in 1:components){
      preds <- predict(mod, xtrain[s[[i]],], ncomp =j)
      pcr.errors[i,j] <- mean((preds>.5)!=ytrain[s[[i]]])
    }
  }
  #CV Error matrix it returns
  pcr.errors
}

##Calculate the mean cross validated errors
pcr.cv.errors <- pcr.cv(xtrain,ytrain, nfolds = 5)
pcr.cv.errors.mean <-apply(pcr.cv.errors,2,mean)

##Plot training, test and cv errors
plot(pcr.train.errors, type='l', ylim=c(0,.05), xlab='# of Components', ylab='Misclassification Rate', col=1,lwd=2)
lines(pcr.test.errors, col=2, lwd=2)
lines(pcr.cv.errors.mean, col=3, lwd=2)
points(30,pcr.cv.errors.mean[30], pch=19, col="pink")
legend("topright", legend = c('Train', 'Test', 'CV'), text.col=seq(3), lty=1, col=seq(3))

pcr.train.errors[30]
pcr.test.errors[30]

#Question 12
library(leaps)
regfit.fwd <- regsubsets(ytrain~., data=fulltrain, method="forward",nvmax=256)

#Make the design matrices to use in making predictions
train.mat <- model.matrix(ytrain~., data=fulltrain)
test.mat <- model.matrix(ytest~., data=fulltest)

#Get train errors
train.errors <- rep(NA,256)
for(i in 1:256){
  coefi <- coef(regfit.fwd, id = i)
  pred <- train.mat[,names(coefi)] %*% coefi
  train.errors[i] <- mean((pred>.5)!=ytrain)
}
train.errors
#Get test errors
test.errors <- rep(NA,256)
for(i in 1:256){
  coefi <- coef(regfit.fwd, id = i)
  pred <- test.mat[,names(coefi)] %*% coefi
  test.errors[i] <- mean((pred>.5)!=ytest)
}

#Get CV errors
k<-5
cv.errors <- matrix(NA,k,256)
#Split the data in 5 folds
ntrain <- nrow(xtrain)
set.seed(1)
s <- split(sample(ntrain),rep(1:k,length=ntrain))

for(j in 1:k){
  fit.fwd <- regsubsets(ytrain~., data=fulltrain[-s[[j]],], method="forward", nvmax=256)
  
  for(i in 1:256){
    coefi <- coef(fit.fwd, id=i)
    pred.mat <- model.matrix(ytrain~., data=fulltrain[s[[j]],])
    pred <- pred.mat[,names(coefi)] %*% coefi
    cv.errors[j,i] <- mean((pred>.5)!=ytrain[s[[j]]])
  }
}
mean.cv.errors <- apply(cv.errors,2,mean,na.rm=TRUE)
mean.cv.errors

#Now we plot the train, test and cv errors against the number of parameters
plot(train.errors, type='l', xlab='Predictors', ylab='Misclassification Rate', col=1,lwd=2)
points(54,train.errors[54], pch=19, col = "blue")
lines(test.errors, col=2, lwd=2)
points(54,test.errors[54], pch=19, col = "blue")
lines(mean.cv.errors, col=3, lwd=2)
legend("topright", legend = c('Train', 'Test', 'CV'), text.col=seq(3), lty=1, col=seq(3))

#The number of predictors with lowest CV error rate is 54.
which.min(mean.cv.errors)

#THe training error rate with 54 predictors is.
train.errors[54]

#The testing error rate with 54 predictors is.
test.errors[54]
