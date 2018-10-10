##Homework 3
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

##Part b - Perform PCA and get first 2 scores
xtrain.svd <- svd(xtrain.standardized)
xtrain.score <- xtrain.standardized%*% xtrain.svd$v
score1 <- xtrain.score[,1]
score2 <- xtrain.score[,2]
plot(score1, score2, col = rep(2:4,rep(c(731,658,542))))

##Part c - FDA

mu.hat <- apply(xtrain, 2 , mean)
mu1.hat <- apply(train2, 2, mean)
mu2.hat <- apply(train3, 2, mean)
mu3.hat <- apply(train8, 2, mean)
n <- 1931
S.b <- ((731) * (mu1.hat - mu.hat) %*% t(mu1.hat - mu.hat) +
           (658) * (mu2.hat - mu.hat) %*% t(mu2.hat - mu.hat) +
           (542)* (mu3.hat - mu.hat) %*% t(mu3.hat - mu.hat))/(n-1)

S.w <- (t(train2 - rep(1,731)%*%t(mu1.hat)) %*% (train2 - rep(1,731) %*% t(mu1.hat)) + 
          t(train3 - rep(1,658) %*% t(mu2.hat)) %*% (train3 - rep(1,658) %*% t(mu2.hat)) +
          t(train8 - rep(1,542) %*% t(mu3.hat)) %*% (train8 - rep(1,542) %*% t(mu3.hat)))/ (n-3)

dim(S.b)

S <- solve(S.w) %*% S.b
S.eig <- eigen(S)
length(Re(S.eig$vectors[,1]))
fda1 <- xtrain %*% Re(S.eig$vectors[,1])
fda2 <- xtrain %*% Re(S.eig$vectors[,2])
plot(fda1, fda2, col = rep(2:4,rep(c(731,658,542))))

##Part d

##OLS
##Turn ytrain vector into multivariate indicator Ytrain matrix
y.indicator <- cbind(ytrain==0,ytrain==1,ytrain==2) * 1
x.design <- cbind(rep(1,nrow(xtrain.standardized)),xtrain.standardized)
Betas <- solve(t(x.design) %*% x.design) %*% t(x.design)%*%y.indicator
Betas

fitted <- x.design %*% Betas
fitted
##Find the max fitted value in each row and the category it corresponds to
##1st col == 0, 2nd col == 1, 3rd col == 2
y.train.hat <- rep(NA, nrow(fitted))
for(i in 1:nrow(fitted)){

  maxcol <- which.max((fitted[i,]))
  if(maxcol == 1){
    y.train.hat[i] <- 0
  } else if(maxcol == 2){
    y.train.hat[i] <- 1
  } else {
    y.train.hat[i] <- 2
  }
}
##Train errors
mean(ytrain != y.train.hat)


##Calculated fitted values for xtest
xtest.design <- cbind(rep(1,nrow(xtest.standardized)),xtest.standardized) 
fitted.test <- xtest.design %*% Betas
y.test.hat <- rep(NA, nrow(fitted.test))
for(i in 1:nrow(fitted.test)){
  
  maxcol <- which.max((fitted.test[i,]))
  if(maxcol == 1){
    y.test.hat[i] <- 0
  } else if(maxcol == 2){
    y.test.hat[i] <- 1
  } else {
    y.test.hat[i] <- 2
  }
}
##Test errors
mean(ytest != y.test.hat)

##LDA
##Make parameter estimates
n1 <- sum(ytrain == 0)
n2 <- sum(ytrain == 1)
n3 <- sum(ytrain == 2)
n <- n1+n2+n3
pi1 <- n1/n
pi2 <- n2/n
pi3 <- n3/n


mu1.hat <- apply(xtrain.standardized[ytrain==0,],2,mean)
mu2.hat <- apply(xtrain.standardized[ytrain==1,],2,mean)
mu3.hat <- apply(xtrain.standardized[ytrain==2,],2,mean)
##Estimate within class covariance
S.w <- (t(xtrain.standardized[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) %*% (xtrain.standardized[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) + 
                 t(xtrain.standardized[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) %*% (xtrain.standardized[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) +
                 t(xtrain.standardized[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)) %*% (xtrain.standardized[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)))/n-3

##Calculate max discriminant function and corresponding yhat for each row of xtrain

y.hat.train <- rep(NA, nrow(xtrain.standardized))
for(i in 1:nrow(xtrain.standardized)){
  discrim1 <- log(pi1) + t(xtrain.standardized[i,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
  discrim2 <- log(pi2) + t(xtrain.standardized[i,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
  discrim3 <- log(pi3) + t(xtrain.standardized[i,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
  maxdiscrim <- max(discrim1, discrim2, discrim3)
  if(maxdiscrim == discrim1){
    y.hat.train[i] <- 0
  } else if (maxdiscrim == discrim2){
    y.hat.train[i] <-1
  } else {
    y.hat.train[i] <- 2
  }
  
}

##Train errors
mean(ytrain != y.hat.train)

##Calculate max discriminant function and corresponding yhat for each row of xtest
y.hat.test <- rep(NA, nrow(xtest.standardized))
for(i in 1:nrow(xtest.standardized)){
  discrim1 <- log(pi1) + t(xtest.standardized[i,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
  discrim2 <- log(pi2) + t(xtest.standardized[i,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
  discrim3 <- log(pi3) + t(xtest.standardized[i,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
  maxdiscrim <- max(discrim1, discrim2, discrim3)
  if(maxdiscrim == discrim1){
    y.hat.test[i] <- 0
  } else if (maxdiscrim == discrim2){
    y.hat.test[i] <-1
  } else {
    y.hat.test[i] <- 2
  }
}

##Test errors
mean(ytest != y.hat.test)


##Logistic Regression
library(nnet)
train <- cbind(xtrain.standardized, ytrain)
train <- as.data.frame(train)
names(train) <- c(paste0('V', 1:256), 'ytrain')
train
log.fit <- multinom(ytrain~., data = train)
summary(log.fit)

##Training errors
y.train.hat <- predict(log.fit, xtrain.standardized)
table(y.train.hat, ytrain)

##Test errors
xtest.df <- as.data.frame(xtest.standardized)
y.test.hat <- predict(log.fit,newdata = xtest.df)
classification <- table(y.test.hat, ytest)
1-sum(diag(classification))/sum(classification)

##Part e

##OLS with first two Principal components

pc.matrix <- cbind(rep(1,length(score1)), score1, score2)
##Use same y-indicator matrix from before
Betas <- solve(t(pc.matrix) %*% pc.matrix) %*% t(pc.matrix)%*%y.indicator

fitted <- pc.matrix %*% Betas

##Find the max fitted value in each row and the category it corresponds to
##1st col == 0, 2nd col == 1, 3rd col == 2
y.train.hat <- rep(NA, nrow(fitted))
for(i in 1:nrow(fitted)){
  
  maxcol <- which.max((fitted[i,]))
  if(maxcol == 1){
    y.train.hat[i] <- 0
  } else if(maxcol == 2){
    y.train.hat[i] <- 1
  } else {
    y.train.hat[i] <- 2
  }
}
##Train errors
mean(ytrain != y.train.hat)

##Calculate yhat test values
xtest.pc <- xtest.standardized %*% xtrain.svd$v[,1:2]
test.pc.matrix <- cbind(rep(1,nrow(xtest.pc)), xtest.pc)
fitted.test <- test.pc.matrix %*% Betas

y.test.hat <- rep(NA, nrow(fitted.test))
for(i in 1:nrow(fitted.test)){
  
  maxcol <- which.max((fitted.test[i,]))
  if(maxcol == 1){
    y.test.hat[i] <- 0
  } else if(maxcol == 2){
    y.test.hat[i] <- 1
  } else {
    y.test.hat[i] <- 2
  }
}
##Test errors
mean(ytest != y.test.hat)


#LDA with leading PC Scores
pc <- cbind(score1, score2)

n1 <- sum(ytrain == 0)
n2 <- sum(ytrain == 1)
n3 <- sum(ytrain == 2)
n <- n1+n2+n3
pi1 <- n1/n
pi2 <- n2/n
pi3 <- n3/n


mu1.hat <- apply(pc[ytrain==0,],2,mean)
mu2.hat <- apply(pc[ytrain==1,],2,mean)
mu3.hat <- apply(pc[ytrain==2,],2,mean)
##Estimate within class covariance
S.w <- (t(pc[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) %*% (pc[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) + 
          t(pc[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) %*% (pc[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) +
          t(pc[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)) %*% (pc[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)))/n-3

y.hat.train <- rep(NA, nrow(pc))
for(i in 1:nrow(xtrain.standardized)){
  discrim1 <- log(pi1) + t(pc[i,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
  discrim2 <- log(pi2) + t(pc[i,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
  discrim3 <- log(pi3) + t(pc[i,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
  maxdiscrim <- max(discrim1, discrim2, discrim3)
  if(maxdiscrim == discrim1){
    y.hat.train[i] <- 0
  } else if (maxdiscrim == discrim2){
    y.hat.train[i] <-1
  } else {
    y.hat.train[i] <- 2
  }
  
}

##Train errors
mean(ytrain != y.hat.train)

##Calculate max discriminant function and corresponding yhat for PC components witht xtest
pc.test <- xtest.standardized %*% xtrain.svd$v[,1:2]
y.hat.test <- rep(NA, nrow(pc.test))
for(i in 1:nrow(xtest.standardized)){
  discrim1 <- log(pi1) + t(pc.test[i,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
  discrim2 <- log(pi2) + t(pc.test[i,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
  discrim3 <- log(pi3) + t(pc.test[i,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
  maxdiscrim <- max(discrim1, discrim2, discrim3)
  if(maxdiscrim == discrim1){
    y.hat.test[i] <- 0
  } else if (maxdiscrim == discrim2){
    y.hat.test[i] <-1
  } else {
    y.hat.test[i] <- 2
  }
}
##Test errors
mean(ytest != y.hat.test)


##Logistic Regression with first 2 PCAs
library(nnet)
train <- cbind(score1, score2, ytrain)
train <- as.data.frame(train)
names(train) <- c('score1', 'score2', 'ytrain')
log.pca <- multinom(ytrain~., data = train)
ytrain.hat <- predict(log.pca, train)

classification <- table(ytrain.hat,ytrain)
#Train error
1-sum(diag(classification))/sum(classification)


#Test error using first 2 PCAs
test <- xtest.standardized %*% xtrain.svd$v[,1:2]
test <- as.data.frame(test)
names(test) <- c('score1', 'score2')
y.test.hat <- predict(log.pca, test)
classification <- table(y.test.hat, ytest)
#Test error
1-sum(diag(classification))/sum(classification)

##Part f - OLS with FDA

y.indicator <- cbind(ytrain==0,ytrain==1,ytrain==2) * 1

fda.matrix <- cbind(rep(1,length(fda1)), fda1, fda2)
##Use same y-indicator matrix from before
Betas <- solve(t(fda.matrix) %*% fda.matrix) %*% t(fda.matrix)%*%y.indicator

fitted <- fda.matrix %*% Betas

##Find the max fitted value in each row and the category it corresponds to
##1st col == 0, 2nd col == 1, 3rd col == 2
y.train.hat <- rep(NA, nrow(fitted))
for(i in 1:nrow(fitted)){
  
  maxcol <- which.max((fitted[i,]))
  if(maxcol == 1){
    y.train.hat[i] <- 0
  } else if(maxcol == 2){
    y.train.hat[i] <- 1
  } else {
    y.train.hat[i] <- 2
  }
}
##Train errors
mean(ytrain != y.train.hat)

##Calculate yhat test values
xtest.fda <- xtest.standardized %*% Re(S.eig$vectors[,1:2])
test.fda.matrix <- cbind(rep(1,nrow(xtest.fda)), xtest.fda)
fitted.test <- test.fda.matrix %*% Betas

y.test.hat <- rep(NA, nrow(fitted.test))
for(i in 1:nrow(fitted.test)){
  
  maxcol <- which.max((fitted.test[i,]))
  if(maxcol == 1){
    y.test.hat[i] <- 0
  } else if(maxcol == 2){
    y.test.hat[i] <- 1
  } else {
    y.test.hat[i] <- 2
  }
}
##Test errors
mean(ytest != y.test.hat)

#LDA with FDA scores 

fda <- cbind(fda1, fda2)

n1 <- sum(ytrain == 0)
n2 <- sum(ytrain == 1)
n3 <- sum(ytrain == 2)
n <- n1+n2+n3
pi1 <- n1/n
pi2 <- n2/n
pi3 <- n3/n


mu1.hat <- apply(fda[ytrain==0,],2,mean)
mu2.hat <- apply(fda[ytrain==1,],2,mean)
mu3.hat <- apply(fda[ytrain==2,],2,mean)
##Estimate within class covariance
S.w <- (t(fda[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) %*% (fda[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) + 
          t(fda[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) %*% (fda[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) +
          t(fda[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)) %*% (fda[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)))/n-3

y.hat.train <- rep(NA, nrow(fda))
for(i in 1:nrow(xtrain.standardized)){
  discrim1 <- log(pi1) + t(fda[i,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
  discrim2 <- log(pi2) + t(fda[i,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
  discrim3 <- log(pi3) + t(fda[i,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
  maxdiscrim <- max(discrim1, discrim2, discrim3)
  if(maxdiscrim == discrim1){
    y.hat.train[i] <- 0
  } else if (maxdiscrim == discrim2){
    y.hat.train[i] <-1
  } else {
    y.hat.train[i] <- 2
  }
  
}

##Train errors
mean(ytrain != y.hat.train)

##Calculate max discriminant function and corresponding yhat for PC components witht xtest
fda.test <- xtest.standardized %*% Re(S.eig$vectors[,1:2])
y.hat.test <- rep(NA, nrow(fda.test))
for(i in 1:nrow(xtest.standardized)){
  discrim1 <- log(pi1) + t(fda.test[i,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
  discrim2 <- log(pi2) + t(fda.test[i,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
  discrim3 <- log(pi3) + t(fda.test[i,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
  maxdiscrim <- max(discrim1, discrim2, discrim3)
  if(maxdiscrim == discrim1){
    y.hat.test[i] <- 0
  } else if (maxdiscrim == discrim2){
    y.hat.test[i] <-1
  } else {
    y.hat.test[i] <- 2
  }
}
##Test errors
mean(ytest != y.hat.test)

#FDA with logistic regression

library(nnet)
train <- cbind(fda1, fda2, ytrain)
train <- as.data.frame(train)
names(train) <- c('score1', 'score2', 'ytrain')
log.fda <- multinom(ytrain~., data = train)
ytrain.hat <- predict(log.fda, train)

classification <- table(ytrain.hat,ytrain)
#Train error
1-sum(diag(classification))/sum(classification)


#Test error using first 2 FDAs
test <- xtest.standardized %*% Re(S.eig$vectors[,1:2])
test <- as.data.frame(test)
names(test) <- c('score1', 'score2')
y.test.hat <- predict(log.fda, test)
classification <- table(y.test.hat, ytest)
#Test error
1-sum(diag(classification))/sum(classification)


##Part h - CV for OLS Regression with PC Components
nfolds <- 5
ntrain <- nrow(xtrain.standardized)
s <- split(sample(ntrain), rep(1:nfolds, length=ntrain))
cv.errors.ols <- matrix(NA,nfolds,256)

for(j in 1:nfolds){
  x.svd <- svd(xtrain.standardized[-s[[j]],])
  scores <- xtrain.standardized[-s[[j]],] %*% x.svd$v
  y.indicator <- cbind(ytrain[-s[[j]],]==0,ytrain[-s[[j]],]==1,ytrain[-s[[j]],]==2) * 1
  
  for(i in 1:256){
    pc.matrix <- cbind(rep(1,nrow(scores)), scores[,1:i])
    Betas <- solve(t(pc.matrix) %*% pc.matrix) %*% t(pc.matrix)%*%y.indicator
    
    x.cv.svd <- svd(xtrain.standardized[s[[j]],])
    scores.cv <- xtrain.standardized[s[[j]],] %*% x.svd$v
    scores.cv.matrix <- cbind(rep(1, nrow(scores.cv)), scores.cv[,1:i])
    
    fitted.cv <- scores.cv.matrix %*% Betas
    
    y.hat.cv <- rep(NA, nrow(fitted.cv))
    
    for(k in 1:nrow(fitted.cv)){
      
      maxcol <- which.max((fitted.cv[k,]))
      if(maxcol == 1){
        y.hat.cv[k] <- 0
      } else if(maxcol == 2){
        y.hat.cv[k] <- 1
      } else {
        y.hat.cv[k] <- 2
      }
    }
    
    cv.errors.ols[j,i] <- mean(ytrain[s[[j]],] != y.hat.cv)
  }
}

mean.cv.errors.ols <- apply(cv.errors.ols,2,mean)
plot(mean.cv.errors.ols)
which.min(mean.cv.errors.ols)
mean.cv.errors.ols[45]

##OLS test error with 45 components. Get betas with training data
pc.matrix <- xtrain.standardized %*% xtrain.svd$v[,1:45]
pc.matrix <- cbind(rep(1,nrow(pc.matrix)), pc.matrix)

##Use same y-indicator matrix from before
Betas <- solve(t(pc.matrix) %*% pc.matrix) %*% t(pc.matrix)%*%y.indicator

##Get test errors now
xtest.pc <- xtest.standardized %*% xtrain.svd$v[,1:45]
test.pc.matrix <- cbind(rep(1,nrow(xtest.pc)), xtest.pc)
fitted.test <- test.pc.matrix %*% Betas

y.test.hat <- rep(NA, nrow(fitted.test))
for(i in 1:nrow(fitted.test)){
  
  maxcol <- which.max((fitted.test[i,]))
  if(maxcol == 1){
    y.test.hat[i] <- 0
  } else if(maxcol == 2){
    y.test.hat[i] <- 1
  } else {
    y.test.hat[i] <- 2
  }
}
##Test errors
mean(ytest != y.test.hat)



##Part I - Do CV with LDA and principal components

cv.errors <- matrix(NA,nfolds,256)

for( j in 1:nfolds){
  n1 <- sum(ytrain[-s[[j]],]==0)
  n2 <- sum(ytrain[-s[[j]],]==1)
  n3 <- sum(ytrain[-s[[j]],]==2)
  n <- n1+n2+n3
  pi1 <- n1/n
  pi2 <- n2/n
  pi3 <- n3/n
  
  ##Get principal component scores with jth fold excluded
  x.svd <- svd(xtrain.standardized[-s[[j]],])
  scores <- xtrain.standardized[-s[[j]],] %*% x.svd$v
  for(i in 1:256){
    pc <- as.matrix(scores[,1:i])
    ##Apply doesn't work on vectors, i.e when #of scores equal to 1
    if(i==1){
      mu1.hat <- mean(pc[ytrain[-s[[j]],]==0,])
      mu2.hat <- mean(pc[ytrain[-s[[j]],]==1,])
      mu3.hat <- mean(pc[ytrain[-s[[j]],]==2,])
    }
    else{
    mu1.hat <- apply(pc[ytrain[-s[[j]],]==0,],2,mean) 
    mu2.hat <- apply(pc[ytrain[-s[[j]],]==1,],2,mean)
    mu3.hat <- apply(pc[ytrain[-s[[j]],]==2,],2,mean)
    }
    
    #Calculate within class covariance with training data
    S.w <- (t(pc[ytrain[-s[[j]],]==0,] - rep(1,n1)%*% t(mu1.hat)) %*% (pc[ytrain[-s[[j]],]==0,] - rep(1,n1)%*% t(mu1.hat)) + 
              t(pc[ytrain[-s[[j]],]==1,] - rep(1,n2)%*% t(mu2.hat)) %*% (pc[ytrain[-s[[j]],]==1,] - rep(1,n2)%*% t(mu2.hat)) +
              t(pc[ytrain[-s[[j]],]==2,] - rep(1,n3)%*% t(mu3.hat)) %*% (pc[ytrain[-s[[j]],]==2,] - rep(1,n3)%*% t(mu3.hat)))/n-3
    
    
    #Get Principal component matrix from Validation Data
    val.svd <- svd(xtrain.standardized[s[[j]],])
    val.scores <- xtrain.standardized[s[[j]],] %*% x.svd$v
    
    ##Prinipal component
    pc.val <- as.matrix(val.scores[,1:i])
    y.hat.cv <- rep(NA, nrow(pc.val))
    
    ##Calculate discriminant functions and max discriminant for each observation in jth fold
    for(k in 1:nrow(pc.val)){
      discrim1 <- log(pi1) + t(pc.val[k,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
      discrim2 <- log(pi2) + t(pc.val[k,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
      discrim3 <- log(pi3) + t(pc.val[k,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
      maxdiscrim <- max(discrim1, discrim2, discrim3)
      if(maxdiscrim == discrim1){
        y.hat.cv[k] <- 0
      } else if (maxdiscrim == discrim2){
        y.hat.cv[k] <-1
      } else {
        y.hat.cv[k] <- 2
      }
    }
    cv.errors[j,i] <- mean(ytrain[s[[j]],] != y.hat.cv)
  }
}
mean.cv.errors <- apply(cv.errors,2,mean, na.rm=TRUE)
plot(mean.cv.errors)
which.min(mean.cv.errors)
mean.cv.errors[45]

##Get test errors for LDA with 45 principal components
##First have to calculate discriminant functions with xtrain data

pc <- xtrain.standardized %*% xtrain.svd$v[,1:45]

n1 <- sum(ytrain == 0)
n2 <- sum(ytrain == 1)
n3 <- sum(ytrain == 2)
n <- n1+n2+n3
pi1 <- n1/n
pi2 <- n2/n
pi3 <- n3/n


mu1.hat <- apply(pc[ytrain==0,],2,mean)
mu2.hat <- apply(pc[ytrain==1,],2,mean)
mu3.hat <- apply(pc[ytrain==2,],2,mean)
##Estimate within class covariance
S.w <- (t(pc[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) %*% (pc[ytrain==0,] - rep(1,n1)%*% t(mu1.hat)) + 
          t(pc[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) %*% (pc[ytrain==1,] - rep(1,n2)%*% t(mu2.hat)) +
          t(pc[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)) %*% (pc[ytrain==2,] - rep(1,n3)%*% t(mu3.hat)))/n-3


#Now calculate test errors with 45 PC
pc.test <- xtest.standardized %*% xtrain.svd$v[,1:45]
y.hat.test <- rep(NA, nrow(pc.test))
for(i in 1:nrow(xtest.standardized)){
  discrim1 <- log(pi1) + t(pc.test[i,])%*%solve(S.w)%*%mu1.hat - 1/2*(t(mu1.hat)%*%solve(S.w)%*%mu1.hat)
  discrim2 <- log(pi2) + t(pc.test[i,])%*%solve(S.w)%*%mu2.hat - 1/2*(t(mu2.hat)%*%solve(S.w)%*%mu2.hat)
  discrim3 <- log(pi3) + t(pc.test[i,])%*%solve(S.w)%*%mu3.hat - 1/2*(t(mu3.hat)%*%solve(S.w)%*%mu3.hat)
  maxdiscrim <- max(discrim1, discrim2, discrim3)
  if(maxdiscrim == discrim1){
    y.hat.test[i] <- 0
  } else if (maxdiscrim == discrim2){
    y.hat.test[i] <-1
  } else {
    y.hat.test[i] <- 2
  }
}
##Test errors
mean(ytest != y.hat.test)
