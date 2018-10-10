##Computer Homework
load("hw1.RData")

##Part a - OLS

#Convery to dataframes for lm function
train2.df<-as.data.frame(train2)
train3.df<-as.data.frame(train3)
test2.df<-as.data.frame(test2)
test3.df<-as.data.frame(test3)

#Add output columns and combine train2 and train3 
##into full training set

y2<-cbind(rep(2,731))
y3<-cbind(rep(3,658))
ytrain<-rbind(y2,y3)
train.full<-rbind(train2.df,train3.df)
train.full<-cbind(train.full,ytrain)

##Add output colums and combine test2 and test3
##into full training set
y2<-cbind(rep(2,198))
y3<-cbind(rep(3,166))
ytest<-rbind(y2,y3)
test.full<-rbind(test2.df,test3.df)


#Build OLS training model and get training and testing error 
lm.train<-lm(ytrain~.,data=train.full)
y.hat<-predict(lm.train)
train.error<-mean((y.hat-ytrain)^2)
train.error
test.full.matrix<-as.matrix(test.full)

y.test.hat<-cbind(1,test.full.matrix) %*% coef(lm.train)
test.resids<-(ytest-y.test.hat)
test.error<-sum((test.resids*test.resids)/length(ytest))
test.error


##NN section
library(class)
train.x<-rbind(train2.df,train3.df)
test.x<-rbind(test2.df,test3.df)
k.vec<-c(1,3,5,7,15)
train.error.knn<-rep(NA,5)
test.error.knn<-rep(NA,5)
for(i in 1:5){
  y.hat<-knn(train.x,rbind(train.x,test.x),ytrain,k=k.vec[i],prob=FALSE)
  train.error.knn[i] <- mean(y.hat[1:1389] != ytrain)
  test.error.knn[i] <- mean(y.hat[1390:1753]!=ytest)
  print(i)
}
par(mfrow=c(1,2))
plot(k.vec,train.error.knn)
plot(k.vec,test.error.knn)
train.error.knn
test.error.knn

##Part B
library(leaps)
regfit.full<-regsubsets(ytrain~.,train.full)
##You can't do this because the dataset is too large
##and checking 2^256 models would take prohibitively long

##Part C - this won't work. leads R to crash
regfit.full<-regsubsets(ytrain~.,train.full,really.big=TRUE)


##Part D
regfit.full<-regsubsets(ytrain~.,train.full,nvmax=3,really.big=TRUE)
summary(regfit.full)
#Pixels 104,166,249 in 3 variable model

##Part E - you can't run forward without specifying the method
regfit.full<-regsubsets(ytrain~.,train.full,nvmax=9,really.big=TRUE)

##Part F
regfit.forward<-regsubsets(ytrain~.,train.full,nvmax=256,method="forward")
coef(regfit.forward,3)
#Pixels 165,168, 249 in 3 variable model
coef(regfit.forward,9)
#Pixels 104,122,124,148,165,168,183,200,249
regfit.fwd.summary<-summary(regfit.forward)

##Part G
regfit.backward<-regsubsets(ytrain~.,train.full,nvmax=256,method="backward")
coef(regfit.backward,3)
regfit.bwd.summary<-summary(regfit.backward)
#Pixels 104,166,249 with 3 predictors.
#Same results as part D but different from Part %. Only pixel 249 is shared

##Part H
par(mfrow=c(2,2))
plot(regfit.fwd.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
points(which.min(regfit.fwd.summary$rss),regfit.fwd.summary$rss[which.min(regfit.fwd.summary$rss)],col="red",cex=2,pch=20)

plot(regfit.fwd.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(regfit.fwd.summary$adjr2),regfit.fwd.summary$adjr2[which.max(regfit.fwd.summary$adjr2)],col="red",cex=2,pch=20)

plot(regfit.fwd.summary$cp,xlab="Number of Variables",ylab="CP",type="l")
points(which.min(regfit.fwd.summary$cp),regfit.fwd.summary$cp[which.min(regfit.fwd.summary$cp)],col="red",cex=2,pch=20)

plot(regfit.fwd.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(regfit.fwd.summary$bic),regfit.fwd.summary$bic[which.min(regfit.fwd.summary$bic)],col="red",cex=2,pch=20)

which.min(regfit.fwd.summary$rss)
which.max(regfit.fwd.summary$adjr2)
which.min(regfit.fwd.summary$cp)
which.min(regfit.fwd.summary$bic)

##Part I

par(mfrow=c(2,2))
plot(regfit.bwd.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
points(which.min(regfit.bwd.summary$rss),regfit.bwd.summary$rss[which.min(regfit.bwd.summary$rss)],col="red",cex=2,pch=20)

plot(regfit.bwd.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(regfit.bwd.summary$adjr2),regfit.bwd.summary$adjr2[which.max(regfit.bwd.summary$adjr2)],col="red",cex=2,pch=20)

plot(regfit.bwd.summary$cp,xlab="Number of Variables",ylab="CP",type="l")
points(which.min(regfit.bwd.summary$cp),regfit.bwd.summary$cp[which.min(regfit.bwd.summary$cp)],col="red",cex=2,pch=20)

plot(regfit.bwd.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(regfit.bwd.summary$bic),regfit.bwd.summary$bic[which.min(regfit.bwd.summary$bic)],col="red",cex=2,pch=20)

which.min(regfit.bwd.summary$rss)
which.max(regfit.bwd.summary$adjr2)
which.min(regfit.bwd.summary$cp)
which.min(regfit.bwd.summary$bic)

##Part J
##Calculating the train error term for Forward CP model
coef.cp<-coef(regfit.forward,75)
coef.cp<-as.matrix(coef.cp)
train.full.cp<-train.full[,c(1,12,13,18,27,30,33,37,40,48,57,58,59,79,88,98,99,101,103,104,105,106,107,109,114,115,121,122,123,124,126,129,131,135,138,148,149,151,153,155,156,157,159,161,165,166,168,172,175,176,178,179,181,183,184,196,199,200,201,202,203,205,211,213,217,218,224,226,228,231,245,247,248,249,253)]
train.full.cp<-as.matrix(cbind(1,train.full.cp))
dim(train.full.cp)
dim(coef.cp)
y.hat.cp<-train.full.cp %*% coef.cp
cp.train.resids<-ytrain-y.hat.cp
cp.train.errors<-sum(cp.train.resids*cp.train.resids)/length(ytrain)
cp.train.errors

##Calculating test error term for Forward CP model
test.full.cp<-test.full[,c(1,12,13,18,27,30,33,37,40,48,57,58,59,79,88,98,99,101,103,104,105,106,107,109,114,115,121,122,123,124,126,129,131,135,138,148,149,151,153,155,156,157,159,161,165,166,168,172,175,176,178,179,181,183,184,196,199,200,201,202,203,205,211,213,217,218,224,226,228,231,245,247,248,249,253)]
test.full.cp<-as.matrix(cbind(1,test.full.cp))
y.test.cp<- test.full.cp %*% coef.cp
cp.test.resids<-ytest-y.test.cp
cp.test.errors<-sum(cp.test.resids*cp.test.resids)/length(ytest)
cp.test.errors

##Calculating the train error term for Forward BIC model
coef.bic<-coef(regfit.forward,34)
coef.bic
coef.bic<-as.matrix(coef.bic)
train.full.bic<-train.full[,c(27,33,37,40,79,88,99,104,105,107,122,124,126,129,131,138,148,151,156,165,168,175,179,183,196,200,203,205,211,213,217,226,231,249)]
train.full.bic<-as.matrix(cbind(1,train.full.bic))
y.hat.bic<-train.full.bic %*% coef.bic
bic.train.resids <- ytrain - y.hat.bic
bic.train.errors<-sum(bic.train.resids*bic.train.resids)/length(ytrain)
bic.train.errors

##Calculating the test error term for the Forward BIC model
test.full.bic<-test.full[,c(27,33,37,40,79,88,99,104,105,107,122,124,126,129,131,138,148,151,156,165,168,175,179,183,196,200,203,205,211,213,217,226,231,249)]
test.full.bic<-as.matrix(cbind(1,test.full.bic))
y.test.bic <- test.full.bic %*% coef.bic
bic.test.resids <- ytest - y.test.bic
bic.test.errors <- sum(bic.test.resids*bic.test.resids)/length(ytest)
bic.test.errors

##Part K
##Calculating the train errors for the Backwards CP Model
coef.bwd.cp<-coef(regfit.backward,89)
coef.bwd.cp
coef.bwd.cp <-as.matrix(coef.bwd.cp)
train.full.cp.b<-train.full[,c(1,2,12,18,23,24,27,31,32,34,35,37,41,56,58,59,64,72,73,76,78,79,80,86,88,93,98,100,102,103,104,105,106,107,109,113,114,117,121,123,124,126,128,129,131,135,138,144,148,149,153,156,157,159,161,165,166,168,170,172,173,174,176,178,180,183,186,197,198,199,201,202,205,208,211,213,217,218,224,226,231,238,239,245,247,248,249,250,253)]
train.full.cp.b<-as.matrix(cbind(1,train.full.cp.b))
y.hat.cp.b <- train.full.cp.b %*% coef.bwd.cp
cp.train.resids.b <- ytrain - y.hat.cp.b
cp.train.errors.b <- sum(cp.train.resids.b * cp.train.resids.b)/length(ytrain)
cp.train.errors.b

##Calculating the test errors for the Backwards CP Model
test.full.cp.b <- test.full[,c(1,2,12,18,23,24,27,31,32,34,35,37,41,56,58,59,64,72,73,76,78,79,80,86,88,93,98,100,102,103,104,105,106,107,109,113,114,117,121,123,124,126,128,129,131,135,138,144,148,149,153,156,157,159,161,165,166,168,170,172,173,174,176,178,180,183,186,197,198,199,201,202,205,208,211,213,217,218,224,226,231,238,239,245,247,248,249,250,253)]
test.full.cp.b <-as.matrix(cbind(1,test.full.cp.b))
y.test.cp.b <- test.full.cp.b %*% coef.bwd.cp
cp.test.resids.b <- ytest - y.test.cp.b
cp.test.errors.b <- sum(cp.test.resids.b * cp.test.resids.b)/length(ytest)

##Calculating the train errors for the Backwards BIC Model
coef.bwd.bic <- coef(regfit.backward,38)
coef.bwd.bic
coef.bwd.bic <- as.matrix(coef.bwd.bic)
train.full.bic.b <- train.full[,c(12,18,27,37,58,59,73,79,98,104,106,107,109,121,123,124,126,129,131,135,138,148,156,159,165,166,168,180,183,197,201,202,205,211,217,231,238,249)]
train.full.bic.b <- as.matrix(cbind(1,train.full.bic.b))
y.hat.bic.b <- train.full.bic.b %*% coef.bwd.bic
bic.train.resids.b <- ytrain - y.hat.bic.b
bic.train.errors.b <- sum(bic.train.resids.b * bic.train.resids.b)/length(ytrain)
bic.train.errors.b

##Calculating the test errors for the Backwards BIC Model
test.full.bic.b <-test.full[,c(12,18,27,37,58,59,73,79,98,104,106,107,109,121,123,124,126,129,131,135,138,148,156,159,165,166,168,180,183,197,201,202,205,211,217,231,238,249)]
test.full.bic.b <-as.matrix(cbind(1,test.full.bic.b))
y.test.bic.b <- test.full.bic.b %*% coef.bwd.bic
bic.test.resids.b <- ytest - y.test.bic.b
bic.test.errors.b <- sum(bic.test.resids.b * bic.test.resids.b)/length(ytest)
bic.test.errors.b

##The Forward BIC model has the smallest training error. The Forward CP Model has the smallest test error.


##Part L
library(glmnet)
lambda.grid <- 10^seq(4,-3,length=100)
xtrain<-as.matrix(train.full[,1:256])
ridge.mod <- glmnet(xtrain,ytrain,alpha=0,lambda=lambda.grid)
coeff.matrix<-coef(ridge.mod)
dim(coeff.matrix)
coeff.matrix

#Plot coefficients with all included
par(mfrow=c(1,2))
plot(coeff.matrix[2,],ylim=c(min(coeff.matrix[-1,]),max(coeff.matrix[-1,])),col=2,type="o",ylab="coefficients")
for(i in 2:257){ 
  lines(coeff.matrix[i,],col=i,type="o") 
}

#Plot coefficients with V16 excluded due to it pulling up the graph
plot(coeff.matrix[2,],ylim=c(-1,1),col=2,type="o",ylab="coefficients")
for(i in 2:257){ 
  if(i == 17){
    next;
  }
  lines(coeff.matrix[i,],col=i,type="o") 
}

##Part M - Compute coefficients, training and testing error using OLS
ridge.ols <- glmnet(xtrain,ytrain,alpha=0,lambda=0)
coef.ols<-as.matrix(coef(ridge.ols))
y.hat.ols <- as.matrix(cbind(1,xtrain)) %*% coef.ols
ols.train.resids<- ytrain-y.hat.ols
ols.train.error <- sum(ols.train.resids * ols.train.resids)/length(ytrain)
ols.train.error

xtest <- as.matrix(test.full)
ridge.ols.test<-glmnet(xtest,ytest,alpha=0,lambda=0)
coef.ols.test <- as.matrix(coef(ridge.ols.test))
y.hat.ols.test <- as.matrix(cbind(1,xtest)) %*% coef.ols.test
ols.test.resids<- ytest-y.hat.ols.test
ols.test.error <- sum(ols.test.resids * ols.test.resids)/length(ytest)
ols.test.error

##Part N - Get OLS coefficients from lm function
##Not the same as ridge lambda=0, because glmnet standardizes
##the inputs before calculating.

ols<-lm(ytrain~xtrain)
coef(ols)
#Plot Ridge OLS against LM OLS
par(mfrow=c(1,1))
plot(coef(ols),ylim=c(-.5,.5),col="blue",ylab="OLS Coefs vs Ridge Coefs")
points(coef(ridge.ols), col = "red")


##Part O
##Training errors and Test errors for Ridge

train.error <-rep(NA,100)
test.error <-rep(NA,100)
for(i in 1:100){
  ridge.train.predict<-predict(ridge.mod,s=lambda.grid[i],newx=xtrain)
  train.error[i] <- mean((ridge.train.predict-ytrain)^2)
  ridge.test.predict<-predict(ridge.mod,s=lambda.grid[i],newx=xtest)
  test.error[i] <- mean((ridge.test.predict-ytest)^2)
}

par(mfrow=c(1,2))
plot(log(lambda.grid),train.error)
plot(log(lambda.grid),test.error)

##Lambda that gives smallest test error
which.min(test.error)
lambda.grid[52]
test.error[52]

##Lambda that gives the smallest train error
which.min(train.error)
lambda.grid[100]
train.error[100]


##Part P
lambda.grid = 10^seq(0,-5,length=100)
lasso.mod <- glmnet(xtrain,ytrain,alpha=1,lambda=lambda.grid)
coeff.lasso<-coef(lasso.mod)

#Plot coefficients with all included

plot(coeff.lasso[2,],ylim=c(min(coeff.lasso[-1,]),max(coeff.lasso[-1,])),col=2,type="o",ylab="coefficients")
for(i in 2:257){ 
  lines(coeff.lasso[i,],col=i,type="o") 
}

#Plot coefficients with V16 excluded due to it pulling up the graph
plot(coeff.matrix[2,],ylim=c(-1,1),col=2,type="o",ylab="coefficients")
for(i in 2:257){ 
  if(i == 17){
    next;
  }
  lines(coeff.matrix[i,],col=i,type="o") 
}

##Part Q

lasso.ols <- glmnet(xtrain,ytrain,alpha=1,lambda=0)
coef.lasso.ols<-as.matrix(coef(lasso.ols))
y.hat.lasso.ols <- as.matrix(cbind(1,xtrain)) %*% coef.lasso.ols
lasso.ols.train.resids<- ytrain-y.hat.lasso.ols
lasso.ols.train.error <- sum(lasso.ols.train.resids * lasso.ols.train.resids)/length(ytrain)
lasso.ols.train.error

xtest <- as.matrix(test.full)
lasso.ols.test<-glmnet(xtest,ytest,alpha=1,lambda=0)
coef.lasso.ols.test <- as.matrix(coef(lasso.ols.test))
y.hat.lasso.ols.test <- as.matrix(cbind(1,xtest)) %*% coef.lasso.ols.test
lasso.ols.test.resids<- ytest-y.hat.lasso.ols.test
lasso.ols.test.error <- sum(lasso.ols.test.resids * lasso.ols.test.resids)/length(ytest)
lasso.ols.test.error

##Part R

par(mfrow=c(1,1))
plot(coef(ols),ylim=c(-.5,.5),col="blue",ylab="OLS Coefs vs Lasso Coefs")
points(coef(lasso.ols), col = "red")
##Not the same because lasso also standardizes the inputs

##Part S

train.error <-rep(NA,100)
test.error <-rep(NA,100)
for(i in 1:100){
  lasso.train.predict<-predict(lasso.mod,s=lambda.grid[i],newx=xtrain)
  train.error[i] <- mean((lasso.train.predict-ytrain)^2)
  lasso.test.predict<-predict(lasso.mod,s=lambda.grid[i],newx=xtest)
  test.error[i] <- mean((lasso.test.predict-ytest)^2)
}

par(mfrow=c(1,2))
plot(log(lambda.grid),train.error)
plot(log(lambda.grid),test.error)

##Lambda that gives smallest test error
which.min(test.error)
lambda.grid[93]
test.error[93]

##Lambda that gives the smallest train error
which.min(train.error)
lambda.grid[100]
train.error[100]

