library(tidyverse)

#Read raw data in
cifar_raw <- read_csv("C:\\Users\\George\\Documents\\Rutgers\\Data Mining\\Project\\cifar_10.csv")

#Split into train and test data. Set seed so dataset can be reproduced
sample_size <- .75 * nrow(cifar_raw)
set.seed(123)
train_ind <- sample(seq_len(nrow(cifar_raw)), size = sample_size)

train <- cifar_raw[train_ind, ]
test <- cifar_raw[-train_ind,]



#Order by y so we can plot PCA more easily
train <- train %>% 
  arrange(y)  


#Split into train1...train10 for FDA later
train1 <- train %>% 
  filter(y == '1') %>% 
  select(-y)
train2 <- train %>% 
  filter(y == '2') %>% 
  select(-y)
train3 <- train %>% 
  filter(y == '3') %>% 
  select(-y)
train4 <- train %>% 
  filter(y == '4') %>% 
  select(-y)
train5 <- train %>% 
  filter(y == '5') %>% 
  select(-y)
train6 <- train %>% 
  filter(y == '6') %>% 
  select(-y)
train7 <- train %>% 
  filter(y == '7') %>% 
  select(-y)
train8 <- train %>% 
  filter(y == '8') %>% 
  select(-y)
train9 <- train %>% 
  filter(y == '9') %>% 
  select(-y)
train10 <- train %>% 
  filter(y == '10') %>% 
  select(-y)

#Number of images for each class in order 1..10
number.per.class <- c(4560,4472,4516,4485,4489,4569,4499,4451,4440,4519)

#Split train and test datasets into xtrain, ytrain, xtest and ytest datasets
xtrain <- train %>% 
  select(-y)

ytrain <- train %>% 
  select(y)

xtest <- test %>% 
  select(-y)
#Standardize xtest
xtest.standardized <- xtest - rep(colMeans(xtrain), nrow(xtest), ncol(xtest))

ytest <- test %>% 
  select(y)

#Principal Components of xtrain
#Center the dataset for PCA

xtrain.standardized <- scale(xtrain, scale = FALSE)
xtrain.svd <- svd(xtrain.standardized)
pca.scores <- xtrain.standardized %*% xtrain.svd$v

#Plot PCA scores to see if there is any clustering
plot(pca.scores[,1], pca.scores[,2], col = rep(2:11,rep(number.per.class)), xlab = "Score 1", ylab = "Score 2", main = "Principal Component Analysis", pch = 19)
plot(pca.scores[,2], pca.scores[,3], col = rep(2:11,rep(number.per.class)), xlab = "Score 2", ylab = "Score 3", main = "Principal Component Analysis", pch = 19)
plot(pca.scores[,3], pca.scores[,4], col = rep(2:11,rep(number.per.class)), xlab = "Score 3", ylab = "Score 4", main = "Principal Component Analysis", pch = 19)

#Plot singular values to see how many principal components to think about using
plot(xtrain.svd$d, ylab = "Singular Values", main = "PCA Singular Values")
abline(v=50)

#FDA 

mu.hat <- apply(xtrain, 2 , mean)
mu1.hat <- apply(train1, 2, mean)
mu2.hat <- apply(train2, 2, mean)
mu3.hat <- apply(train3, 2, mean)
mu4.hat <- apply(train4, 2, mean)
mu5.hat <- apply(train5, 2, mean)
mu6.hat <- apply(train6, 2, mean)
mu7.hat <- apply(train7, 2, mean)
mu8.hat <- apply(train8, 2, mean)
mu9.hat <- apply(train9, 2, mean)
mu10.hat <- apply(train10, 2, mean)

n <- nrow(xtrain)

S.b <- ((nrow(train1)) * (mu1.hat - mu.hat) %*% t(mu1.hat - mu.hat) +
          (nrow(train2)) * (mu2.hat - mu.hat) %*% t(mu2.hat - mu.hat) +
          (nrow(train3))* (mu3.hat - mu.hat) %*% t(mu3.hat - mu.hat) +
          (nrow(train4))* (mu4.hat - mu.hat) %*% t(mu4.hat - mu.hat) +
          (nrow(train5))* (mu5.hat - mu.hat) %*% t(mu5.hat - mu.hat) +
          (nrow(train6))* (mu6.hat - mu.hat) %*% t(mu6.hat - mu.hat) +
          (nrow(train7))* (mu7.hat - mu.hat) %*% t(mu7.hat - mu.hat) +
          (nrow(train8))* (mu8.hat - mu.hat) %*% t(mu8.hat - mu.hat) +
          (nrow(train9))* (mu9.hat - mu.hat) %*% t(mu9.hat - mu.hat) +
          (nrow(train10))* (mu10.hat - mu.hat) %*% t(mu10.hat - mu.hat))/(n-1)

S.w <- (t(as.matrix(train1) - rep(1,nrow(train1))%*%t(mu1.hat)) %*% (as.matrix(train1) - rep(1,nrow(train1)) %*% t(mu1.hat)) + 
          t(as.matrix(train2) - rep(1,nrow(train2)) %*% t(mu2.hat)) %*% (as.matrix(train2) - rep(1,nrow(train2)) %*% t(mu2.hat)) +
          t(as.matrix(train3) - rep(1,nrow(train3)) %*% t(mu3.hat)) %*% (as.matrix(train3) - rep(1,nrow(train3)) %*% t(mu3.hat)) +
          t(as.matrix(train4) - rep(1,nrow(train4)) %*% t(mu4.hat)) %*% (as.matrix(train4) - rep(1,nrow(train4)) %*% t(mu4.hat)) +
          t(as.matrix(train5) - rep(1,nrow(train5)) %*% t(mu5.hat)) %*% (as.matrix(train5) - rep(1,nrow(train5)) %*% t(mu5.hat)) +
          t(as.matrix(train6) - rep(1,nrow(train6)) %*% t(mu6.hat)) %*% (as.matrix(train6) - rep(1,nrow(train6)) %*% t(mu6.hat)) +
          t(as.matrix(train7) - rep(1,nrow(train7)) %*% t(mu7.hat)) %*% (as.matrix(train7) - rep(1,nrow(train7)) %*% t(mu7.hat)) +
          t(as.matrix(train8) - rep(1,nrow(train8)) %*% t(mu8.hat)) %*% (as.matrix(train8) - rep(1,nrow(train8)) %*% t(mu8.hat)) +
          t(as.matrix(train9) - rep(1,nrow(train9)) %*% t(mu9.hat)) %*% (as.matrix(train9) - rep(1,nrow(train9)) %*% t(mu9.hat)) +
          t(as.matrix(train10) - rep(1,nrow(train10)) %*% t(mu10.hat)) %*% (as.matrix(train10) - rep(1,nrow(train10)) %*% t(mu10.hat)))/ (n-10)

S <- solve(S.w) %*% S.b
S.eig <- eigen(S)

fda.scores <- as.matrix(xtrain) %*% Re(S.eig$vectors)

#Plot FDA scores to see if there is any clustering
plot(fda.scores[,1], fda.scores[,2], col = rep(2:11,rep(number.per.class)), xlab = "Score 1", ylab = "Score 2", main = "Fischer Discriminant Analysis Components", pch = 19)
plot(fda.scores[,2], fda.scores[,3], col = rep(2:11,rep(number.per.class)), xlab = "Score 2", ylab = "Score 3", main = "Fischer Discriminant Analysis Components", pch = 19)
plot(fda.scores[,3], fda.scores[,4], col = rep(2:11,rep(number.per.class)), xlab = "Score 3", ylab = "Score 4", main = "Fischer Discriminant Analysis Components", pch = 19)

#Plot eigenvalues to see how many FDA scores to keep
plot(Re(S.eig$values), ylab = "Eigenvalues", main = "FDA Eigenvalues")


#KNN with PCA Components

library(kknn)

#Put first 50 principal components into a matrix
pca.matrix <- pca.scores[,1:50]


#Combine the pca matrix with the ytrain matrix to make a full training matrix
pca.matrix <- data.frame(x=pca.matrix, y=ytrain.factor)

#Create an pca test matrix to use later to find test error
pca.test.matrix <- as.matrix(xtest.standardized) %*% xtrain.svd$v[,1:50]
pca.test.matrix <- data.frame(x=pca.test.matrix)

#Create 5 folds for splitting training matrix
n.train <- nrow(xtrain)
nfolds <- 5
set.seed(123)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))

#Create matrix to hold CV errors for each k
klist <- seq(1,100,1)
knn.pca.cv <- matrix(NA, nfolds, length(klist))

#Loop to fill the CV matrix 
for (i in seq(nfolds)) { 
  #To know what fold we're on
  print(i)
  for(j in klist){
    knn.fit <- kknn(formula = y ~., train = pca.matrix[-s[[i]],], pca.matrix[s[[i]],], k = j, kernel = "rectangular")
    #Round fitted values so we can get misclassification rate and not MSE
    fitted <- fitted(knn.fit)
    #Find error by comparing fitted values with y-column on the validation set
    error <- mean(fitted != pca.matrix[s[[i]], 51])
    knn.pca.cv[i,j] <- error
  }
}

mean.knn.pca.cv <- apply(knn.pca.cv, 2, mean)
plot(mean.knn.pca.cv, xlab = "Neighbors", ylab = "Misclassification Rate", main = "CV error for Regular KNN with 50 Principal Components", type = "l", col = "red")

which.min(mean.knn.pca.cv)

#Loop to fill in CV matrix using E-kernel smoothing
knn.pca.e.cv <- matrix(NA, nfolds, length(klist))

for (i in seq(nfolds)) { 
  #To know what fold we're on
  print(i)
  for(j in klist){
    knn.fit <- kknn(formula = y ~., train = pca.matrix[-s[[i]],], pca.matrix[s[[i]],], k = j, kernel = "epanechnikov")
    #Round fitted values so we can get misclassification rate and not MSE
    fitted <- fitted(knn.fit)
    #Find error by comparing fitted values with y-column on the validation set
    error <- mean(fitted != pca.matrix[s[[i]], 51])
    knn.pca.e.cv[i,j] <- error
  }
}

mean.knn.pca.e.cv <- apply(knn.pca.e.cv, 2, mean)
plot(mean.knn.pca.e.cv, xlab = "Neighbors", ylab = "Misclassification Rate", main = "CV error for E-Kernel KNN")


#Plot both KNN models on some graph
plot(mean.knn.pca.cv, xlab = "Neighbors", ylab = "Misclassification Rate", main = "CV error for Regular KNN with 50 Principal Components", type = "l", col = "red", ylim=c(.65,.75))
lines(mean.knn.pca.e.cv, col = "blue")
legend("bottomright", legend = c("Unweighted", "E-Kernel"), col = c("red", "blue"), lty = c(1,1))

#Get testing errors for rectangular, e-kernel 

#Find the k with the lowest cross validated error
which.min(mean.knn.pca.cv)
which.min(mean.knn.pca.e.cv)

knn.fit <- kknn(formula = y ~., train = pca.matrix, test = pca.test.matrix[,1:50], k=3, kernel = "rectangular")
fitted <- fitted(knn.fit)
knn.test.error <- mean(fitted != ytest.factor)


knn.e.fit <- kknn(formula = y ~., train = pca.matrix, test = pca.test.matrix[,1:50], k=16, kernel = "epanechnikov")
knn.e.fitted <- fitted(knn.e.fit)
knn.e.test.error <- mean(knn.e.fitted != ytest.factor)

knn.test.error
knn.e.test.error

#LDA CV with PCA Components - Only first 300 components used in CV

library(MASS)

#Same split as from other analyses - already in environment. Just here for book-keeping
n.train <- nrow(xtrain)
nfolds <- 5
set.seed(123)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))

#Matrix to hold CV errors
lda.pca.cv <- matrix(NA, 5, 300)

for (i in seq(nfolds)) { 
  #To know what fold we're in
  print(i)
  #Calculate PCA scores excluding i-th fold
  x.svd <- svd(xtrain.standardized[-s[[i]],])
  scores <- xtrain.standardized[-s[[i]],] %*% x.svd$v
  
  #Get y-train excluding i-th fold
  y <- ytrain[-s[[i]],]
  
  #Get Principal component scores for CV error calculation
  valid.scores <- xtrain.standardized[s[[i]],] %*% x.svd$v
  
  #Get y-valid with i-th fold
  y.valid <- ytrain[s[[i]],]
  
  for(j in 1:300){
    pc <- as.matrix(scores[,1:j])
    #combine scores and y-train vector to input into LDA formula
    pca.matrix <- cbind(pc, y)
    #Compute LDA 
    pca.lda <- lda(y~., data = as.data.frame(pca.matrix))
    
    #Get test predictions with validation fold
    pca.valid <- as.matrix(valid.scores[,1:j])
    pca.lda.y.hat <- predict(pca.lda, as.data.frame(pca.valid))$class
    
    #Get test error
    error <- mean(pca.lda.y.hat != y.valid)
    
    lda.pca.cv[i,j] <- error
  }
}

mean.lda.pca.cv <- apply(lda.pca.cv, 2, mean)
sd.lda.pca.cv <- apply(lda.pca.cv, 2 , sd)

#Plot the mean CV error for LDA
plot(mean.lda.pca.cv, xlab = "# of Scores", ylab = "Misclassification Rate", main = "CV Error for LDA with First 300 Principal Components", col = "black", pch = 19)
lines(mean.lda.pca.cv+sd.lda.pca.cv, col = "red", lty = 2)
lines(mean.lda.pca.cv-sd.lda.pca.cv, col = "red", lty = 2)

#Zoomed in Plot for first 100 Components so we can more easily make a 1-Standard error rule judgement
plot(mean.lda.pca.cv[1:100], xlab = "# of Scores", ylab = "Misclassification Rate", main = "CV Error for LDA with First 100 Principal Components", col = "black", pch = 19)
lines(mean.lda.pca.cv[1:100]+sd.lda.pca.cv[1:100], col = "red", lty = 2)
lines(mean.lda.pca.cv[1:100]-sd.lda.pca.cv[1:100], col = "red", lty = 2)
abline(v = 77, lty = 3)
abline(h = mean.lda.pca.cv[77] + sd.lda.pca.cv[77], lty = 3)
abline(h = mean.lda.pca.cv[77] - sd.lda.pca.cv[77], lty = 3)
#Choose 37 Principal Components according to 1-Standard Error Rule
points(37, mean.lda.pca.cv[37], col = "green" , pch = 15, cex = 3 )
legend("topright", legend = c("Mean Error", "Standard Error"), col = c("black","red"), pch = c(19,NA), lty = c(NA,2))

#Compute LDA train & test error with first 37 Principal Components

#Get first 37 scores
pca.37 <- pca.scores[,1:37]
pca.37 <- cbind(pca.37, ytrain)

#Do LDA
pca.37.lda <- lda(y~., data = pca.37)
#Get training errors
pca.37.train.preds <- as.integer(predict(pca.37.lda, pca.37[,1:37])$class)
pca.37.train.error <- mean(pca.37.train.preds != ytrain)

#Get test errors
pca.test.37 <- as.matrix(xtest.standardized) %*% xtrain.svd$v[,1:37]
pca.test.37 <- cbind(pca.test.37, ytest)

pca.37.test.preds <- as.integer(predict(pca.37.lda, pca.test.37[,1:37])$class)
pca.37.test.error <- mean(pca.37.test.preds != ytest)

pca.37.train.error
pca.37.test.error

#Logistic Regression CV with PCA Components - Only first 300 Components

library(glmnet)

#Same split as from other analyses - already in environment. Just here for book-keeping
n.train <- nrow(xtrain)
nfolds <- 5
set.seed(123)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))

#Matrix to hold CV errors for logistic regression
log.pca.cv <- matrix(NA,5,300)

for (i in seq(nfolds)) { 
  #To know what fold we're in
  print(i)
  #Calculate PCA scores excluding i-th fold
  x.svd <- svd(xtrain.standardized[-s[[i]],])
  scores <- xtrain.standardized[-s[[i]],] %*% x.svd$v
  
  #Get y-train excluding i-th fold
  y <- ytrain[-s[[i]],]
  
  #Get Principal component scores for CV error calculation
  valid.scores <- xtrain.standardized[s[[i]],] %*% x.svd$v
  
  #Get y-valid with i-th fold
  y.valid <- ytrain[s[[i]],]
  
  for(j in 2:300){
    pc <- as.matrix(scores[,1:j])
    #Compute Log 
    pca.log <- glmnet(pc, as.matrix(y), family = "multinomial", lambda = 0)
    
    #Get test predictions with validation fold
    pca.valid <- as.matrix(valid.scores[,1:j])
    pca.log.y.hat <- predict(pca.log, pca.valid, type = "class")
    
    #Get test error
    error <- mean(pca.log.y.hat != y.valid)
    
    log.pca.cv[i,j] <- error
  }
}

#Remove 1st column on NA's which is where the logistic regression with one PC score would have gone
#Now we can use apply since there are no NAs

log.pca.cv <- log.pca.cv[,2:300]

#Get mean and Standard deviation of CV Errors
mean.log.pca.cv <- apply(log.pca.cv, 2, mean)
sd.log.pca.cv <- apply(log.pca.cv, 2, sd)

#Plot CV errors with standard deviation
plot(mean.log.pca.cv, xlab = "# of Scores", ylab = "Misclassification Rate", main = "CV Error for Logistic Regression with First 300 Principal Components", col = "black", pch = 19, ylim = c(.68,.82))
lines(mean.log.pca.cv+sd.log.pca.cv, col = "red", lty = 2)
lines(mean.log.pca.cv-sd.log.pca.cv, col = "red", lty = 2)

#Find Minimum Point
which.min(mean.log.pca.cv) ##Occurs at 109


#Zoomed in Plot for first 125 Components so we can more easily make a 1-Standard error rule judgement
plot(mean.log.pca.cv[1:125], xlab = "# of Scores", ylab = "Misclassification Rate", main = "CV Error for Logistic Regression with First 125 Principal Components", col = "black", pch = 19, ylim = c(.69,.82))
lines(mean.log.pca.cv[1:125]+sd.log.pca.cv[1:125], col = "red", lty = 2)
lines(mean.log.pca.cv[1:125]-sd.log.pca.cv[1:125], col = "red", lty = 2)
abline(v = 109, lty = 3)
abline(h = mean.log.pca.cv[109] + sd.log.pca.cv[109], lty = 3)
abline(h = mean.log.pca.cv[109] - sd.log.pca.cv[109], lty = 3)

#Choose 21 Principal Components according to 1-Standard Error Rule
points(21, mean.log.pca.cv[21], col = "green" , pch = 15, cex = 3 )
legend("topright", legend = c("Mean Error", "Standard Error"), col = c("black","red"), pch = c(19,NA), lty = c(NA,2))

#Get training and test errors with first 21 principal components

#Make principal component matrix
pca.21 <- pca.scores[,1:21]

#Fit a logistic regression with it
pca.21.log <- glmnet(pca.21, as.matrix(ytrain), family = "multinomial", lambda = 0)

#Get training errors
pca.21.train.preds <- predict(pca.21.log, pca.21, type = "class")
pca.21.train.error <- mean(pca.21.train.preds != ytrain)

#Get test errors
pca.21.test <- as.matrix(xtest.standardized) %*% xtrain.svd$v[,1:21]

pca.21.test.preds <- predict(pca.21.log, pca.21.test, type = "class")
pca.21.test.error <- mean(pca.21.test.preds != ytest)

pca.21.train.error
pca.21.test.error

#Penalized LDA with PCA Components

library(penalizedLDA)

lambdas <- c(.0001,.001,.01,.1,1,10)

#Do the cross validation with first 50 PCA components
plda.pca.cv <- PenalizedLDA.cv(as.matrix(pca.matrix[,1:50]), as.factor(unlist(ytrain)), lambdas = lambdas, nfold = 5, type = "standard")

#Training fit
plda.pca.fit <- PenalizedLDA(as.matrix(pca.matrix[,1:50]), 
                             as.factor(unlist(ytrain)), 
                             xte = as.matrix(pca.matrix[,1:50]), 
                             type = "standard",
                             lambda = plda.pca.cv$bestlambda,
                             K = plda.pca.cv$bestK)

#Get training predictions
plda.pca.train.preds <- plda.pca.fit$ypred[,plda.pca.fit$K]

#Get training errors
plda.pca.train.error <- mean(plda.pca.train.preds != ytrain)
plda.pca.train.error

#Get testing predictions and errors
plda.pca.test.fit <- PenalizedLDA(as.matrix(pca.matrix[,1:50]) , 
                                  as.factor(unlist(ytrain)), 
                                  xte = as.matrix(pca.test.matrix), 
                                  type = "standard",
                                  lambda = plda.pca.cv$bestlambda,
                                  K = plda.pca.cv$bestK)

#Get test predictions
plda.pca.test.preds <- plda.pca.test.fit$ypred[,plda.pca.test.fit$K]

#Get test errors
plda.pca.test.error <- mean(plda.pca.test.preds != ytest)
plda.pca.test.error


#QDA CV with PCA Components - Only first 300 components used in CV

library(MASS)

#Same split as from other analyses - already in environment. Just here for book-keeping
n.train <- nrow(xtrain)
nfolds <- 5
set.seed(123)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))

#Matrix to hold CV errors
qda.pca.cv <- matrix(NA, 5, 300)

for (i in seq(nfolds)) { 
  #To know what fold we're in
  print(i)
  #Calculate PCA scores excluding i-th fold
  x.svd <- svd(xtrain.standardized[-s[[i]],])
  scores <- xtrain.standardized[-s[[i]],] %*% x.svd$v
  
  #Get y-train excluding i-th fold
  y <- ytrain[-s[[i]],]
  
  #Get Principal component scores for CV error calculation
  valid.scores <- xtrain.standardized[s[[i]],] %*% x.svd$v
  
  #Get y-valid with i-th fold
  y.valid <- ytrain[s[[i]],]
  
  for(j in 1:300){
    pc <- as.matrix(scores[,1:j])
    #combine scores and y-train vector to input into LDA formula
    pca.matrix <- cbind(pc, y)
    #Compute LDA 
    pca.qda <- qda(y~., data = as.data.frame(pca.matrix))
    
    #Get test predictions with validation fold
    pca.valid <- as.matrix(valid.scores[,1:j])
    pca.lda.y.hat <- predict(pca.qda, as.data.frame(pca.valid))$class
    
    #Get test error
    error <- mean(pca.lda.y.hat != y.valid)
    
    qda.pca.cv[i,j] <- error
  }
}

mean.qda.pca.cv <- apply(qda.pca.cv, 2, mean)
sd.qda.pca.cv <- apply(qda.pca.cv, 2 , sd)


#Plot the mean CV error for LDA
plot(mean.qda.pca.cv, xlab = "# of Scores", ylab = "Misclassification Rate", main = "CV Error for QDA with First 300 Principal Components", col = "black", pch = 19)
lines(mean.qda.pca.cv+sd.qda.pca.cv, col = "red", lty = 2)
lines(mean.qda.pca.cv-sd.qda.pca.cv, col = "red", lty = 2)
which.min(mean.qda.pca.cv)

#Zoomed in Plot of last 150 Components so we can more easily make a 1-Standard error rule judgement
plot(mean.qda.pca.cv[150:300], xlab = "# of Scores", ylab = "Misclassification Rate", main = "CV Error for QDA with Last 150 Principal Components", col = "black", pch = 19, ylim = c(.53,.55), axes = FALSE)
box()
axis(1, at = 0:150, labels = seq(150,300))
axis(2)
lines(mean.qda.pca.cv[150:300]+sd.qda.pca.cv[150:300], col = "red", lty = 2)
lines(mean.qda.pca.cv[150:300]-sd.qda.pca.cv[150:300], col = "red", lty = 2)
abline(v = 134, lty = 3)
abline(h = mean.qda.pca.cv[283] + sd.qda.pca.cv[283], lty = 3)
abline(h = mean.qda.pca.cv[283] - sd.qda.pca.cv[283], lty = 3)

#Choose 257 Principal Components according to 1-Standard Error Rule
points(108, mean.qda.pca.cv[257], col = "green" , pch = 15, cex = 3 )
legend("topright", legend = c("Mean Error", "Standard Error"), col = c("black","red"), pch = c(19,NA), lty = c(NA,2))

#Compute LDA train & test error with first 257 Principal Components

#Get first 37 scores
pca.257 <- pca.scores[,1:257]
pca.257 <- cbind(pca.257, ytrain)

#Do LDA
pca.257.qda <- qda(y~., data = pca.257)
#Get training errors
pca.257.train.preds <- as.integer(predict(pca.257.qda, pca.257[,1:257])$class)
pca.257.train.error <- mean(pca.257.train.preds != ytrain)

#Get test errors
pca.test.257 <- as.matrix(xtest.standardized) %*% xtrain.svd$v[,1:257]
pca.test.257 <- cbind(pca.test.257, ytest)

pca.257.test.preds <- as.integer(predict(pca.257.lda, pca.test.257[,1:257])$class)
pca.257.test.error <- mean(pca.257.test.preds != ytest)

pca.257.train.error
pca.257.test.error


#KNN with FDA components

install.packages("kknn")
library(kknn)

#Put first 9 principal components into a matrix
fda.matrix <- as.matrix(xtrain) %*% Re(S.eig$vectors[,1:9])

ytrain.factor <- as.factor(unlist(ytrain))
ytest.factor <- as.factor(unlist(ytest))

#Combine the fda matrix with the ytrain matrix to make a full training matrix
fda.matrix <- data.frame(x=fda.matrix, y = ytrain.factor)

#Create an fda test matrix to use later to find test error
fda.test.matrix <- as.matrix(xtest) %*% Re(S.eig$vectors[,1:9])
fda.test.matrix <- data.frame(x=fda.test.matrix)

#Create 5 folds for splitting training matrix
n.train <- nrow(xtrain)
nfolds <- 5
set.seed(123)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))

#Create matrix to hold CV errors for each k
klist <- seq(1,100,1)
knn.cv <- matrix(NA, nfolds, length(klist))

#Loop to fill the CV matrix 
for (i in seq(nfolds)) { 
  #To know what fold we're on
  print(i)
  for(j in klist){
    knn.fit <- kknn(formula = y ~., train = fda.matrix[-s[[i]],], fda.matrix[s[[i]],], k = j, kernel = "rectangular")
    #Get fitted values
    fitted <- fitted(knn.fit)
    #Find error by comparing fitted values with y-column on the validation set
    error <- mean(fitted != fda.matrix[s[[i]],10])
    knn.cv[i,j] <- error
  }
}

mean.knn.cv <- apply(knn.cv, 2, mean)
plot(mean.knn.cv, xlab = "Neighbors", ylab = "Misclassification Rate", main = "CV error for Regular KNN", type = "l", col = "red")

#Loop to fill in CV matrix using E-kernel smoothing
knn.e.cv <- matrix(NA, nfolds, length(klist))

for (i in seq(nfolds)) { 
  #To know what fold we're on
  print(i)
  for(j in klist){
    knn.fit <- kknn(formula = y ~., train = fda.matrix[-s[[i]],], fda.matrix[s[[i]],], k = j, kernel = "epanechnikov")
    #Get fitted values
    fitted <- fitted(knn.fit)
    #Find error by comparing fitted values with y-column on the validation set
    error <- mean(fitted != fda.matrix[s[[i]],10])
    knn.e.cv[i,j] <- error
  }
}

mean.knn.e.cv <- apply(knn.e.cv, 2, mean)
plot(mean.knn.e.cv, xlab = "Neighbors", ylab = "Misclassification Rate", main = "CV error for E-Kernel KNN")

plot(mean.knn.cv, xlab = "Neighbors", ylab = "Misclassification Rate", main = "CV Error for KNN with 9 FDA Components", type = "l", col = "red", lwd = 2)
lines(mean.knn.e.cv, xlab = "Neighbors", ylab = "Misclassification Rate", main = "CV error for E-Kernel KNN", col = "blue", lwd = 2)
legend("topright", legend = c("Unweighted", "E-Kernel"), col = c("red", "blue"), lty = 1)



#Get testing errors for rectangular, e-kernel 

#Find the k with the lowest cross validated error
which.min(mean.knn.cv)
which.min(mean.knn.e.cv)

knn.fit <- kknn(formula = y ~., train = fda.matrix, test = fda.matrix[,1:9], k=91, kernel = "rectangular")
fitted <- fitted(knn.fit)
knn.test.error <- mean(fitted != ytrain.factor)


knn.e.fit <- kknn(formula = y ~., train = fda.matrix, test = fda.matrix[,1:9], k=100, kernel = "epanechnikov")
knn.e.fitted <- fitted(knn.e.fit)
knn.e.test.error <- mean(knn.e.fitted != ytrain.factor)

knn.test.error
knn.e.test.error


#LDA with FDA Components

library(MASS)

#Make LDA model with first 9 components
fda.lda <- lda(y~., data = fda.matrix)

#Get training predictions
fda.lda.y.hat <- predict(fda.lda, fda.matrix[,1:9])$class

fda.lda.y.hat

length(fda.lda.y.hat)
dim(ytrain)

#Get training errors
fda.lda.train.error <- mean(as.matrix(fda.lda.y.hat) != ytrain)

#Get test errors

fda.lda.y.test.hat <- predict(fda.lda, fda.test.matrix[,1:9])$class
fda.lda.test.error <- mean(as.matrix(fda.lda.y.test.hat) != ytest)

fda.lda.train.error
fda.lda.test.error

#Logistic Regression with FDA Components

library(glmnet)

#Run logistic regression with first 9 FDA components
#Create fda x-matrix

fda.x.matrix <- as.matrix(xtrain) %*% Re(S.eig$vectors[,1:9])

class(fda.x.matrix)
#Create logistic model
fda.logistic <- glmnet(fda.x.matrix, as.matrix(ytrain), family = "multinomial", lambda = 0)

#Get predictions with logistic model
fda.log.y.hat <- predict(fda.logistic, fda.x.matrix, type = "class")

dim(fda.log.y.hat)

#Get training error
fda.log.train.error <- mean(fda.log.y.hat != ytrain)

#Get test error using fda.x.test.matrix

fda.x.test.matrix <- as.matrix(xtest) %*% Re(S.eig$vectors[,1:9])

fda.log.y.test.hat <- predict(fda.logistic, fda.x.test.matrix, type = "class")
fda.log.test.error <- mean(fda.log.y.test.hat != ytest)

fda.log.train.error
fda.log.test.error

#Penalized LDA with FDA Components
install.packages("penalizedLDA")
library(penalizedLDA)

lambdas <- c(.0001,.001,.01,.1,1,10)

plda.fda <- PenalizedLDA.cv(as.matrix(fda.matrix[,1:9]), as.factor(unlist(ytrain)), lambdas = lambdas, nfold = 5, type = "standard")

plda.fda.fit <- PenalizedLDA(as.matrix(fda.matrix[,1:9]), 
                             as.factor(unlist(ytrain)), 
                             xte = as.matrix(fda.matrix[,1:9]), 
                             type = "standard",
                             lambda = plda.fda$bestlambda,
                             K = plda.fda$bestK)

#Get training predictions
plda.fda.train.preds <- plda.fda.fit$ypred[,9]

#Get training errors
plda.fda.train.error <- mean(plda.fda.train.preds != ytrain)
plda.fda.train.error

#Get testing predictions and errors
plda.fda.test.fit <- PenalizedLDA(as.matrix(fda.matrix[,1:9]), 
                                  as.factor(unlist(ytrain)), 
                                  xte = as.matrix(fda.test.matrix), 
                                  type = "standard",
                                  lambda = plda.fda$bestlambda,
                                  K = plda.fda$bestK)

#Get test predictions
plda.fda.test.preds <- plda.fda.test.fit$ypred[,9]

#Get test errors
plda.fda.test.error <- mean(plda.fda.test.preds != ytest)
plda.fda.test.error

#QDA with FDA Components

library(MASS)

#Make LDA model with first 9 components
fda.qda <- qda(y~., data = fda.matrix)

#Get training predictions
fda.qda.y.hat <- predict(fda.qda, fda.matrix[,1:9])$class

#Get training errors
fda.qda.train.error <- mean(as.matrix(fda.qda.y.hat) != ytrain)

#Get test errors

fda.qda.y.test.hat <- predict(fda.qda, fda.test.matrix[,1:9])$class
fda.qda.test.error <- mean(as.matrix(fda.qda.y.test.hat) != ytest)

fda.qda.train.error
fda.qda.test.error

