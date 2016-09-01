#################################################
# CHAPTER 3: Ensemble Learning for Classification
#################################################

# Check for missing packages and install if missing
list.of.packages <- c("ggplot2", "gridExtra", "rpart", "caret",
                      "MASS", "class", "reshape2", "rpart.plot", "gbm",
                      "survival", "splines", "parallel", "plyr", "lattice",
                      "ipred")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
load <- lapply(list.of.packages, require, character.only = TRUE)

#########################################################################
# Figure 3.1: Improving the accuracy of trees with the AdaBoost algorithm
#########################################################################
# ---------------------------------------------------------------------------
# plot contribution curve
Err <- seq(0.5, 0.01, len=100)
alpha <- log((1-Err)/Err)
contributionData <- data.frame(Err=Err, alpha=alpha)
weightUpdateData <- data.frame(Err=Err, weight=exp(alpha)/sum(exp(alpha)))
p1 <- ggplot(contributionData, aes(Err, alpha)) + geom_line(color="blue") +
      geom_point(color="blue")+
      xlab(expression(paste("Error"[b]))) + ylab(expression(paste(alpha[b])))+
      theme_bw()

p2 <- ggplot(weightUpdateData, aes(Err, weight)) + geom_line(color="purple")+
      geom_point(color="purple")+
      xlab(expression(paste("Error"[b]))) + ylab("Misclassified observation weight")+
      theme_bw()
grid.arrange(p1, p2, ncol=2)
# ---------------------------------------------------------------------------
##########################################################################
# Figure 3.2: Test Error rates on elemStat data for a stump, a fully grown
# tree and for AdaBoost.
##########################################################################
# ---------------------------------------------------------------------------
# Simulate elemStat data
set.seed(3)
X <- NULL
for(i in 1:10){
      X <- cbind(X, rnorm(12000))
}
y <- factor(apply(X, 1, function(x){ ifelse(sum(x^2) > 9.34, 1, -1)}))
trainHastie <- data.frame(y=y[1:2000], x=X[1:2000, ])
colnames(trainHastie) <- c("y", paste("X", 1:10, sep=""))
testHastie <- data.frame(y=y[2001:12000], x=X[2001:12000, ])
colnames(testHastie) <- c("y", paste("X", 1:10, sep=""))

# Fit tree stump
fit <- rpart(y~., data=trainHastie, control=rpart.control(maxdepth=1))

# Test error of tree stump
preds <- predict(fit, testHastie, type="class")
errorStump <- sum(as.numeric(preds != testHastie$y))/length(testHastie$y)

# Fit full tree
fit <- rpart(y~., data=trainHastie)

# Test error of full tree
preds <- predict(fit, testHastie, type="class")
errorFullTree <- sum(as.numeric(preds != testHastie$y))/length(testHastie$y)

# Fit adaBoost model
trainErrorVec <- NULL
testErrorVec <- NULL
expLoss <- NULL
minPlus <- function(x){
      x <- as.numeric(x)
      x <- x-1
      x[x == 0] <- -1
      return(x)
}
fitControl <- trainControl(method = "none")
M <- 600
for(i in 1:M){
      fit <- train(y~., data = trainHastie, method = "gbm", distribution="adaboost", trControl = fitControl, verbose = FALSE,
                   tuneGrid = data.frame(interaction.depth = 1,
                                         n.trees = i,
                                         shrinkage = 1,
                                         n.minobsinnode = 20))
      predsTest <- predict(fit, testHastie)
      predsTrain <- predict(fit, trainHastie)
      expLoss[i] <- mean(exp(minPlus(trainHastie$y)*predict(fit$finalModel, trainHastie[,-1], n.trees=i)))
      trainErrorVec[i] <- sum(as.numeric(predsTrain != trainHastie$y))/length(trainHastie$y)
      testErrorVec[i] <- sum(as.numeric(predsTest != testHastie$y))/length(testHastie$y)
}

# Plot errors
TestErrors <- data.frame(x=1:M, y=testErrorVec)
TrainErrors <- data.frame(x=1:M, tr=trainErrorVec, expl=expLoss)
ggplot(TestErrors, aes(x=x, y=y)) + geom_line(color="red") +
      geom_hline(yintercept=errorFullTree, linetype="dashed", color="orange", show.legend = TRUE, size=1.2)+
      geom_hline(yintercept=errorStump, linetype="dashed", color="green", show.legend = TRUE, size=1.2)+
      ylab("Test error") + xlab("Number of boosting iterations")+
      theme_bw() +
      annotate("text", x = 150, y = 0.425, label = "Stump")+
      annotate("text", x = 300, y = 0.25, label = "Tree")+
      annotate("text", x = 450, y = 0.12, label = "Boosting")
# ---------------------------------------------------------------------------
########################################################################
# Figure 3.3: Top: AdaBoost compared to bagging using 100 classification
# trees fitted to the mixture data. Bottom: A random sample of three
# classification trees from the bagged ensemble
########################################################################
# ---------------------------------------------------------------------------
# Generate training data
set.seed(1)
mBlue <- mvrnorm(n=10, mu = c(1,0),Sigma = diag(1,2,2))
mOrange <- mvrnorm(n=10, mu = c(0,1),Sigma = diag(1,2,2))
B <- matrix(0,nrow=100,ncol=2)
O <- matrix(0,nrow=100,ncol=2)

for(i in 1:100){
      sample1 = sample(1:10, 1)
      sample2 = sample(1:10, 1)
      meanB = mBlue[sample1,]
      meanO = mOrange[sample2,]
      B[i,] = mvrnorm(1,mu=meanB,Sigma=diag(1/5,2,2))
      O[i,] = mvrnorm(1,mu=meanO,Sigma=diag(1/5,2,2))
}

Btrain <- cbind(B[1:100,],matrix(0,100,1))
Otrain <- cbind(O[1:100,],matrix(1,100,1))
datatrain <- rbind(Btrain,Otrain)
Xtrain <- datatrain[,1:2]
Ytrain <- datatrain[,3]
train <- data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2])

# create decision boundary plotting grid
x1min <- min(Xtrain[,1])
x1max <- max(Xtrain[,1])
x2min <- min(Xtrain[,2])
x2max <- max(Xtrain[,2])
x1seq <- seq(from=x1min,to=x1max,length=100)
x2seq <- seq(from=x2min,to=x2max,length=100)
plotGrid <- data.frame(as.matrix(expand.grid(x1seq,x2seq)))
colnames(plotGrid) <- colnames(train)[2:3]

# create test set
B <- matrix(0,nrow=5000,ncol=2)
O <- matrix(0,nrow=5000,ncol=2)
for(i in 1:5000){
      sample1 <- sample(1:10, 1)
      sample2 <- sample(1:10, 1)
      meanB <- mBlue[sample1,]
      meanO <- mOrange[sample2,]
      B[i,] <- mvrnorm(1,mu=meanB,Sigma=diag(1/5,2,2))
      O[i,] <- mvrnorm(1,mu=meanO,Sigma=diag(1/5,2,2))
}

Btest <- cbind(B[1:5000,],matrix(0,5000,1))
Otest <- cbind(O[1:5000,],matrix(1,5000,1))
datatest <- rbind(Btest,Otest)
Xtest <- datatest[,1:2]
Ytest <- datatest[,3]
test <- data.frame(y=factor(Ytest), X1=Xtest[,1], X2=Xtest[,2])

# fit boosted model
boost.fit <- train(y~., data = train, method = "gbm", distribution="adaboost")
# compute training and test error
boostTrainPreds <- predict(boost.fit)
boostTrainingError <- sum(as.numeric(train$y != boostTrainPreds))/nrow(train)

# Compute test error
boostTestPreds <- predict(boost.fit, test)
boostTestError <- sum(as.numeric(test$y != boostTestPreds))/nrow(test)

# construct decision boundary plot
color <- ifelse(train$y == 0, "blue", "darkorange")
# Bayes decision boundary
p <- function(x) {
      s <- sqrt(1/5)
      p0 <- mean(dnorm(x[1], mBlue[,1], s) * dnorm(x[2], mBlue[,2], s))
      p1 <- mean(dnorm(x[1], mOrange[,1], s) * dnorm(x[2], mOrange[,2], s))
      p1/(p0+p1)
}
bayesrule <- apply(plotGrid, 1, p)
bayesPr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
                    z=as.vector(bayesrule))
bayesProbs <- apply(test[,2:3], 1, p)
bayesError <- sum(as.numeric(test$y != factor(ifelse(bayesProbs>0.5, 1, 0))))/nrow(test)
# boosting probabilities
boostProbs <- predict(boost.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(boostProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
gboost <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(boostProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="top")+
      scale_color_manual(name="AdaBoost decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','AdaBoost'))+
      scale_linetype_manual(name = 'AdaBoost decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','AdaBoost'))+
      annotate("text", x = 2.2, y = -1.6, size=3,
               label = paste("Training error:",
                             round(boostTrainingError, 3),
                              "\nTest error:", round(boostTestError,3),
                              "\nBayes error:", round(bayesError,3)), hjust=0)
gboost

# fit bagging to the data
set.seed(11)
bagging.fit <- train(y~., data=train, method="treebag",
                     nbagg=100, control=
                           rpart.control(minsplit=2, cp=0.044444, xval=0))

# plot three randomly selected trees from the bagging model
set.seed(6)
treeIndex <- sample(1:100, 3)
t1 <- bagging.fit$finalModel$mtrees[[treeIndex[1]]]$btree
t2 <- bagging.fit$finalModel$mtrees[[treeIndex[2]]]$btree
t3 <- bagging.fit$finalModel$mtrees[[treeIndex[3]]]$btree
prp(t1, type=3, varlen=0, faclen=0, fallen.leaves=TRUE, box.col=ifelse(t1$frame$yval == 1, "blue", "orange"))
prp(t2, type=3, varlen=0, faclen=0, fallen.leaves=TRUE, box.col=ifelse(t2$frame$yval == 1, "blue", "orange"))
prp(t3, type=3, varlen=0, faclen=0, fallen.leaves=TRUE, box.col=ifelse(t3$frame$yval == 1, "blue", "orange"))

# compute training and test error
baggingPreds <- predict(bagging.fit)
baggingTrainingError <- sum(as.numeric(train$y != baggingPreds))/nrow(test)

# Compute test error
baggingTestPreds <- predict(bagging.fit, test)
baggingTestError <- sum(as.numeric(test$y != baggingTestPreds))/nrow(test)

# construct decision boundary plot
baggingProbs <- predict(bagging.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(baggingProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
gbagging <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(baggingProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw()+
      theme(legend.position="top")+
      scale_color_manual(name="Bagging decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','Bagging'))+
      scale_linetype_manual(name = 'Bagging decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','Bagging'))+
      annotate("text", x = 2.2, y = -1.6, size=3,
               label = paste("Training error:",
                             round(baggingTrainingError, 3),
                             "\nTest error:", round(baggingTestError,3),
                             "\nBayes error:", round(bayesError,3)), hjust=0)
gbagging
# ---------------------------------------------------------------------------
