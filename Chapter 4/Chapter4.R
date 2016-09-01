###########################
# CHAPTER 4: Random Forests
###########################

# Check for missing packages and install if missing
list.of.packages <- c("gridExtra", "ggplot2", "caret", "reshape2", "kernlab", "randomForest",
                      "randomForestSRC", "ggRandomForests", "latex2exp", "ROCR", "lattice",
                      "grid", "MASS", "ipred", "plyr", "e1071")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
load <- lapply(list.of.packages, require, character.only = TRUE)

###########################################################################
# Figure 4.1: 10-fold cross-validation errors per additional 10 trees for a
# random forest fit on the mixture data.
###########################################################################
# ---------------------------------------------------------------------------
# Simulate data
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

# learning curve mixture data with standard error bars
tuneControl <- data.frame(mtry=1)
fitControl <- trainControl(method="cv", number=10)
overError <- NULL
overSD <- NULL
for(i in 1:50){
      rf.fit <- train(y~., data=trainHastie, method="rf", trControl=fitControl,
                      tuneGrid=tuneControl, ntree=i*10)
      overError[i] <- 1-rf.fit$result[2]
      overSD[i] <- rf.fit$result[4]
}

# plot errors with SD for RF fit to the mixture data
fraction <- 1:50*10
errorRF.fit <- unlist(overError)
sdRF.fit <- unlist(overSD)
RF.fiterror <- data.frame(x=fraction, y=errorRF.fit)
RF.fitsdPlus <- data.frame(x=fraction, y=errorRF.fit+sdRF.fit)
RF.fitsdMin <- data.frame(x=fraction, y=errorRF.fit-sdRF.fit)
RFPlot <- ggplot(data=RF.fiterror, aes(x, y)) + geom_line(color="orange", size=1) +
      geom_errorbar(aes(ymax=errorRF.fit+sdRF.fit, ymin=errorRF.fit-sdRF.fit), width=5, col="blue")+
      geom_point(col="orange", size=4) + xlab("Number of trees") + ylab("Error (ten-fold CV)") +
      ggtitle("Random Forest Learning: elemStat data")+
      theme_bw()+
      geom_vline(xintercept=which(errorRF.fit == min(errorRF.fit))*10, col="purple", linetype="dashed")
RFPlot
# ---------------------------------------------------------------------------
###########################################################################
# Figure 4.2: A Forest-RI fitted to the mixture data: The decision boundary
# is represented by the solid brown line.
###########################################################################
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

# fit a random forest to the data
fitControl <- trainControl(method="none")
tuneControl <- data.frame(mtry=1)
set.seed(13)
rf.fit <- train(y~., data=train, method="rf", trControl=fitControl,
                tuneGrid=tuneControl, ntree=100, proximity=TRUE)
rfProx <- rf.fit

# compute training and test error
rfTrainPreds <- predict(rf.fit)
rfTrainingError <- sum(as.numeric(train$y != rfTrainPreds))/nrow(train)

# Compute test error
rfTestPreds <- predict(rf.fit, test)
rfTestError <- sum(as.numeric(test$y != rfTestPreds))/nrow(test)

# construct decision boundary plot
rfProbs <- predict(rf.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(rfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
grf <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(rfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="top")+
      scale_color_manual(name="Forest-RI decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','Forest-RI'))+
      scale_linetype_manual(name = 'Forest-RI decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','Forest-RI'))+
      annotate("text", x = 2.2, y = -1.6, size=3,
               label = paste("Training error:", round(rfTrainingError, 3),
                             "\nTest error:", round(rfTestError,3),
                             "\nBayes error:", round(bayesError,3)), hjust=0)
grf

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

# trees learning curves for bagging
M <- 50
trainErrorBag <- NULL
testErrorBag <- NULL
for(i in 1:M){
      fit <- train(y~., data = trainHastie, method = "treebag", trControl = fitControl, verbose = FALSE, nbagg=i*10)
      predsTest <- predict(fit, testHastie)
      predsTrain <- predict(fit, trainHastie)
      trainErrorBag[i] <- sum(as.numeric(predsTrain != trainHastie$y))/length(trainHastie$y)
      testErrorBag[i] <- sum(as.numeric(predsTest != testHastie$y))/length(testHastie$y)
}

# trees learning curves for random forest
trainErrorRF <- NULL
testErrorRF <- NULL
for(i in 1:M){
      fit <- train(y~., data = trainHastie, method = "rf", trControl = fitControl, verbose = FALSE,
                   tuneGrid = data.frame(mtry=3), ntree=i*10)
      predsTest <- predict(fit, testHastie)
      predsTrain <- predict(fit, trainHastie)
      trainErrorRF[i] <- sum(as.numeric(predsTrain != trainHastie$y))/length(trainHastie$y)
      testErrorRF[i] <- sum(as.numeric(predsTest != testHastie$y))/length(testHastie$y)
}

# Plot error curves
errors <- data.frame(x=seq(from=10, to=500, by=10), teBag=testErrorBag, teRF=testErrorRF)
mel <- melt(errors, id.var="x")
ggplot(mel, aes(x=x, y=value, col=variable)) + geom_line() +
      geom_point()+
      geom_hline(yintercept=mean(testErrorBag), col="green", linetype="dashed")+
      geom_hline(yintercept=mean(testErrorRF), col="orange", linetype="dashed")+
      theme_bw()+
      theme(legend.position="top")+
      xlab("Number of trees") + ylab("Test Error") +
      scale_colour_manual(name = 'elemStat Data Fit:', values=c("green", "orange"), labels = c("Bagging","Forest-RI"))
# ---------------------------------------------------------------------------
###########################################################################
# Figure 4.3: OOB error computed on the Spam training data, compared to the
# test error.
###########################################################################
# ---------------------------------------------------------------------------
# trees learning curves for random forest
M <- 50
trainErrorRF <- NULL
testErrorRF <- NULL
for(i in 1:M){
      fit <- train(y~., data = trainHastie, method = "rf", trControl = fitControl, verbose = FALSE,
                   tuneGrid = data.frame(mtry=3), ntree=i*10)
      predsTest <- predict(fit, testHastie)
      predsTrain <- predict(fit, trainHastie)
      trainErrorRF[i] <- sum(as.numeric(predsTrain != trainHastie$y))/length(trainHastie$y)
      testErrorRF[i] <- sum(as.numeric(predsTest != testHastie$y))/length(testHastie$y)
}

# Compute out of bag error rates for spam data and plot with test error rate
fitControl <- trainControl(method="none")
rf.fit <- train(y~., data = trainHastie, method = "rf", trControl = fitControl, verbose = FALSE,
                tuneGrid = data.frame(mtry=3), ntree=500)
OOBErrorRF <- rf.fit$finalModel$err.rate[seq(from=10, to=500, by=10),1]
OOBTesterrors <- data.frame(x=seq(from=10, to=500, by=10), teRF=testErrorRF, oobRF=OOBErrorRF)
mel <- melt(OOBTesterrors, id.var="x")
ggplot(mel, aes(x=x, y=value, col=variable)) + geom_line() +
      theme_bw()+
      geom_point()+
      theme(legend.position="top")+
      geom_hline(yintercept=mean(testErrorRF), col="red", linetype="dashed")+
      geom_hline(yintercept=mean(OOBErrorRF), col="blue", linetype="dashed")+
      xlab("Number of trees") + ylab("Error") +
      scale_colour_manual(name = 'OOB vs Test elemStat Data:', values=c("red", "blue"), labels = c("Test Error","OOB Error"))
# ---------------------------------------------------------------------------
###################################################
# Figure 4.4: Variable importance for the spam data
###################################################
# ---------------------------------------------------------------------------
# load data
data(spam)
spamData <- data.frame(y=spam$type, spam[,-58])

# split into training and test
set.seed(3)
trainIndex <- createDataPartition(spamData$y, p=0.6, list=FALSE)
spamTrain <- spamData[trainIndex,]
spamTest <- spamData[-trainIndex,]

# RF variable importance
set.seed(123)
rf <- rfsrc(y~., data=spamTrain, importance="TRUE")

# compute prediction error
rfPreds <- predict(rf, spamTest, type="prob")
rfClassPreds <- rfPreds$class
rfMisclassError <- mean(rfClassPreds != spamTest$y)

# compute variable importance
vimp <- gg_vimp(rf, which.outcome="all")
plot(vimp) + theme_bw() +theme(legend.position="none")
# ---------------------------------------------------------------------------
##########################################################################
# Figure 4.5: Spam data variable exploration plot: The top two rows
# correspond to the eight most important variables and the bottom two rows
# the least important.
##########################################################################
# ---------------------------------------------------------------------------
# plot variable relationships
ylabel <- TeX("$\\hat{P}(spam|\\underline{x})$")
gg_v <- gg_variable(rf)
xvar <- vimp$vars[c(1:3, 55:57)]
plot(gg_v, xvar=xvar, panel=TRUE) + scale_colour_manual(values = c("skyblue", "red")) +
      theme_bw()+theme(legend.position="bottom", legend.title=element_blank()) + ylab(ylabel)
# ---------------------------------------------------------------------------
###########################################################################
# Table 4.1: Significant predictors from the logistic regression fit to the 
# spam data.
###########################################################################
# ---------------------------------------------------------------------------
# fitting a logistic regression model to the spam data
lrSpam <- glm(y ~.,family=binomial(link='logit'),data=spamTrain)

# Compute prediction error
lrPredsProbs <- predict(lrSpam, spamTest, type='response')
lrPreds <- ifelse(lrPredsProbs > 0.5,"spam","nonspam")
LRmisClasificError <- mean(lrPreds != spamTest$y)

# find significant variables
modCoefs <- summary(lrSpam)$coefficients
sigVarIndex <- which(modCoefs[,4] < 0.05)
sigVar <- modCoefs[sigVarIndex,]
sigVarOrd <- round(sigVar[order(sigVar[,4]),], 4)
sigVarOrd
# ---------------------------------------------------------------------------
#####################################################
# In text: Rank correlation between VIMP and p-values
#####################################################
# ---------------------------------------------------------------------------
# rank correlations based on p-value for all predictors
vimpVars <- vimp$vars
lrVars <- sapply(rownames(modCoefs[order(modCoefs[,4]),])[-1], function(x) which(vimpVars == x))
rankCorrelation <- cor(1:57, lrVars, method="spearman")
# ---------------------------------------------------------------------------
#############################################################################
# Figure 4.6: Random Forest partial dependence plot: Left: Partial dependence 
# for the word “free”. Right: Partial dependence for the word “george”.
#############################################################################
# ---------------------------------------------------------------------------
# rf partial dependence plots
partialFree <- plot.variable(rf, xvar.names="free", partial=TRUE)
partialGeorge <- plot.variable(rf, xvar.names="george", partial=TRUE)
freeData <- gg_partial(partialFree)
georgeData <- gg_partial(partialGeorge)
fplotDat <- data.frame(y = 1-freeData$yhat, x=freeData$free)
gplotDat <- data.frame(y = 1-georgeData$yhat, x=georgeData$george)
parFree <- ggplot(fplotDat, aes(x=x, y=y)) + geom_line() + geom_point(col="darkgreen", size=3)+
      theme_bw()+
      ylab(TeX("$\\hat{P}(spam|\\underline{x})$")) + xlab("Percentage of the word 'free' in email") +
      geom_rug(sides="b", col="blue")
parGeorge <- ggplot(gplotDat, aes(x=x, y=y)) + geom_line() + geom_point(col="darkgreen", size=3)+
      theme_bw()+
      ylab(TeX("$\\hat{P}(spam|\\underline{x})$")) + xlab("Percentage of the word 'george' in email") +
      geom_rug(sides="b", col="blue")
grid.arrange(parFree, parGeorge, ncol=2)
# ---------------------------------------------------------------------------
#############################################################################
# Table 4.2: Model confusion matrices (logistic regression abbreviated as LR)
#############################################################################
# ---------------------------------------------------------------------------
# helper function to compute probabilities from odds
computeProb <- function(coef){
      odds <- exp(coef)
      prob <- odds/(1+odds)
      return(prob)
}

# compare confusion matrices of the two models
rfConfMat <- confusionMatrix(rfClassPreds, spamTest$y, dnn=c("Predicted", "Actual"))[[2]]
lrConfMat <- confusionMatrix(lrPreds, spamTest$y, dnn=c("Predicted", "Actual"))[[2]]
# ---------------------------------------------------------------------------
##########################################################################
# Figure 4.7: ROC curve for a Random Forest and logistic regression fit to 
# the spam data.
##########################################################################
# ---------------------------------------------------------------------------
# compare using ROC curves
# calculating the values for ROC curve
pred <- prediction(predictions=data.frame(rf=1-rfPreds$predicted[,1], lr=lrPredsProbs),labels=data.frame(rf=as.numeric(spamTest$y),lr=as.numeric(spamTest$y)))
perf <- performance(pred,"tpr","fpr")

# plotting the ROC curve
plot(perf,colorize=TRUE, main="ROC spam data: RF vs Logistic regression")
text(0.01, 0.92, "RF")
text(0.15, 0.8, "Logistic regression")
# ---------------------------------------------------------------------------
########################################################################
# Figure 4.8: Random Forest proximity plots: a comparison of a proximity 
# plot with RF decision boundary.
########################################################################
# ---------------------------------------------------------------------------
# proximity plots
nrf <- rfProx$finalModel
mdsPoints <- MDSplot(nrf, fac=train$y)$points
labelPoints <- mdsPoints[c(4, 74, 87, 116, 155, 186),]
grfProx1 <- ggplot(data.frame(x=mdsPoints[,1], y=mdsPoints[,2], response=train$y), aes(x=x, y=y, col=response))+
      geom_point()+xlab("Dimension 1") + ylab("Dimension 2") +
      theme_bw() +
      theme(legend.position="none")+
      scale_colour_manual(name="Proximity Plot", values=c("blue", "orange", "purple")) +
      geom_label(data=data.frame(x=labelPoints[,1], y=labelPoints[,2],label=paste(1:6)), aes(x=x,y=y,label=label), col="darkgreen", size=5)+
      ggtitle("Proximity Plot")

# decision boundary with 2 dimensions
labelPoints2 <- train[c(4, 74, 87, 116, 155, 186),2:3]
proxRF <- nrf
rfProbs <- predict(proxRF, newdata=plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(rfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
grfProx2 <- ggplot(data.frame(y=factor(Ytrain), x1=Xtrain[,1], x2=Xtrain[,2]), aes(x=x1, y=x2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(rfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple"), breaks=c(0,.5)) +
      theme_bw()+
      theme(legend.position="none")+
      scale_color_manual(name="Forest-RI decision boundary:",values=c("brown"),
                         labels = c(''))+ xlab("X1") + ylab("X2")+
      geom_label(data=data.frame(x=labelPoints2[,1], y=labelPoints2[,2],label=paste(1:6)), aes(x=x,y=y,label=label), col="darkgreen", size=5)+
      ggtitle("Forest-RI Decision Boundary")

# compare proximity plot with decision boundary
grid.arrange(grfProx1, grfProx2, ncol=2)
# ---------------------------------------------------------------------------
