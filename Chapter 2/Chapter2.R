#################################
# CHAPTER 2: Classification Trees
#################################

# Check for missing packages and install if missing
list.of.packages <- c("latex2exp", "MASS", "class", "caret",
                      "ggplot2", "lattice", "rpart", "rpart.plot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
load <- lapply(list.of.packages, require, character.only = TRUE)

##############################################
# Figure 2.1: Recursive binary partitioning
##############################################
# ---------------------------------------------------------------------------
# create empty plot
par(mar=c(0,0,0,0))
plot(0:22, 0:22, type="n", xlab="", ylab="",
     xlim=c(0, 22), ylim=c(-1, 22),
     main="", axes=FALSE)

# draw partitioned input space
# draw box
lines(c(1,1), c(1, 20))
lines(c(1,10), c(1, 1))
lines(c(1,10), c(20, 20))
lines(c(10,10), c(1, 20))
# draw partitions
lines(c(3,3), c(1,20))
lines(c(1,3), c(5,5), col="brown")
lines(c(3,3), c(5,1), col="brown")
lines(c(3,10), c(14, 14))
lines(c(3,7), c(14,14), col="brown")
lines(c(7,7), c(1, 14), col="brown")
lines(c(3,3), c(14,20), col="brown")

# generate random points
set.seed(1)
# R1
points(runif(4, min=1.5, max=2.5), runif(4, min = 1.5, max = 4.5), col="blue")
points(runif(1, min=1.5, max=2.5), runif(1, min = 1.5, max = 4.5), col="orange", pch=2)
# R2
points(runif(12, min=1.5, max=2.5), runif(12, min = 5.5, max = 19.5), col="orange", pch=2)
points(runif(3, min=1.5, max=2.5), runif(3, min = 5.5, max = 19.5), col="blue")
# R3
points(runif(17, min=3.5, max=9.5), runif(17, min = 14.5, max = 19.5), col="blue")
points(runif(1.5, min=3.5, max=9.5), runif(1, min = 14.5, max = 19.5), col="orange", pch=2)
# R4
points(runif(30, min=3.5, max=6.5), runif(30, min = 1.5, max = 13.5), col="orange", pch=2)
points(runif(5, min=3.5, max=6.5), runif(5, min = 1.5, max = 13.5), col="blue")
# R5
points(runif(9, min=7.5, max=9.5), runif(9, min = 1.5, max = 13.5), col="blue")
points(runif(2, min=7.5, max=9.5), runif(2, min = 1.5, max = 13.5), col="orange", pch=2)

# add text
# axis
text(0, 10.5, TeX("$X_2$"))
text(5.5, -1, TeX("$X_1$"))
# split points
text(3, -0.1, TeX("$s_1$"))
text(0.6, 5, TeX("$s_2$"))
text(10.4, 14, TeX("$s_3$"))
text(7, -0.1, TeX("$s_4$"))
# regions
text(2, 2.5, TeX("$R_1$"), col="blue", cex=1.5)
text(2, 12, TeX("$R_2$"), col="darkorange", cex=1.5)
text(6.5, 17, TeX("$R_3$"), col="blue", cex=1.5)
text(5, 7.5, TeX("$R_4$"), col="darkorange", cex=1.5)
text(8.5, 7.5, TeX("$R_5$"), col="blue", cex=1.5)
# create tree
text(16.5, 20, TeX("$X_1 \\leq s_1$"))
text(15.5, 18.5, "<< Yes", col="green")
text(17.5, 18.5, "No >>", col="red")
lines(c(16.5, 16.5), c(18.5, 19.5))
lines(c(14, 19),c(19, 19))
# splits
lines(c(14, 14), c(19, 16))
lines(c(19, 19), c(19, 16))
# internal nodes
text(14, 15, TeX("$X_2 \\leq s_2$"))
text(19, 15, TeX("$X_2 \\leq s_3$"))
lines(c(14, 14), c(13.5, 14.5))
lines(c(13, 15), c(14, 14))
lines(c(19, 19), c(13.5, 14.5))
lines(c(18, 20), c(14, 14))
# split internal node 1
lines(c(13, 13), c(14, 9))
lines(c(15, 15), c(14, 9))
# split internal node 2
lines(c(18, 18), c(14, 9))
lines(c(20, 20), c(14, 9))
# root nodes 1, 2, 3
text(13, 8, TeX("$R_1$"), col="blue", cex=1.5)
text(15, 8, TeX("$R_2$"), col="darkorange", cex=1.5)
text(20, 8, TeX("$R_3$"), col="blue", cex=1.5)
# internal node 3
text(18, 8, TeX("$X_1 \\leq s_4$"))
lines(c(18, 18), c(6.5, 7.5))
lines(c(17, 19), c(7, 7))
lines(c(17, 17), c(7, 3))
lines(c(19, 19), c(7, 3))
# root node 4 and 5
text(17, 2, TeX("$R_4$"), col="darkorange", cex=1.5)
text(19, 2, TeX("$R_5$"), col="blue", cex=1.5)

# ---------------------------------------------------------------------------
#####################################
# Figure 2.2: Simulated mixture data
#####################################
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

# plot data
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
gd <- expand.grid(x=x1seq, y=x2seq)
bayesPlot <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2) ) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z), breaks=c(0,.5), col="purple",
                   linetype=2)+
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(bayesrule<0.5, "skyblue", "orange"))+
      theme_bw()+
      geom_point(size = 3, pch = train$y, col=color) +
      ggtitle("Bayes decision boundary: Mixture data")
bayesPlot
bayesProbs <- apply(test[,2:3], 1, p)
bayesError <- sum(as.numeric(test$y != factor(ifelse(bayesProbs>0.5, 1, 0))))/nrow(test)
# ---------------------------------------------------------------------------
############################################################
# Figure 2.3: Classification tree fitted to the mixture data
############################################################
# ---------------------------------------------------------------------------
# fit a classification tree to the data
tree.fit <- train(y~., data=train, method="rpart")

# plot fit
prp(tree.fit$finalModel, type=3, varlen=0, faclen=0, fallen.leaves=TRUE, box.col=c("orange", "blue"))

# compute training and test error
treeTrainingPreds <- predict(tree.fit)
treeTrainingError <- sum(as.numeric(train$y != treeTrainingPreds))/nrow(train)

# Compute test error
treeTestPreds <- predict(tree.fit, test)
treeTestError <- sum(as.numeric(test$y != treeTestPreds))/nrow(test)

# construct decision boundary plot
treeProbs <- predict(tree.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(treeProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
gtree <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(treeProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw()+
      theme(legend.position="top")+
      scale_color_manual(name="Tree decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','Tree'))+
      scale_linetype_manual(name = 'Tree decision boundary:', values = c("dashed", "solid"),
                          labels = c('Bayes','Tree'))+
    annotate("text", x = 2.2, y = -1.6, size=3, 
             label = paste("Training error:", round(treeTrainingError, 3),
                        "\nTest error:", round(treeTestError,3),
                        "\nBayes error:", round(bayesError,3)), hjust=0)
gtree
# ---------------------------------------------------------------------------
#############################################################################
# Figure 2.4: Changes in decision boundary as a result of changes in the data
#############################################################################
# ---------------------------------------------------------------------------
for(i in 1:3){
      # Generate training data
      set.seed(i+2)
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
      trainTemp <- data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2])

      # create decision boundary plotting grid
      x1min <- min(Xtrain[,1])
      x1max <- max(Xtrain[,1])
      x2min <- min(Xtrain[,2])
      x2max <- max(Xtrain[,2])
      x1seq <- seq(from=x1min,to=x1max,length=100)
      x2seq <- seq(from=x2min,to=x2max,length=100)
      plotGrid <- data.frame(as.matrix(expand.grid(x1seq,x2seq)))
      colnames(plotGrid) <- colnames(trainTemp)[2:3]

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
      testTemp <- data.frame(y=factor(Ytest), X1=Xtest[,1], X2=Xtest[,2])

      # Compute Bayes related quantities
      color <- ifelse(trainTemp$y == 0, "blue", "darkorange")
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
      gd <- expand.grid(x=x1seq, y=x2seq)
      bayesProbs <- apply(testTemp[,2:3], 1, p)
      bayesError <- sum(as.numeric(testTemp$y != factor(ifelse(bayesProbs>0.5, 1, 0))))/nrow(testTemp)

      # Fit fully grown tree
      contr <- rpart.control(minsplit = 2, minbucket = 1, cp = 0, maxdepth = 30)
      tree.fit <- rpart(y~., data=trainTemp, control=contr)

      # construct decision boundary plot for tree
      treeProbs <- predict(tree.fit, plotGrid, type="prob")[,2]
      pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
                     z=as.vector(treeProbs))
      gd <- expand.grid(x=x1seq, y=x2seq)
      gtree <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
            geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                       col=ifelse(treeProbs<0.5, "skyblue", "orange")) +
            geom_point(size = 3, pch = trainTemp$y, col=color) +
            geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
            geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
            theme_bw()+
            theme(legend.position="none")+
            scale_color_manual(name="tree decision boundary:",values=c("purple", "brown"),
                               labels = c('Bayes','Tree'))+
            scale_linetype_manual(name = 'tree decision boundary:', values = c("dashed", "solid"),
                                  labels = c('Bayes','Tree'))
      print(gtree)

      # Fit fully grown treePrune
      treePrune.fit <- train(y~., data=trainTemp, method="rpart")

      # construct decision boundary plot for treePrune
      treePruneProbs <- predict(treePrune.fit, plotGrid, type="prob")[,2]
      pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
                     z=as.vector(treePruneProbs))
      gd <- expand.grid(x=x1seq, y=x2seq)
      gtreePrune <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
            geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                       col=ifelse(treePruneProbs<0.5, "skyblue", "orange")) +
            geom_point(size = 3, pch = trainTemp$y, col=color) +
            geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
            geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
            theme_bw()+
            theme(legend.position="none")+
            scale_color_manual(name="treePrune decision boundary:",values=c("purple", "brown"),
                               labels = c('Bayes','treePrune'))+
            scale_linetype_manual(name = 'treePrune decision boundary:', values = c("dashed", "solid"),
                                  labels = c('Bayes','treePrune'))
      print(gtreePrune)

      # Fit logistic regression model
      lr.fit <- glm(y ~.,family=binomial(link='logit'),data=trainTemp)

      # construct decision boundary plot for LR
      LRProbs <- predict(lr.fit, plotGrid, type="response")
      pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
                     z=as.vector(LRProbs))
      gd <- expand.grid(x=x1seq, y=x2seq)
      gLR <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
            geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                       col=ifelse(LRProbs<0.5, "skyblue", "orange")) +
            geom_point(size = 3, pch = trainTemp$y, col=color) +
            geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
            geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
            theme_bw()+
            theme(legend.position="none")+
            scale_color_manual(name="LR decision boundary:",values=c("purple", "brown"),
                               labels = c('Bayes','LR'))+
            scale_linetype_manual(name = 'LR decision boundary:', values = c("dashed", "solid"),
                                  labels = c('Bayes','LR'))
      print(gLR)
}
# ---------------------------------------------------------------------------
