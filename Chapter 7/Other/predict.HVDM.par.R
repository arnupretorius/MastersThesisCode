exportList <- c("predTestHVDM", "distNew", "HVDM", "dp", "norm_diff", "norm_vdm", "npx", "npxc", "Ppxc")
library(foreach)
# make predictions for all test cases
predict.HVDM <- function(model, Xtest, k){
      Xtest <- Xtest[, !names(Xtest) %in% as.character(model[[21]][[2]])]
      preds <- foreach(i=1:nrow(Xtest), .combine = c, .export = exportList) %dopar% {
            predTestHVDM(model, Xtest[i,], 
                         model$trainingData[, !names(model$trainingData) %in% ".outcome"],
                         model$trainingData[, ".outcome"], k)
      }
      preds
}

# make prediction for test instance
predTestHVDM <- function(model, new, Xtrain, C, k){
      nn <- distNew(new, Xtrain, C, k)
      classes <- levels(C)
      preds <- predict(model, Xtrain[nn$index,])
      voteCount <- sapply(1:length(classes), function(i) {
            c <- classes[i]
            indexc <- which(preds == c)
            sum(1/nn$dists[indexc])
      })
      classes[which(voteCount == max(voteCount))]
}

# find k nearest neighbours to new instance
distNew <- function(new, Xtrain, C, k){
      N <- nrow(Xtrain)
      distVec <- sapply(1:N, function(i) {
            HVDM(new, Xtrain[i,], Xtrain, C)
      })
      indexMat <- data.frame(dists=distVec, index=1:N)
      indexMat <- indexMat[order(indexMat$dists),]
      return(indexMat[1:k,])
}

# compute HVDM between obs x and y
HVDM <- function(x, y, Xtrain, C){
      P <- ncol(Xtrain)
      distVar <- sapply(1:P, function(i) {
            dp(x[i], y[i], Xtrain[,i], C)^2
      })
      dist <- sum(distVar)
      sqrt(dist)
}

# compute distance between x and y on var p using HVDM 
dp <- function(x, y, p, C){
      if(class(p) != "numeric" && class(p) != "integer"){
            if(length(which(p == x)) == 0 || length(which(p == y)) == 0){
                  return(1)
            } else {
                  norm_vdm(x, y, p, C)
            }
      } else {
            norm_diff(x, y, p)
      }
}

# compute the normalized diff for x and y on var p [linear variable]
norm_diff <- function(x, y, p){
      abs(x - y)/4*sd(p)
}

# compute normalized vdm for x and y on var p [nominal variable]
norm_vdm <- function(x, y, p, C){
      vdmTerm <- sapply(1:length(levels(C)), function(i) {
            c <- levels(C)[i]
            abs(Ppxc(p, x, C, c) - Ppxc(p, y, C, c))^2
      })
      varDist <- sum(vdmTerm)
      sqrt(varDist)
}

# compute number of obs in training set with value x for var p
npx <- function(p, x){
      length(which(p == x))
}
# compute number of obs in training set with value x for var p and class c
npxc <- function(p, x, C, c){
      length(intersect(which(p == x), which(C == c)))
}
# compute conditional probability of class c given value of x for var p
Ppxc <- function(p, x, C, c){
      npxc(p, x, C, c)/npx(p, x)
}
