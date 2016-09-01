##############################################
# Chapter 1: Random Rotation Forests R package
##############################################

# Check for missing packages and install if missing
list.of.packages <- c("devtools", "caret", "randomForest", "obliqueRF")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
load <- lapply(list.of.packages, require, character.only = TRUE)

# download and load random rotation forests package
if("RRotF" %in% installed.packages()[,"Package"] == FALSE){
      library(devtools)
      # Github profile: Arnu Pretorius
      install_github("arnupretorius/RRotF")
}
library(RRotF)

################################
# Random Rotation Forest (RRotF)
################################
# ---------------------------------------------------------------------------
RRotF <- function (x, y, K = round(ncol(x)/3, 0), L = 10, mtry=floor(sqrt(ncol(x))), model="log", ...){
      
      require(randomForest)
      require(obliqueRF)
      x <- data.frame(sapply(x, as.numeric))
      y <- factor(as.numeric(y)-1)
      while (ncol(x)%%K != 0) {
            K <- K - 1
      }
      M <- round(ncol(x)/K)
      predicted <- list()
      fit <- numeric()
      Ri <- list()
      Ria <- list()
      fit <- list()
      predicted <- matrix(NA, nrow = nrow(x), ncol = L)
      subsets <- list()
      SelectedClass <- list()
      IndependentsClassSubset <- list()
      IndependentsClassSubsetBoot <- list()
      pcdata <- list()
      loadings <- list()
      for (i in 1:L) {
            Independents <- x[, sample(1:ncol(x), ncol(x))]
            n <- 0
            subsets[[i]] <- list()
            SelectedClass[[i]] <- list()
            IndependentsClassSubset[[i]] <- list()
            IndependentsClassSubsetBoot[[i]] <- list()
            pcdata[[i]] <- list()
            loadings[[i]] <- list()
            for (j in seq(1, K)) {
                  n <- n + M
                  subsets[[i]][[j]] <- data.frame(Independents[, (n -
                                                                        (M - 1)):n], y)
                  SelectedClass[[i]][[j]] <- as.integer(sample(levels(as.factor(y)),
                                                               1))
                  IndependentsClassSubset[[i]][[j]] <- subsets[[i]][[j]][subsets[[i]][[j]]$y ==
                                                                               SelectedClass[[i]][[j]], ]
                  IndependentsClassSubsetBoot[[i]][[j]] <- IndependentsClassSubset[[i]][[j]][sample(1:dim(IndependentsClassSubset[[i]][[j]])[1],
                                                                                                    round(0.75 * nrow(IndependentsClassSubset[[i]][[j]])),
                                                                                                    replace = TRUE), ]
                  pcdata[[i]][[j]] <- princomp(IndependentsClassSubsetBoot[[i]][[j]][,
                                                                                     !colnames(IndependentsClassSubsetBoot[[i]][[j]]) %in%
                                                                                           "y"])
                  loadings[[i]][[j]] <- pcdata[[i]][[j]]$loadings[,
                                                                  ]
                  colnames(loadings[[i]][[j]]) <- dimnames(loadings[[i]][[j]])[[1]]
                  loadings[[i]][[j]] <- data.frame(dimnames(loadings[[i]][[j]])[[1]],
                                                   loadings[[i]][[j]])
                  colnames(loadings[[i]][[j]])[1] <- "rowID"
            }
            Ri[[i]] <- Reduce(function(x, y) merge(x, y, by = "rowID",
                                                   all = TRUE), loadings[[i]])
            Ri[[i]][is.na(Ri[[i]])] <- 0
            Ria[[i]] <- Ri[[i]][order(match(Ri[[i]]$rowID, colnames(x))),
                                order(match(colnames(Ri[[i]]), colnames(x)))]
            rownames(Ria[[i]]) <- Ria[[i]]$rowID
            Ria[[i]]$rowID <- NULL
            finalx <- data.frame(as.matrix(x) %*% as.matrix(Ria[[i]]))
            final <- data.frame(finalx, y)
            if(model=="rf"){
                  fit[[i]] <- randomForest(y ~ ., data = final, mtry=mtry, ntree=1,
                                           ...)
            } else if(model %in% c("ridge", "pls", "log", "svm", "rnd")){
                  capture.output(fit[[i]] <- obliqueRF(x = as.matrix(finalx), y=as.numeric(y), mtry=mtry, ntree=1,
                                                       training_method=model, verbose = FALSE, ...))
            } else {
                  stop("Argument 'model' not a valid model type.")
            }
      }
      res <- list(models = fit, loadings = Ria, columnnames = colnames(x), mod=model)
      class(res) <- "RRotF"
      res
}

# prediction function
predict.RRotF <- function (object, newdata, type="class"){
      if(class(object) != "RRotF"){
            stop("Object must be of class 'RRotF'")
      }
      newdata <- data.frame(sapply(newdata, as.numeric))
      if (!identical(colnames(newdata), object$columnnames))
            stop("Variable names and/or order of variables in newdata is not identical to training set. Please check if variables are exactly the same in both sets.")
      predicted <- matrix(NA, nrow = nrow(newdata), ncol = length(object$models))
      for (i in 1:length(object$models)) {
            final <- data.frame(as.matrix(newdata) %*% as.matrix(object$loadings[[i]]))
            predicted[, i] <- predict(object$models[[i]], final,
                                      type = "prob")[, 2]
      }
      if (type=="class") {
            ifelse(rowMeans(predicted) > 0.5, 1, 0)
      }
      else if(type=="prob") {
            rowMeans(predicted)
      } else {
            stop("Argument 'type' must be either 'class' or 'prob'")
      }
}

# parameter tuning function
findOptimalTuning <- function(x, y, k=10, paraGrid, verboset=TRUE,  ...){
      CVerrorVec <- NULL
      nconfig <- nrow(paraGrid)
      for(i in 1:nconfig){
            if(verboset){
                  print(paste("para config",i, "out of", nconfig))
            }
            K <- paraGrid[i,1]
            L <- paraGrid[i,2]
            mtry <- paraGrid[i,3]
            CVerrorVec[i] <- kFoldRun(x=x, y=y, k=k, K=K, L=L, mtry=mtry, ...)[[1]]
      }
      optParaIndex <- which(CVerrorVec == min(CVerrorVec))[1]
      list(optTuneVals=paraGrid[optParaIndex,], tuneValErrors=data.frame(paraGrid,CVerrorVec))
}

# k-folc cross validation function
kFoldRun <- function(x,y,k=10, seed=1, verbose=TRUE, ...){
      # Test preparations
      library(caret)
      foldError <- NULL
      CVError <- NULL
      
      # create folds
      set.seed(seed)
      folds <- createFolds(y, k=k, list = FALSE)
      
      # perform k-fold CV
      for(i in 1:k){
            if(verbose){
                  print(paste("fold:", i))
            }
            trainFolds <- folds != i
            model <- RRotF(x=x[trainFolds,],y=y[trainFolds], ...)
            preds <- predict.RRotF(model, x[!trainFolds,])
            foldError[i] <- mean(preds != as.numeric(factor(y[!trainFolds]))-1)
      }
      CVError <- mean(foldError)
      return(list(avgCVError=CVError, perFoldError=foldError))
}
# ---------------------------------------------------------------------------