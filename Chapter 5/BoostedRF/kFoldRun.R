kFoldRun <- function(x,y,k=5,M=100, method="boostRF", seed=1, ...){
      # Test preparations
      library(caret)
      foldError <- NULL
      CVError <- NULL
      
      # create folds
      set.seed(seed)
      folds <- createFolds(y, k=k, list = FALSE)
      
      # perform k-fold CV
      for(i in 1:k){
            trainFolds <- folds != i
            if(method=="boostRF"){
                  model <- boostRF(x=x[trainFolds,],y=y[trainFolds],M=M, ...)
                  preds <- predict.boostRF(model, x[!trainFolds,])
                  foldError[i] <- mean(preds != 2*(as.numeric(y[!trainFolds])-1)-1) 
            } else {
                  model <- train(y[trainFolds]~., data=x[trainFolds,], method=method, ...)  
                  preds <- predict(model, x[!trainFolds,])
                  foldError[i] <- mean(preds != y[!trainFolds]) 
            }
           
      }
      CVError <- mean(foldError)
      return(list(avgCVError=CVError, perFoldError=foldError))
}

