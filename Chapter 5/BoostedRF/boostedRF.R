###########################
# boosted random forests
###########################

boostRF <- function(formula, data, mtry=floor(sqrt(ncol(data)-1)), ntree=100, numRanCuts=5, tune=FALSE){
    
    ########################################################################
    #This program is an implementation of the AdaBoost algorithm.          #
    #                                                                      #
    #Arguments:                                                            #
    #1) x: Training data matrix.                                           #
    #2) y: Training reponse observations.                                  #
    #3) xtest: Test data matrix.                                           #
    #4) ytest: Test response observations.                                 #
    #5) newy: Boolean value indicating whether ytest is given or not.      #
    #6) M: Number of iterations.                                           #
    #7) plot: Boolean value indicating whether a plot is requested or not. #
    #8) int: Intercept term                                                #
    #                                                                      #
    #Return: A list containing the classification of the given test data,  #
    #        a confusion matrix and the test error rate if ytest was given.#
    ########################################################################
    
    #Import rpart package (Breiman et al., 1984).
    #library(rpart)
    library(extraTrees)
    library(caret)
    
    #Initialise variables
    yname <- as.character(formula[[2]])
    y <- data[,yname]
    x <- data[, !names(data) %in% yname]
    model <- list()
    levels <- levels(y)
    n <- length(y)
    I <- numeric(length=n)
    e <- NULL
    a <- NULL
    #control <- rpart.control(maxdepth=1)
    
    #STEP 1: Initialise training data weights 
    W <<- rep(1/n,n)
    
    #STEP 2: for m=1 to M
    for(m in 1:ntree){
        
        #(a) Fit the classifier to the training data with weights W
        if(tune==FALSE){
            tuneControl <- data.frame(mtry=mtry, numRandomCuts=numRanCuts)
            fitControl <- trainControl(method="none")
            model$et[[m]] <- train(formula, data=data,method="extraTrees",
                                   trControl=fitControl, tuneGrid=tuneControl,
                                   weights=W, ntree=1, nodesize=n/3)
        } else {
            fitControl <- trainControl(method="cv", number=5)
            model$et[[m]] <- train(formula, data=data,method="extraTrees",
                                   trControl=fitControl, weights=W, ntree=1, nodesize = n/2)
        }
        pred <- predict(model$et[[m]], x)
        I <- as.numeric(pred!=y)
        
        #(b) Compute the weighted misclassification rate
        e[m] <- sum(W*I)/sum(W) 
        
        #(c) Compute the weight associated with the classifier at 
        #    itertaion m
        model$weight[m] <- log((1-e[m])/e[m])
        
        #(d) Update the weights
        W <- W*exp(model$weight[m]*I)
    }
    #End loop
    return(model)
}