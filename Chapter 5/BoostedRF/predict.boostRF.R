predict.boostRF <- function(model, newdata){
    ylabels <-  mod[[1]][[1]]$levels
    M <- length(model$weight)
    classifier <- 0
    for(m in 1:M){
        gpred.et <- as.numeric(predict(model$et[[m]],newdata=newdata))-1
        g2 <- 2*gpred.et-1
        classifier <- classifier + model$weight[m]*g2
    }
    return(ifelse(sign(classifier) < 0, ylabels[1], ylabels[2]))
}