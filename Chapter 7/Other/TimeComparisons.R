# ----------------------------------------------------------------------------------------
######################################
# Time comparison with RF and rf-wv3
#####################################
# ----------------------------------------------------------------------------------------
library(mlbench)
# simulate twonorm data for different values of N and p
# experiment with growth in N
genExp1Data <- function(Ngrid){
      tnTrain <- data.frame(mlbench.twonorm(100, d=5))
      tnTest <- list()
      for(i in 1:length(Ngrid)){
            set.seed(i)
            tnTest[[i]] <- data.frame(mlbench.twonorm(Ngrid[i], d=5))
      }
      list(tnTrain, tnTest)
}
# Experiment with growth in p
genExp2Data <- function(pGrid){
      tnTrain <- list()
      tnTest <- list()
      for(i in 1:length(pGrid)){
            set.seed(i)
            tnTrain[[i]] <- data.frame(mlbench.twonorm(100, d=pGrid[i]))
            tnTest[[i]] <- data.frame(mlbench.twonorm(20, d=pGrid[i]))
      }
      list(tnTrain, tnTest)
}
exp1Data <- genExp1Data(seq(1, 100, by=10))
exp2Data <- genExp2Data(seq(2, 20, by=2))
# perform N experiment
library(caret)
rfTime <- NULL
rfwv3Time <- NULL

# Parallel computing
library(doMC)
registerDoMC(cores=3)

# run experiment 1
for(i in 1:length(exp1Data[[2]])){
      start <- Sys.time()
      rf <- train(classes~., data=exp1Data[[1]], method="rf", trControl=tControl, tuneGrid=tgrid)
      preds <- predict(rf, exp1Data[[2]][[i]])
      rfTime[i] <- as.numeric(Sys.time() - start)
      start <- Sys.time()
      rf <- train(classes~., data=exp1Data[[1]], method="rf", trControl=tControl, tuneGrid=tgrid)
      preds <- predict.HVDM(rf, exp1Data[[2]][[i]], k=nrow(exp1Data[[1]]))
      rfwv3Time[i] <- as.numeric(Sys.time() - start)
}

rfTime2 <- NULL
rfwv3Time2 <- NULL
tControl <- trainControl(method="none")
# run experiment 2
for(i in 1:length(exp2Data[[2]])){
      start <- Sys.time()
      rf <- train(classes~., data=exp2Data[[1]][[i]], method="rf", trControl=tControl, 
                  tuneGrid=data.frame(mtry=floor(sqrt(ncol(exp2Data[[1]][[i]])))))
      preds <- predict(rf, exp2Data[[2]][[i]])
      rfTime2[i] <- as.numeric(Sys.time() - start)
      start <- Sys.time()
      rf <- train(classes~., data=exp2Data[[1]][[i]], method="rf", trControl=tControl, 
                  tuneGrid=data.frame(mtry=floor(sqrt(ncol(exp2Data[[1]][[i]])))))
      preds <- predict.HVDM(rf, exp2Data[[2]][[i]], k=nrow(exp2Data[[1]][[1]]))
      rfwv3Time2[i] <- as.numeric(Sys.time() - start)
}

# plot comparisons with increase in N
time1Data <- data.frame(N=seq(1, 100, by=10), time=c(rfwv3Time, rfTime), Algorithm=c(rep("rf-wv3", 10), rep("rf", 10)))
ggplot(time1Data, aes(x=N, y=time, col=Algorithm)) + geom_line() + geom_point() +
      theme_bw() + xlab("Number of test observations (p fixed at 5)") + ylab("Prediction time (in secs)") +
      scale_color_manual(values=c("skyblue", "red")) + theme(legend.position=c(0.2, 0.7))

# plot comparisons with increase in p
time2Data <- data.frame(p=seq(2, 20, by=2), time=c(rfwv3Time2, rfTime2), Algorithm=c(rep("rf-wv3", 10), rep("rf", 10)))
ggplot(time2Data, aes(x=p, y=time, col=Algorithm)) + geom_line() + geom_point() +
      theme_bw() + xlab("Number of input variables (N fixed at 20)") + ylab("Prediction time (in secs)") +
      scale_color_manual(values=c("skyblue", "red")) + theme(legend.position=c(0.2, 0.7))


 



