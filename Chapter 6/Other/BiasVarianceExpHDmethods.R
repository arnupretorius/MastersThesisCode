##########################################################
# BIAS VARIANCE EXPERMIMENT for HIGH-DIMENSIONAL methods #
##########################################################
# ----------------------------------------------------------------------------------------
library(mlbench)
library(caret)
library(ggplot2)
majVote <- function(x){names(which.max(table(x)))}
nTrain <- 400
nTest <- 1000
Models <- factor(rep(c("Forest-RI", "WSRF", "RRFglobal", "RRF", "svmLinear", "NSC"), each=6),
                 level=c("Forest-RI", "WSRF", "RRFglobal", "RRF", "svmLinear", "NSC"))

# performs computations in parallel
library(doSNOW)
cl <- makeCluster(3, type="SOCK")
registerDoSNOW(cl)

# MAIN EXPERIMENT FUNCTIONS
runBiasVarSimulationHD <- function(trainingSets, simTest, BayesPreds){
      
      # find optimal tuning parameters for each method on each data set using 10-fold CV
      fitControl <- trainControl(method = "cv", number = 10)
      rfparaGrid <- expand.grid(mtry=c(1, 15, floor(sqrt(ncol(simTest)-1)), 500))
      wsrfparaGrid <- expand.grid(mtry=c(1, 15, floor(sqrt(ncol(simTest)-1)), 500))
      rrfgparaGrid <- expand.grid(mtry=c(1, 15, floor(sqrt(ncol(simTest)-1)), 500), coefReg=c(0.1, 0.5, 0.9, 1))
      rrfparaGrid <- expand.grid(mtry=c(1, 15, floor(sqrt(ncol(simTest)-1)), 500), coefReg=c(0.1, 0.5, 0.9, 1),
                                 coefImp=c(0.1, 0.5, 0.9, 1))
      svmparaGrid <- expand.grid(C=c(0.1, 0.3, 0.5, 0.7, 0.9, 1))
      
      # random forest model
      sim.RF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                      method="rf", paraGrid = rfparaGrid, tControl=fitControl,
                                      BayesPreds=BayesPreds, ntree=200)
      # weighted subspace random forest model
      sim.WSRF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                        method="wsrf", paraGrid = wsrfparaGrid, tControl=fitControl,
                                        BayesPreds=BayesPreds, ntrees=200)
      # global regularised random forest model
      sim.RRFg <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                        method="RRFglobal", paraGrid = rrfgparaGrid,
                                        tControl=fitControl, BayesPreds=BayesPreds, ntree=200)
      # reguralised random forest model
      sim.RRF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                       method="RRF", paraGrid = rrfparaGrid, tControl=fitControl,
                                       BayesPreds=BayesPreds, ntree=200)
      # Support vector machine model
      sim.SVM <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                       method="svmLinear", paraGrid = svmparaGrid, tControl=fitControl,
                                       BayesPreds=BayesPreds)
      # Nearest shrunkern centroids model
      sim.NSC <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                       method="pam", paraGrid = NULL, tControl=fitControl,
                                       BayesPreds=BayesPreds)
      list(results=rbind(sim.RF$results, sim.WSRF$results, sim.RRFg$results, sim.RRF$results, sim.SVM$results, sim.NSC$results),
           tuneValues=list(sim.RF$tuneValues, sim.WSRF$tuneValues, sim.RRFg$tuneValues,
                           sim.RRF$tuneValues, sim.SVM$tuneValues, sim.NSC$tuneValues))
      
}
# ----------------------------------------------------------------------------------------
#############################
# twonorm simulation data
#############################
# ----------------------------------------------------------------------------------------
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.twonorm(400, d=1500)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.twonorm(1000, d=1500)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
twonormResults <- runBiasVarSimulationHD(trainingSets, simTest, bayesclass(test))
twonormResults$results$model <- Models
saveRDS(twonormResults, "twonormResultsTune.rda")
ggplot(data=twonormResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Twonorm (1500, 2)")
# ----------------------------------------------------------------------------------------
#############################
# threenorm simulation data
#############################
# ----------------------------------------------------------------------------------------
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.threenorm(400, d=1500)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.threenorm(1000, d=1500)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
threenormResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
threenormResults$results$model <- Models
saveRDS(threenormResults$results, "threenormResultsTune.rda")
ggplot(data=threenormResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Threenorm (1500, 2)")
# ----------------------------------------------------------------------------------------
#############################
# ringnorm simulation data
#############################
# ----------------------------------------------------------------------------------------
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.ringnorm(400, d=1500)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.ringnorm(1000, d=1500)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
ringnormResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
ringnormResults$results$model <- Models
saveRDS(ringnormResults, "ringnormResultsTune.rda")
ggplot(data=ringnormResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Ringnorm (1500, 2)")
# ----------------------------------------------------------------------------------------
#############################
# circle simulation data
#############################
# ----------------------------------------------------------------------------------------
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.circle(400, d=1500)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.circle(1000, d=1500)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
circleResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
circleResults$results$model <- Models
saveRDS(circleResults, "circleResultsTune.rda")
ggplot(data=circleResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position = "identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "bottom", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Circle (1500, 2)")
# ----------------------------------------------------------------------------------------
#######################
# Designed scenarios  #
#######################
# ----------------------------------------------------------------------------------------
####################################################################################
# J = 2
####################################################################################
# ----------------------------------------------------------------------------------------
# simluating training data sets
q <- 0.15
J <- 2
simData1 <- generateMeasedata(nTrain=100, nTest=200, J=2, p=1000)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup5Results <- runBiasVarSimulationHD(trainingSets, simTest, BayesClasses)
setup5Results$results$model <- Models
saveRDS(setup5Results, "setup5ResultsHD.rda")
ggplot(data=setup5Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 5")

# ----------------------------------------------------------------------------------------
####################################################################################
# J = 25
####################################################################################
# ----------------------------------------------------------------------------------------
J <- 25
# simluating training data sets
simData1 <- generateMeasedata(nTrain=100, nTest=200, J=25, p=1000)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup6Results <- runBiasVarSimulationHD(trainingSets, simTest, BayesClasses)
setup6Results$results$model <- Models
saveRDS(setup6Results, "setup6ResultsHD.rda")
ggplot(data=setup6Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 6")

# ----------------------------------------------------------------------------------------
####################################################################################
# J = 100
####################################################################################
# ----------------------------------------------------------------------------------------
J <- 100
# simluating training data sets
simData1 <- generateMeasedata(nTrain=100, nTest=200, J=100, p=1000)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup7Results <- runBiasVarSimulationHD(trainingSets, simTest, BayesClasses)
setup7Results$results$model <- Models
saveRDS(setup7Results, "setup7ResultsHD.rda")
ggplot(data=setup7Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 7")

# ----------------------------------------------------------------------------------------
####################################################################################
# J = 500
####################################################################################
# ----------------------------------------------------------------------------------------
J <- 500
# simluating training data sets
simData1 <- generateMeasedata(nTrain=100, nTest=200, J=500, p=1000)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup8Results <- runBiasVarSimulationHD(trainingSets, simTest, BayesClasses)
setup8Results$results$model <- Models
saveRDS(setup8Results, "setup8ResultsHD.rda")
ggplot(data=setup8Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 8")

# ----------------------------------------------------------------------------------------
