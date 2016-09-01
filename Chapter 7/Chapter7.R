#####################################
# CHAPTER 7: Comparing Random Forests
#####################################

# Check for missing packages and install if missing
list.of.packages <- c("dplyr","latex2exp", "mlbench", "ggplot2", "caret", "doSNOW", "lattice",
                      "obliqueRF", "MASS", "stargazer", "rotationForest", "randomForest",
                      "scmamp", "surv2sampleComp", "ElemStatLearn", "hmeasure")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
load <- lapply(list.of.packages, require, character.only = TRUE)

# required packages from bioconductor for scmamp package
source("https://bioconductor.org/biocLite.R")
biocLite("graph")
n
biocLite("Rgraphviz")
n

# download and load random rotation forests package
if("RRotF" %in% installed.packages()[,"Package"] == FALSE){
      library(devtools)
      install_github("arnupretorius/RRotF")
}
library(RRotF)

#######################################################################
# Table 7.1: Available software for random forests in the R programming 
# language
#######################################################################
# ---------------------------------------------------------------------------
data <- read.csv("RFvariantsData.csv")
data <- arrange(data, year)
psals <- paste(data[,2], " (", data[,3], ")", sep="")
pks <- c("unavailable", "unavailable", "ipred", "unavailable", "unavailable",
         "unavailable", "unavailable", "unavailable", "randomForest",
         "unavailable", "unavailable", "caret", "unavailable", "extraTrees",
         "unavailable", "unavailable", "unavailable", "unavailable", "unavailable",
         "party", "unavailable", "unavailable", "obliqueRF", "unavailable",
         "RRF", "wsrf", "unavailable", "unavailable", "RRF", "RRF", "unavailable",
         "unavailable", "unavailable", "unavailable", "unavailable", "unavailable",
         "unavailable")
softTable <- data.frame("Proposals"=psals, "R package"=pks)
# make latex table
stargazer(softTable, summary = FALSE, rownames=FALSE)
# ---------------------------------------------------------------------------
#################################################################
# Table B.1: Papers considered in the meta-analysis. (Appendix B)
#################################################################
# ---------------------------------------------------------------------------
data <- read.csv("RFComparisonsData.csv")
lop <- arrange(data, year) %>% select(paper_title, author, year, journal) %>%
      group_by(paper_title) %>% distinct()
lop <- as.data.frame(lop)
colnames(lop) <- c("Paper title", "Author(s)", "Year", "Journal")
stargazer(lop, summary = FALSE, rownames = FALSE)
# ---------------------------------------------------------------------------
#####################################################################
# Table 7.3: Algorithm performance measures for binary classification
#####################################################################
# ---------------------------------------------------------------------------
measures <- c("Error", "Accuracy", "Sensitivity", "Specificity", "Precision",
              "Kappa", "AUC", "F-score", "H-measure")
formula <- 1:9
aps <- c("Balanced data", "Balanced data", "Skew data/minority class", "Skew data",
         "Skew data", "Skew data", "Not recommended", "Skew data/minority class",
         "Balanced data/skew data")
measFrame <- data.frame("Performance measure"=measures, "Calculation"=formula,
                        "Appropriate scenario"=aps)
stargazer(measFrame, summary = FALSE, rownames = FALSE)
# ---------------------------------------------------------------------------
############################################################################
# Figure 7.2: Performance estimation method used in the papers considered in 
# the meta-analysis.
############################################################################
# ---------------------------------------------------------------------------
# load data
data <- read.csv("RFComparisonsData.csv")
evalMeth <- data %>% select(paper_title, evaluation)
evalMeth <- unique(evalMeth)
evalMeth <- as.data.frame(evalMeth %>% count(evaluation))
evalMeth <- evalMeth[order(evalMeth$n, decreasing = TRUE),]

# mark which estimation methods are not "reliable"
notIndex <- c(4, 9, 22, 27)
grp <- rep("Reliable", 27)
grp[notIndex] <- "Not reliable"
evalMeth$grp <- grp

# plot estimation methods used
ggplot(evalMeth, aes(x=evaluation, y=n, fill=grp)) + geom_bar(stat="identity") +
      scale_x_discrete(limits=evalMeth$evaluation) +
      scale_fill_manual(name="" ,values=c("darkgreen", "skyblue")) +
      theme_bw() + ylab("#Papers") + xlab("Estimation method") +
      theme(legend.position = c(0.9,0.7), axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
# ---------------------------------------------------------------------------
#########################################################################
# Figure 7.5: Reported error rates for Breiman’s Forest-RI on the top ten 
# most popular data sets used in papers.
#########################################################################
# ---------------------------------------------------------------------------
# load data
data <- read.csv("RFComparisonsData.csv")

# compute top used data sets across papers
ds <- data %>% select(paper_title, dataset)
allDS <- unique(ds$dataset)
allDSCount <- rep(0, length(allDS))
dsSplit <- split(ds, ds$paper_title)
for(i in 1:length(dsSplit)){
      dsPerPaper <-  unique(dsSplit[[i]][,2])
      for(j in 1:length(dsPerPaper)){
            index <- which(allDS == dsPerPaper[j])
            allDSCount[index] <- allDSCount[index] + 1
      }
}
dsFrame <- data.frame(dataset=allDS, freqUsed=allDSCount)
dsFrame <- dsFrame[order(dsFrame$freqUsed, decreasing = TRUE),]

# get dataset characteristics
dataChar <- data %>% select(dataset, dataset_size, num_inputs, classes) %>% distinct()
dataChar <- merge(dsFrame, dataChar)
dataChar <- dataChar[order(dataChar$freqUsed, decreasing = TRUE),]

# plot variatability of RF on above data sets
rfDataSets <- factor(unique(dataChar$dataset)[1:10])
keepIndex <- NULL
count <- 1
for(i in 1:nrow(data)){
      if(data[i,]$method == "rf" && data[i,]$dataset %in% rfDataSets){
            keepIndex[count] <- i
            count <- count + 1
      }
}
rfCompData <- data[keepIndex,] %>% select(paper_title, dataset, method, error)
rfCompData <- rfCompData[order(rfCompData$dataset),]
# remove other lymphoma
lympRemove <- NULL
count <- 1
for(i in 1:nrow(rfCompData)){
      if(rfCompData[i,]$dataset == "lymphoma" && rfCompData[i,]$error > 3){
            lympRemove[count] <- i
            count <- count + 1
      }
}
rfCompData <- rfCompData[-lympRemove,]
rfCompData <- rfCompData[-101, ] # remove gross outlier in: On extreme pruning (possibly reported error instead of acc)
# plot errors
ggplot(rfCompData, aes(y=error, x=dataset)) + geom_boxplot(fill="skyblue", outlier.colour = "red")+
      theme_bw() + ylab("Reported error rates") + xlab("Benchmark data set")+
      scale_x_discrete(limits=unique(dataChar$dataset)[1:10])+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

# outlier breast cancer: On extreme pruning (Possibly not the same breast cancer dataset)
# outlier glass: Tripoli et al. paper
# outlier sonar: On extreme pruning (possibly reported error instead of acc)
# ---------------------------------------------------------------------------
############################################################
# In text: Omnibus p-val for Forest-RI over different papers
############################################################
# ---------------------------------------------------------------------------
# test between random forests from different papers
lop <- arrange(data, paper_title) %>% select(paper_title, dataset, method, error)
lop <- filter(lop, lop$method == "rf")
lop$acc <- round(100-lop$error,3)

# top 10 used data sets for Forest-RI
topDataset <- factor(dsFrame[1:15,1])
papers <- factor(unique(lop$paper_title))

# compute all combinations
allcomb <- combn(1:15, 10, simplify = FALSE)
candidateList <- list()
canL <- NULL
# find combinations of 15 choose 10 such that largest number of rfs can be compared
for (k in 1:length(allcomb)) {
      datasetTop10 <- topDataset[allcomb[[k]]]
      lop10 <- filter(lop, lop$dataset %in% datasetTop10)
      splitlop <- split(lop10, factor(lop10$dataset))
      candidatePapers <- NULL
      check <- 0
      count <- 1
      for(i in 1:length(papers)){
            for(j in 1:length(splitlop)){
                  papersPresent <- factor(splitlop[[j]][,1])
                  if(!(papers[i] %in% papersPresent)){
                        check <- 1
                  }
            }
            if(check == 0){
                  candidatePapers[count] <- as.character(papers[i])
                  count <- count + 1
            } else {
                  check <- 0
            }
      }
      candidateList[[k]] <- candidatePapers
      canL[[k]] <- length(candidatePapers)
}

# find combination sharing the maximum number of datasets
maxIndex <- which(canL == max(canL))[1]
# list of papers
candidateList[[maxIndex]]

# create compare matrix
dsets <- factor(topDataset[allcomb[[maxIndex]]])
filterIndex <- sapply(lop$dataset, function(x){ifelse(x %in% dsets, TRUE, FALSE)})
lop <- lop[filterIndex,]
lopSplit <- split(lop, factor(lop$dataset))
compareMat <- matrix(0, nrow=length(dsets), ncol=length(papers))
rownames(compareMat) <- names(lopSplit)
colnames(compareMat) <- papers
for(i in 1:length(lopSplit)){
      for(j in 1:nrow(lopSplit[[i]])){
            compareMat[i, which(papers == as.character(lopSplit[[i]]$paper_title[j]))] <- lopSplit[[i]]$acc[j]
      }
}
# prune to include papers containing all top ten data sets
keepIndex <- apply(compareMat, 2, function(x){
      ifelse(length(which(x == 0)) == 0, TRUE, FALSE)
})
rCompareMat <- compareMat[,keepIndex]

# compute omnibus tests (#algorithm < 5)
imanDavenportTest(rCompareMat)
friedmanAlignedRanksTest(rCompareMat)
quadeTest(rCompareMat)
# ---------------------------------------------------------------------------
########################################################################
# Figure 7.6: Methods used to compare different algorithms over multiple 
# data sets in the papers considered for the meta-analysis.
########################################################################
# ---------------------------------------------------------------------------
# plot evaluation method used
evalsData <- data %>% select(paper_title, comparison) %>% distinct()
lims <- unique(evalsData$comparison)[c(1,2,4,3,5,6,7,8)]
ggplot(evalsData, aes(x=comparison)) + geom_bar(fill="darkgreen") +
      xlab("Comparison method") + ylab("#Papers")+
      theme_bw()+
      scale_x_discrete(limits=lims)+
      scale_y_continuous(breaks = seq(0, 20, by = 2))+
      theme(axis.text.x = element_text(angle = 10, vjust = 1, hjust = 1))
# ---------------------------------------------------------------------------
########################################################################
# Figure 7.6: Methods used to compare different algorithms over multiple 
# data sets in the papers considered for the meta-analysis.
########################################################################
# ---------------------------------------------------------------------------
# Redo analyses using omnibus and post-hoc tests
dataSplit <- split(data, factor(data$paper_title))
pvals <- NULL
checkPhTest <- NULL
phTests <- list()

# for each paper build a compare matrix and compute the Ivan-Davenport test p-value
for(k in 1:length(dataSplit)){
      lop <- arrange(dataSplit[[k]], dataset) %>% select(dataset, method, error)
      lop <- as.data.frame(summarise(group_by(lop, dataset, method), mean(error)))
      lop$acc <- round(100-lop$`mean(error)`,3)

      # create compare matrix
      lopSplit <- split(lop, factor(lop$dataset))
      dsets <- unique(lop$dataset)
      methods <- unique(lop$method)
      compareMat <- matrix(0, nrow=length(dsets), ncol=length(methods))
      rownames(compareMat) <- dsets
      colnames(compareMat) <- methods
      for(i in 1:length(lopSplit)){
            for(j in 1:nrow(lopSplit[[i]])){
                  compareMat[i, which(methods == as.character(lopSplit[[i]]$method[j]))] <- lopSplit[[i]]$acc[j]
            }
      }
      pvals[k] <- round(imanDavenportTest(compareMat)[[3]], 3)
      if(!is.na(pvals[k]) && pvals[k] < 0.05 && "rf" %in% colnames(compareMat)){
            phTests[[k]] <- postHocTest(compareMat, test = "friedman",
                                        control = "rf", correct = "finner", alpha = 0.05)
            checkPhTest[k] <- ifelse(length(which(phTests[[k]]$corrected.pval < 0.05)) == 0, TRUE, FALSE)
      }
}

# paper where no significant result was found
omnibusFailed <- which(pvals > 0.05)
PHvsRFFailed <- which(checkPhTest)
nonSigResult <- c(omnibusFailed, PHvsRFFailed)

# plot pvals
pvals[PHvsRFFailed] <- 0.05
grp <- rep("Omnibus: Iman-Davenport", length(pvals))
grp[PHvsRFFailed] <- "Post-hoc: Finner (Forest-RI as control)"
grp <- factor(grp)
pvalData <- data.frame(pv = pvals[-29], Test=grp[-29])
ggplot(pvalData, aes(x=1:nrow(pvalData), y=pv, fill=Test)) + geom_bar(stat="identity") +
      theme_bw() + xlab("'Papers' (no names given)") +
      ylab("p-value") + geom_hline(yintercept = 0.05, col="red", linetype="dashed") +
      scale_fill_manual(values=c("darkgreen", "skyblue"))+
      scale_x_continuous(breaks = seq(from=1,to=34, by=1))+
      annotate("text", x=0.5, y=0.09, label = "alpha==0.05 ",parse = TRUE)+
      theme(legend.position=c(0.2,0.8))
# ---------------------------------------------------------------------------
############################################
# In text: Omnibus p-val for Breiman (2001a)
############################################
# ---------------------------------------------------------------------------
# Breiman RF paper omnibus p-val = 0.014!
# pairwise corrected p-values
phTests[[26]]$corrected.pval
# ---------------------------------------------------------------------------
#########################################################
# Figure 7.8: The adjusted ranks for all-round algorithms
#########################################################
# ---------------------------------------------------------------------------
# load data
data <- read.csv("RFComparisonsData.csv")

# remove observations that are not "reliable"
data <- filter(data, data$evaluation != "OOB")
data <- filter(data, data$evaluation != "1 run")
data <- filter(data, data$evaluation != "3-fold cv")
data <- filter(data, !is.na(data$evaluation))

# split between allround situations and high-dimensional situations
allround <- filter(data, data$situation == "allround")
HD <- filter(data, data$situation == "HD" | data$situation == "select-HD")

# ALLROUND methods 
lop <- arrange(allround, dataset) %>% select(dataset, method, error)
lop <- as.data.frame(summarise(group_by(lop, dataset, method), mean(error)))
lop$acc <- round(100-lop$`mean(error)`,3)

# create compare matrix
lopSplit <- split(lop, factor(lop$dataset))
dsets <- unique(lop$dataset)
methods <- unique(lop$method)
compareMat <- matrix(0, nrow=length(dsets), ncol=length(methods))
rownames(compareMat) <- dsets
colnames(compareMat) <- methods
for(i in 1:length(lopSplit)){
      for(j in 1:nrow(lopSplit[[i]])){
            compareMat[i, which(methods == as.character(lopSplit[[i]]$method[j]))] <- lopSplit[[i]]$acc[j]
      }
}
#remove rare data sets
removeRowIndex <- apply(compareMat, 1, function(x){
      ifelse(length(which(x != 0)) < 3, 1, 0)
})
removeColIndex <- apply(compareMat, 2, function(x){
      ifelse(length(which(x != 0)) < 10, 1, 0)
})
removeRowIndex <- which(removeRowIndex == 1)
removeColIndex <- which(removeColIndex == 1)
compareMat <- compareMat[-removeRowIndex,-removeColIndex]

# compute nomial ranks
rankMat <- apply(compareMat, 1, function(x){
      index <- which(x != 0)
      rankVec <- rank(-x[index], ties.method = "average")
      x[index] <- rankVec/length(rankVec)
      x
})
rankMat <- t(rankMat)

# adjust for number of datasets used
rankMat <- apply(rankMat, 2, function(x){
      prop <- length(which(x == 0))/length(x)
      x*prop
})

# compute average rank per method
avgRanks <- apply(rankMat, 2, function(x){
      index <- which(x != 0)
      mean(x[index])
})

# scale ranks
range2 = length(avgRanks) - 1
avgRanksStand = (avgRanks*range2) + 1

# sorted ranks
sortAvgRanks <- sort(avgRanksStand)

# plot sorted ranks
plotData <- data.frame(rank=sortAvgRanks, names=names(sortAvgRanks))
ggplot(plotData, aes(x=names, y=rank)) +
      geom_bar(stat="identity", fill="orange") + ylab("Adjusted Rank") +
      xlab("Algorithm") +
      scale_x_discrete(limits=names(sortAvgRanks))+ theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      ggtitle("All-round algorithms")
# ---------------------------------------------------------------------------
###########################################################################
# Figure 7.9: Results from comparing the top five all-round algorithms: Top 
# left: Kernel (Gaussian) density estimations based on accuracy. Top right: 
# Adjusted p-value matrix using the Shaffer static approach. Bottom: 
# Pairwise comparisons plot.
###########################################################################
# ---------------------------------------------------------------------------
# choose top 5 algorithms
top5Algs <- names(sortAvgRanks)[1:5]

# find data sets associated with each rf
rfsAlgs <- top5Algs
algsDatasets <- list()
algsDatLen <- NULL
temp <- NULL
for(i in 1:length(rfsAlgs)){
      for(j in 1:length(lopSplit)){
            if(rfsAlgs[i] %in% lopSplit[[j]][,2]){
                  temp <- c(temp, as.character(lopSplit[[j]][1,1]))
            }
      }
      algsDatasets[[i]] <- temp
      algsDatLen[i] <- length(temp)
      temp <- NULL
}
# (all algs come from same article => same 24 datasets)
# define reduced comare matrix
datasets <- algsDatasets[[2]]
cAlgs <- top5Algs
rowIndex <- sapply(rownames(compareMat) , function(x) x %in% datasets)
colIndex <- sapply(colnames(compareMat), function(x) x %in% cAlgs)
rCompareMat <- compareMat[rowIndex, colIndex]

# plot densities
plotDensities(rCompareMat) + xlab("Accuracy") + theme_bw() + theme(legend.position=c(0.2, 0.7))

# perform Iman-Devenport test
imanDavenportTest(rCompareMat)
# (significant dfference found => perform post-hoc test)

# perform Shaffer's static test
pvalsShaffer <- postHocTest(rCompareMat, test = "friedman", use.rank=TRUE, correct="shaffer")

# plot p-values and hypothesis tests
# Shaffer
plotPvalues(pvalsShaffer$corrected.pval) + ggtitle("Shaffer's static")
drawAlgorithmGraph(pvalsShaffer$corrected.pval, pvalsShaffer$summary, font.size = 5)
# ---------------------------------------------------------------------------
######################################################################
# Figure 7.10: Results from comparing the top five high-dimensional 
# algorithms: Top left: Adjusted ranks. Top right: Kernel (Gaussian) 
# density estimations based on accuracy. Bottom left: Adjusted p-value 
# matrix using the Shaffer static approach. Bottom right: Pairwise 
# comparisons plot.
######################################################################
# ---------------------------------------------------------------------------
# HD methods
lopHD <- arrange(HD, dataset) %>% select(dataset, method, error)
lopHD <- as.data.frame(summarise(group_by(lopHD, dataset, method), mean(error)))
lopHD$acc <- round(100-lopHD$`mean(error)`,3)

# create compare matrix
lopHDSplit <- split(lopHD, factor(lopHD$dataset))
dsets <- factor(unique(lopHD$dataset))
methods <- factor(unique(lopHD$method))
compareMat <- matrix(0, nrow=length(dsets), ncol=length(methods))
rownames(compareMat) <- dsets
colnames(compareMat) <- methods
for(i in 1:length(lopHDSplit)){
      for(j in 1:nrow(lopHDSplit[[i]])){
            compareMat[i, which(methods == as.character(lopHDSplit[[i]]$method[j]))] <- lopHDSplit[[i]]$acc[j]
      }
}
#remove rare data sets
removeRowIndex <- apply(compareMat, 1, function(x){
      ifelse(length(which(x != 0)) < 3, 1, 0)
})
#remove algorithms fitted to only a very small number of data sets
removeColIndex <- apply(compareMat, 2, function(x){
      ifelse(length(which(x != 0)) < 5, 1, 0)
})
removeRowIndex <- which(removeRowIndex == 1) #(none found)
removeColIndex <- which(removeColIndex == 1)
compareMat <- compareMat[, -removeColIndex]

# compute nomial ranks
rankMat <- apply(compareMat, 1, function(x){
      index <- which(x != 0)
      rankVec <- rank(-x[index], ties.method = "average")
      x[index] <- rankVec/length(rankVec)
      x
})
rankMat <- t(rankMat)

# adjust for number of datasets used
rankMat <- apply(rankMat, 2, function(x){
      prop <- length(which(x == 0))/length(x) 
      if(prop == 0){
            prop <- 1/(length(x)+1)
      }
      x*prop
})

# compute average rank per method
avgRanks <- apply(rankMat, 2, function(x){
      index <- which(x != 0)
      mean(x[index])
})

# scale ranks
range2 = length(avgRanks) - 1
avgRanksStand = (avgRanks*range2) + 1

# sorted ranks
sortAvgRanks <- sort(avgRanksStand)

# plot sorted ranks
plotData <- data.frame(rank=sortAvgRanks, names=names(sortAvgRanks))
ggplot(plotData, aes(x=names, y=rank)) +
      geom_bar(stat="identity", fill="orange") + ylab("Adjusted Rank") +
      xlab("Algorithm") +
      scale_x_discrete(limits=names(sortAvgRanks))+ theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      ggtitle("Hi-dimensional algorithms")

# choose top 5 algorithms
top5Algs <- names(sortAvgRanks)[1:5]

# find data sets associated with each rf
rfsAlgs <- top5Algs
algsDatasets <- list()
algsDatLen <- NULL
temp <- NULL
for(i in 1:length(rfsAlgs)){
      for(j in 1:length(lopHDSplit)){
            if(rfsAlgs[i] %in% lopHDSplit[[j]][,2]){
                  temp <- c(temp, as.character(lopHDSplit[[j]][1,1]))
            }
      }
      algsDatasets[[i]] <- temp
      algsDatLen[i] <- length(temp)
      temp <- NULL
}

# define reduced comare matrix
datasets <- intersect(intersect(algsDatasets[[1]], algsDatasets[[2]]), algsDatasets[[3]])
cAlgs <- top5Algs
rowIndex <- sapply(rownames(compareMat) , function(x) x %in% datasets)
colIndex <- sapply(colnames(compareMat), function(x) x %in% cAlgs)
rCompareMat <- compareMat[rowIndex, colIndex]

# plot densities
plotDensities(rCompareMat) + xlab("Accuracy") + theme_bw() + theme(legend.position=c(0.2, 0.7))

# perform Iman-Devenport test
imanDavenportTest(rCompareMat)
# (significant dfference found => perform post-hoc test)

# perform Shaffer's static test
pvalsShaffer <- postHocTest(rCompareMat, test = "friedman", use.rank=TRUE, correct="shaffer")

# plot p-values and hypothesis tests
# Shaffer
plotPvalues(pvalsShaffer$corrected.pval) + ggtitle("Shaffer's static")
drawAlgorithmGraph(pvalsShaffer$corrected.pval, pvalsShaffer$summary, font.size = 5)
# ---------------------------------------------------------------------------
###################
# Algorithm: rf-wv3
###################
# ---------------------------------------------------------------------------
exportList <- c("predTestHVDM", "distNew", "HVDM", "dp", "norm_diff", "norm_vdm", "npx", "npxc", "Ppxc")

# make predictions for all test cases
predict.HVDM <- function(model, Xtest, k){
      responseVarName <- as.character(model[[21]][[2]])
      Xtest <- Xtest[, !names(Xtest) %in% responseVarName]
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
# ---------------------------------------------------------------------------
###########################################################################
# Figure 7.11: Prediction time comparisons between Forest-RI and rf-wv3. 
# Left: Prediction time as a function of the number of test observations. 
# Right: Pre- diction time for twenty test observations for different sizes 
# of the input space.
###########################################################################
# ---------------------------------------------------------------------------
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
cl <- makeCluster(3, type="SOCK")
registerDoSNOW(cl)

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
# ---------------------------------------------------------------------------
#####################################################################
# Table 7.5 (RESULTS): Win/Tie analysis of benchmark performances for 
# oblique random rotation forests.
#####################################################################
# ---------------------------------------------------------------------------
# prepare the data
# data from UCI

#######################
# All-round data sets
#######################
# SAheart
data("SAheart")
colnames(SAheart)[10] <- "response"
SAheart$response <- factor(SAheart$response)
levels(SAheart$response) <- c("A", "B")
# spam
data("spam")
colnames(spam)[58] <- "response"
# Adult
adult <- read.csv("adult.data", header = FALSE)
colnames(adult)[15] <- "response"
# bank
bank <- read.table("bank-full.csv", sep=";", header = TRUE)
colnames(bank)[17] <- "response"
# bank note
bankNote <- read.csv("data_banknote_authentication.txt", header = FALSE)
colnames(bankNote)[5] <- "response"
bankNote$response <- factor(bankNote$response)
levels(bankNote$response) <- c("A", "B")
# popFailure
popFailure <- read.table("pop_failures.dat", header = TRUE)
colnames(popFailure)[21] <- "response"
popFailure$response <- factor(popFailure$response)
levels(popFailure$response) <- c("A", "B")
# wisconsin breaset cancer data
wdbc <- read.csv("wdbc.data", header = FALSE)
colnames(wdbc)[2] <- "response"
wdbc$response <- factor(wdbc$response)
# Breast cancer
data("BreastCancer")
BreastCancer <- BreastCancer[,-1]
BreastCancer <- BreastCancer[complete.cases(BreastCancer),]
colnames(BreastCancer)[10] <- "response"
# German credit
data("GermanCredit")
colnames(GermanCredit)[10] <- "response"
# Votes
data("HouseVotes84")
HouseVotes84 <- HouseVotes84[complete.cases(HouseVotes84),]
colnames(HouseVotes84)[1] <- "response"
# pima
data("PimaIndiansDiabetes")
colnames(PimaIndiansDiabetes)[9] <- "response"
# Sonar
data("Sonar")
colnames(Sonar)[61] <- "response"

# create benchmark dataset list
mlbList <- list(adult=adult, bank=bank, bankNote=bankNote, breastCancer=BreastCancer, pima=PimaIndiansDiabetes,
                germandCredit=GermanCredit, popFailure=popFailure, saheart=SAheart, sonar=Sonar, spam=spam,
                votes=HouseVotes84, wdbc=wdbc)

# split into training and test sets
mlTrainingSets <- list()
mlTestingSets <- list()
for(i in 1:length(mlbList)){
      dat <- mlbList[[i]]
      trainIndex <- createDataPartition(dat$response, p=0.7, list=FALSE)
      mlTrainingSets[[i]] <- dat[trainIndex,]
      mlTestingSets[[i]] <- dat[-trainIndex,]
}

# estimate performance
perfMeasuresRF <- matrix(0, nrow=length(mlbList), ncol=7)
rownames(perfMeasuresRF) <- names(mlbList)
colnames(perfMeasuresRF) <- c("Acc", "Sens", "Spec", "Prec", "Kappa", "F", "H")
perfMeasuresORRF <- matrix(0, nrow=length(mlbList), ncol=7)
rownames(perfMeasuresORRF) <- names(mlbList)
colnames(perfMeasuresORRF) <- c("Acc", "Sens", "Spec", "Prec", "Kappa", "F", "H")
perfMeasuresORRFlog <- matrix(0, nrow=length(mlbList), ncol=7)
rownames(perfMeasuresORRFlog) <- names(mlbList)
colnames(perfMeasuresORRFlog) <- c("Acc", "Sens", "Spec", "Prec", "Kappa", "F", "H")
perfMeasuresRotF <- matrix(0, nrow=length(mlbList), ncol=7)
rownames(perfMeasuresRotF) <- names(mlbList)
colnames(perfMeasuresRotF) <- c("Acc", "Sens", "Spec", "Prec", "Kappa", "F", "H")
perfMeasuresORFlog <- matrix(0, nrow=length(mlbList), ncol=7)
rownames(perfMeasuresORFlog) <- names(mlbList)
colnames(perfMeasuresORFlog) <- c("Acc", "Sens", "Spec", "Prec", "Kappa", "F", "H")

for(j in 1:length(mlbList)){
      # training data
      trainData <- mlTrainingSets[[j]]
      x <- trainData[, !names(trainData) %in% "response"]
      y <- trainData$response

      # testing data
      testData <- mlTestingSets[[j]]
      xtest <- testData[, !names(testData) %in% "response"]
      ytest <- testData$response

      # model parameter grids
      # parameter tuning settings
      fitControl <- trainControl(method = "cv", number = 10)
      orfparaGrid <- expand.grid(mtry=c(1, floor(sqrt(ncol(x))), floor(ncol(x)/2)))
      rrfparaGrid <- expand.grid(L=200, K=floor((ncol(x))/c(2, 3, 4)))
      orrfparaGrid <- expand.grid(K=floor((ncol(x))/c(3)), L=200, mtry=c(1, floor(sqrt(ncol(x))), floor(ncol(x)/2)))

      # Forest-RI
      print(paste("Method: Forest-RI; Data set:", names(mlbList)[j]))
      Mod <- train(response~., data=trainData, method="rf", trControl=fitControl,
                   tuneGrid=orfparaGrid)
      preds <- predict(Mod, testData)
      confMat <- confusionMatrix(preds, ytest)
      probs <- predict(Mod, xtest, type="prob")[,2]

      # Forest-RI: performance measures
      results <- summary(HMeasure(ytest, probs), show.all = TRUE)
      results$ACC <- confMat$overall[1]
      results$Kappa <- confMat$overall[2]
      perfMeasuresRF[j,] <- as.numeric(results[,c(23,11,12,13,24,17,1)])


      # oblique rotation random forest: predictions
      print(paste("Method: rotation random forest; Data set:", names(mlbList)[j]))
      optPara <- findOptimalTuning(x=x, y=y, paraGrid = orrfparaGrid, model="rf")
      optTune <- as.numeric(optPara$optTuneVals)
      Mod <- RRotF(x=x, y=y, K=optTune[1], L=optTune[2], mtry=optTune[3], model="rf")
      preds <- predict(Mod, xtest)
      confMat <- confusionMatrix(preds, as.numeric(ytest)-1)
      probs <- predict(Mod, xtest, type="prob")

      # oblique rotation random forest: performance measures
      results <- summary(HMeasure(ytest, probs), show.all = TRUE)
      results$ACC <- confMat$overall[1]
      results$Kappa <- confMat$overall[2]
      perfMeasuresORRF[j,] <- as.numeric(results[,c(23,11,12,13,24,17,1)])

      # oblique rotation random forest with logsitic splits: predictions
      print(paste("Method: oblique rotation random forest with logistic splits; Data set:", names(mlbList)[j]))
      optPara <- findOptimalTuning(x=x, y=y, paraGrid = orrfparaGrid, model="log")
      optTune <- as.numeric(optPara$optTuneVals)
      Mod <- RRotF(x=x, y=y, K=optTune[1], L=optTune[2], mtry=optTune[3], model="log")
      preds <- predict(Mod, xtest)
      confMat <- confusionMatrix(preds, as.numeric(ytest)-1)
      probs <- predict(Mod, xtest, type="prob")

      # oblique rotation random forest with logsitic splits: performance measures
      results <- summary(HMeasure(ytest, probs), show.all = TRUE)
      results$ACC <- confMat$overall[1]
      results$Kappa <- confMat$overall[2]
      perfMeasuresORRFlog[j,] <- as.numeric(results[,c(23,11,12,13,24,17,1)])

      # rotation forest: predictions
      print(paste("Method: rotation forest; Data set:", names(mlbList)[j]))
      Mod <- train(response~., data=trainData, method="rotationForest", trControl=fitControl,
                   tuneGrid=rrfparaGrid)
      preds <- predict(Mod, testData)
      confMat <- confusionMatrix(preds, ytest)
      probs <- predict(Mod, xtest, type="prob")[,2]

      # rotation forest: performance measures
      results <- summary(HMeasure(ytest, probs), show.all = TRUE)
      results$ACC <- confMat$overall[1]
      results$Kappa <- confMat$overall[2]
      perfMeasuresRotF[j,] <- as.numeric(results[,c(23,11,12,13,24,17,1)])

      # oblique random forest using logistic splits: predictions
      print(paste("Method: oblique random forest with log splits; Data set:", names(mlbList)[j]))
      Mod <- train(response~., data=trainData, method="ORFlog", trControl=fitControl,
                   tuneGrid=orfparaGrid)
      preds <- predict(Mod, testData)
      confMat <- confusionMatrix(preds, ytest)
      probs <- predict(Mod, xtest, type="prob")[,2]

      # oblique random forest using logistic splits: performance measures
      results <- summary(HMeasure(ytest, probs), show.all = TRUE)
      results$ACC <- confMat$overall[1]
      results$Kappa <- confMat$overall[2]
      perfMeasuresORFlog[j,] <- as.numeric(results[,c(23,11,12,13,24,17,1)])

}

compareResultsList <- list("rotationForest"=perfMeasuresRotF, "obliqueRFlog"=perfMeasuresORFlog,
                           "obliqueRRF"=perfMeasuresORRF, "obliqueORRFlog"=perfMeasuresORRFlog, "Forest-RI"=perfMeasuresRF)
saveRDS(compareResultsList, "benchMarkComparisonsRFs.rda")

# format benchmark results for thesis
B <- compareResultsList
finalResultList <- list()
algDat <- matrix(0, nrow=7, ncol=5)
rownames(algDat) <- colnames(B[[1]])
colnames(algDat) <- names(B)
for(i in 1:nrow(B[[1]])){
      for(j in 1:length(B)){
            algDat[,j] <- B[[j]][i,]
      }
      finalResultList[[i]] <- algDat
}

# name list with data set names
names(finalResultList) <- rownames(B[[1]])
saveRDS(finalResultList, "finalBenchmarkResults.rda")

# make latex tables for the results from each data set
library(stargazer)
for(i in 1:length(finalResultList)){
      stargazer(finalResultList[[i]], summary = FALSE)
}

# Compute omnibus tests for different performance metrics
finalResultList <- readRDS("finalBenchmarkResults.rda")
perfMat <- matrix(0, nrow=length(finalResultList), ncol=5)
perfMatList <- list()
for(i in 1:nrow(finalResultList[[1]])){
      for(j in 1:length(finalResultList)){
            perfMat[j,] <- finalResultList[[j]][i,]
            rownames(perfMat) <- names(finalResultList)
            colnames(perfMat) <- colnames(finalResultList[[1]])
      }
      perfMatList[[i]] <- perfMat
}
names(perfMatList) <- rownames(finalResultList[[1]])

# compute Iman-Devenport omnibus p-value per performance metric
omniBusTest <- list()
for(i in 1:length(perfMatList)){
      omniBusTest[[i]] <- imanDavenportTest(perfMatList[[i]][,-3])
}
names(omniBusTest) <- names(perfMatList)
# ---------------------------------------------------------------------------