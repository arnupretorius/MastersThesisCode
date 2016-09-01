# Investigate reproducibility #
###############################

# find algorithms associated with each paper
ds <- data[order(data$year),]
ds <- ds %>% select(paper_title, method)
umethods <- factor(unique(ds$method))
upapers <- factor(unique(ds$paper_title))
freqCount <- numeric(length(umethods))
splitDS <- split(ds, ds$paper_title)
for(j in 1:length(splitDS)){
      algPresent <- factor(unique(splitDS[[j]]$method))
      for(k in 1:length(algPresent)){
            countIndex <- which(umethods == as.character(algPresent[k]))
            freqCount[countIndex] <- freqCount[countIndex] + 1
      }
}

# plot algorithms used
plotDat <- data.frame(methods=umethods, freq=freqCount)[order(freqCount, decreasing = TRUE),]
plotDat <- as.data.frame(summarise(group_by(plotDat, methods), freq=sum(freq)))
plotDat <- plotDat[plotDat$freq > 1,]
plotDat <- plotDat[grep("rf|RF|Forest|forest|Rot|bagging", plotDat$methods),]
plotDat <- plotDat[order(plotDat$freq, decreasing = TRUE),]
plotDat$group <- c(rep("darkorange",5), "skyblue", "darkorange", "skyblue",
                   "darkorange", "skyblue", "skyblue")
ggplot(plotDat, aes(x=methods, y=freq, fill=group)) + geom_bar(stat="identity") +
      scale_x_discrete(limits=plotDat$methods)+ theme_bw()+
      scale_y_continuous(breaks = seq(0, 32, by = 2)) +
      scale_fill_manual(name = 'R package',
                        values = c("darkorange", "skyblue"),
                        labels = c("available", "unavailable"))+
      theme(legend.position = c(0.9, 0.8))+
      xlab("Algorithm") + ylab("#Papers")


# Include Disjunctive normal random forest
top3Algs <- c(top5Algs[3], "dnrf", "RotationForest", "adaboost")
datasets <- c("ionosphere", "wdbc", "pima", "sonar")
rowIndex <- sapply(rownames(compareMat) , function(x) x %in% datasets)
colIndex <- sapply(colnames(compareMat), function(x) x %in% top3Algs)
r2CompareMat <- compareMat[rowIndex, colIndex]

# plot densities
plotDensities(r2CompareMat)

# perform Friedman aligned ranks test (number of algorithms < 5)
friedmanAlignedRanksTest(r2CompareMat)
# (no significant dfference found)