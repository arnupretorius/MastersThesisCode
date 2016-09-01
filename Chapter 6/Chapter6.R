#####################################
# CHAPTER 6: Random Forest Algorithms
#####################################

# Check for missing packages and install if missing
list.of.packages <- c("latex2exp", "mlbench", "ggplot2", "caret", "doSNOW", "lattice",
                      "obliqueRF", "MASS", "pensim", "stargazer", "e1071", "mda",
                      "class", "pls", "ROCR", "snow", "gplots", "extraTrees", "RRF",
                      "wsrf", "rotationForest", "randomForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
load <- lapply(list.of.packages, require, character.only = TRUE)

# download and load random rotation forests package
if("RRotF" %in% installed.packages()[,"Package"] == FALSE){
      library(devtools)
      install_github("arnupretorius/RRotF")
}
library(RRotF)

############################################################################
# Figure 6.2: Performance of Forest-RI as a function of noise /
# Figure 6.7: Comparing the performance of Forest-RI with WSRF as a function 
# of noise.
############################################################################
# ---------------------------------------------------------------------------
# perform simulation
# iterated 50 times
# Some of the code is taken from http://www.davemease.com/contraryevidence/code1.txt
SimErrorsRF <- list(set1=NULL, set2=NULL, set3=NULL, set4=NULL, set5=NULL)
SimErrorsWSRF <- list(set1=NULL, set2=NULL, set3=NULL, set4=NULL, set5=NULL)
vars <- c(12, 52, 102, 202, 1002)
J <- 2
q <- 0.15
nTrain <- 400
nTest <- 1000
for(v in 1:length(vars)){
      for(iter in 1:50){
            set.seed(iter)
            p <- vars[v]
            Xtrain<-matrix(0,nTrain,p)
            Xtest<-matrix(0,nTest,p)
            for (i in 1:p){
                  Xtrain[,i]<-runif(nTrain)
                  Xtest[,i]<-runif(nTest)
            }
            ytrain<-rep(0,nTrain)
            for (i in 1:nTrain){
                  ytrain[i]<-1*(runif(1)<(q+(1-2*q)*1*(sum((Xtrain[i,1:J]))>(J/2))))
            }
            ytest<-rep(0,nTest)
            for (i in 1:nTest){
                  ytest[i]<-1*(runif(1)<(q+(1-2*q)*1*(sum((Xtest[i,1:J]))>(J/2))))
            }
            # training data
            train <- data.frame(y=factor(ytrain), Xtrain)
            # test data
            test <- data.frame(y=factor(ytest), Xtest)
            # Compute errors of rf models
            fitControl <- trainControl(method="none")
            tuneControl <- data.frame(mtry=floor(sqrt(vars[v])))
            rf.fit <- train(y~., data=train, method="rf", trControl=fitControl,
                             tuneGrid=tuneControl, ntree=100)
            wsrf.fit <- train(y~., data=train, method="wsrf", trControl=fitControl,
                            tuneGrid=tuneControl, ntree=100)
            SimErrorsRF[[v]][iter] <- mean(test$y != predict(rf.fit, test[,-1]))
            SimErrorsWSRF[[v]][iter] <- mean(test$y != predict(wsrf.fit, test[,-1]))
      }
}
# plot results
resultsRF <- data.frame(set=rep(c("set1", "set2", "set3", "set4", "set5"), each=50),
                      error=c(SimErrorsRF[[1]], SimErrorsRF[[2]],SimErrorsRF[[3]],SimErrorsRF[[4]],SimErrorsRF[[5]]))
resultsWSRF <- data.frame(set=rep(c("set1", "set2", "set3", "set4", "set5"), each=50),
                        error=c(SimErrorsWSRF[[1]], SimErrorsWSRF[[2]],SimErrorsWSRF[[3]],
                                SimErrorsWSRF[[4]],SimErrorsWSRF[[5]]))
# comupte relevant variables sampling probabilities
subSampleProbs <- sapply(vars, function(x){
      mtry <- floor(sqrt(x))
      round((2*choose(x-2, mtry-1) + choose(x-2, mtry-2))/choose(x,mtry), 2)
} )
# plot boxplots for random forest (forest-RI)
ggplot(resultsRF, aes(y=error, x=set)) + stat_boxplot(geom ='errorbar', width=0.5) +
      geom_boxplot(notch = TRUE, fill="darkgreen", outlier.color = "red") +
      geom_hline(yintercept = q, linetype="dashed", col="purple") +
      scale_x_discrete(labels=c("(2, 10)", "(2, 50)", "(2, 100)", "(2, 200)", "(2, 1000)"))+
      theme_bw() + xlab("Number of (relevant, noise) variables") + ylim(0, 0.75) +
      ylab("Test Misclassification Error") + annotate("text",x=1, y=0.15+q, label=subSampleProbs[1])+
      annotate("text",x=2, y=0.19+q, label=subSampleProbs[2])+
      annotate("text",x=3, y=0.23+q, label=subSampleProbs[3])+
      annotate("text",x=4, y=0.28+q, label=subSampleProbs[4])+
      annotate("text",x=5, y=0.37+q, label=subSampleProbs[5])+
      annotate("text",x=3, y=0.1, label="Bayes Error")
# plot boxplots for WSRF
resultsWSRF <- data.frame(set=rep(c("set1", "set2", "set3", "set4", "set5","set6", "set7", "set8", "set9", "set99"), each=50),
                          error=c(SimErrorsRF[[1]], SimErrorsWSRF[[1]],SimErrorsRF[[2]],
                                  SimErrorsWSRF[[2]],SimErrorsRF[[3]], SimErrorsWSRF[[3]],
                                  SimErrorsRF[[4]],SimErrorsWSRF[[4]],
                                  SimErrorsRF[[5]],SimErrorsWSRF[[5]]),
                          grp=rep(rep(c("Forest-RI", "WSRF"), each=50),5))
ggplot(resultsWSRF, aes(y=error, x=set, fill=grp)) + stat_boxplot(geom ='errorbar', width=0.5)+
      geom_boxplot(notch = TRUE, outlier.color = "red") +
      geom_hline(yintercept = q, linetype="dashed", col="purple") +
      xlab("Number of (relevant, noise) variables") + ylim(0, 0.75) +
      ylab("Test Misclassification Error") +
      scale_x_discrete(labels=rep(c("(2, 10)", "(2, 50)", "(2, 100)", "(2, 200)", "(2, 1000)"), each=2))+
      scale_fill_manual(name="Model", labels=c("Forest-RI", "WSRF"), values=c("darkgreen", "blue"))+
      theme_bw()+
      theme(legend.position=c(0.1, 0.6))+
      annotate("text",x=5.5, y=0.1, label="Bayes Error")
# ---------------------------------------------------------------------------
########################################
# Figure 6.3: Binary tree representation
########################################
# ---------------------------------------------------------------------------
# create empty plot
dmar <- par()$mar
par(mar=c(0,0,0,0))
plot(11:22, 11:22, type="n", xlab="", ylab="",
     xlim=c(11, 22), ylim=c(1, 22),
     main="", axes=FALSE)
# create tree
text(16.5, 20, TeX("Node 1"), col="darkgreen")
text(15.5, 17.5, TeX("<< if $\\phi_1(\\underline{x}, \\Theta) = 0,$"))
text(17.5, 17.5, TeX("if $\\phi_1(\\underline{x}, \\Theta) = 1$ >>"))
lines(c(16.5, 16.5), c(18.5, 19.5))
lines(c(14, 19),c(19, 19))
# splits
lines(c(14, 14), c(19, 16))
lines(c(19, 19), c(19, 16))
# internal nodes
text(14, 15, TeX("Node 2"), col="darkgreen")
text(19, 15, TeX("Node 3"), col="darkgreen")
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
text(13, 8, TeX("$-$"), col="blue", cex=1.5)
text(15, 8, TeX("$+_{(1)}$"), col="darkorange", cex=1.5)
text(20, 8, TeX("$-$"), col="blue", cex=1.5)
# internal node 3
text(18, 8, TeX("Node 4"), col="darkgreen")
lines(c(18, 18), c(6.5, 7.5))
lines(c(17, 19), c(7, 7))
lines(c(17, 17), c(7, 3))
lines(c(19, 19), c(7, 3))
# root node 4 and 5
text(17, 2, TeX("$+_{(2)}$"), col="darkorange", cex=1.5)
text(19, 2, TeX("$-$"), col="blue", cex=1.5)
par(mar=dmar)
# ---------------------------------------------------------------------------
#######################################################################
# Figure 6.4: Logistic sigmoid function used to approximate a tree node 
# splitting rule.
#######################################################################
# ---------------------------------------------------------------------------
x <- seq(from=-10, to=10, by=0.01)
y <- 1/(1+exp(-x))
sig <- data.frame(x=x, y=y)
ggplot(sig, aes(x=x,y=y)) + geom_line() + theme_bw()+
      geom_vline(xintercept = 0, col="orange")+
      geom_hline(yintercept = 0.5, col="purple", linetype="dashed")+
      scale_x_discrete(limits=c(0), labels=c("-b"))+
      ylab(TeX("$\\hat{\\phi}(\\underline{x}, \\Theta)$"))+
      annotate("text", x=-4.7, y=0.25, label="1/(1+exp(-b-X))")
# ---------------------------------------------------------------------------
##################################################################
# Table 6.1: The variables describing each random forest algorithm
##################################################################
# ---------------------------------------------------------------------------
# make mds plot of variants
# read in data
data <- read.csv("RFvariantsData.csv")
tableframe <- data.frame("variable"=colnames(data)[-1], "type"=c("categorical", rep("numeric", 17)),
           "range"=c("NA", "1988 - 2015", rep("{0, 1}", 16)))
stargazer(tableframe, summary = FALSE)
# ---------------------------------------------------------------------------
###########################################################################
# Figure 6.5: Trait based comparison of random forest proposals by way of a 
# best two-dimensional MDS approximation of the full trait space.
###########################################################################
# ---------------------------------------------------------------------------
# Compute group colors
# sources of randomness
gcols <- NULL
for(i in 1:nrow(data)){
      if(data[i,]$r_data == 0
         && data[i,]$r_subsample_var == 0 && data[i,]$r_split_points == 0
         && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 0){
            gcols[i] <- "navy"
      } else if(data[i,]$r_data == 1
                && data[i,]$r_subsample_var == 0 && data[i,]$r_split_points == 0
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 0){
            gcols[i] <- "blue"
      } else if(data[i,]$r_data == 1
                && data[i,]$r_subsample_var == 1 && data[i,]$r_split_points == 0
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 0){
            gcols[i] <- "orange"
      } else if(data[i,]$r_data == 1
                && data[i,]$r_subsample_var == 1 && data[i,]$r_split_points == 1
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 0){
            gcols[i] <- "tan"
      } else if(data[i,]$r_data == 0
                && data[i,]$r_subsample_var == 1 && data[i,]$r_split_points == 0
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 0){
            gcols[i] <- "darkgreen"
      } else if(data[i,]$r_data == 0
                && data[i,]$r_subsample_var == 1 && data[i,]$r_split_points == 1
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 0){
            gcols[i] <- "tomato4"
      } else if(data[i,]$r_data == 0
                && data[i,]$r_subsample_var == 0 && data[i,]$r_split_points == 1
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 0){
            gcols[i] <- "red"
      } else if(data[i,]$r_data == 0
                && data[i,]$r_subsample_var == 0 && data[i,]$r_split_points == 0
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 1){
            gcols[i] <- "purple"
      } else if(data[i,]$r_data == 1
                && data[i,]$r_subsample_var == 1 && data[i,]$r_split_points == 0
                && data[i,]$r_voting == 0 && data[i,]$r_ensemble == 1){
            gcols[i] <- "green"
      }
}

cols <- c("forestgreen", "darkred", "gold4", "skyblue",
          "orange3", "magenta", "royalblue", "seagreen",
          "red3", "peru", "violet", "yellow4",
          "springgreen3", "tomato2", "skyblue3", "sienna2",
          "plum4")
ccols <- NULL
for(i in 1:nrow(data)){
      if(sum(data[i, 9:10]) > 0 && sum(data[i,11:19]) == 0){
            ccols[i] <- cols[1]
      } else if(sum(data[i,9:10]) == 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:19]) == 0){
            ccols[i] <- cols[2]
      } else if(sum(data[i, 9:10]) == 0 && sum(data[i,11:14]) == 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) == 0){
            ccols[i] <- cols[3]
      } else if(sum(data[i, 9:10]) == 0 && sum(data[i,11:14]) == 0 && sum(data[i,15:16]) == 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[4]
      } else if(sum(data[i, 9:10]) > 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:16]) == 0 && sum(data[i,17:19]) == 0){
            ccols[i] <- cols[5]
      } else if(sum(data[i, 9:10]) > 0 && sum(data[i,11:14]) == 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) == 0){
            ccols[i] <- cols[6]
      } else if(sum(data[i, 9:10]) > 0 && sum(data[i,11:14]) == 0 && sum(data[i,15:16]) == 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[7]
      } else if(sum(data[i, 9:10]) == 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) == 0){
            ccols[i] <- cols[8]
      } else if(sum(data[i, 9:10]) == 0 && sum(data[i,11:14]) == 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[9]
      } else if(sum(data[i, 9:10]) == 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:16]) == 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[10]
      } else if(sum(data[i, 9:10]) > 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) == 0){
            ccols[i] <- cols[11]
      } else if(sum(data[i, 9:10]) > 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:16]) == 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[12]
      } else if(sum(data[i, 9:10]) > 0 && sum(data[i,11:14]) == 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[13]
      } else if(sum(data[i, 9:10]) == 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[14]
      } else if(sum(data[i, 9:10]) > 0 && sum(data[i,11:14]) > 0 && sum(data[i,15:16]) > 0 && sum(data[i,17:19]) > 0){
            ccols[i] <- cols[15]
      }
}

# use MDS to obtain optimal 2d approx space
num_data <- data[,-c(1,2,3)]
p <- cmdscale(dist(num_data))

# plot display
par(mar=c(1,1,1,1))
plot(x=p[,1], y=p[,2], xlim=c(-1.5, 3), ylim=c(-1,1.2), xaxt="n", yaxt="n",
     xlab="", ylab="", col=ccols, pch=18)
text(p[1,1], p[1,2], paste(data$author[1], data$year[1]), pos=1, cex=0.6, col = gcols[1])
text(p[2,1], p[2,2], paste(data$author[2], data$year[2]), pos=1, cex=0.6, col = gcols[2])
text(p[3,1], p[3,2], paste(data$author[3], data$year[3]), pos=3, cex=0.6, col = gcols[3], offset=0.8)
text(p[4,1], p[4,2], paste(data$author[4], data$year[4]), pos=1, cex=0.6, col = gcols[4])
text(p[5,1], p[5,2], paste(data$author[5], data$year[5]), pos=4, cex=0.6, col = gcols[5])
text(p[6,1], p[6,2], paste(data$author[6], data$year[6]), pos=3, cex=0.6, col = gcols[6])
text(p[7,1], p[7,2], paste(data$author[7], data$year[7]), pos=3, cex=0.6, col = gcols[7])
text(p[8,1], p[8,2], paste(data$author[8], data$year[8]), pos=4, cex=0.6, col = gcols[8])
text(p[9,1], p[9,2], paste(data$author[9], data$year[9]), pos=4, cex=0.6, col = gcols[9])
text(p[10,1],p[10,2], paste(data$author[10], data$year[10]), pos=2, cex=0.6, col = gcols[10])
text(p[11,1],p[11,2], paste(data$author[11], data$year[11]), pos=2, cex=0.6, col = gcols[11])
text(p[12,1],p[12,2], paste(data$author[12], data$year[12]), pos=3, cex=0.6, col = gcols[12])
text(p[13,1],p[13,2], paste(data$author[13], data$year[13]), pos=4, cex=0.6, col = gcols[13])
text(p[14,1],p[14,2], paste(data$author[14], data$year[14]), pos=4, cex=0.6, col = gcols[14])
text(p[15,1], p[15,2], paste(data$author[15], data$year[15]), pos=1, cex=0.6, col = gcols[15])
text(p[16,1], p[16,2], paste(data$author[16], data$year[16]), pos=1, cex=0.6, col = gcols[16])
text(p[17,1], p[17,2], paste(data$author[17], data$year[17]), pos=1, cex=0.6, col = gcols[17])
text(p[18,1], p[18,2], paste(data$author[18], data$year[18]), pos=2, cex=0.6, col = gcols[18])
text(p[19,1], p[19,2], paste(data$author[19], data$year[19]), pos=2, cex=0.6, col = gcols[19])
text(p[20,1], p[20,2], paste(data$author[20], data$year[20]), pos=4, cex=0.6, col = gcols[20])
text(p[21,1], p[21,2], paste(data$author[21], data$year[21]), pos=2, cex=0.6, col = gcols[21])
text(p[22,1], p[22,2], paste(data$author[22], data$year[22]), pos=4, cex=0.6, col = gcols[22])
text(p[23,1], p[23,2], paste(data$author[23], data$year[23]), pos=3, cex=0.6, col = gcols[23])
text(p[24,1], p[24,2], paste(data$author[24], data$year[24]), pos=2, cex=0.6, col = gcols[24])
text(p[25,1], p[25,2], paste(data$author[25], data$year[25]), pos=2, cex=0.6, col = gcols[25])
text(p[26,1], p[26,2], paste(data$author[26], data$year[26]), pos=3, cex=0.6, col = gcols[26])
text(p[27,1], p[27,2], paste(data$author[27], data$year[27]), pos=2, cex=0.6, col = gcols[27])
text(p[28,1], p[28,2], paste(data$author[28], data$year[28]), pos=2, cex=0.6, col = gcols[28])
text(p[29,1], p[29,2], paste(data$author[29], data$year[29]), pos=2, cex=0.6, col = gcols[29])
text(p[30,1], p[30,2], paste(data$author[30], data$year[30]), pos=4, cex=0.6, col = gcols[30])
text(p[31,1], p[31,2], paste(data$author[31], data$year[31]), pos=3, cex=0.6, col = gcols[31], offset = 1)
text(p[32,1], p[32,2], paste(data$author[32], data$year[32]), pos=1, cex=0.6, col = gcols[32])
text(p[33,1], p[33,2], paste(data$author[33], data$year[33]), pos=1, cex=0.6, col = gcols[33], offset = 1)
text(p[34,1], p[34,2], paste(data$author[34], data$year[34]), pos=4, cex=0.6, col = gcols[34])
text(p[35,1], p[35,2], paste(data$author[35], data$year[35]), pos=1, cex=0.6, col = gcols[35])
text(p[36,1], p[36,2], paste(data$author[36], data$year[36]), pos=2, cex=0.6, col = gcols[36])
text(p[37,1], p[37,2], paste(data$author[37], data$year[37]), pos=1, cex=0.6, col = gcols[37])
# add custom legend
text(2.5, 1+0.2, "Randomisation Sources", cex=0.6)
text(2.3, 0.94+0.2, "auth+year:", col="blue", cex=0.6)
text(2.3, 0.89+0.2, "auth+year:", col="orange", cex=0.6)
text(2.3, 0.84+0.2, "auth+year:", col="tan", cex=0.6)
text(2.3, 0.79+0.2, "auth+year:", col="green", cex=0.6)
text(2.3, 0.74+0.2, "auth+year:", col="darkgreen", cex=0.6)
text(2.3, 0.69+0.2, "auth+year:", col="tomato4", cex=0.6)
text(2.3, 0.64+0.2, "auth+year:", col="red", cex=0.6)
text(2.3, 0.59+0.2, "auth+year:", col="purple", cex=0.6)
# add categories
text(2.78, 0.94+0.2, "R.1", cex=0.6)
text(2.78, 0.89+0.2, "R.1, R.2", cex=0.6)
text(2.78, 0.84+0.2, "R.1, R.2, R.3", cex=0.6)
text(2.78, 0.79+0.2, "R.1, R.2, R.4", cex=0.6)
text(2.78, 0.74+0.2, "R.2", cex=0.6)
text(2.78, 0.69+0.2, "R.2, R.3", cex=0.6)
text(2.78, 0.64+0.2, "R.3", cex=0.6)
text(2.78, 0.59+0.2, "R.4", cex=0.6)
# add deterministic modifications
text(2.53, 0.5, "Deterministic Modifications", cex=0.6)
pcols <- c("orange3", "violet", "skyblue3", "darkred",
           "seagreen", "peru", "tomato2")
xpoints <- rep(2.2, 7)
ypoints <- c(0.44, 0.39, 0.34, 0.29, 0.24, 0.19, 0.14)
points(xpoints,ypoints,col=pcols,pch=18)
# add categories DM
text(2.5, 0.44, "A, B", cex=0.6)
text(2.5, 0.39, "A, B, C", cex=0.6)
text(2.5, 0.34, "A, B, C, D", cex=0.6)
text(2.5, 0.29, "B", cex=0.6)
text(2.5, 0.24, "B, C", cex=0.6)
text(2.5, 0.19, "B, D", cex=0.6)
text(2.5, 0.14, "B, C, D", cex=0.6)
# ---------------------------------------------------------------------------
############################################################################
# Figure 6.6: Random forest decision boundaries: top left: extremely 
# randomised forest; top right: rotation random forest; middle left: oblique 
# random forest with logistic regression splits; middle right: weighted 
# subspace random forest; bottom left: regularised random forest (λ = 0.1); 
# bottom right: regularised random forest (λ = 0.6).
############################################################################
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
bayesProbs <- apply(test[,2:3], 1, p)
bayesError <- sum(as.numeric(test$y != factor(ifelse(bayesProbs>0.5, 1, 0))))/nrow(test)

# Extremely randomised trees
fitControl <- trainControl(method="none")
tuneControl <- data.frame(mtry=1, numRandomCuts=1)
set.seed(13)
erf.fit <- train(y~., data=train, method="extraTrees", trControl=fitControl,
                tuneGrid=tuneControl, ntree=100)

# compute training and test error
erfTrainPreds <- predict(erf.fit, newdata=train[,-1])
erfTrainingError <- sum(as.numeric(train$y != erfTrainPreds))/nrow(train)

# Compute test error
erfTestPreds <- predict(erf.fit, test)
erfTestError <- sum(as.numeric(test$y != erfTestPreds))/nrow(test)

# construct decision boundary plot
erfProbs <- predict(erf.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(erfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
gerf <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(erfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="none")+
      scale_color_manual(name="ERF decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','ERF'))+
      scale_linetype_manual(name = 'ERF decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','ERF'))+
      annotate("text", x = 2.2, y = -1.6, size=3, 
               label = paste("Training error:", round(erfTrainingError, 3),
                        "\nTest error:", round(erfTestError,3),
                        "\nBayes error:", round(bayesError,3)), hjust=0)+
      ggtitle("Extremely Randomised Forest")
gerf
# Rotation forest
fitControl <- trainControl(method="none")
tuneControl <- data.frame(K=1, L=10)
set.seed(13)
rotrf.fit <- train(y~., data=train, method="rotationForest", trControl=fitControl,
                 tuneGrid=tuneControl)

# compute training and test error
rotrfTrainPreds <- predict(rotrf.fit, newdata=train[,-1])
rotrfTrainingError <- sum(as.numeric(train$y != rotrfTrainPreds))/nrow(train)

# Compute test error
rotrfTestPreds <- predict(rotrf.fit, test)
rotrfTestError <- sum(as.numeric(test$y != rotrfTestPreds))/nrow(test)

# construct decision boundary plot
rotrfProbs <- predict(rotrf.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(rotrfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
grotrf <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(rotrfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="none")+
      scale_color_manual(name="RotRF decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','RotRF'))+
      scale_linetype_manual(name = 'RotRF decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','RotRF'))+
      annotate("text", x = 2.2, y = -1.6, size=3, 
               label = paste("Training error:", round(rotrfTrainingError, 3),
                        "\nTest error:", round(rotrfTestError,3),
                        "\nBayes error:", round(bayesError,3)), hjust=0)+
      ggtitle("Rotation Random Forest")
grotrf

# Oblique RF - logistice regression splits
set.seed(13)
orf.fit <- obliqueRF(y=as.numeric(train$y), x=as.matrix(train[,2:3]),
                       mtry=2, training_method="log", ntree=100)

# compute training and test error
orfTrainPreds <- predict(orf.fit, newdata=train[,-1])
orfTrainingError <- sum(as.numeric(as.numeric(train$y) != as.numeric(orfTrainPreds)))/nrow(train)

# Compute test error
orfTestPreds <- predict(orf.fit, test[,-1])
orfTestError <- sum(as.numeric(as.numeric(test$y) != as.numeric(orfTestPreds)))/nrow(test)

# construct decision boundary plot
orfProbs <- predict(orf.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(orfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
gorf <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(orfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="none")+
      scale_color_manual(name="ORF-log decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','ORF-log'))+
      scale_linetype_manual(name = 'ORF-log decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','ORF-log'))+
      annotate("text", x = 2.2, y = -1.6, size=3, 
               label = paste("Training error:", round(orfTrainingError, 3),
                        "\nTest error:", round(orfTestError,3),
                        "\nBayes error:", round(bayesError,3)), hjust=0)+
      ggtitle("Oblique Random Forest - logistic regression splits")
gorf

# weighted subspace random forests
fitControl <- trainControl(method="none")
tuneControl <- data.frame(mtry=2)
set.seed(13)
wsrf.fit <- train(y~., data=train, method="wsrf", trControl=fitControl,
                  tuneGrid=tuneControl)

# compute training and test error
wsrfTrainPreds <- predict(wsrf.fit, newdata=train[,-1])
wsrfTrainingError <- sum(as.numeric(train$y != wsrfTrainPreds))/nrow(train)

# Compute test error
wsrfTestPreds <- predict(wsrf.fit, test)
wsrfTestError <- sum(as.numeric(test$y != wsrfTestPreds))/nrow(test)

# construct decision boundary plot
wsrfProbs <- predict(wsrf.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(wsrfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
gwsrf <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(wsrfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="none")+
      scale_color_manual(name="WSRF decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','WSRF'))+
      scale_linetype_manual(name = 'WSRF decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','WSRF'))+
      annotate("text", x = 2.2, y = -1.6, size=3, 
               label = paste("Training error:", round(wsrfTrainingError, 3),
                        "\nTest error:", round(wsrfTestError,3),
                        "\nBayes error:", round(bayesError,3)), hjust=0)+
      ggtitle("Weighted Subspace Random Forest")
gwsrf

# regularised random forests (0.1)
fitControl <- trainControl(method="none")
tuneControl <- data.frame(mtry=2, coefReg=0.1)
set.seed(13)
rrf.fit <- train(y~., data=train, method="RRFglobal", trControl=fitControl,
                 tuneGrid=tuneControl)

# compute training and test error
rrfTrainPreds <- predict(rrf.fit, newdata=train[,-1])
rrfTrainingError <- sum(as.numeric(train$y != rrfTrainPreds))/nrow(train)

# Compute test error
rrfTestPreds <- predict(rrf.fit, test)
rrfTestError <- sum(as.numeric(test$y != rrfTestPreds))/nrow(test)

# construct decision boundary plot
rrfProbs <- predict(rrf.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(rrfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
grrf <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(rrfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="none")+
      scale_color_manual(name="RRF decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','RRF'))+
      scale_linetype_manual(name = 'RRF decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','RRF'))+
      annotate("text", x = 2.2, y = -1.6, size=3, 
               label = paste("Training error:", round(rrfTrainingError, 3),
                        "\nTest error:", round(rrfTestError,3),
                        "\nBayes error:", round(bayesError,3)), hjust=0)+
      ggtitle(TeX("Regularised Random Forest ($\\lambda = 0.1$)"))
grrf

# regularised random forests (0.6)
fitControl <- trainControl(method="none")
tuneControl <- data.frame(mtry=2, coefReg=0.6)
set.seed(13)
rrf.fit <- train(y~., data=train, method="RRFglobal", trControl=fitControl,
                 tuneGrid=tuneControl)

# compute training and test error
rrfTrainPreds <- predict(rrf.fit, newdata=train[,-1])
rrfTrainingError <- sum(as.numeric(train$y != rrfTrainPreds))/nrow(train)

# Compute test error
rrfTestPreds <- predict(rrf.fit, test)
rrfTestError <- sum(as.numeric(test$y != rrfTestPreds))/nrow(test)

# construct decision boundary plot
rrfProbs <- predict(rrf.fit, plotGrid, type="prob")[,2]
pr<-data.frame(x=rep(x1seq, length(x2seq)), y=rep(x2seq, each=length(x1seq)),
               z=as.vector(rrfProbs))
gd <- expand.grid(x=x1seq, y=x2seq)
grrf <- ggplot(data.frame(y=factor(Ytrain), X1=Xtrain[,1], X2=Xtrain[,2]), aes(x=X1, y=X2)) +
      geom_point(data=data.frame(gd), aes(x=x, y=y),pch=".", cex=1.2,
                 col=ifelse(rrfProbs<0.5, "skyblue", "orange")) +
      geom_point(size = 3, pch = train$y, col=color) +
      geom_contour(data=bayesPr, aes(x=x, y=y, z=z, col="brown", linetype="dashed"), breaks=c(0,.5))+
      geom_contour(data=pr, aes(x=x, y=y, z=z, col="purple", linetype="solid"), breaks=c(0,.5)) +
      theme_bw() +
      theme(legend.position="none")+
      scale_color_manual(name="RRF decision boundary:",values=c("purple", "brown"),
                         labels = c('Bayes','RRF'))+
      scale_linetype_manual(name = 'RRF decision boundary:', values = c("dashed", "solid"),
                            labels = c('Bayes','RRF'))+
      annotate("text", x = 2.2, y = -1.6, size=3, 
               label = paste("Training error:", round(rrfTrainingError, 3),
                        "\nTest error:", round(rrfTestError,3),
                        "\nBayes error:", round(bayesError,3)), hjust=0)+
      ggtitle(TeX("Regularised Random Forest ($\\lambda = 0.6$)"))
grrf
# ---------------------------------------------------------------------------
##########################################################################
# Table 6.2: Estimated bias, variance, systematic and variance effects for 
# random forest algorithms.
##########################################################################
# ---------------------------------------------------------------------------
majVote <- function(x){names(which.max(table(x)))}
nTrain <- 400
nTest <- 1000
Models <- factor(rep(c("Forest-RI","ERF", "RotationRF", "ORF-log"), each=6), level=c("Forest-RI","ERF", "RotationRF", "ORF-log"))

# performs computations in parallel
cl <- makeCluster(3, type="SOCK")
registerDoSNOW(cl)

# MAIN EXPERIMENT FUNCTIONS
runBiasVarSimulation <- function(trainingSets, simTest, BayesPreds){

    # parameter tuning settings
    fitControl <- trainControl(method = "cv", number = 10)
    rfparaGrid <- expand.grid(mtry=c(1, floor(sqrt(ncol(simTest)-1)), floor(ncol(simTest)/2)))
    orfparaGrid <- expand.grid(mtry=c(1, floor(sqrt(ncol(simTest)-1)), floor(ncol(simTest)/2)))
    rrfparaGrid <- expand.grid(L=200, K=floor((ncol(simTest)-1)/c(2, 3, 4)))
    erfparaGrid <- expand.grid(mtry=c(1, floor(sqrt(ncol(simTest)-1)), floor(ncol(simTest)/2)), numRandomCuts=c(1, 5, 10, nrow(simTest)/2))

    # extremely randomised trees model
    sim.ERF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                        method="extraTrees", paraGrid = erfparaGrid,
                                     tControl = fitControl, BayesPreds = BayesPreds, ntree=200)
    # rotation random forest
    sim.RRF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                        method="rotationForest", paraGrid = rrfparaGrid,
                                     tControl = fitControl, BayesPreds = BayesPreds)
    # oblique random forest (logistic) model
    sim.ORF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                        method="ORFlog", paraGrid = orfparaGrid,
                                     tControl = fitControl, BayesPreds = BayesPreds, ntree=200)
    # random forest model
    sim.RF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                     method="rf", paraGrid = rfparaGrid,
                                     tControl = fitControl, BayesPreds = BayesPreds, ntree=200)


    list(results=rbind(sim.RF$results, sim.ERF$results, sim.RRF$results, sim.ORF$results),
         tuneValues=list(sim.RF$tuneValues, sim.ERF$tuneValues, sim.RRF$tuneValues, sim.ORF$tuneValues))
}


simulateBiasVarDecomp <- function(trainingSets, simTest, method, paraGrid, tControl, BayesPreds, ...){
    
    majVote <- function(x){names(which.max(table(x)))}
    tuneVals <- paraGrid[1,]
    numOfExp <- 100
    # train models and make predictions
    BVpreds <- matrix(0, nrow=numOfExp, ncol=nTest)
    var.T <- NULL
    var <- NULL
    bias <- NULL
    VE <- NULL
    SE <- NULL
    misclassError <- NULL
    C <- as.numeric(simTest$classes)

    # train models
    for(j in 1:numOfExp){
        Model <- train(classes~., data=trainingSets[[j]], method=method,
                       tuneGrid=paraGrid, trControl=tControl, ...)
        tuneVals <- rbind(tuneVals, Model$bestTune)
        BVpreds[j,] <- as.numeric(predict(Model, simTest))
        print(paste("Method: ", method, ", Iter: ", j, " out of ", numOfExp))
    }

    # James (2003) decomposition estimates
    BayesClassifier <- BayesPreds
    majVoteClassifier <- apply(BVpreds, 2, function(x)majVote(x))
    var.T <- mean(BayesClassifier != C)
    var <- mean(apply(BVpreds, 1, function(x) mean(x != majVoteClassifier)))
    bias <- mean(majVoteClassifier != BayesClassifier)
    VE <- mean(apply(BVpreds, 1, function(x) mean(x != C)) - mean(majVoteClassifier != C))
    SE <- mean(majVoteClassifier != C) - mean(BayesClassifier != C)
    meanError <- mean(apply(BVpreds, 1, function(x){ mean(x != C) }))

    # store bias and variance and systematic effect and variance effect
    vb <- c(meanError, var.T, SE, VE, bias, var)
    bar <- factor(c(1,2,3,4,5,6))
    type <- c("Error", "Bayes Error", "Systematic Effect", "Variance Effect", "Bias", "Variance")
    model <- rep(method, 6)
    biasVarPlotData <- data.frame(vb=vb, Decomposition=type, bar=bar, model=model)
    list(results=biasVarPlotData, tuneValues=tuneVals[-1,])
}
####################
# Designed scenarios  
####################
# load data generation library
# simulate data function from "pensim" package
simData <- function (nvars = c(100, 100, 100, 100, 600), cors = c(0.8, 0, 0.8, 0, 0),
                     associations = c(0.5, 0.5, 0.3, 0.3, 0), firstonly = c(TRUE, FALSE, TRUE, FALSE, FALSE),
                     nsamples = 100, censoring = "none",
                     labelswapprob = 0, response = "timetoevent", basehaz = 0.2,
                     logisticintercept = 0)
{
      if (labelswapprob < 0)
            stop("labelswapprob cannot be negative")
      if (labelswapprob > 0 & response == "timetoevent")
            stop("labelswapprob is only implemented for binary response")
      if (!class(nvars) %in% c("numeric", "integer"))
            stop("nvars must be a numeric vector")
      if (!class(cors) %in% c("numeric", "integer"))
            stop("cors must be a numeric vector")
      if (class(firstonly) != "logical")
            stop("firstonly must be a logical vector")
      if (!class(associations) %in% c("numeric", "integer"))
            stop("associations must be a numeric vector")
      if (length(nvars) != length(cors) | length(nvars) != length(firstonly) |
          length(nvars) != length(associations))
            stop("nvars, cors, firstonly, and associations must all have the same length.")
      x.out <- matrix(0, ncol = sum(nvars), nrow = nsamples)
      definecors <- data.frame(start = c(1, cumsum(nvars[-length(nvars)]) +
                                               1), end = cumsum(nvars), cors = cors, associations = associations,
                               num = nvars, firstonly = firstonly, row.names = letters[1:length(nvars)])
      Sigma <- matrix(0, ncol = sum(nvars), nrow = sum(nvars))
      wts <- rep(0, sum(nvars))
      for (i in 1:nrow(definecors)) {
            thisrange <- definecors[i, "start"]:definecors[i, "end"]
            Sigma[thisrange, thisrange] <- definecors[i, "cors"]
            diag(Sigma) <- 1
            x.out[, thisrange] <- mvrnorm(n = nsamples, mu = rep(0,
                                                                 nvars[i]), Sigma = Sigma[thisrange, thisrange])
            if (definecors[i, "firstonly"]) {
                  wts[definecors[i, "start"]] <- definecors[i, "associations"]
            }
            else {
                  wts[definecors[i, "start"]:definecors[i, "end"]] <- definecors[i,"associations"]
            }
            varnames <- paste(letters[i], 1:nvars[i], sep = ".")
            names(wts)[definecors[i, "start"]:definecors[i, "end"]] <- varnames
      }
      names(wts) <- make.unique(names(wts))
      dimnames(Sigma) <- list(colnames = names(wts), rownames = names(wts))
      colnames(x.out) <- names(wts)
      betaX <- x.out %*% wts
      x.out <- data.frame(x.out)
      if (identical(response, "timetoevent")) {
            h = basehaz * exp(betaX[, 1])
            x.out$time <- rexp(length(h), h)
            x.out$cens <- 1
            if (class(censoring) == "numeric" | class(censoring) ==
                "integer") {
                  if (length(censoring) == 2) {
                        censtimes <- runif(length(h), min = censoring[1],
                                           max = censoring[2])
                  }
                  else if (length(censoring) == 1) {
                        censtimes <- rep(censoring, length(h))
                  }
                  x.out$cens[x.out$time > censtimes] <- 0
                  x.out$time[x.out$time > censtimes] <- censtimes[x.out$time > censtimes]
            }
      }
      else if (identical(response, "binary")) {
            p <- 1/(1 + exp(-(betaX + logisticintercept)))
            x.out$outcome <- ifelse(p > runif(length(p)), 1, 0) 
            if (labelswapprob > 0) {
                  do.swap <- runif(length(p)) < labelswapprob
                  new.outcome <- x.out$outcome
                  new.outcome[x.out$outcome == 1 & do.swap] <- 0
                  new.outcome[x.out$outcome == 0 & do.swap] <- 1
                  x.out$outcome <- new.outcome
            }
            x.out$outcome <- factor(x.out$outcome+1)
      }
      else stop("response must be either timetoevent or binary")
      return(list(summary = definecors, associations = wts, covariance = Sigma,
                  data = x.out, probs=p))
}

###################
# SETUP 1: corr=0.9
###################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0.9), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0.9), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup1Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup1Results$results$model <- Models
saveRDS(setup1Results, "setup1Results.rda")
###################
# SETUP 2: corr=0.5
###################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0.5), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0.5), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup2Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup2Results$results$model <- Models
saveRDS(setup2Results, "setup2Results.rda")
###################
# SETUP 3: corr=0.1
###################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0.1), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0.1), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup3Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup3Results$results$model <- Models
saveRDS(setup3Results, "setup3Results.rda")
#################
# SETUP 4: corr=0
#################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup4Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup4Results$results$model <- Models
saveRDS(setup4Results, "setup4Results.rda")

# Mease et al. data scenarios
# simulate data function
generateMeasedata <- function(nTrain=400, nTest=1000, Ndata=100, p=30, J=2, seedStart=1, q = 0.15){
      
      trainingSets <- list()
      # simulate data
      for(iter in 1:Ndata){
            set.seed(iter+1)
            Xtrain<-matrix(0,nTrain,p)
            for (i in 1:p){
                  Xtrain[,i]<-runif(nTrain)
            }
            ytrain<-rep(0,nTrain)
            for (i in 1:nTrain){
                  ytrain[i]<-1*(runif(1)<(q+(1-2*q)*1*(sum((Xtrain[i,1:J]))>(J/2))))+1
            }
            # training data
            trainingSets[[iter]] <- data.frame(classes=factor(ytrain), Xtrain)
      }
      set.seed(1)
      Xtest<-matrix(0,nTest,p)
      for (i in 1:p){
            Xtest[,i]<-runif(nTest)
      }
      ytest<-rep(0,nTest)
      for (i in 1:nTest){
            ytest[i]<-1*(runif(1)<(q+(1-2*q)*1*(sum((Xtest[i,1:J]))>(J/2))))+1
      }
      # training sets and test set data
      testingSets <- data.frame(classes=factor(ytest), Xtest)
      list(trainingSets=trainingSets, testingSets=testingSets)
}

################
# Setup 5: J = 2
################
# simluating training data sets
q <- 0.15
simData1 <- generateMeasedata(J=2)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup5Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup5Results$results$model <- Models
saveRDS(setup5Results, "setup5ResultsAR.rda")
################
# Setup 6: J = 5
################
# simluating training data sets
simData1 <- generateMeasedata(J=5)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup6Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup6Results$results$model <- Models
saveRDS(setup6Results, "setup6ResultsAR.rda")
#################
# Setup 7: J = 15
#################
# simluating training data sets
simData1 <- generateMeasedata(J=15)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup7Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup7Results$results$model <- Models
saveRDS(setup7Results, "setup7ResultsAR.rda")
#################
# Setup 8: J = 20
#################
# simluating training data sets
simData1 <- generateMeasedata(J=20)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
BayesClasses <- as.numeric(factor(apply(simTest[,-1], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup8Results <- runBiasVarSimulation(trainingSets, simTest, BayesClasses)
setup8Results$results$model <- Models
saveRDS(setup8Results, "setup8ResultsAR.rda")

# MLBENCH DATA
#########################
# twonorm simulation data
#########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
    set.seed(i+1)
    train <- mlbench.twonorm(400, d=20)
    train <- as.data.frame(train)
    trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.twonorm(1000, d=20)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
twonormResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
twonormResults$results$model <- Models
saveRDS(twonormResults, "twonormResultsAR.rda")
###########################
# threenorm simulation data
###########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
    set.seed(i+1)
    train <- mlbench.threenorm(400, d=20)
    train <- as.data.frame(train)
    trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.threenorm(1000, d=20)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
threenormResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
threenormResults$results$model <- Models
saveRDS(threenormResults, "threenormResultsAR.rda")
##########################
# ringnorm simulation data
##########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
    set.seed(i+1)
    train <- mlbench.ringnorm(400, d=20)
    train <- as.data.frame(train)
    trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.ringnorm(1000, d=20)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
ringnormResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
ringnormResults$results$model <- Models
saveRDS(ringnormResults, "ringnormResultsAR.rda")

###############################################
# Make table for all roud bias variance results
###############################################
res1 <- readRDS("twonormResultsAR.rda")
res2 <- list(results=readRDS("threenormResultsAR.rda"))
res3 <- readRDS("ringnormResultsAR.rda")
res4 <- readRDS("circleResultsAR.rda")
res5 <- readRDS("setup1ResultsAR.rda")
res6 <- readRDS("setup2ResultsAR.rda")
res7 <- readRDS("setup3ResultsAR.rda")
res8 <- readRDS("setup4ResultsAR.rda")
res9 <- readRDS("setup5ResultsAR.rda")
res10 <- readRDS("setup6ResultsAR.rda")
res11 <- readRDS("setup7ResultsAR.rda")
res12 <- readRDS("setup8ResultsAR.rda")
resList <- list(res1, res2, res3, res4, res5, res6, res7, res8,
                res9, res10, res11, res12)
tableFinal <- NULL
for(k in 1:length(resList)){
      res <- resList[[k]]
      splitDat <- split(res$results, res$results$model)
      cname <- unique(res$results$model)
      rname <- unique(res$results$Decomposition)
      tableFrame <- matrix(0, nrow=length(rname), ncol=length(cname))
      for(i in 1:length(splitDat)){
            tableFrame[,i] <- splitDat[[i]]$vb
      }
      rownames(tableFrame) <- paste(k, rname)
      colnames(tableFrame) <- cname
      tableFinal <- rbind(tableFinal, tableFrame)
}
tableFinal <- as.data.frame(tableFinal)

n <- nrow(tableFinal)
errorTable <- tableFinal[seq(1, n, by=6),]
SEtable <- tableFinal[seq(3, n, by=6),]
VEtable <- tableFinal[seq(4, n, by=6),]
biasTable <- tableFinal[seq(5, n, by=6),]
varTable <- tableFinal[seq(6, n, by=6),]
compTableList <- list(errorTable, SEtable, VEtable, biasTable, varTable)
compPVals <- list()

# compute omnibus p-vals
library(scmamp)
for(i in 1:length(compTableList)){
      compPVals[[i]] <- friedmanAlignedRanksTest(compTableList[[i]])
}

# compute post-hoc p-vals
postPVals <- list()
for(i in 1:length(compTableList)){
      postPVals[[i]] <- postHocTest(compTableList[[i]], test="friedman",
                                    correct="shaffer")
}

# create latex table
stargazer(tableFinal, summary = FALSE)
# ---------------------------------------------------------------------------
#########################################################################
# Table 6.5 (RESULTS): Win/Tie analysis of bias, variance, systematic and 
# variance effects for random forests, including random rotation forests. 
#########################################################################
# ---------------------------------------------------------------------------
majVote <- function(x){names(which.max(table(x)))}
nTrain <- 400
nTest <- 1000

# MAIN EXPERIMENT FUNCTIONS
runBiasVarSimulation <- function(trainingSets, simTest, paraGrid, BayesPreds){
      
      # linear combination oblique trees
      sim.obliqueRRFrf <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                                model="rf", paraGrid = paraGrid, BayesPreds = BayesPreds)
      
      # randomised oblique trees using logistic splits
      sim.obliqueRRFlog <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                                 model="log", paraGrid = paraGrid, BayesPreds = BayesPreds)
      list(sim.obliqueRRFlog, sim.obliqueRRFrf)
}


simulateBiasVarDecomp <- function(trainingSets, simTest, model, paraGrid, BayesPreds, ...){
      
      numOfExp <- 100
      # train models and make predictions
      BVpreds <- matrix(0, nrow=numOfExp, ncol=nTest)
      var.T <- NULL
      var <- NULL
      bias <- NULL
      VE <- NULL
      SE <- NULL
      misclassError <- NULL
      p <- ncol(simTest)
      C <- as.numeric(simTest$classes)
      
      # train models
      for(j in 1:numOfExp){
            x <- trainingSets[[j]][,-p]
            y <- trainingSets[[j]][,p]
            mod <- RRotF(x=x, y=y, K=paraGrid[1], L=200, mtry=paraGrid[2], model=model)
            BVpreds[j,] <- predict(mod, simTest[,-p])+1
            print(paste("Method: ", model, ", Iter: ", j, " out of ", numOfExp))
      }
      
      # James (2003) decomposition estimates
      BayesClassifier <- BayesPreds
      majVoteClassifier <- apply(BVpreds, 2, function(x)majVote(x))
      var.T <- mean(BayesClassifier != C)
      var <- mean(apply(BVpreds, 1, function(x) mean(x != majVoteClassifier)))
      bias <- mean(majVoteClassifier != BayesClassifier)
      VE <- mean(apply(BVpreds, 1, function(x) mean(x != C)) - mean(majVoteClassifier != C))
      SE <- mean(majVoteClassifier != C) - mean(BayesClassifier != C)
      meanError <- mean(apply(BVpreds, 1, function(x){ mean(x != C) }))
      
      # plot bias and variance and systematic effect and variance effect
      vb <- c(meanError, var.T, SE, VE, bias, var)
      bar <- factor(c(1,2,3,4,5,6))
      type <- c("Error", "Bayes Error", "Systematic Effect", "Variance Effect", "Bias", "Variance")
      modelName <- rep(model, 6)
      biasVarPlotData <- data.frame(vb=vb, Decomposition=type, bar=bar, model=modelName)
      biasVarPlotData
}

###################
# SETUP 1: corr=0.9
###################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0.9), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0.9), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
setup1TuneVals <- readRDS("setup1ResultsAR.rda")[[2]]
setup1ParaGrid <- c(median(setup1TuneVals[[3]][,2]), median(setup1TuneVals[[4]]))
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup1Results <- runBiasVarSimulation(trainingSets, simTest, setup1ParaGrid, BayesClasses)
saveRDS(setup1Results, "obliqueRRFsetup1Results.rda")
###################
# SETUP 2: corr=0.5
###################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0.5), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0.5), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
setup2TuneVals <- readRDS("setup2ResultsAR.rda")[[2]]
setup2ParaGrid <- c(median(setup2TuneVals[[3]][,2]), median(setup2TuneVals[[4]]))
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup2Results <- runBiasVarSimulation(trainingSets, simTest, setup2ParaGrid, BayesClasses)
saveRDS(setup2Results, "obliqueRRFsetup2Results.rda")
###################
# SETUP 3: corr=0.1
###################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0.1), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0.1), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
setup3TuneVals <- readRDS("setup3ResultsAR.rda")[[2]]
setup3ParaGrid <- c(median(setup3TuneVals[[3]][,2]), median(setup3TuneVals[[4]]))
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup3Results <- runBiasVarSimulation(trainingSets, simTest, setup3ParaGrid, BayesClasses)
saveRDS(setup3Results, "obliqueRRFsetup3Results.rda")
#################
# SETUP 4: corr=0
#################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- simData(nvars=c(15), cors=c(0), associations=c(1),
                       firstonly=c(FALSE), nsamples=400, response="binary")
      train <- train$data
      train$classes <- train$outcome
      trainingSets[[i]] <- train[,-16]
}

# simulate test data set
set.seed(1)
test <- simData(nvars=c(15), cors=c(0), associations=c(1),
                firstonly=c(FALSE), nsamples=1000, response="binary")
testData <- test$data
testData$classes <- testData$outcome
simTest <- testData[,-16]

# run simulation and plot data
setup4TuneVals <- readRDS("setup4ResultsAR.rda")[[2]]
setup4ParaGrid <- c(median(setup4TuneVals[[3]][,2]), median(setup4TuneVals[[4]]))
BayesClasses <- as.numeric(factor(ifelse(test$probs > 0.5, 1, 0)))
setup4Results <- runBiasVarSimulation(trainingSets, simTest, setup4ParaGrid, BayesClasses)
saveRDS(setup4Results, "obliqueRRFsetup4Results.rda")
#################
# Setup 5: J = 2
#################
# simluating training data sets
q <- 0.15
simData1 <- generateMeasedata(J=2)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
setup5TuneVals <- readRDS("setup5ResultsAR.rda")[[2]]
setup5ParaGrid <- c(median(setup5TuneVals[[3]][,2]), median(setup5TuneVals[[4]]))
BayesClasses <- as.numeric(factor(apply(simTest[,-31], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup5Results <- runBiasVarSimulation(trainingSets, simTest, setup5ParaGrid, BayesClasses)
saveRDS(setup5Results, "obliqueRRFsetup5ResultsAR.rda")
#################
# Setup 6: J = 5
#################
# simluating training data sets
simData1 <- generateMeasedata(J=5)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
setup6TuneVals <- readRDS("setup6ResultsAR.rda")[[2]]
setup6ParaGrid <- c(median(setup6TuneVals[[3]][,2]), median(setup6TuneVals[[4]]))
BayesClasses <- as.numeric(factor(apply(simTest[,-31], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup6Results <- runBiasVarSimulation(trainingSets, simTest, setup6ParaGrid, BayesClasses)
saveRDS(setup6Results, "obliqueRRFsetup6ResultsAR.rda")
#################
# Setup 7: J = 15
#################
# simluating training data sets
simData1 <- generateMeasedata(J=15)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
setup7TuneVals <- readRDS("setup7ResultsAR.rda")[[2]]
setup7ParaGrid <- c(median(setup7TuneVals[[3]][,2]), median(setup7TuneVals[[4]]))
BayesClasses <- as.numeric(factor(apply(simTest[,-31], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup7Results <- runBiasVarSimulation(trainingSets, simTest, setup7ParaGrid, BayesClasses)
saveRDS(setup7Results, "obliqueRRFsetup7ResultsAR.rda")
#################
# Setup 8: J = 20
#################
# simluating training data sets
simData1 <- generateMeasedata(J=20)
trainingSets <- simData1[[1]]
simTest <- simData1[[2]]
# run simulation and plot data
setup8TuneVals <- readRDS("setup8ResultsAR.rda")[[2]]
setup8ParaGrid <- c(median(setup8TuneVals[[3]][,2]), median(setup8TuneVals[[4]]))
BayesClasses <- as.numeric(factor(apply(simTest[,-31], 1, function(x) 1*(0.5<(q+(1-2*q)*1*(sum((x[1:J]))>(J/2)))))))
setup8Results <- runBiasVarSimulation(trainingSets, simTest, setup8ParaGrid, BayesClasses)
saveRDS(setup8Results, "obliqueRRFsetup8ResultsAR.rda")
#########################
# twonorm simulation data
#########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.twonorm(400, d=20)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.twonorm(1000, d=20)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
twonormTuneVals <- readRDS("twonormResultsAR.rda")[[2]]
twonormParaGrid <- c(median(twonormTuneVals[[3]][,2]), median(twonormTuneVals[[4]]))
twonormResults <- runBiasVarSimulation(trainingSets, simTest, twonormParaGrid, bayesclass(test))
saveRDS(twonormResults, "obliqueRRFtwonormResults.rda")
###########################
# threenorm simulation data
###########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.threenorm(400, d=20)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.threenorm(1000, d=20)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
threenormTuneVals <- readRDS("threenormResultsAR.rda")[[2]]
threenormParaGrid <- c(5, 4)
threenormResults <- runBiasVarSimulation(trainingSets, simTest, threenormParaGrid, bayesclass(test))
saveRDS(threenormResults, "obliqueRRFthreenormResultsAR.rda")
##########################
# ringnorm simulation data
##########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.ringnorm(400, d=20)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.ringnorm(1000, d=20)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
ringnormTuneVals <- readRDS("ringnormResultsAR.rda")[[2]]
ringnormParaGrid <- c(median(ringnormTuneVals[[3]][,2]), median(ringnormTuneVals[[4]]))
ringnormResults <- runBiasVarSimulation(trainingSets, simTest, ringnormParaGrid, bayesclass(test))
saveRDS(ringnormResults, "obliqueRRFringnormResultsAR.rda")

#############################################################
# Make table for rotation random forest bias variance results
#############################################################
res1 <- readRDS("obliqueRRFtwonormResults.rda")
res2 <- readRDS("obliqueRRFthreenormResultsAR.rda")
res3 <- readRDS("obliqueRRFringnormResultsAR.rda")
res5 <- readRDS("obliqueRRFsetup1Results.rda")
res6 <- readRDS("obliqueRRFsetup2Results.rda")
res7 <- readRDS("obliqueRRFsetup3Results.rda")
res8 <- readRDS("obliqueRRFsetup4Results.rda")
res9 <- readRDS("obliqueRRFsetup5ResultsAR.rda")
res10 <- readRDS("obliqueRRFsetup6ResultsAR.rda")
res11 <- readRDS("obliqueRRFsetup7ResultsAR.rda")
res12 <- readRDS("obliqueRRFsetup8ResultsAR.rda")
ORRFresList <- list(res1, res2, res3, res5, res6, res7, res8,
                    res9, res10, res11, res12)

# rbind results
for(i in 1:length(ORRFresList)){
      temp <- ORRFresList[[i]]
      if(i < 4 || i > 7){
            ORRFresList[[i]] <- rbind(temp[[3]], temp[[1]])
      } else {
            ORRFresList[[i]] <- rbind(temp[[2]], temp[[1]])
      }
}

# import old results
res1 <- readRDS("twonormResultsAR.rda")$results
res2 <- readRDS("threenormResultsAR.rda")
res3 <- readRDS("ringnormResultsAR.rda")$results
res5 <- readRDS("setup1ResultsAR.rda")$results
res6 <- readRDS("setup2ResultsAR.rda")$results
res7 <- readRDS("setup3ResultsAR.rda")$results
res8 <- readRDS("setup4ResultsAR.rda")$results
res9 <- readRDS("setup5ResultsAR.rda")$results
res10 <- readRDS("setup6ResultsAR.rda")$results
res11 <- readRDS("setup7ResultsAR.rda")$results
res12 <- readRDS("setup8ResultsAR.rda")$results
resList <- list(res1, res2, res3, res5, res6, res7, res8,
                res9, res10, res11, res12)

# combine old with new results
for(i in 1:length(ORRFresList)){
      temp <- resList[[i]]
      ORRFresList[[i]] <- rbind(temp, ORRFresList[[i]])
}

# make tables for thesis
tableFinal <- NULL
for(k in 1:length(resList)){
      res <- ORRFresList[[k]]
      splitDat <- split(res, res$model)
      cname <- unique(res$model)
      rname <- unique(res$Decomposition)
      tableFrame <- matrix(0, nrow=length(rname), ncol=length(cname))
      for(i in 1:length(splitDat)){
            tableFrame[,i] <- splitDat[[i]]$vb
      }
      rownames(tableFrame) <- paste(k, rname)
      colnames(tableFrame) <- cname
      tableFinal <- rbind(tableFinal, tableFrame)
}
tableFinal <- as.data.frame(tableFinal)

# makes tables
n <- nrow(tableFinal)
errorTable <- tableFinal[seq(1, n, by=6),]
SEtable <- tableFinal[seq(3, n, by=6),]
VEtable <- tableFinal[seq(4, n, by=6),]
biasTable <- tableFinal[seq(5, n, by=6),]
varTable <- tableFinal[seq(6, n, by=6),]
compTableList <- list(errorTable, SEtable, VEtable, biasTable, varTable)
compPVals <- list()

# compute omnibus p-vals
library(scmamp)
for(i in 1:length(compTableList)){
      compPVals[[i]] <- friedmanAlignedRanksTest(compTableList[[i]][,-5])
}

# compute post-hoc p-vals
postPVals <- list()
for(i in 1:length(compTableList)){
      postPVals[[i]] <- postHocTest(compTableList[[i]][,-5], test="aligned ranks",
                                    correct="finner", control=5)
}

# create latex table
stargazer(tableFinal, summary = FALSE)
# ---------------------------------------------------------------------------
