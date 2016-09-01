################################################
# CHAPTER 5: BIAS AND VARIANCE IN RANDOM FORESTS
################################################

# Check for missing packages and install if missing
list.of.packages <- c("latex2exp", "mlbench", "ggplot2", "caret", "doSNOW", "lattice",
                      "gridExtra", "grid", "stargazer", "ipred", "gbm", "randomForest",
                      "rpart")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
load <- lapply(list.of.packages, require, character.only = TRUE)

#############################################
# Figure 5.1: Bias and variance in regression
# ###########################################
# ---------------------------------------------------------------------------
# Specifiy means and standard deviation
dmar <- par()$mar
par(mar=c(0,0,0,0))
mean1=65; sd1=15
lb1=80; ub1=120
mean2=100; sd2=25
lb2=80; ub2=120

# simulate data
x1 <- seq(-4,4,length=100)*sd1 + mean1
hx1 <- dnorm(x1,mean1,sd1)
x2 <- seq(-4,4,length=100)*sd2 + mean2
hx2 <- dnorm(x2,mean2,sd2)

# create empty plot
plot(x1, hx1, type="n", xlab="", ylab="",
     xlim=c(0, 200), ylim=c(-0.003, 0.04),
     main="", axes=FALSE)

# Draw distributions
lines(x1, hx1)
lines(x2, hx2)

# make arrows and mean lines
arrows(0, 0, 0, 0.04, xpd = TRUE, length = 0.1)
arrows(0, 0, 200, 0, xpd = TRUE, length = 0.1)
arrows(mean1-sd1, max(hx1), mean1+sd1, max(hx1), length = 0.05, code=3, col="darkorange")
arrows(mean2-sd2, max(hx2), mean2+sd2, max(hx2), length = 0.05, code=3, col="darkorange")
arrows(mean1, 0.029, mean2, 0.029, length = 0.05, code=3, col="darkgreen")
lines(c(mean1,mean1), c(0, 0.035), lty=2, col="blue")
lines(c(mean2, mean2), c(0, 0.035), lty=2, col="blue")

# add text to the plot
proby <- TeX("$P(Y|x)$")
probtry <- TeX("$P_{\\Omega_{tr}}(Y|x)$")
bayes <- TeX("$Irreducible Error$")
var <- TeX("$Variance$")
bias <- TeX("$Bias$")
bayesModel <- TeX("$f_B(x)$")
avgModel <- TeX("$\\bar{f}(x)$")
text(30, 0.005,proby,cex=0.8)
text(150, 0.005, probtry, cex=0.8)
text(35, max(hx1), bayes, cex=0.8)
text(135, max(hx2), var, cex=0.8)
text((mean1+mean2)/2, 0.032, bias, cex=0.8)
text(mean1, -0.0029, bayesModel, cex=0.8)
text(mean2, -0.0029, avgModel, cex=0.8)
text(-6, 0.038, "P")
text(190, -0.0025, "Y")
# ---------------------------------------------------------------------------
#########################################################################
# Figure 5.2: Bias and variance of an estimated distribution: Left: Large 
# bias and small variance. Right: Small bias and large variance.
#########################################################################
# ---------------------------------------------------------------------------
# Large bias, small variance
# Specify means and standard deviations
mean1=85; sd1=15
lb1=80; ub1=120
mean2=140; sd2=5
lb2=80; ub2=120

# simulate data
x1 <- seq(-4,4,length=100)*sd1 + mean1
hx1 <- dnorm(x1,mean1,sd1)
x2 <- seq(-4,4,length=100)*sd2 + mean2
hx2 <- dnorm(x2,mean2,sd2)

# create empty plot
plot(x1, hx1, type="n", xlab="", ylab="",
     xlim=c(0, 200), ylim=c(-0.003, 0.08),
     main="", axes=FALSE)

# draw distributions
lines(x1, hx1)
lines(x2, hx2)
arrows(0, 0, 0, 0.08, xpd = TRUE, length = 0.1)
arrows(0, 0, 200, 0, xpd = TRUE, length = 0.1)
arrows(mean2-sd2, max(hx2), mean2+sd2, max(hx2), length = 0.05, code=3, col="darkorange")
arrows(mean1-sd1, max(hx1), mean1+sd1, max(hx1), length = 0.05, code=3, col="darkorange")
arrows(mean1, 0.5*max(hx2), mean2, 0.5*max(hx2), length = 0.05, code=3, col="darkgreen")
lines(c(mean1,mean1), c(0, 0.08), lty=2, col="blue")
lines(c(mean2, mean2), c(0, 0.08), lty=2, col="blue")

# add text to the plot
proby <- TeX("$P(y|x)$")
probtry <- TeX("$P_{\\Omega_{tr}}(y|x)$")
bayesModel <- TeX("$f_B(x)$")
avgModel <- TeX("$\\bar{f}(x)$")
text(65, 0.02,proby,cex=0.9)
text(165, 0.005, probtry, cex=0.9)
text((mean1+mean2)/2, 0.5*max(hx2)+0.003, "Large bias", cex=0.9)
text(mean2+sd2+17, max(hx2), "Small variance", cex=0.9)
text(mean1, -0.0029, bayesModel, cex=0.9)
text(mean2, -0.0029, avgModel, cex=0.9)
text(-5, 0.078, "P")
text(190, -0.0025, "Y")
# Small bias, large variance
# specify means and standard deviations
mean1=85; sd1=15
lb1=80; ub1=120
mean2=90; sd2=25
lb2=80; ub2=120

# simulate data
x1 <- seq(-4,4,length=100)*sd1 + mean1
hx1 <- dnorm(x1,mean1,sd1)
x2 <- seq(-3,4,length=100)*sd2 + mean2
hx2 <- dnorm(x2,mean2,sd2)

# create empty plot
plot(x1, hx1, type="n", xlab="", ylab="",
     xlim=c(0, 200), ylim=c(-0.003, 0.08),
     main="", axes=FALSE)

# draw distributions
lines(x1, hx1)
lines(x2, hx2)
arrows(0, 0, 0, 0.08, xpd = TRUE, length = 0.1)
arrows(0, 0, 200, 0, xpd = TRUE, length = 0.1)
arrows(mean1-sd1, max(hx1), mean1+sd1, max(hx1), length = 0.05, code=3, col="darkorange")
arrows(mean2-sd2, max(hx2), mean2+sd2, max(hx2), length = 0.05, code=3, col="darkorange")
arrows(mean1, 0.05, mean2, 0.05, length = 0.05, code=3, col="darkgreen")
lines(c(mean1,mean1), c(0, 0.08), lty=2, col="blue")
lines(c(mean2, mean2), c(0, 0.08), lty=2, col="blue")

# add text
proby <- TeX("$P(y|x)$")
probtry <- TeX("$P_{\\Omega_{tr}}(y|x)$")
bayesModel <- TeX("$f_B(x)$")
avgModel <- TeX("$\\bar{f}(x)$")
text(65, 0.02,proby,cex=0.9)
text(140, 0.007, probtry, cex=0.9)
text(mean2+15, 0.05, "Small bias", cex=0.9)
text(mean2+sd2+18, max(hx2), "Large variance", cex=0.9)
text(mean1-2, -0.0029, bayesModel, cex=0.9)
text(mean2+2, -0.0029, avgModel, cex=0.9)
text(-4, 0.078, "P")
text(190, -0.0025, "Y")
# ---------------------------------------------------------------------------
############################################################################
# Figure 5.3: The effect of decreasing the variance of probability estimates 
# on classification when f > 0.5 and E(PΩT R ) > 0.5.
############################################################################
# ---------------------------------------------------------------------------
# specify means and standard deviations
mean1=720; sd1=15
lb1=700; ub1=720
mean2=220; sd2=40
lb2=200; ub2=220

# simulate data
x1 <- seq(-4,4,length=100)*sd1 + mean1
hx1 <- dnorm(x1,mean1,sd1)
x2 <- seq(-4,4,length=100)*sd2 + mean2
hx2 <- dnorm(x2,mean2,sd2)

# create empty plot
plot(c(x1, x2), c(hx1, hx2), type="n", xlab="", ylab="",
     xlim=c(0, 900), ylim=c(-0.003, 0.04),
     main="", axes=FALSE)

# draw distributions
lines(x1, hx1)
lines(x2, hx2)
# draw probabilit areas
i <- x1 >= lb1
polygon(c(lb1,x1[i],ub1), c(0,hx1[i],0), col="red")
j <- x2 >= lb2
polygon(c(lb2,x2[j],ub2), c(0,hx2[j],0), col="red")

# make arrows and decision boundary/mean lines
arrows(0, 0, 0, 0.04, xpd = TRUE, length = 0.05)
arrows(500, 0, 500, 0.04, xpd = TRUE, length = 0.05)
arrows(0, 0, 400, 0, xpd = TRUE, length = 0.05)
arrows(500, 0, 900, 0, xpd = TRUE, length = 0.05)
arrows(350, 0.02, 490, 0.02, xpd = TRUE, length = 0.2, lwd=2, col="purple")
arrows(mean1-sd1, max(hx1), mean1+sd1, max(hx1), length = 0.05, code=3, col="darkorange")
arrows(310, 0.01, 250, 0.003, length = 0.05)
arrows(mean2-sd2, max(hx2), mean2+sd2, max(hx2), length = 0.05, code=3, col="darkorange")
arrows(820, 0.01, 740, 0.003, length = 0.05)
lines(c(200,200), c(0, 0.035), lty=2, col="green")
lines(c(700, 700), c(0, 0.035), lty=2, col="green")
lines(c(mean1, mean1), c(0, 0.03), lty=2, col="blue")
lines(c(mean2, mean2), c(0, 0.03), lty=2, col="blue")

# add text
text(-10, 0.038, "P")
text(490, 0.038, "P")
text(0, -0.002, "0.0", cex=0.8)
text(200, -0.002, "0.5", cex=0.8)
text(400, -0.002, "1.0", cex=0.8)
text(500, -0.002, "0.0", cex=0.8)
text(700, -0.002, "0.5", cex=0.8)
text(900, -0.002, "1.0", cex=0.8)
text(415, 0.025, "Decrease Variance", cex = 0.8)
text(200, 0.037, "Decision threshold", cex=0.7)
text(700, 0.037, "Decision threshold", cex=0.7)
text(mean1+5, 0.032, TeX("$E(P_{\\Omega_{TR}})$"), cex=0.8)
text(mean2+5, 0.032, TeX("$E(P_{\\Omega_{TR}})$"), cex=0.8)
text(mean1+sd1+45, max(hx1), TeX("$Var(P_{\\Omega_{TR}})$"), cex=0.8)
text(mean2-sd2-45, max(hx2), TeX("$Var(P_{\\Omega_{TR}})$"), cex=0.8)
text(310, 0.012, TeX("$P(\\bar{g}(x) = g_B(x))$"), cex=0.8)
text(820, 0.012, TeX("$P(\\bar{g}(x) = g_B(x))$"), cex=0.8)
# ---------------------------------------------------------------------------
############################################################################
# Figure 5.4: The effect of increasing the variance of probability estimates 
# on classification when f > 0.5 and E(PΩT R ) < 0.5.
############################################################################
# ---------------------------------------------------------------------------
# specify means and standard deviations
mean1=180; sd1=15
lb1=200; ub1=220
mean2=680; sd2=40
lb2=700; ub2=720

# simulate data
x1 <- seq(-4,4,length=100)*sd1 + mean1
hx1 <- dnorm(x1,mean1,sd1)
x2 <- seq(-4,4,length=100)*sd2 + mean2
hx2 <- dnorm(x2,mean2,sd2)

# create empty plot
plot(c(x1, x2), c(hx1, hx2), type="n", xlab="", ylab="",
     xlim=c(0, 900), ylim=c(-0.003, 0.04),
     main="", axes=FALSE)

# draw distributions
lines(x1, hx1)
lines(x2, hx2)
# draw probabilit areas
i <- x1 >= lb1
polygon(c(lb1,x1[i],ub1), c(0,hx1[i],0), col="red")
j <- x2 >= lb2
polygon(c(lb2,x2[j],ub2), c(0,hx2[j],0), col="red")

# make arrows and decision boundary, mean lines
arrows(0, 0, 0, 0.04, xpd = TRUE, length = 0.05)
arrows(500, 0, 500, 0.04, xpd = TRUE, length = 0.05)
arrows(0, 0, 400, 0, xpd = TRUE, length = 0.05)
arrows(500, 0, 900, 0, xpd = TRUE, length = 0.05)
arrows(350, 0.02, 490, 0.02, xpd = TRUE, length = 0.2, lwd=2, col="purple")
arrows(mean1-sd1, max(hx1), mean1+sd1, max(hx1), length = 0.05, code=3, col="darkorange")
arrows(260, 0.01, 208, 0.003, length = 0.05)
arrows(mean2-sd2, max(hx2), mean2+sd2, max(hx2), length = 0.05, code=3, col="darkorange")
arrows(800, 0.005, 720, 0.003, length = 0.05)
lines(c(200,200), c(0, 0.035), lty=2, col="green")
lines(c(700, 700), c(0, 0.035), lty=2, col="green")
lines(c(180, 180), c(0, 0.03), lty=2, col="blue")
lines(c(680, 680), c(0, 0.03), lty=2, col="blue")

# add text
text(-10, 0.038, "P")
text(490, 0.038, "P")
text(0, -0.002, "0.0", cex=0.8)
text(200, -0.002, "0.5", cex=0.8)
text(400, -0.002, "1.0", cex=0.8)
text(500, -0.002, "0.0", cex=0.8)
text(700, -0.002, "0.5", cex=0.8)
text(900, -0.002, "1.0", cex=0.8)
text(415, 0.025, "Increase Variance", cex=0.8)
text(200, 0.037, "Decision threshold", cex=0.7)
text(700, 0.037, "Decision threshold", cex=0.7)
text(179, 0.032, TeX("$E(P_{\\Omega_{TR}})$"), cex=0.8)
text(679, 0.032, TeX("$E(P_{\\Omega_{TR}})$"), cex=0.8)
text(mean1-sd1-45, max(hx1), TeX("$Var(P_{\\Omega_{TR}})$"), cex=0.8)
text(mean2+sd2+45, max(hx2), TeX("$Var(P_{\\Omega_{TR}})$"), cex=0.8)
text(270, 0.012, TeX("$P(\\bar{g}(x) = g_B(x))$"), cex=0.8)
text(800, 0.006, TeX("$P(\\bar{g}(x) = g_B(x))$"), cex=0.8)
# ---------------------------------------------------------------------------
###########################################################################
# Figure 5.5: Class distributions for a three class classification task: 
# Left: The true distribution. Middle: Class distribution over training set 
# samples for the first classifier. Right: Class distribution over training 
# set samples for the second classifier.
###########################################################################
# ---------------------------------------------------------------------------
# create empty plot
plot(1:12, 1:12, type="n", xlab="", ylab="",
     xlim=c(0, 12), ylim=c(-0.04, 1),
     main="", axes=FALSE)

# add arrows
arrows(0, 0, 0, 1, xpd = TRUE, length = 0.1)
arrows(0, 0, 12, 0, xpd = TRUE, length = 0.1)
# first distribution
lines(c(0,1), c(0.6, 0.6), col="darkgreen")
lines(c(1,1), c(0.6, 0), col="darkgreen")
lines(c(1,2), c(0.3, 0.3), col="darkorange")
lines(c(2,2), c(0.3, 0), col="darkorange")
lines(c(2,3), c(0.1, 0.1), col="skyblue")
lines(c(3,3), c(0.1, 0), col="skyblue")
# second dist
lines(c(4,4), c(0, 0.1), col="darkgreen")
lines(c(4,5), c(0.1, 0.1), col="darkgreen")
lines(c(5,5), c(0, 0.7), col="darkorange")
lines(c(5,6), c(0.7, 0.7), col="darkorange")
lines(c(6,6), c(0.7, 0), col="darkorange")
lines(c(6,7), c(0.2, 0.2), col="skyblue")
lines(c(7,7), c(0.2, 0), col="skyblue")
# third dist
lines(c(8,8), c(0, 0.2), col="darkgreen")
lines(c(8,9), c(0.2, 0.2), col="darkgreen")
lines(c(9,9), c(0, 0.5), col="darkorange")
lines(c(9,10), c(0.5, 0.5), col="darkorange")
lines(c(10,10), c(0.5, 0), col="darkorange")
lines(c(10,11), c(0.3, 0.3), col="skyblue")
lines(c(11,11), c(0.3, 0), col="skyblue")
# place text
text(-0.17, 0.95, "P")
text(11.6, -0.03, "C")
text(0.5, 0.65, "0.6")
text(1.5, 0.35, "0.3")
text(2.5, 0.15, "0.1")
text(4.5, 0.15, "0.1")
text(5.5, 0.75, "0.7")
text(6.5, 0.25, "0.2")
text(8.5, 0.25, "0.2")
text(9.5, 0.55, "0.5")
text(10.5, 0.35, "0.3")
#classes
text(0.5, -0.03, "1")
text(1.5, -0.03, "2")
text(2.5, -0.03, "3")
text(4.5, -0.03, "1")
text(5.5, -0.03, "2")
text(6.5, -0.03, "3")
text(8.5, -0.03, "1")
text(9.5, -0.03, "2")
text(10.5, -0.03, "3")
# math text
text(1.5, 0.85, TeX("$P(C|x)$"))
text(5.5, 0.85, TeX("$P^1_{\\Omega_{TR}}$"))
text(9.5, 0.85, TeX("$P^2_{\\Omega_{TR}}$"))
# ---------------------------------------------------------------------------
############################################################################
# Figure 5.6: Class distributions for a three class classification task with 
# both estimated distributions having equal variance. The true distribution 
# is given on the left.
############################################################################
# ---------------------------------------------------------------------------
# create empty plot
plot(1:12, 1:12, type="n", xlab="", ylab="",
     xlim=c(0, 12), ylim=c(-0.04, 1),
     main="", axes=FALSE)

# add arrows
arrows(0, 0, 0, 1, xpd = TRUE, length = 0.1)
arrows(0, 0, 12, 0, xpd = TRUE, length = 0.1)
# first distribution
lines(c(0,1), c(0.6, 0.6), col="darkgreen")
lines(c(1,1), c(0.6, 0), col="darkgreen")
lines(c(1,2), c(0.3, 0.3), col="darkorange")
lines(c(2,2), c(0.3, 0), col="darkorange")
lines(c(2,3), c(0.1, 0.1), col="skyblue")
lines(c(3,3), c(0.1, 0), col="skyblue")
# second dist
lines(c(4,4), c(0, 0.3), col="darkgreen")
lines(c(4,5), c(0.3, 0.3), col="darkgreen")
lines(c(5,5), c(0, 0.5), col="darkorange")
lines(c(5,6), c(0.5, 0.5), col="darkorange")
lines(c(6,6), c(0.5, 0), col="darkorange")
lines(c(6,7), c(0.2, 0.2), col="skyblue")
lines(c(7,7), c(0.2, 0), col="skyblue")
# third dist
lines(c(8,8), c(0, 0.2), col="darkgreen")
lines(c(8,9), c(0.2, 0.2), col="darkgreen")
lines(c(9,9), c(0, 0.5), col="darkorange")
lines(c(9,10), c(0.5, 0.5), col="darkorange")
lines(c(10,10), c(0.5, 0), col="darkorange")
lines(c(10,11), c(0.3, 0.3), col="skyblue")
lines(c(11,11), c(0.3, 0), col="skyblue")
# place text
text(-0.17, 0.95, "P")
text(11.6, -0.03, "C")
text(0.5, 0.65, "0.6")
text(1.5, 0.35, "0.3")
text(2.5, 0.15, "0.1")
text(4.5, 0.35, "0.3")
text(5.5, 0.55, "0.5")
text(6.5, 0.25, "0.2")
text(8.5, 0.25, "0.2")
text(9.5, 0.55, "0.5")
text(10.5, 0.35, "0.3")
#classes
text(0.5, -0.03, "1")
text(1.5, -0.03, "2")
text(2.5, -0.03, "3")
text(4.5, -0.03, "1")
text(5.5, -0.03, "2")
text(6.5, -0.03, "3")
text(8.5, -0.03, "1")
text(9.5, -0.03, "2")
text(10.5, -0.03, "3")
# math text
text(1.5, 0.75, TeX("$P(C|x)$"))
text(5.5, 0.75, TeX("$P^1_{\\Omega_{TR}}$"))
text(9.5, 0.75, TeX("$P^2_{\\Omega_{TR}}$"))
par(mar=dmar)
# ---------------------------------------------------------------------------
#########################################################################
# Figure 5.8: A two-dimensional representation of the simulated data from 
# the machine learning benchmark problems found in the mlbench R package.
#########################################################################
# ---------------------------------------------------------------------------
# 2dnormals: 2d example of data distribution
example <- mlbench.2dnormals(400, cl=6)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("2dnormals") +
      theme(legend.position="none")

# Twonorm: 2d example of data distribution
example <- mlbench.twonorm(400, d=2)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("Twonorm") +
      theme(legend.position="none")

# Threenorm: 2d example of data distribution
example <-  mlbench.threenorm(400, d=2)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("Threenorm") +
      theme(legend.position="none")

# Ringnorm: 2d example of data distribution
example <-  mlbench.ringnorm(400, d=2)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("Ringnorm") +
      theme(legend.position="none")

# Circle: 2d example of data distribution
example <-  mlbench.circle(400, d=2)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("Circle") +
      theme(legend.position="none")

# Cassini: 2d example of data distribution
example <-  mlbench.cassini(400)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("Cassini") +
      theme(legend.position="none")

# Cuboids: 2d example of data distribution
example <-  mlbench.cuboids(400)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("Cuboids") +
      theme(legend.position="none")

# XOR: 2d example of data distribution
example <- mlbench.xor(400, d=2)
example <- as.data.frame(example)
ggplot(example, aes(x=x.1, y=x.2, col=classes)) + geom_point(size=2) +
      theme_bw() + xlab("X1") + ylab("X2") + ggtitle("XOR") +
      theme(legend.position="none")
# ---------------------------------------------------------------------------
############################################################################
# Table 5.1: Estimated bias, variance, systematic effect and variance effect 
# on simulated data. 
############################################################################
# ---------------------------------------------------------------------------
majVote <- function(x){names(which.max(table(x)))}
nTrain <- 400
nTest <- 1000
Models <- factor(rep(c("Tree", "Bagging", "Forest-RI", "Boosting"), each=6), level=c("Tree", "Bagging", "Forest-RI", "Boosting"))

# performs computations in parallel
cl <- makeCluster(3, type="SOCK")
registerDoSNOW(cl)

# MAIN EXPERIMENT FUNCTIONS
runBiasVarSimulation <- function(trainingSets, simTest, BayesPreds){

    loss <- ifelse(length(levels(simTest$classes)) > 2, "multinomial", "adaboost")

    # parameter tuning settings
    fitControl <- trainControl(method = "cv", number = 10)
    treeparaGrid <- expand.grid(cp=seq(0.1, 1, by=0.1))
    rfparaGrid <- expand.grid(mtry=seq(1, ncol(simTest)-2, by=2))
    gbmparaGrid <- expand.grid(n.trees=200, interaction.depth=c(1, 6), shrinkage=c(0.01, 0.05, 0.1),
                               n.minobsinnode=10)
    # boosting model
    sim.Boost <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                       method="gbm", paraGrid = gbmparaGrid, tControl=fitControl,
                                       BayesPreds=BayesPreds, distribution=loss ,verbose=FALSE)
    # single tree model
    sim.Tree <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                      method="rpart", paraGrid = treeparaGrid, tControl=fitControl, BayesPreds=BayesPreds)
    # bagging model
    sim.Bag <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                     method="treebag", paraGrid=NULL, tControl=trainControl(method="none"),
                                     BayesPreds=BayesPreds, nbagg=200)
    # random forest model
    sim.RF <- simulateBiasVarDecomp(trainingSets=trainingSets, simTest=simTest,
                                    method="rf", paraGrid = rfparaGrid, tControl=fitControl,
                                    BayesPreds=BayesPreds, ntree=200)

    list(results=rbind(sim.Tree$results, sim.Bag$results, sim.RF$results, sim.Boost$results),
         tuneValues=list(sim.Tree$tuneValues, sim.Bag$tuneValues, sim.RF$tuneValues, sim.Boost$tuneValues))
}


simulateBiasVarDecomp <- function(trainingSets, simTest, method, paraGrid, tControl, BayesPreds, ...){
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

    # plot bias and variance and systematic effect and variance effect
    vb <- c(meanError, var.T, SE, VE, bias, var)
    bar <- factor(c(1,2,3,4,5,6))
    type <- c("Error", "Bayes Error", "Systematic Effect", "Variance Effect", "Bias", "Variance")
    model <- rep(method, 6)
    biasVarPlotData <- data.frame(vb=vb, Decomposition=type, bar=bar, model=model)
    list(results=biasVarPlotData, tuneValues=tuneVals[-1,])
}

#######################
# Designed scenarios  #
#######################
# load data generation library
library(pensim)
# simulate data function from "pensim" package
simData <- function (nvars = c(100, 100, 100, 100, 600), cors = c(0.8, 0, 0.8, 0, 0),
                     associations = c(0.5, 0.5, 0.3, 0.3, 0), firstonly = c(TRUE, FALSE, TRUE, FALSE, FALSE),
                     nsamples = 100, censoring = "none",
                     labelswapprob = 0, response = "timetoevent", basehaz = 0.2,
                     logisticintercept = 0)
{
      library(MASS)
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
                  wts[definecors[i, "start"]:definecors[i, "end"]] <- definecors[i,
                                                                                 "associations"]
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
                  x.out$time[x.out$time > censtimes] <- censtimes[x.out$time >
                                                                        censtimes]
            }
      }
      else if (identical(response, "binary")) {
            p <- 1/(1 + exp(-(betaX + logisticintercept)))
            x.out$outcome <- ifelse(p > runif(length(p)), 1, 0) #rbinom(length(p), 1, p)
            if (labelswapprob > 0) {
                  do.swap <- runif(length(p)) < labelswapprob
                  new.outcome <- x.out$outcome
                  new.outcome[x.out$outcome == 1 & do.swap] <- 0
                  new.outcome[x.out$outcome == 0 & do.swap] <- 1
                  x.out$outcome <- new.outcome
            }
            x.out$outcome <- factor(x.out$outcome)
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
ggplot(data=setup1Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 1")
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
ggplot(data=setup2Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 2")
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
ggplot(data=setup3Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 3")
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
ggplot(data=setup4Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 4")

# Mease et al. data scenarios
# simulate data function
generateMeasedata <- function(nTrain=400, nTest=1000, Ndata=100, J=2, seedStart=1, q = 0.15){
      
      trainingSetsHD <- list()
      # simulate data
      for(iter in 1:Ndata){
            set.seed(iter+1)
            p <- 30
            Xtrain<-matrix(0,nTrain,p)
            for (i in 1:p){
                  Xtrain[,i]<-runif(nTrain)
            }
            ytrain<-rep(0,nTrain)
            for (i in 1:nTrain){
                  ytrain[i]<-1*(runif(1)<(q+(1-2*q)*1*(sum((Xtrain[i,1:J]))>(J/2))))
            }
            # training data
            trainingSetsHD[[iter]] <- data.frame(classes=factor(ytrain), Xtrain)
      }
      set.seed(1)
      Xtest<-matrix(0,nTest,p)
      for (i in 1:p){
            Xtest[,i]<-runif(nTest)
      }
      ytest<-rep(0,nTest)
      for (i in 1:nTest){
            ytest[i]<-1*(runif(1)<(q+(1-2*q)*1*(sum((Xtest[i,1:J]))>(J/2))))
      }
      # training sets and test set data
      testingSetsHD <- data.frame(classes=factor(ytest), Xtest)
      list(trainingSetsHD=trainingSetsHD, testingSetsHD=testingSetsHD)
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
saveRDS(setup5Results, "setup5Results.rda")
ggplot(data=setup5Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 5")

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
saveRDS(setup6Results, "setup6Results.rda")
ggplot(data=setup6Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 6")

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
saveRDS(setup7Results, "setup7Results.rda")
ggplot(data=setup7Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 7")

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
saveRDS(setup8Results, "setup8Results.rda")
ggplot(data=setup8Results$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Scenario 8")

# MLBENCH DATA
###########################
# 2dnormals simulation data
###########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.2dnormals(400, cl=6)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.2dnormals(1000, cl=6)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
dnormalsResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
dnormalsResults$results$model <- Models
saveRDS(dnormalsResults, "2dnormalsResultsTune.rda")
ggplot(data=dnormalsResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("2dnormals (2, 6)")

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
saveRDS(twonormResults, "twonormResultsTune.rda")
ggplot(data=twonormResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity") +
    theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
    theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    guides(fill = guide_legend(title = "Decomposition:"))+
    geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Twonorm (20, 2)")

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
saveRDS(threenormResults, "threenormResultsTune.rda")
ggplot(data=threenormResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity") +
    theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
    theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    guides(fill = guide_legend(title = "Decomposition:"))+
    geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Threenorm (20, 2)")

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
saveRDS(ringnormResults, "ringnormResultsTune.rda")
ggplot(data=ringnormResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
    theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
    theme(legend.position = "none", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    guides(fill = guide_legend(title = "Decomposition:"))+
    geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Ringnorm (20, 2)")

########################
# circle simulation data
########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
    set.seed(i+1)
    train <- mlbench.circle(400, d=20)
    train <- as.data.frame(train)
    trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.circle(1000, d=20)
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
    geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Circle (20, 2)")

#########################
# cassini simulation data
#########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
      set.seed(i+1)
      train <- mlbench.cassini(400)
      train <- as.data.frame(train)
      trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.cassini(1000)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
cassiniResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
cassiniResults$results$model <- Models
saveRDS(cassiniResults, "cassiniResultsTune.rda")
ggplot(data=cassiniResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
      theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
      theme(legend.position = "bottom", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
      guides(fill = guide_legend(title = "Decomposition:"))+
      geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Cassini (2, 3)")

#########################
# cuboids simulation data
#########################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
    set.seed(i+1)
    train <- mlbench.cuboids(400)
    train <- as.data.frame(train)
    trainingSets[[i]] <- train[1:400,]
}

# simulate test data set
set.seed(1)
test <- mlbench.cuboids(1000)
testFrame <- as.data.frame(test)
simTest <- testFrame[1:1000,]

# run simulation and plot data
cuboidsResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test)[1:1000])
cuboidsResults$results$model <- Models
saveRDS(cuboidsResults, "cuboidsResultsTune.rda")
ggplot(data=cuboidsResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position="identity") +
    theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
    theme(legend.position = "bottom", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    guides(fill = guide_legend(title = "Decomposition:"))+
    geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("Cuboids (3, 4)")

#####################
# xor simulation data
#####################
# simluating training data sets
trainingSets <- list()
for(i in 1:100){
    set.seed(i+1)
    train <- mlbench.xor(400, d=2)
    train <- as.data.frame(train)
    trainingSets[[i]] <- train
}

# simulate test data set
set.seed(1)
test <- mlbench.xor(1000, d=2)
testFrame <- as.data.frame(test)
simTest <- testFrame

# run simulation and plot data
xorResults <- runBiasVarSimulation(trainingSets, simTest, bayesclass(test))
xorResults$results$model <- Models
saveRDS(xorResults, "xorResultsTune.rda")
ggplot(data=xorResults$results, aes(x=bar, y=vb, fill=Decomposition)) + geom_bar(stat="identity", position = "identity") +
    theme_bw() + ylab("Error/Bias+Variance") + xlab("") +
    theme(legend.position = "bottom", axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    guides(fill = guide_legend(title = "Decomposition:"))+
    geom_hline(yintercept = 0, col="red") + facet_grid(. ~ model) + ggtitle("XOR (2, 2)")

#################################
# create simulation results table 
#################################
res1 <- readRDS("setup1Results.rda")
res2 <- readRDS("setup2Results.rda")
res3 <- readRDS("setup3Results.rda")
res4 <- readRDS("setup4Results.rda")
res5 <- readRDS("setup5Results.rda")
res6 <- readRDS("setup6Results.rda")
res7 <- readRDS("setup7Results.rda")
res8 <- readRDS("setup8Results.rda")
res9 <- readRDS("2dnormalsResultsTune.rda")
res10 <- readRDS("twonormResultsTune.rda")
res11 <- readRDS("threenormResultsTune.rda")
res12 <- readRDS("ringnormResultsTune.rda")
res13 <- readRDS("circleResultsTune.rda")
res14 <- readRDS("cassiniResultsTune.rda")
res15 <- readRDS("cuboidsResultsTune.rda")
res16 <- readRDS("xorResultsTune.rda")
resList <- list(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10, res11,
                res12, res13, res14, res15, res16)
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

# perform statistical tests
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
      compPVals[[i]] <- friedmanAlignedRanksTest(compTableList[[i]][,-1])
}

# compute post-hoc p-vals
postPVals <- list()
for(i in 1:length(compTableList)){
      postPVals[[i]] <- postHocTest(compTableList[[i]][,-1], test="aligned ranks",
                                    correct="shaffer")
}

# create latex table
stargazer(tableFinal, summary = FALSE)
# ---------------------------------------------------------------------------
####################################################################
# In text: Correlation between bias, systematic effect, variance and 
# variance effect
####################################################################
# ---------------------------------------------------------------------------
# compute bias, variance, systematic effect and variance effect correlations
SEIndex <- seq(3, 96, by=6)
VEIndex <- seq(4, 96, by=6)
biasIndex <- seq(5, 96, by=6)
varIndex <- seq(6, 96, by=6)
treeCors <- c(cor(tableFinal[biasIndex, 1], tableFinal[SEIndex, 1]),
                    cor(tableFinal[varIndex, 1], tableFinal[VEIndex, 1]))
baggingCors <- c(cor(tableFinal[biasIndex, 2], tableFinal[SEIndex, 2]),
              cor(tableFinal[varIndex, 2], tableFinal[VEIndex, 2]))
RFCors <- c(cor(tableFinal[biasIndex, 3], tableFinal[SEIndex, 3]),
              cor(tableFinal[varIndex, 3], tableFinal[VEIndex, 3]))
boostingCors <- c(cor(tableFinal[biasIndex, 4], tableFinal[SEIndex, 4]),
              cor(tableFinal[varIndex, 4], tableFinal[VEIndex, 4]))

# compute median correlation over the different algorithms
biasSECor <- median(c(treeCors[1], baggingCors[1], RFCors[1], boostingCors[1]))
varVECor <- median(c(treeCors[2], baggingCors[2], RFCors[2], boostingCors[2]))
# ---------------------------------------------------------------------------
##########################################################################
# Figure 5.9: Variation in the selection of the optimal subset size of 
# randomly selected input variables at each node for Forest-RI over 100 
# training sets dis- played for the first eight simulation configurations.
##########################################################################
# ---------------------------------------------------------------------------
# plot parameter histogram for each data set and compute the standard deviation
# Sim 1
# plot line and bar plot
tuneVals1 <- res1$tuneValues[[3]]
sd1 <- round(sd(tuneVals1), 2)
barData1 <- summary(factor(tuneVals1, levels=sort(unique(tuneVals1))))
g1 <- ggplot(data.frame(mtry=tuneVals1), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData1))), y=barData1), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 1: mvnorm, p=15, corr=0.9; [ SD = ", sd1, " ]")))
# Sim 2
# plot line and bar plot
tuneVals2 <- res2$tuneValues[[3]]
sd2 <- round(sd(tuneVals2), 2)
barData2 <- summary(factor(tuneVals2, levels=sort(unique(tuneVals2))))
g1 <- ggplot(data.frame(mtry=tuneVals2), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData2))), y=barData2), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 2: mvnorm, p=15, corr=0.5; [ SD = ", sd2, " ]")))
# Sim 3
# plot line and bar plot
tuneVals3 <- res3$tuneValues[[3]]
sd3 <- round(sd(tuneVals3), 2)
barData3 <- summary(factor(tuneVals3, levels=sort(unique(tuneVals3))))
g1 <- ggplot(data.frame(mtry=tuneVals3), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData3))), y=barData3), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 3: mvnorm, p=15, corr=0.1; [ SD = ", sd3, " ]")))
# Sim 4
# plot line and bar plot
tuneVals4 <- res4$tuneValues[[3]]
sd4 <- round(sd(tuneVals4), 2)
barData4 <- summary(factor(tuneVals4, levels=sort(unique(tuneVals4))))
g1 <- ggplot(data.frame(mtry=tuneVals4), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData4))), y=barData4), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 4: mvnorm, p=15, corr=0; [ SD = ", sd4, " ]")))
# Sim 5
# plot line and bar plot
tuneVals5 <- res5$tuneValues[[3]]
sd5 <- round(sd(tuneVals5), 2)
barData5 <- summary(factor(tuneVals5, levels=sort(unique(tuneVals5))))
g1 <- ggplot(data.frame(mtry=tuneVals5), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData5))), y=barData5), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 5: Mease (2008), p=30, J=2; [ SD = ", sd5, " ]")))
# Sim 6
# plot line and bar plot
tuneVals6 <- res6$tuneValues[[3]]
sd6 <- round(sd(tuneVals6), 2)
barData6 <- summary(factor(tuneVals6, levels=sort(unique(tuneVals6))))
g1 <- ggplot(data.frame(mtry=tuneVals6), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData6))), y=barData6), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 6: Mease (2008), p=30, J=5; [ SD = ", sd6, " ]")))
# Sim 7
# plot line and bar plot
tuneVals7 <- res7$tuneValues[[3]]
sd7 <- round(sd(tuneVals7), 2)
barData7 <- summary(factor(tuneVals7, levels=sort(unique(tuneVals7))))
g1 <- ggplot(data.frame(mtry=tuneVals7), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData7))), y=barData7), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 7: Mease (2008), p=30, J=15; [ SD = ", sd7, " ]")))
# Sim 8
# plot line and bar plot
tuneVals8 <- res8$tuneValues[[3]]
sd8 <- round(sd(tuneVals8), 2)
barData8 <- summary(factor(tuneVals8, levels=sort(unique(tuneVals8))))
g1 <- ggplot(data.frame(mtry=tuneVals8), aes(x=1:100, y=mtry)) + geom_line(col="darkorange") + geom_point() +
      theme_bw() + ylab("Variable subsample size") + xlab("Training set")
g2 <- ggplot(data.frame(x=factor(as.numeric(names(barData8))), y=barData8), aes(x=x, y=y)) + geom_bar(stat="identity", fill="skyblue") +
      theme_bw() + xlab("Variable subsample size") + ylab("Frequency")
grid.arrange(g1,g2, ncol=2, top = textGrob(label = paste("Sim 8: Mease (2008), p=30, J=20; [ SD = ", sd8, " ]")))
# ---------------------------------------------------------------------------

