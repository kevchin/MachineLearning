library(ggplot2)
# hex colors
library(hexbin)

#raw <- read.csv("cart_tree_input.csv")

saveOut <- FALSE
raw <- read.csv("d0_100.csv")
raw[is.na(raw)] <- 0
L <- (raw$EnergyChangeRate > 0)
raw <- raw[L,]


dripsDrain.df <- raw[,c('DripsPercentage', 'EnergyChangeRate', 'Duration')]
dripsDrain.df['DripsCategory'] <- '0% DRIPS'
L <- (dripsDrain.df$DripsPercentage > 0) & (dripsDrain.df$DripsPercentage < 95)
dripsDrain.df[L,]$DripsCategory <- '1-94% DRIPS'
L <- (dripsDrain.df$DripsPercentage > 94) 
dripsDrain.df[L,]$DripsCategory <- '95%+ DRIPS'



#library(plyr)
#cdf <- ddply(dripsDrain.df[L,], "Category", summarise, EnergyChangeRate.mean=mean(EnergyChangeRate))

# Density plots
#ggplot(dripsDrain.df[L,], aes(x=EnergyChangeRate, colour=Category, fill=Category)) +
#  geom_density(alpha=.1) +
#  geom_vline(data=cdf, aes(xintercept=EnergyChangeRate.mean,  colour="red"),
#             linetype="dashed", size=1)

L <- (dripsDrain.df$EnergyChangeRate < 1000 )
# Density plots
labelText <- paste("Avg Drain Rates by DRIPS grouping\n (< 1 Watt)")
ggplot(dripsDrain.df[L,], aes(x=EnergyChangeRate, colour=DripsCategory, fill=DripsCategory)) +
  geom_density(alpha=.1) +
  annotate("text", label=labelText, colour = "black", x=500, y=0.006)

if (saveOut) ggsave(file="ggChart1.png", width=6, height=5)

#ggplot(dripsDrain.df[L,], aes(x=EnergyChangeRate, fill=Category)) +
#  geom_histogram(binwidth=.5, alpha=.5, position="identity")

attach(dripsDrain.df)
hist(DripsPercentage)
if (saveOut) dev.copy(png,"myhist7.png", width=400, height=400)
if (saveOut) dev.off()
dev.set(2)
  
L <- (dripsDrain.df$DripsPercentage > 90) 
hist(dripsDrain.df[L,]$DripsPercentage, xlab="DRIPS Rates (%)", main="DRIPS Rates > 90%")

if (saveOut) dev.copy(png,"myhist8.png", width=400, height=400)
if (saveOut) dev.off()
#dev.set(1)
  
allRow <- nrow(dripsDrain.df)
L <- (dripsDrain.df$DripsPercentage < 1) 
percentZero <- (nrow(dripsDrain.df[L,]) * 1.0) / allRow
mw0 <- mean(dripsDrain.df[L,]$EnergyChangeRate)
duration0 <- sum(dripsDrain.df[L,]$Duration)
avg0Duration <- (duration0 *1.0) / (nrow(dripsDrain.df[L,]))
L <- (dripsDrain.df$EnergyChangeRate < 1000 ) & L
ggplot(dripsDrain.df[L,], aes(x=EnergyChangeRate)) + 
  geom_histogram(binwidth=.5, colour="grey", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
if (saveOut) ggsave(file="ggChart2.png", width=6, height=5)

L <- (dripsDrain.df$DripsPercentage > 0) &(dripsDrain.df$DripsPercentage < 95)
percentMiddle <- (nrow(dripsDrain.df[L,]) *1.0 ) / allRow
mwMid <- mean(dripsDrain.df[L,]$EnergyChangeRate)
duration1 <- sum(dripsDrain.df[L,]$Duration)
avg1Duration <- (duration1 *1.0) / (nrow(dripsDrain.df[L,]))

L <- (dripsDrain.df$EnergyChangeRate < 1000 ) & L
ggplot(dripsDrain.df[L,], aes(x=EnergyChangeRate)) + 
  geom_histogram(aes(y=..density..),binwidth=.5, colour="grey", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
if (saveOut) ggsave(file="ggChart3.png", width=6, height=5)

L <- (dripsDrain.df$DripsPercentage > 94) 
mw95 <- mean(dripsDrain.df[L,]$EnergyChangeRate)
percent95up <- (nrow(dripsDrain.df[L,]) *1.0) / allRow
duration2 <- sum(dripsDrain.df[L,]$Duration)
avg2Duration <- (duration2 *1.0) / (nrow(dripsDrain.df[L,]))
                                                                      
avgDrainRates<- c(mw0, mwMid, mw95)
dripsDist <- c(percentZero, percentMiddle, percent95up)
durations <- c(duration0, duration1,duration2)
avgDur <- c(avg0Duration, avg1Duration, avg2Duration)                        

dripsSummary <- data.frame(category=c("0 Drips %", "1-94%", "95+%"), 
                           drainRate=avgDrainRates, 
                           percentage=dripsDist,
                           duration=durations,
                           avgDuration=avgDur)

labelText <- paste("CS Session by DRIPS % Rate")
ggplot(dripsSummary, aes(x=category, y=percentage, fill=category)) + 
  geom_bar(stat="identity", fill=c("#CC0000", "#000099", "#009E73")) + 
  geom_text(aes(label=round(percentage,2)), vjust=-0.2) +
  annotate("text", label=labelText, colour = "black", x=2, y=0.6)
if (saveOut) ggsave(file="ggChart4.png", width=6, height=5)

labelText <- paste("CS Session Drain Rate by DRIPS Group")
ggplot(dripsSummary, aes(x=category, y=drainRate, fill=category)) + 
  geom_bar(stat="identity", fill=c("#CC0000", "#000099", "#009E73")) + 
  geom_text(aes(label=round(drainRate,2)), vjust=-0.2) +
  annotate("text", label=labelText, colour = "black", x=2, y=900)
if (saveOut) ggsave(file="ggChart5.png", width=6, height=5)

labelText <- paste("Avg Session Duration  by DRIPS Group")
ggplot(dripsSummary, aes(x=category, y=avgDuration, fill=category)) + 
  geom_bar(stat="identity", fill=c("#CC0000", "#000099", "#009E73")) + 
  geom_text(aes(label=round(avgDuration,2)), vjust=-0.2) +
  annotate("text", label=labelText, colour = "black", x=2, y=900)
if (saveOut) ggsave(file="ggChart6.png", width=6, height=5)

#plot(dripsDist)

hist(EnergyChangeRate)
if (saveOut) dev.copy(png,"myhist1.png", width=400, height=400)
if (saveOut) dev.off()
dev.set(2)
#dev.set(1)

L <- (dripsDrain.df$EnergyChangeRate < 1000) 
hist(dripsDrain.df[L,]$EnergyChangeRate, xlab="Drain Rates (mwh)", main="Drain Rates < 1Watt")
if (saveOut) dev.copy(png,"myhist2.png", width=400, height=400)
if (saveOut) dev.off()
dev.set(2)
#dev.set(1)

plot(DripsPercentage, EnergyChangeRate, main=paste("CS Sessions (n =", nrow(dripsDrain.df), ")", sep=''))
if (saveOut) dev.copy(png,"myplot1.png", width=400, height=400)
if (saveOut) dev.off()
dev.set(2)
#dev.set(1)

# R sample to take smaller dataset
#mySample <- dripsDrain.df[sample(1:nrow(dripsDrain.df), 500, replace=FALSE),]

# remove the top and bottom 10% DRIPS
L <- (dripsDrain.df$DripsPercentage > 4) & (dripsDrain.df$DripsPercentage < 95)  
mySample <- dripsDrain.df[L,-(3:4)]

# K-means

#library(stats)
#set.seed(1234)
#nc <- NbClust(mySample, min.nc=2, max.nc=15, method="kmeans")
#table(nc$Best.n[1,])

#barplot(table(nc$Best.n[1,]), 
#          xlab="Numer of Clusters", ylab="Number of Criteria",
#          main="Number of Clusters Chosen by 26 Criteria")

labelText <- paste("Heat Map of Density: DRIPS[5%-95%]")

sp <- ggplot(mySample, aes(x=DripsPercentage, y=EnergyChangeRate))
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red", limits=c(0,5000))+
  annotate("text", label=labelText, colour = "red", x=50.5, y=4000)
if (saveOut) ggsave(file="ggChart7.png", width=6, height=5)


kSize=1
cl <- kmeans(mySample, kSize)
#myTitle <- paste("K-Means Clustering k=",kSize,sep='')
#plot(mySample, col = cl$cluster, main=myTitle)

labelText <- paste("K=1 Mean = (", round(cl$centers[1,1],1), ", ", round(cl$centers[1,2],1), ")", sep='')
sp <- ggplot(mySample, aes(x=DripsPercentage, y=EnergyChangeRate, 
      #colour = c("darkred", "orange", "yellow", "black"))) + 
      colour = cl$cluster))+
#  scale_colour_manual(values=c("red", "blue"))+
  scale_fill_hue() +
  geom_point(size=3)+
  theme(legend.position="none")+
  annotate("text", label=labelText, colour = "red", x=50.5, y=4000)+
  annotate("text", label="X", colour = "red", 
           x=cl$centers[1,1], 
           y=cl$centers[1,2])
sp
if (saveOut) ggsave(file="ggChart8.png", width=6, height=5)

labelText <- paste("Heat Map w/ K Means (k=1): DRIPS[5%-95%] \nmean=(85,256)")
sp <- ggplot(mySample, aes(x=DripsPercentage, y=EnergyChangeRate))
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red", limits=c(0,5000))+
  annotate("text", label=labelText, colour = "red", x=50.5, y=4000)+
  annotate("text", label="X", colour = "black", 
           x=cl$centers[1,1], 
           y=cl$centers[1,2])

if (saveOut) ggsave(file="ggChart9.png", width=6, height=5)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(mySample)

#  annotate("segment", x=10,xend=10,y=20,yend=0,arrow=arrow(), color="red") +

kSize=4
cl <- kmeans(mySample, kSize)
labelText <- paste("K=4 Mean", "(n=", nrow(mySample), ")", sep='')

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sp <- ggplot(mySample, aes(x=DripsPercentage, y=EnergyChangeRate, 
                           #colour = c("darkred", "orange", "yellow", "black"))) + 
                           colour = cl$cluster))+
  #  scale_colour_manual(values=c("red", "blue"))+
  scale_fill_hue() +
  geom_point(size=3)+
  annotate("text", label=labelText, colour = "red", x=50.5, y=4000)+
  annotate("text", label=paste("X", " ", "(n = ", cl$size[1],")"), 
           colour = "red", 
           x=cl$centers[1,1], 
           y=cl$centers[1,2])+
  annotate("text", label=paste("X", " ", "(n = ", cl$size[2],")"), 
           colour = "red", 
           x=cl$centers[2,1], 
         y=cl$centers[2,2])+
  annotate("text", label=paste("X", " ", "(n = ", cl$size[3],")"), 
           colour = "red", 
           x=cl$centers[3,1], 
         y=cl$centers[3,2])+
annotate("text", label=paste("X", " ", "(n = ", cl$size[4],")"), 
         colour = "red", 
         x=cl$centers[4,1], 
         y=cl$centers[4,2])

sp

if (saveOut) ggsave(file="ggChart10.png", width=6, height=5)

#
# Tells me the optimal number of clusters given this dataset
#
cl$size
cl$centers



# remove the 0 drips entries
L <- (dripsDrain.df$DripsPercentage > 0) 
dripsDrainGT0.df <- dripsDrain.df[L,]

model <- lm(EnergyChangeRate ~ DripsPercentage, data = dripsDrainGT0.df )
summary(model)

# Given a model, predict values of yvar from xvar
# This supports one predictor and one predicted variable
# xrange: If NULL, determine the x range from the model object. If a vector with
# two numbers, use those as the min and max of the prediction range.
# samples: Number of samples across the x range.
# ...: Further arguments to be passed to predict()
predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
  # If xrange isn't passed in, determine xrange from the models.
  # Different ways of extracting the x range, depending on model type
  if (is.null(xrange)) {
    if (any(class(model) %in% c("lm", "glm")))
      xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% "loess"))
      xrange <- range(model$x)
  }
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}

interceptText <- summary(model)$coefficients[1,1]
coeffText <- summary(model)$coefficients[2,1]
labelText <- paste("y= ",
                   round(interceptText,digits=1), 
                   " + (", round(coeffText,digits=1), " * Drips%)", sep='')

pred <- predictvals(model, "DripsPercentage", "EnergyChangeRate")
  
sp <- ggplot(dripsDrain.df, aes(x=DripsPercentage, y=EnergyChangeRate)) 
sp + stat_bin2d(bins=50) +
  scale_fill_gradient(low="lightblue", high="red", limits=c(0,5000)) +
  geom_line(data=pred,colour = "red", size = 1) +
  theme(legend.position="none")+
  annotate("text", label=labelText, colour = "red", x=50.5, y=3000)

if (saveOut) ggsave(file="ggChart11.png", width=6, height=5)

# Remove non-meaningful factors
# removing uSession,  Duration
offender <- raw[,-c(1,4)]

# Only Look at the 0 DRIPS
L <- (offender$DripsPercentage == 0) 
offender <- offender[L,]

# Now don't need rownum, drips anymore
offender <- offender[,-c(1)]

# save the long names
saveLongNames <- colnames(offender)

offenderNames <- colnames(offender)[-1]
colnames(offender) <- sub(".*\\._SB.", "", saveLongNames, perl=TRUE)
colnames(offender) <- sub("Marvell.AVASTAR.Wireless.", "", colnames(offender), perl=TRUE)

#colnames(offender) <- c(saveLongNames[1],1:(length(saveLongNames)-1))

# examine the offender data
str(offender)

#pairs.panels(offender[c("1", "2", "3", "4")])
#pairs.panels(offender[c("15", "2")])

# the distribution of EnergyChangeRate
hist(offender$EnergyChangeRate,xlab="Drain Rates (mw)", main=paste("Zero DRIPS ", "(n=", nrow(offender),")", sep=''))
if (saveOut) dev.copy(png,"myhist3.png", width=400, height=400)
if (saveOut) dev.off()
dev.set(2)

# summary statistics of the offender data
summary(offender)
nrow(offender)

offender_train <- offender[1:(nrow(offender)/2), ]
offender_test <- offender[((nrow(offender)/2) + 1):nrow(offender), ]

library(rpart)
m.rpart <- rpart(EnergyChangeRate ~ ., data = offender_train)

#m.rpart <- rpart(EnergyChangeRate ~ ., data = offender)
# get basic information about the tree
m.rpart

# get more detailed information about the tree
summary(m.rpart)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# this Type=2 is pretty good
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, 
           main="Zero DRIPS: Drain Rate v. Offender Active %",
           type = 2, extra = 1)

if (saveOut) dev.copy(png,"myrpart1.png", width=400, height=400)
if (saveOut) dev.off()
dev.set(2)

#rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 1)
#rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 4, extra = 1)

## Step 4: Evaluate model performance ----

# generate predictions for the testing dataset
p.rpart <- predict(m.rpart, offender_test)

# compare the distribution of predicted values vs. actual values
summary(p.rpart)
summary(offender_test$EnergyChangeRate)

# compare the correlation
cor(p.rpart, offender_test$EnergyChangeRate)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, offender_test$EnergyChangeRate)

# mean absolute error between actual values and mean value
mean(offender_train$EnergyChangeRate) # result = 782.9983


###-----------------------
### Correlation
###-----------------------


shortNameOffender <- offender[-1]
L <- colSums(shortNameOffender) > 1000
shortNameOffender <- shortNameOffender[,L]
colnames(shortNameOffender) <- colnames(saveLongNames[,L])

#apply(shortNameOffender, 2, FUN=function(x) length(which(x > 50)))

#shortNames <- sub(".*(.....)$","\\1",grep("_SB",saveLongNames,value=TRUE))

#colnames(shortNameOffender) <- sub(".*(.....)$","\\1",grep("_SB",saveLongNames,value=TRUE))

colnames(shortNameOffender) <- sub(".*\\._SB.", "", colnames(shortNameOffender), perl=TRUE)
colnames(shortNameOffender) <- sub("Marvell.AVASTAR.Wireless.", "", colnames(shortNameOffender), perl=TRUE)
colnames(shortNameOffender) <- sub("(.....).*", "\\1", colnames(shortNameOffender), perl=TRUE)


# Corrgram based on offenders
library(corrgram)
corrgram(shortNameOffender, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Zero DRIPS Offender Correlation")
        
if (saveOut) dev.copy(png,"mycorr1.png", width=400, height=400)
if (saveOut) dev.off()
dev.set(2)

# Find out worse Offenders during Zero DRIPS
gt10Count <- apply(shortNameOffender, MARGIN=2, function(x) { sum(x >10)})
df2 <- data.frame(gt10Count)
df3 <- data.frame(rownames(df2), df2$gt10Count)
colnames(df3) <- c("Offender", "Count")

labelText <- paste("Zero Drips: Offenders > 10% Busy")
ggplot(df3, aes(x=Offender, y=Count)) + geom_bar(stat = "bin")+
  annotate("text", label=labelText, colour = "red", x=4, y=1500)

if (saveOut) ggsave(file="ggChart12.png", width=6, height=5)

#write.table(shortNameOffender, file="zeroDripsClassify.csv", sep=",", row.names=FALSE)

#### 
###--------------
### SVM DEMO
library(rgl)
#install.packages('misc3d')
#require('misc3d')

library(misc3d)

n    = 100
nnew = 50

# Simulate some data
set.seed(12345)
group = sample(2, n, replace=T)
# group is now 100 long, with either 1 or 2

dat   = data.frame(group=factor(group), matrix(rnorm(n*3, rep(group, each=3)), ncol=3, byrow=T))

# Fit SVM
fit = svm(group ~ ., data=dat)

# Plot original data
plot3d(dat[,-1], col=dat$group)

# Get decision values for a new data grid
newdat.list = lapply(dat[,-1], function(x) seq(min(x), max(x), len=nnew))
newdat      = expand.grid(newdat.list)
newdat.pred = predict(fit, newdata=newdat, decision.values=T)
newdat.dv   = attr(newdat.pred, 'decision.values')
newdat.dv   = array(newdat.dv, dim=rep(nnew, 3))

# Fit/plot an isosurface to the decision boundary

contour3d(newdat.dv, level=0, x=newdat.list$X1, y=newdat.list$X2, z=newdat.list$X3, add=T)

######## END Demo


### SVM
library(e1071)

dataset.df <- read.csv("zeroDripsClassify_HandTuned3000.csv")
index <- 1:nrow(dataset.df)

# testindex is 1/3 of the sample
testindex <- sample(index, trunc(length(index)/3)) 

# get rid of some not needed columns
#
dataColumns <-  !grepl("Count", colnames(dataset.df))
offenders.df <- dataset.df[,dataColumns]

gt10Count <- apply(offenders.df[,-1], MARGIN=2, function(x) { sum(x >10)})
f2 <- data.frame(offender=rownames(as.data.frame(gt10Count)), 
                 gt10Count)
colnames(f2) <- c("Offender", "Count")
attach(f2)
f2 <- f2[order(Count),]
detach(f2)

f2$Offender <- factor(f2$Offender, levels=f2[order(f2$Count),"Offender"])
my.color <- "#CC6600"

labelText <- paste("Count of Common Root Cause Offenders (count > 10)\nZero DRIPS Sessions (n=", sum(f2$Count), ")", sep="")

ggplot(f2, aes(x=Offender, y=Count)) +
  geom_bar(stat = "identity", fill=my.color)+
  annotate("text", label=labelText, colour = "black", x=10, y=1500) +
  coord_flip()

if (saveOut) ggsave(file="ggChart13.png", width=6, height=5)

# Get a test and training set
testset <- offenders.df[testindex,]
trainset <- offenders.df[-testindex,]

# Run SVM naively (without training on cost or gamma)
svm.model <- svm(Classify ~., data=trainset, cost=10, gamma=1, cross=10)

#Call:
#  svm(formula = Classify ~ ., data = trainset, cost = 10, gamma = 1, cross = 10)


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  10 
#gamma:  1 

#Number of Support Vectors:  580

svm.pred <- predict(svm.model, testset[,-1])

tab <- table(pred=svm.pred, true=testset[,1])
matchDiagPercentage <- classAgreement(tab)$diag

# Higher is better
# cost =10, gamma = 1, result = 93%
matchDiagPercentage


df2 <- data.frame(summary(svm.pred))
colnames(df2) <- c("Count")
df3 <- data.frame(rownames(df2), df2$Count)
colnames(df3) <- c("Offender", "Count")
df2 <- df3

L <- (df3$Count > 10)
df3 <- df3[L,]
df3["percentage"] <- (df3$Count *1.0)/ nrow(df3)
#df3[,$percentage] <- (df3$Count *1.0)/ nrow(df3)

# Sort
attach(df3)
df3 <- df3[order(Count),]
detach(df3)
#df3$Count <- factor(df3$Count, levels=df3[order(df3$percentage), "Count"])
df3$Offender <- factor(df3$Offender, levels=df3[order(df3$percentage), "Offender"])


labelText <- paste("SVM Model of Common Root Cause Offenders (count > 10)\nZero DRIPS Sessions (n=", sum(df3$Count), ")", sep="")
maxY <- as.integer((max(df3$percentage) * 1.2))
maxX <- nrow(df3)+3

#my.color <- "#CC0000" # Orange , use mspaint dropper | custom | convert to HEX
my.color <- "#CC6600"

ggplot(df3, aes(x=Offender, y=percentage)) + geom_bar(stat = "identity", fill=my.color)+
  annotate("text", label=labelText, colour = "black", x=2, y=12) +
  coord_flip()

if (saveOut) ggsave(file="ggChart14.png", width=6, height=5)


###-----------

L <- (df2$Count <= 10) & (df2$Count > 1)
df3 <- df2[L,]
df3["percentage"] <- as.factor((df3$percentage *1.0)/ nrow(df3))
attach(df3)
df3 <- df3[order(Count),]
detach(df3)
#df3$Count <- factor(df3$Count, levels=df3[order(df3$percentage), "Count"])
df3$Offender <- factor(df3$Offender, levels=df3[order(df3$percentage), "Offender"])


labelText <- paste("SVM Model of Rare Root Cause Offenders (count <= 10)\nZero DRIPS Sessions (n=", sum(df3$Count), ")", sep="")
maxY <- as.integer(round((max(df3$Count) * 1.2),0))
ggplot(df3, aes(x=Offender, y=Count)) + geom_bar(stat = "identity")+
  annotate("text", label=labelText, colour = "red", x=3, y=7) + coord_flip()

if (saveOut) ggsave(file="ggChart15.png", width=6, height=5)

#---------------------------------
