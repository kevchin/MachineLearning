library(ggplot2)
# hex colors
library(hexbin)

#raw <- read.csv("cart_tree_input.csv")

raw <- read.csv("d0_100.csv")
raw[is.na(raw)] <- 0

dripsDrain.df <- raw[,c('DripsPercentage', 'EnergyChangeRate')]
L <- (dripsDrain.df$EnergyChangeRate > 0) 
dripsDrain.df <- dripsDrain.df[L,]

attach(dripsDrain.df)
hist(DripsPercentage)
L <- (dripsDrain.df$DripsPercentage > 90) 
hist(dripsDrain.df[L,]$DripsPercentage, xlab="DRIPS Rates (%)", main="DRIPS Rates > 90%")

hist(EnergyChangeRate)
L <- (dripsDrain.df$EnergyChangeRate < 1000) 
hist(dripsDrain.df[L,]$EnergyChangeRate, xlab="Drain Rates (mwh)", main="Drain Rates < 1Watt")
plot(DripsPercentage, EnergyChangeRate, main=paste("CS Sessions (n =", nrow(dripsDrain.df), ")", sep=''))

# R sample to take smaller dataset
#mySample <- dripsDrain.df[sample(1:nrow(dripsDrain.df), 500, replace=FALSE),]

# remove the top and bottom 10% DRIPS
L <- (dripsDrain.df$DripsPercentage > 4) & (dripsDrain.df$DripsPercentage < 95)  
mySample <- dripsDrain.df[L,]

# K-means

#library(NbClust)
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
  annotate("text", label=labelText, colour = "red", x=50.5, y=4000)+
  annotate("text", label="X", colour = "red", 
           x=cl$centers[1,1], 
           y=cl$centers[1,2])
sp

labelText <- paste("Heat Map w/ K Means (k=1): DRIPS[5%-95%] \nmean=(85,256)")
sp <- ggplot(mySample, aes(x=DripsPercentage, y=EnergyChangeRate))
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red", limits=c(0,5000))+
  annotate("text", label=labelText, colour = "red", x=50.5, y=4000)+
  annotate("text", label="X", colour = "black", 
           x=cl$centers[1,1], 
           y=cl$centers[1,2])



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
  annotate("text", label=labelText, colour = "red", x=50.5, y=3000)


# Remove non-meaningful factors
# removing uSession,  Duration
offender <- raw[,-c(1,4)]

# Remove the 0 ChangeRate
L <- (offender$EnergyChangeRate > 0) 
offender <- offender[L,]

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

# summary statistics of the offender data
summary(offender)
nrow(offender)

offender_train <- offender[1:(nrow(offender)/2), ]
offender_test <- offender[((nrow(offender)/2) + 1):nrow(offender), ]

library(rpart)
m.rpart <- rpart(EnergyChangeRate ~ ., data = offender_train)

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
        

# Find out worse Offenders during Zero DRIPS
gt10Count <- apply(shortNameOffender, MARGIN=2, function(x) { sum(x >10)})
df2 <- data.fram(gt10Count)
df3 <- data.frame(rownames(df2), df2$gt10Count)
colnames(df3) <- c("Offender", "Count")

labelText <- paste("Zero Drips: Offenders > 10% Busy")
ggplot(df3, aes(x=Offender, y=Count)) + geom_bar(stat = "bin")+
  annotate("text", label=labelText, colour = "red", x=4, y=1500)
  
write.table(shortNameOffender, file="zeroDripsClassify.csv", sep=",", row.names=FALSE)

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

rightAnswers <- read.csv("zeroDripsClassifyRightAnswers.csv")
testSet <- shortNameOffender[-(1:nrow(rightAnswers)),]
svm.model <- svm(Classify ~., data=rightAnswers, cost=100, gamma=1)
svm.pred <- predict(svm.model, rightAnswers[,-1])
table(pred=svm.pred, true=rightAnswers[,1])

svm.pred <- predict(svm.model, testSet)

df2 <- data.frame(summary(svm.pred))
colnames(df2) <- c("Count")
df3 <- data.frame(rownames(df2), df2$Count)
colnames(df3) <- c("Offender", "Count")
labelText <- paste("SVM Model of Root Cause Offenders\nZero DRIPS Sessions")
ggplot(df3, aes(x=Offender, y=Count)) + geom_bar(stat = "bin")+
  annotate("text", label=labelText, colour = "red", x=4, y=1000)

df4 <- data.frame(svm.pred)
colnames(df4) <- c("Root")
df4 <- data.frame(rownames(df4), df4$Root)
colnames(df4) <- c("rowIndex", "Root")
rIndex <- c(nrow(rightAnswers)+1:(nrow(df4)))
df4$rowIndex <- rIndex
predictions.output <- data.frame(df4$Root,testSet )
write.table(predictions.output, file="zeroDripsPredictClassify.csv", sep=",", row.names=FALSE)
