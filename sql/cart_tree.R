#raw <- read.csv("cart_tree_input.csv")

raw <- read.csv("d0_100.csv")
raw[is.na(raw)] <- 0

dripsDrain.df <- raw[,c('DripsPercentage', 'EnergyChangeRate')]
L <- (dripsDrain.df$EnergyChangeRate > 0) 
dripsDrain.df <- dripsDrain.df[L,]

attach(dripsDrain.df)
hist(DripsPercentage)
hist(EnergyChangeRate)
plot(DripsPercentage, EnergyChangeRate)

# Use semi-transparent dots
library(ggplot2)
#sp <- ggplot(dripsDrain.df, aes(x=DripsPercentage, y=EnergyChangeRate))
#sp + geom_point(alpha=.01)

# hex colors
library(hexbin)
sp <- ggplot(dripsDrain.df, aes(x=DripsPercentage, y=EnergyChangeRate))
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red", limits=c(0,5000))

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
labelText <- paste("y= ",round(interceptText,digits=1), " + (", round(coeffText,digits=1), " * Drips)", sep='')

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
colnames(offender) <- c(saveLongNames[1],1:(length(saveLongNames)-1))

# examine the offender data
str(offender)

#pairs.panels(offender[c("1", "2", "3", "4")])
#pairs.panels(offender[c("15", "2")])

# the distribution of EnergyChangeRate
hist(offender$EnergyChangeRate)

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
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 2, extra = 1)
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
mean(offender_train$EnergyChangeRate) # result = 678.0393
