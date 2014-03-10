raw <- read.csv("drips_regression.csv")

L <- (raw$EnergyChangeRate > 50)  & (raw$EnergyChangeRate < 1000)
filter.df <- raw[L,]


fit <- lm(EnergyChangeRate ~ DripsPercentage, data=filter.df)
summary(fit)

#with (raw)
plot(filter.df$DripsPercentage, filter.df$EnergyChangeRate)

changeRate = filter.df$EnergyChangeRate
hist(changeRate, right=FALSE)

dripsRate = filter.df$DripsPercentage
hist(dripsRate, right=FALSE)

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics 
