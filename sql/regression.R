raw <- read.csv("drips_regression.csv")

fit <- lm(EnergyChangeRate ~ DripsPercentage, data=raw)
summary(fit)

#with (raw)
plot(raw$DripsPercentage, raw$EnergyChangeRate)

changeRate = raw$EnergyChangeRate
hist(changeRate, right=FALSE)

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics 