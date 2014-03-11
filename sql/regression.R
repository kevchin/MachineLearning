raw <- read.csv("drips_regression.csv")

L <- (raw$EnergyChangeRate > 50)  & (raw$EnergyChangeRate < 1000)
filter.df <- raw[L,]

L <- (filter.df$DripsPercentage > 0)
filter.df <- filter.df[L,]


dripsRange <- filter.df$DripsPercentage
hist(dripsRange, right=FALSE, main="DRIPS Distribution")

changeRate = filter.df$EnergyChangeRate
hist(changeRate, right=FALSE, main="Change Distribution")

durationRange = filter.df$Duration
hist(durationRange, right=FALSE, main="Duration Range")

attach(filter.df)
fit <- lm(EnergyChangeRate ~ DripsPercentage, data=filter.df)
summary(fit)
plot(DripsPercentage, EnergyChangeRate)
abline(fit)


#plot(EnergyChangeRate, Duration)


# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics 
