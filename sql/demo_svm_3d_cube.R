# http://stackoverflow.com/questions/10266195/plot-svm-in-3-dimension

library(e1071)
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