rm( list = ls() )

# Make sure you're in the 'src' directory.
# REGRESSION ANALYSIS

library(tidyverse)
library(lubridate)
library(leaps)

# Load `df` and `ds`.
load('../data/newData.Rdata')
# df -- this is the complete dataset, unstandardized.
# ds -- same as df, but standardized.


# X is all the quantitative data, due to causing linear dependencies
X.ind <- data %>% select( -c(1, 3:5) )

num.features <- length( names(X.ind) ) - 1

# forward selection performed works for 90 even with dependencies *doublecheck later*
step.forward <- regsubsets( Salary ~ ., data = X.ind, method = "forward", nvmax = num.features )
step.forward.sum <- summary(step.forward)

# formulas for determining best model with many variables
adjusted.fits <- as.data.frame( cbind( Rsqr = step.forward.sum$rsq, adjRsqr = step.forward.sum$adjr2, 
                                      bic = step.forward.sum$bic, cp = step.forward.sum$cp ) )

# r squared value useless in this case due to amount of variables get rid
best.models <- apply( adjusted.fits[2:4], 2, which.min )

# best model determined by bic, which usually produces a small model (penalty on more variables)
best.bic <- as.data.frame( coef( step.forward.sum$obj, best.models[1] ) )
names(best.bic)[1] <- "value"

#best model determined by cp values
best.cp <- as.data.frame( coef( step.forward.sum$obj, best.models[2] ) )
names(best.cp)[1] <- "value"

set.seed(1)
k = 10
folds = sample( 1:k, nrow(X.ind), replace=TRUE )
cv.errors = matrix( NA, k, num.features, dimnames = list( NULL,c(1:num.features) ) )

for (j in 1:k){
  best.fit = regsubsets( Salary ~., data=X.ind[folds!=j,], nvmax=num.features,method="forward" )
  testmat = model.matrix( Salary ~., data = X.ind[folds==j,] )
  for (i in 1:num.features){
    coefi = coef( best.fit, id=i )
    xvars = names( coefi )
    pred = testmat[,xvars]%*%coefi
    cv.errors[j,i] = mean( (X.ind$Salary[folds==j]-pred)^2 )
  }
}
msep <- apply( cv.errors, 2, mean )
number.variables <- which.min(msep)
msep.min <- min(msep)

best.fit <- regsubsets( Salary ~., data = X.ind, nvmax = number.variables, method = "forward" )
final.variables <- coef( best.fit,id=number.variables )

xvars = names(final.variables)
testmat <- model.matrix( Salary ~., data=X.ind )
Xmat <- testmat[,xvars]

Salary.hat <- Xmat %*% final.variables
msep2 <- mean( (Salary.hat-X.ind$Salary)^2 )

resid <- X.ind$Salary - Salary.hat

plot( Salary.hat, resid, pch = 20, cex = 0.5, axes = TRUE, xlab = "Fitted Values", ylab = "Residuals" )
abline( h = 0, col = 'red', lwd = 2 )











