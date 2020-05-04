rm( list = ls() )

# Make sure you're in the 'src' directory.
# SHRINKAGE ANALYSIS

library(tidyverse)
library(lubridate)
library(leaps)
library(glmnet)

# Load `df` and `ds`.
load('../data/newData.Rdata')
# df -- this is the complete dataset, unstandardized.
# ds -- same as df, but standardized.

# X is all the quantitative data.
X.ind <- as_tibble(data %>% select( -c(1, 3:5) ) %>% scale())

num.features <- length( names(X.ind) ) - 1

set.seed(1)
k = 10
grid = 10^seq(-2, 8, length=100)
folds = sample(1:k, nrow(X.ind), replace=TRUE)
cv.errors = matrix(NA, k, 100, dimnames=list(NULL, c(1:100)))

x = model.matrix(Salary ~ ., data=X.ind)[,-1]
y = X.ind$Salary
for (j in 1:k) {
  lasso = glmnet(x[folds!=j,], y[folds!=j], alpha=1, lambda=grid)
  testmat = model.matrix(G ~ ., data=X.ind[folds==j,])
  for (i in 1:100) {
    coefi = coef(lasso)[,i]
    pred = testmat%*%coefi
    cv.errors[j, i] = mean((X.ind$Salary[folds==j]-pred)^2)
  }
}

msep <- apply(cv.errors, 2, mean)
min.index <- which.min(msep)
lambda.min <- grid[101-min.index]
msep.min <- min(msep)

lasso.full <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.full, s=lambda.min, type="coefficients")
testmat <- model.matrix(Salary ~ ., data=X.ind)
Salary.hat <- (testmat %*% lasso.coef)@x
msep.lasso <- mean((Salary.hat-X.ind$Salary)^2)

# extract the sparse matrix elinating the . entries
lass.coef.tibble<- tibble(name = lasso.coef@Dimnames[[1]][lasso.coef@i + 1], coefficient = lasso.coef@x)
lass.coef.tibble$name<- lass.coef.tibble$name %>% str_replace_all("`", "")

X.pred.lasso <- cbind(X.ind$Salary, X.ind %>% select(lass.coef.tibble$name[-1]))
names(X.pred.lasso)[1] <- 'Salary'

resid <- X.ind$Salary - Salary.hat

adjr2.lasso <- summary(lm(Salary ~ ., data = X.pred.lasso))$adj.r.squared

# plot of residuals vs. fitted 
pdf(file="../plots/shrinkage/resid_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(Salary.hat, resid, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Residuals"))
text(-0.7,-1.5, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso)), cex=0.85)
abline( h = 0, col = 'red', lwd = 1, lty=2 )
dev.off()

# plot of actual vs. fitted
pdf(file="../plots/shrinkage/actVSfit_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(Salary.hat, X.ind$Salary, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Actual Values"))
text(-0.5,2.5, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso)), cex=0.85)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
dev.off()

#Q-Q plot
pdf(file="../plots/shrinkage/qqplot_lasso.pdf", bg="transparent", width=6, height=4.8)
qqplot(X.ind$Salary, Salary.hat, xlab=TeX("Theoretical Quantiles"), ylab=TeX("Standardized Residuals"))
qqline(X.ind$Salary, Salary.hat, lty=3, col='seashell4')
dev.off()



