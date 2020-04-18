rm( list = ls() )

# Make sure you're in the 'src' directory.
# SHRINKAGE ANALYSIS

library(tidyverse)
library(lubridate)
library(leaps)
library(glmnet)

# Load `df` and `ds`.
load('../data/data.Rdata')
# df -- this is the complete dataset, unstandardized.
# ds -- same as df, but standardized.


# X is all the quantitative data.
X <- df %>% select( -c(1:11) ) 
dep <- c('P', 'SHG', 'SHP', 'FO', 'EV FO', 'PP FO', 'SH FO', 'SH FOW', 'SH FOL',
         'OZ FO', 'NZ FO', 'DZ FO', 'On-Ice EV GD', 'ENP', 'Net Pen',
         'Net Pen/60', 'G Msct', 'SHA', 'SHA2', 'PPA', 'PPA2', 'EVG', 'PPG', 'OTG',
         'GWG', 'On-Ice PP GF', 'On-Ice SH GF', 'On-Ice EV GF', 'On-Ice EV GF%',
         'ENG', '1g', 'SHG/60', 'PPG/60', 'PP GF/60', 'P/GP', 'EVP', 'PPP', 'S',
         'S%', 'MsS', 'MsS Wide', 'MsS Over', 'MsS Post', 'MsS Cross', 'SHP/60',
         'PP Shots', 'PP S%', 'PP S/60')

X.ind <- X %>% select( -all_of(dep) )
num.features <- length( names(X.ind) ) - 1

set.seed(1)
k = 10
grid = 10^seq(-2, 8, length=100)
folds = sample(1:k, nrow(X.ind), replace=TRUE)
cv.errors = matrix(NA, k, 100, dimnames=list(NULL, c(1:100)))

x = model.matrix(G~., data=X.ind)[,-1]
y = X.ind$G
for (j in 1:k) {
  lasso = glmnet(x[folds!=j,], y[folds!=j], alpha=1, lambda=grid)
  testmat = model.matrix(G~., data=X.ind[folds==j,])
  for (i in 1:100) {
    coefi = coef(lasso)[,i]
    pred = testmat%*%coefi
    cv.errors[j, i] = mean((X.ind$G[folds==j]-pred)^2)
  }
}

msep <- apply(cv.errors, 2, mean)
min.index <- which.min(msep)
lambda.min <- grid[101-min.index]
msep.min <- min(msep)

lasso.full <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.full, s=lambda.min, type="coefficients")
testmat <- model.matrix(G~., data=X.ind)
G.hat <- testmat%*%lasso.coef
msep.lasso <- mean((G.hat-X.ind$G)^2)

# extract the sparse matrix elinating the . entries
lass.coef.tibble<- tibble(name = lasso.coef@Dimnames[[1]][lasso.coef@i + 1], coefficient = lasso.coef@x)
lass.coef.tibble$name<- lass.coef.tibble$name %>% str_replace_all("`", "")


X.pred.lasso <- cbind(X.ind$G, X.ind %>% select(lass.coef.tibble$name[-1]))
names(X.pred.lasso)[1] <- 'G'

fit <- lm(G~., data=X.pred.lasso)
plot(fit,cex=0.5,which=1)









