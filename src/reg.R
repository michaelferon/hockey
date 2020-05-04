rm( list = ls() )

# Make sure you're in the 'src' directory.
# REGRESSION ANALYSIS

library(tidyverse)
library(lubridate)
library(leaps)
library(latex2exp)

# Load `data` 
load('../data/newData.Rdata')

# X.ind is all the quantitative data and standardized
X.ind <- as_tibble(data %>% select( -c(1, 3:5) ) %>% scale())

# start by fitting a simple linear model
simple.model <- lm(Salary ~ ., data = X.ind)

# plot of residuals vs. fitted 
pdf(file="../plots/regression/resid_simp_linear.pdf", bg="transparent", width=6, height=4.8)
plot(simple.model$fitted.values, simple.model$residuals, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Residuals"))
text(2.5,1.6, TeX(sprintf("Adj. $R^2 = %.3f", summary(simple.model)$adj.r.squared)), cex=0.85)
abline( h = 0, col = 'red', lwd = 1, lty=2 )
dev.off()

# plot of actual vs. fitted
pdf(file="../plots/regression/actVSfit_simp_linear.pdf", bg="transparent", width=6, height=4.8)
plot(simple.model$fitted.values, X.ind$Salary, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Actual Values"))
text(2.5,-0.5, TeX(sprintf("Adj. $R^2 = %.3f", summary(simple.model)$adj.r.squared)), cex=0.85)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
dev.off()

# Q-Q plot
pdf(file="../plots/regression/qqplot_simp_linear.pdf", bg="transparent", width=6, height=4.8)
qqnorm(simple.model$residuals, xlab=TeX("Theoretical Quantiles"), ylab=TeX("Standardized Residuals"), main="")
qqline(simple.model$residuals, lty=3, col='seashell4')
dev.off()

num.features <- length( names(X.ind) ) - 1

# forward selection performed using goodness of fit
step.forward <- regsubsets( Salary ~ ., data = X.ind, method = "forward", nvmax = num.features )
step.forward.sum <- summary(step.forward)

# formulas for determining best model with many variables
adjusted.fits <- as.data.frame( cbind( Rsqr = step.forward.sum$rsq, adjRsqr = step.forward.sum$adjr2, 
                                      bic = step.forward.sum$bic, cp = step.forward.sum$cp ) )

best.models <- as.data.frame(t(c(apply(adjusted.fits[1:2], 2, which.max), apply( adjusted.fits[3:4], 2, which.min ))))
names(best.models) <- c('Rsqr', 'adjRsqr', 'bic', 'cp')

# best model determined by bic, which usually produces a small model (penalty on more variables)
best.bic <- as.data.frame( coef( step.forward.sum$obj, best.models$bic ) )
names(best.bic)[1] <- "value"
row.names(best.bic) <- str_replace_all(row.names(best.bic),"`", "")

#best model determined by cp values
best.cp <- as.data.frame( coef( step.forward.sum$obj, best.models$cp ) )
names(best.cp)[1] <- "value"
row.names(best.cp) <- str_replace_all(row.names(best.cp),"`", "")

# gather data based only on the variables bic chose
X.bic <- cbind(X.ind$Salary, X.ind %>% select(row.names(best.bic)[-1]))
names(X.bic)[1] <- 'Salary'

fit.bic <- lm(Salary~., data=X.bic)
resid.bic <- fit.bic$residuals
adjr2.bic <- summary(fit.bic)$adj.r.squared
Salary.bic <- fit.bic$fitted.values

# plot of residuals vs. fitted 
pdf(file="../plots/regression/resid_forwd_bic.pdf", bg="transparent", width=6, height=4.8)
plot(Salary.bic, resid.bic, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Residuals"))
text(2,1.6, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.bic)), cex=0.85)
abline( h = 0, col = 'red', lwd = 1, lty=2 )
dev.off()

# plot of actual vs. fitted
pdf(file="../plots/regression/actVSfit_forwd_bic.pdf", bg="transparent", width=6, height=4.8)
plot(Salary.bic, X.ind$Salary, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Actual Values"))
text(2,-0.5, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.bic)), cex=0.85)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
dev.off()

# Q-Q plot
pdf(file="../plots/regression/qqplot_forwd_bic.pdf", bg="transparent", width=6, height=4.8)
qqnorm(resid.bic, xlab=TeX("Theoretical Quantiles"), ylab=TeX("Standardized Residuals"), main="")
qqline(resid.bic, lty=3, col='seashell4')
dev.off()

# k fold cross validation with forward selection
set.seed(1)
k = 10
folds = sample( 1:k, nrow(X.ind), replace=TRUE )
cv.errors = matrix( NA, k, num.features, dimnames = list( NULL,c(1:num.features) ) )

for (j in 1:k){
  best.fit = regsubsets( Salary ~., data=X.ind[folds!=j,], nvmax=num.features, method="forward" )
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
best.kfold <- as.matrix(coef( best.fit,id=number.variables ))
names(best.kfold)[1] <- "value"

rownames(best.kfold) <- rownames(best.kfold) %>% str_replace_all("`", "")
X.kfold <- cbind(X.ind$Salary, X.ind %>% select(rownames(best.kfold)[-1]))
names(X.kfold)[1] <- 'Salary'

fit.kfold <- lm(Salary~., data=X.kfold)
resid.kfold <- fit.kfold$residuals
adjr2.kfold <- summary(fit.kfold)$adj.r.squared
Salary.kfold <- fit.kfold$fitted.values

# plot of residuals vs. fitted 
pdf(file="../plots/regression/resid_forwd_kfold.pdf", bg="transparent", width=6, height=4.8)
plot(Salary.kfold, resid.kfold, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Residuals"))
text(2.5,1.6, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.kfold)), cex=0.85)
abline( h = 0, col = 'red', lwd = 1, lty=2 )
dev.off()

# plot of actual vs. fitted
pdf(file="../plots/regression/actVSfit_kfold.pdf", bg="transparent", width=6, height=4.8)
plot(Salary.kfold, X.ind$Salary, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Actual Values"))
text(2,-0.5, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.kfold)), cex=0.85)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
dev.off()

# Q-Q plot
pdf(file="../plots/regression/qqplot_forwd_kfold.pdf", bg="transparent", width=6, height=4.8)
qqnorm(resid.kfold, xlab=TeX("Theoretical Quantiles"), ylab=TeX("Standardized Residuals"), main="")
qqline(resid.kfold, lty=3, col='seashell4')
dev.off()








