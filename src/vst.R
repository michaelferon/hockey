rm(list = ls())

### Variance Stabilizing Transform ###

library(MASS)

load('../data/dataLASSO.Rdata')

fit <- lm(Salary~., data = X.pred.lasso)
lam.hat = boxcox(fit, lambda = seq(-0.25, 0.75, by = 0.05), plotit = FALSE)
lambda = lam.hat$x[which.max(lam.hat$y)]

X.pred.lasso$Salary = ((X.pred.lasso$Salary)^lambda - 1)/lambda

bc.fit <- lm(Salary~., data = X.pred.lasso)

num.features = ncol(X.pred.lasso)-1
step.forward <- regsubsets( Salary ~ ., data = X.pred.lasso, method = "forward", nvmax = num.features )
step.forward.sum <- summary(step.forward)

keep <- step.forward.sum$which[max(which.max(step.forward.sum$adjr2),
    which.min(step.forward.sum$cp),
    which.min(step.forward.sum$bic)),]


# Fit the new model
best.forward.fit <- lm(X.pred.lasso$Salary~., data=X.pred.lasso[which(keep)])
best.forward.fit.sum <- summary(best.forward.fit)


# Save the plots
pdf(file = '../plots/vst/resid_vst.pdf', height = 6.5, width = 8.0)
plot(best.forward.fit, which = 1)
dev.off()

pdf(file = '../plots/vst/normQQ_vst.pdf', height = 6.5, width = 8.0)
plot(best.forward.fit, which = 2)
dev.off()

pdf(file = '../plots/vst/leverage_vst.pdf', height = 6.5, width = 8.0)
plot(best.forward.fit, which = 5)
dev.off()