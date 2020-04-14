rm( list = ls() )

# Make sure you're in the 'src' directory.
setwd("~/Desktop/hockey/src")
# REGRESSION ANALYSIS

library(tidyverse)
library(lubridate)
library(leaps)

# Load `df` and `ds`.
load('../data/data.Rdata')
# df -- this is the complete dataset, unstandardized.
# ds -- same as df, but standardized.


# X is all the quantitative data.
X <- df %>% select( -c(1:11) ) 

# forward selection performed works for 90 even with dependencies *doublecheck later*
step.forward <- regsubsets( P~., data=X, method="forward", nvmax=90 )
step.forward.sum <- summary(step.forward)

# formulas for determining best model with many variables
adjusted.fits <- as.data.frame( cbind( Rsqr = step.forward.sum$rsq, adjRsqr = step.forward.sum$adjr2, 
                                      bic = step.forward.sum$bic, cp = step.forward.sum$cp ) )

write.csv(adjusted.fits, '~/Desktop/hockey/data/forward-results.csv')

# both r squared values are useless in this case due to amount of variables get rid
best.models <- apply( adjusted.fits[3:4], 2, which.min )

# best model determined by bic, which usually produces a small model (penalty on more variables)
best.bic <- as.data.frame( coef( step.forward.sum$obj, best.models[1] ) )
names(best.bic)[1] <- "value"

#best model determined by cp values
best.cp <- as.data.frame( coef( step.forward.sum$obj, best.models[2] ) )
names(best.cp)[1] <- "value"













