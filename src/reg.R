rm( list = ls() )

# Make sure you're in the 'src' directory.
# REGRESSION ANALYSIS

library(dplyr)
library(ggplot2)
library(lubridate)

# Load `df` and `ds`.
load('../data/data.Rdata')
# df -- this is the complete dataset, unstandardized.
# ds -- same as df, but standardized.


# X is all the quantitative data.
X <- df %>% select(-c(1:11))
model <- lm(P ~ ., data = X)

predictPoints <- X %>% select(c(`P`,`GP`,`+/-`, `S`,`TOI/GP`, `PP TOI/GP`)) 






