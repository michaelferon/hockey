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

