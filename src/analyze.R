rm( list = ls() )

# Make sure you're in the 'src' directory.

library(dplyr)
library(ggplot2)
library(lubridate)

# Load `df` and `X` data objects.
load('../data/data.Rdata')



X <- X %>% group_by(Season)
X.mean <- X %>% summarise_all(mean)
X.sd <- X %>% summarise_all(sd)

for (i in 1:10) {
  m <- as.numeric(X.mean[i, -1])
  s <- as.numeric(X.sd[i, -1])
  # X[X$Season == i + 2008, -1] <- X[X$Season == i + 2008, -1]
}
