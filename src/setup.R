rm( list = ls() )

# Make sure you're in the 'src' directory.

library(dplyr)

# Load `df` and `X` data objects.
load('../data/dataFull.Rdata')



# Standardize data by year.
X <- X %>% group_by(Season)
X.mean <- X %>% summarise_all(mean)
X.sd <- X %>% summarise_all(sd)
for (i in 1:10) {
  m <- as.numeric(X.mean[i, -1])
  s <- as.numeric(X.sd[i, -1])
  X[X$Season == i+2008, -1] <- sweep(X[X$Season == i+2008, -1], 2, m, '-')
  X[X$Season == i+2008, -1] <- sweep(X[X$Season == i+2008, -1], 2, s, '/')
}
X <- X %>% ungroup

ds <- df %>%
  .[, !(names(.) %in% names(X))] %>%
  bind_cols(X)

df <- df %>%
  .[, names(ds)]

rm(i, m, s, X, X.mean, X.sd)

save(list = ls(), file = '../data/data.Rdata')

