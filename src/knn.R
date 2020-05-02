rm( list = ls() )

# Make sure you're in the 'src' directory.
# k-Nearest Neighbors analysis.

library(ggplot2)
library(lubridate)
library(MASS)
library(nnet)
library(class)
library(dplyr)

# Load `df` and `ds`.
load('../data/data.Rdata')
load('../data/bio.Rdata')

## INFO
# df -- this is the complete dataset, unstandardized.
# ds -- same as df, but standardized.
rm(ds)


dep <- c('P', 'SHG', 'SHP', 'FO', 'EV FO', 'PP FO', 'SH FO', 'SH FOW', 'SH FOL',
         'OZ FO', 'NZ FO', 'DZ FO', 'On-Ice EV GD', 'ENP', 'Net Pen',
         'Net Pen/60', 'G Msct', 'SHA', 'SHA2', 'PPA', 'PPA2')

dc <- df %>%
  .[, !(names(df) %in% dep)] %>%
  select(-c(1:2, 5:7, 9:11)) %>%
  as.data.frame
dc$Pos <- as.factor(dc$Pos)
dc$Ntnlty <- as.factor(dc$Ntnlty)
dc$`S/C` <- as.factor(dc$`S/C`)



my.knn <- function(data) {
  print(names(data[1]))
  set.seed(1)
  nk <- 50
  c <- 10
  n <- nrow(data)
  fold <- sample(c, n, replace = TRUE)
  misclassrate <- rep(0, nk)
  se <- rep(0, nk)
  nfold <- rep(0, c)
  for (i in 1:c) {
    nfold[i] <- length(fold[fold == i])
  }
  for (k in 1:nk) {
    print(k)
    mcl <- rep(0, c)
    mclrate <- rep(0, c)
    for (i in 1:c) {
      pre <- knn(data[fold != i, -1], data[fold == i, -1], data[fold != i, 1], k)
      mcl[i] <- sum(pre != data[fold == i, 1])
      mclrate[i] <- sum(pre != data[fold == i, 1])/nfold[i]
    }
    se[k] <- sd(mclrate)/sqrt(c)
    misclassrate[k] <- sum(mcl)/n
  }
  
  return(list(rates = misclassrate, se = se))
}

## Position.
pos.knn <- dc %>%
  select(-c('S/C', 'Ntnlty')) %>%
  my.knn

## Adjusted position.
temp.dc <- dc %>%
  mutate(Pos = as.character(Pos))
temp.dc$Pos[temp.dc$Pos == 'L' | temp.dc$Pos == 'R'] <- 'F'
temp.dc$Pos <- as.factor(temp.dc$Pos)
pos.knn.adj <- temp.dc %>%
  select(-c('S/C', 'Ntnlty')) %>%
  my.knn

## Nationality.
nty.knn <- dc %>%
  select(-c('S/C', 'Pos')) %>%
  my.knn

## Shoots/catches.
sc.knn <- dc %>%
  select(-c('Pos', 'Ntnlty')) %>%
  my.knn



### Standardized variables.
dc[, 4:ncol(dc)] <- scale(dc[, 4:ncol(dc)])
temp.dc[, 4:ncol(temp.dc)] <- scale(temp.dc[, 4:ncol(temp.dc)])

## Position.
pos.knn.scaled <- dc %>%
  select(-c('S/C', 'Ntnlty')) %>%
  my.knn

## Adjusted position.
pos.knn.scaled.adj <- temp.dc %>%
  select(-c('S/C', 'Ntnlty')) %>%
  my.knn


## Nationality.
nty.knn.scaled <- dc %>%
  select(-c('S/C', 'Pos')) %>%
  my.knn

## Shoots/catches.
sc.knn.scaled <- dc %>%
  select(-c('Pos', 'Ntnlty')) %>%
  my.knn

save(nty.knn, nty.knn.scaled, pos.knn, pos.knn.adj, pos.knn.scaled,
     pos.knn.scaled.adj, sc.knn, sc.knn.scaled,
     file = '../data/knn.Rdata')












