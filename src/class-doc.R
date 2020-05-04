rm( list = ls() )

# Make sure you're in the 'src' directory.
#HEY.

library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(MASS)
library(nnet)
library(class)
library(e1071)
library(tidymodels)
library(latex2exp)
library(dplyr)

# Load data.
load('../data/data.Rdata')
rm(ds)
dep <- c('P', 'SHG', 'SHP', 'FOL', 'EV FOL', 'PP FOL', 'SH FO', 'SH FOW',
         'SH FOL', 'OZ FOL', 'NZ FOL', 'DZ FO', 'DZ FOW', 'DZ FOL',
         'On-Ice EV GD', 'ENP', 'MsS Cross', 'Net Pen', 'G Msct', 'SHA', 'SHA2',
         'PPA', 'PPA2')
data <- df %>%
  .[, !(names(df) %in% dep)] %>%
  select(-c(1:2, 5:7, 9:11)) %>%
  as.data.frame
data$Pos <- as.factor(data$Pos)
data$Ntnlty <- as.factor(data$Ntnlty)
data$`S/C` <- as.factor(data$`S/C`)

# Load LDA, QDA, and Logistic Regression results.
load(file = '../data/class.Rdata')
# Load k-Nearest Neighbors results.
load(file = '../data/knn.Rdata')

# For KNN.
do.knn <- function(data, k) {
  set.seed(1)
  c <- 10
  n <- nrow(data)
  fold <- sample(c, n, replace = TRUE)
  g <- length(unique(data[[1]]))
  confuse <- matrix(rep(0, g^2), nrow = g)
  pred <- data[[1]]
  for (i in 1:c) {
    zcv <- knn(data[fold != i, -1], data[fold == i, -1], data[fold != i, 1], k)
    confuse <- confuse + table(data[fold == i, 1], zcv)
    pred[fold == i] <- zcv
  }
  rate <- 1 - sum(diag(confuse)) / sum(confuse)
  rates <- 1 - diag(confuse) / apply(confuse, 2, sum)
  
  return(list(confuse = confuse, rate = rate, rates = rates, pred = pred))
}




### Position
## LDA.
temp <- tibble(
  truth = data$Pos,
  estimate = pos.lda$pred
)
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
pos.lda$rate
pos.lda$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## QDA.
temp$estimate <- pos.qda$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
pos.qda$rate
pos.qda$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## Logistic.
for (i in 1:4) {
  pos.log$pred[pos.log$pred == i] <- levels(data$Pos)[i]
}
temp$estimate <- pos.log$pred %>% as.factor
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
pos.log$rate
pos.log$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN.
pdf(file='../plots/class/pos-knn-rates.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn$rates, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(pos.knn$rates, lwd = 0.75)
dev.off()
pdf(file='../plots/class/pos-knn-se.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn$se, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Standard error'))
lines(pos.knn$se, lwd = 0.75)
dev.off()

thing <- data %>%
  select(-c('S/C', 'Ntnlty')) %>%
  do.knn(k = which.min(pos.knn$rates))
temp$estimate <- thing$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
thing$rate
thing$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN scaled.
pdf(file='../plots/class/pos-knn-scaled-rates.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn.scaled$rates, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(pos.knn.scaled$rates, lwd = 0.75)
dev.off()
pdf(file='../plots/class/pos-knn-scaled-se.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn.scaled$se, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Standard error'))
lines(pos.knn.scaled$se, lwd = 0.75)
dev.off()

thing <- data %>% select(-c('S/C', 'Ntnlty'))
thing[, 2:ncol(thing)] <- scale(thing[, 2:ncol(thing)])
thing <- thing %>% do.knn(k = which.min(pos.knn.scaled$rates)) # or 23, 29
temp$estimate <- thing$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
thing$rate
thing$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN comparison.
ylim <- c(min(c(pos.knn$rates, pos.knn.scaled$rates)),
          max(c(pos.knn$rates, pos.knn.scaled$rates)))
pdf(file='../plots/class/pos-knn-comp.pdf', bg='transparent', height=4, width=10)
par(mfrow = c(1, 2))
plot(1:50, pos.knn$rates, pch=20, cex=0.75, main='Un-standardized', ylim=ylim,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(pos.knn$rates, lwd = 0.75)
plot(1:50, pos.knn.scaled$rates, pch=20, cex=0.75, main='Standardized', ylim=ylim,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(pos.knn.scaled$rates, lwd = 0.75)
dev.off()




### Adjusted Position.
data <- data %>% mutate(Pos = as.character(Pos))
data$Pos[data$Pos == 'L' | data$Pos == 'R'] <- 'F'
data$Pos <- as.factor(data$Pos)
## LDA.
temp <- tibble(
  truth = data$Pos,
  estimate = pos.lda.adj$pred
)
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
pos.lda.adj$rate
pos.lda.adj$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## QDA.
temp$estimate <- pos.qda.adj$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
pos.qda.adj$rate
pos.qda.adj$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## Logistic.
for (i in 1:3) {
  pos.log.adj$pred[pos.log.adj$pred == i] <- levels(data$Pos)[i]
}
temp$estimate <- pos.log.adj$pred %>% as.factor
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
pos.log.adj$rate
pos.log.adj$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN.
pdf(file='../plots/class/pos-knn-adj-rates.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn.adj$rates, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(pos.knn.adj$rates, lwd = 0.75)
dev.off()
pdf(file='../plots/class/pos-knn-adj-se.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn.adj$se, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Standard error'))
lines(pos.knn.adj$se, lwd = 0.75)
dev.off()

thing <- data %>%
  select(-c('S/C', 'Ntnlty')) %>%
  do.knn(k = which.min(pos.knn.adj$rates))
temp$estimate <- thing$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
thing$rate
thing$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN scaled.
pdf(file='../plots/class/pos-knn-scaled-adj-rates.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn.scaled.adj$rates, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(pos.knn.scaled.adj$rates, lwd = 0.75)
dev.off()
pdf(file='../plots/class/pos-knn-scaled-adj-se.pdf', bg='transparent', height=4.8, width = 6)
plot(1:50, pos.knn.scaled.adj$se, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Standard error'))
lines(pos.knn.scaled.adj$se, lwd = 0.75)
dev.off()

thing <- data %>% select(-c('S/C', 'Ntnlty'))
thing[, 2:ncol(thing)] <- scale(thing[, 2:ncol(thing)])
thing <- thing %>% do.knn(k = 32) # which.min = 37
temp$estimate <- thing$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
thing$rate
thing$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)




### Subset selection.
rates <- read_csv('../data/class-subset-data.csv') %>%
  .$rates
rates[1] <- NA
rates[2] <- NA
pdf(file='../plots/class/subset-cv.pdf', bg='transparent', height=4.8, width = 6)
plot(1:length(rates), rates, pch=20, cex=0.75,
     xlab=TeX('Subset Size'), ylab = TeX('Misclassification rate'))
lines(rates, lwd = 0.75)
dev.off()

vars <- read_csv('../data/class-subset-vars.csv') %>%
  .$vars

df <- data %>%
  .[, names(.) %in% c(vars[1:2], 'Pos')]
priors <- tapply(df$Pos, df$Pos, function(x) length(x) / nrow(df)) %>%
  as.numeric
model <- lda(Pos ~ ., data = df, prior = priors)
df$pred <- predict(model, df[, -1])$class

xstart <- min(df$`EV FO`); xend <- max(df$`EV FO`); xlen <- xend - xstart
ystart <- min(df$`BkS/60`); yend <- max(df$`BkS/60`); ylen <- yend - ystart
mesh <- expand_grid(
  `EV FO` = seq(xstart - 0.05*xlen, xend + 0.05*xlen, by = 0.05),
  `BkS/60` = seq(ystart - 0.05*ylen, yend + 0.05*ylen, by = 0.05)
)
mesh$pred <- predict(model, mesh)$class

pdf(file = '../plots/class/viz.pdf', height = 3.5, width = 7)
ggplot(mesh, aes(`EV FO`, `BkS/60`, fill = pred)) +
  geom_raster(alpha = 0.25) +
  geom_point(data = df, aes(`EV FO`, `BkS/60`, color = Pos), size = 0.6) +
  labs(fill = 'Position') + guides(color = FALSE) +
  scale_fill_discrete(labels = c('Center', 'Defense', 'Forward')) +
  xlab('Even Strength Faceoffs') + ylab('Blocked Shots per 60 Minutes') +
  theme_minimal()
dev.off()









### Shoots/catches.
## LDA.
temp <- tibble(
  truth = data$`S/C`,
  estimate = sc.lda$pred
)
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
sc.lda$rate
sc.lda$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## QDA.
temp$estimate <- sc.qda$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
sc.qda$rate
sc.qda$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## Logistic.
sc.log$pred[sc.log$pred == 'TRUE'] <- 'R'
sc.log$pred[sc.log$pred == 'FALSE'] <- 'L'
temp$estimate <- sc.log$pred %>% as.factor
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
sc.log$rate
sc.log$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN.
pdf(file='../plots/class/sc-knn-rates.pdf', bg='transparent', height=5)
plot(1:50, sc.knn$rates, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(sc.knn$rates, lwd = 0.75)
dev.off()
pdf(file='../plots/class/sc-knn-se.pdf', bg='transparent', height=5)
plot(1:50, sc.knn$se, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Standard error'))
lines(sc.knn$se, lwd = 0.75)
dev.off()

thing <- data %>%
  select(-c('Pos', 'Ntnlty')) %>%
  do.knn(k = which.min(sc.knn$rates)) # anything >= 34
temp$estimate <- thing$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
thing$rate
thing$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN scaled.
pdf(file='../plots/class/sc-knn-scaled-rates.pdf', bg='transparent', height=5)
plot(1:50, sc.knn.scaled$rates, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(sc.knn.scaled$rates, lwd = 0.75)
dev.off()
pdf(file='../plots/class/sc-knn-scaled-se.pdf', bg='transparent', height=5)
plot(1:50, sc.knn.scaled$se, pch=20, cex=0.75,
     xlab=TeX('k'), ylab = TeX('Standard error'))
lines(sc.knn.scaled$se, lwd = 0.75)
dev.off()

thing <- data %>% select(-c('Pos', 'Ntnlty'))
thing[, 2:ncol(thing)] <- scale(thing[, 2:ncol(thing)])
thing <- thing %>% do.knn(k = which.min(sc.knn.scaled$rates))
temp$estimate <- thing$pred
temp %>% conf_mat(truth = truth, estimate = estimate) %>% .$table %>% t
thing$rate
thing$rates
temp %>% accuracy(truth = truth, estimate = estimate)
temp %>% sensitivity(truth = truth, estimate = estimate)
temp %>% specificity(truth = truth, estimate = estimate)

## KNN comparison.
ylim <- c(min(c(sc.knn$rates, sc.knn.scaled$rates)),
          max(c(sc.knn$rates, sc.knn.scaled$rates)))
pdf(file='../plots/class/sc-knn-comp.pdf', bg='transparent', height=4, width=10)
par(mfrow = c(1, 2))
plot(1:50, sc.knn$rates, pch=20, cex=0.75, main='Un-standardized', ylim=ylim,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(sc.knn$rates, lwd = 0.75)
plot(1:50, sc.knn.scaled$rates, pch=20, cex=0.75, main='Standardized', ylim=ylim,
     xlab=TeX('k'), ylab = TeX('Misclassification rate'))
lines(sc.knn.scaled$rates, lwd = 0.75)
dev.off()







