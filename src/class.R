rm( list = ls() )

# Make sure you're in the 'src' directory.
# CLASSIFICATION ANALYSIS

library(dplyr)
library(ggplot2)
library(lubridate)
library(MASS)

# Load `df` and `ds`.
load('../data/data.Rdata')

## INFO
# df -- this is the complete dataset, unstandardized.
# ds -- same as df, but standardized.


dep <- c('P', 'SHG', 'SHP', 'FO', 'EV FO', 'PP FO', 'SH FO', 'SH FOW', 'SH FOL',
         'OZ FO', 'NZ FO', 'DZ FO', 'On-Ice EV GD', 'ENP', 'Net Pen',
         'Net Pen/60', 'G Msct', 'SHA', 'SHA2', 'PPA', 'PPA2')
del <- c('P', 'P/GP', 'EVG', 'EVP', 'PPG', 'PPP', 'SHG', 'SHP', 'S', 'S%', 'FO',
         'EV FO', 'PP FO', 'SH FO', 'SH FOW', 'SH FOL', 'OZ FO', 'NZ FO',
         'DZ FO', 'On-Ice PP GF', 'On-Ice SH GF', 'On-Ice EV GF',
         'On-Ice EV GD', 'On-Ice EV GF%', 'ENG', 'ENP', 'MsS', 'MsS Wide',
         'MsS Over', 'MsS Post', 'MsS Cross', 'Net Pen', 'Net Pen/60', 'G Msct',
         'SHA1', 'SHA2', 'SH Shots', 'SH S%', 'SHG/60', 'SHA1/60', 'SHA2/60',
         'SHP/60', 'SH S/60', 'PPA1', 'PPA2', 'PP Shots', 'PP S%', 'PPG/60',
         'PPA1/60', 'PPA2/60', 'PPP/60', 'PP S/60', 'PP GF/60')
del <- c(del, '+/-', 'GP', 'GWG', 'OTG', '1g', 'SH iSAT', 'SH iSAT/60',
         'PP iSAT', 'PP iSAT/60')

dc <- df %>%
  .[, !(names(df) %in% dep)] %>%
  dplyr::select(-c(1:2, 5:7, 9:11)) %>%
  as.data.frame
dc$Pos <- as.factor(dc$Pos)
dc$Ntnlty <- as.factor(dc$Ntnlty)
dc$`S/C` <- as.factor(dc$`S/C`)


## Position LDA
set.seed(1)
k <- 10
n <- dim(dc)[1]
p <- dim(dc)[2]
start <- 4
fold <- sample(k, n, replace = TRUE)

g.pos <- length(unique(dc$Pos))
prior.pos <- as.numeric(tapply(dc$Pos, dc$Pos, function(x) length(x) / n))
confuse.pos.lda <- matrix(rep(0, g.pos^2), nrow = g.pos)

for (i in 1:k) {
  zcv <- lda(dc[fold != i, 'Pos'] ~ ., dc[fold != i, start:p],
             prior = prior.pos)
  ppcv <- predict(zcv, dc[fold == i, start:p])
  confuse.pos.lda <- confuse.pos.lda + table(dc[fold == i, 'Pos'], ppcv$class)
}
rate.pos.lda <- 1 - sum(diag(confuse.pos.lda)) / sum(confuse.pos.lda)

## Position QDA
confuse.pos.qda <- matrix(rep(0, g.pos^2), nrow = g.pos)

for (i in 1:k) {
  zcv <- qda(dc[fold != i, 'Pos'] ~ ., dc[fold != i, start:p],
             prior = prior.pos)
  ppcv <- predict(zcv, dc[fold == i, start:p])
  confuse.pos.qda <- confuse.pos.qda + table(dc[fold == i, 'Pos'], ppcv$class)
}
rate.pos.qda <- 1 - sum(diag(confuse.pos.qda)) / sum(confuse.pos.qda)


## Nationality LDA
g.nty <- length(unique(dc$Ntnlty))
prior.nty <- tapply(dc$Ntnlty, dc$Ntnlty, function(x) length(x) / n) %>%
                as.numeric
confuse.nty.lda <- matrix(rep(0, g.nty^2), nrow = g.nty)

for (i in 1:k) {
  zcv <- lda(dc[fold != i, 'Ntnlty'] ~ ., dc[fold != i, start:p],
             prior = prior.nty)
  ppcv <- predict(zcv, dc[fold == i, start:p])
  confuse.nty.lda <- confuse.nty.lda + table(dc[fold == i, 'Ntnlty'],
                                             ppcv$class)
}
rate.nty.lda <- 1 - sum(diag(confuse.nty.lda)) / sum(confuse.nty.lda)



## Shoots/Catches LDA
g.sc <- length(unique(dc$`S/C`))
prior.sc <- tapply(dc$`S/C`, dc$`S/C`, function(x) length(x) / n) %>%
            as.numeric
confuse.sc.lda <- matrix(rep(0, g.sc^2), nrow = g.sc)

for (i in 1:k) {
  zcv <- lda(dc[fold != i, 'S/C'] ~ ., dc[fold != i, start:p],
             prior = prior.sc)
  ppcv <- predict(zcv, dc[fold == i, start:p])
  confuse.sc.lda <- confuse.sc.lda + table(dc[fold == i, 'S/C'], ppcv$class)
}
rate.sc.lda <- 1 - sum(diag(confuse.sc.lda)) / sum(confuse.sc.lda)


## Shoots/Catches QDA
confuse.sc.qda <- matrix(rep(0, g.sc^2), nrow = g.sc)

for (i in 1:k) {
  zcv <- qda(dc[fold != i, 'S/C'] ~ ., dc[fold != i, start:p],
             prior = prior.sc)
  ppcv <- predict(zcv, dc[fold == i, start:p])
  confuse.sc.qda <- confuse.sc.qda + table(dc[fold == i, 'S/C'], ppcv$class)
}
rate.sc.qda <- 1 - sum(diag(confuse.sc.qda)) / sum(confuse.sc.qda)









