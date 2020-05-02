rm( list = ls() )

# Make sure you're in the 'src' directory.
# CLASSIFICATION ANALYSIS

library(ggplot2)
library(lubridate)
library(MASS)
library(nnet)
library(dplyr)

# Load `df` and `ds`.
load('../data/data.Rdata')
load('../data/bio.Rdata')

## INFO
# df -- this is the complete dataset, unstandardized.
rm(ds)


dep <- c('P', 'SHG', 'SHP', 'FOL', 'EV FOL', 'PP FOL', 'SH FO', 'SH FOW',
         'SH FOL', 'OZ FOL', 'NZ FOL', 'DZ FO', 'DZ FOW', 'DZ FOL',
         'On-Ice EV GD', 'ENP', 'MsS Cross', 'Net Pen', 'G Msct', 'SHA', 'SHA2',
         'PPA', 'PPA2')

dc <- df %>%
  .[, !(names(df) %in% dep)] %>%
  select(-c(1:2, 5:7, 9:11)) %>%
  as.data.frame
dc$Pos <- as.factor(dc$Pos)
dc$Ntnlty <- as.factor(dc$Ntnlty)
dc$`S/C` <- as.factor(dc$`S/C`)



### DISCRIMINANT ANALYSIS
my.da <- function(data, var, type = 'lda', priors = NULL) {
  set.seed(1)
  k <- 10
  n <- nrow(data)
  p <- ncol(data)
  fold <- sample(k, n, replace = TRUE)
  
  g <- length(unique(data[[var]]))
  confuse <- matrix(rep(0, g^2), nrow = g)
  pred <- as.character(rep(0, n))
  if (is.null(priors)) {
    priors <- tapply(data[[var]], data[[var]], function(x) length(x) / n) %>%
             as.numeric
  }
  
  for (i in 1:k) {
    if (type == 'lda') {
      zcv <- lda(data[fold != i, var]~., data[fold != i, 2:p],
                 prior = priors)
    } else if (type == 'qda') {
      zcv <- qda(data[fold != i, var]~., data[fold != i, 2:p],
                 prior = priors)
    } else {
      stop('Invalid type.')
    }
    ppcv <- predict(zcv, data[fold == i, 2:p])
    confuse <- confuse + table(data[fold == i, var], ppcv$class)
    pred[fold == i] <- ppcv$class
  }
  rate <- 1 - sum(diag(confuse)) / sum(confuse)
  rates <- 1 - diag(confuse) / apply(confuse, 2, sum)
  
  return(list(confuse = confuse, rate = rate, rates = rates, pred = pred))
}


## Position
pos.lda <- dc %>% select(-c('S/C', 'Ntnlty')) %>% my.da('Pos', 'lda')
pos.qda <- dc %>% select(-c('S/C', 'Ntnlty')) %>% my.da('Pos', 'qda')

temp.dc <- dc %>% mutate(Pos = as.character(Pos))
temp.dc$Pos[temp.dc$Pos == 'L' | temp.dc$Pos == 'R'] <- 'F'
temp.dc$Pos <- as.factor(temp.dc$Pos)

pos.lda.adj <- temp.dc %>% select(-c('S/C', 'Ntnlty')) %>% my.da('Pos', 'lda')
pos.qda.adj <- temp.dc %>% select(-c('S/C', 'Ntnlty')) %>% my.da('Pos', 'qda')

pos.vars <- read.csv('../data/class-subset-vars.csv', header = TRUE,
                     stringsAsFactors = FALSE)$vars
pos.lda.sub <- temp.dc %>% select(c('Pos', pos.vars)) %>% my.da('Pos', 'lda')


## Nationality
nty.lda <- dc %>% select(-c('Pos', 'S/C')) %>% my.da('Ntnlty', 'lda')


## Shoots/Catches
sc.lda <- dc %>% select(-c('Pos', 'Ntnlty')) %>% my.da('S/C', 'lda')
sc.qda <- dc %>% select(-c('Pos', 'Ntnlty')) %>% my.da('S/C', 'qda')




### LOGISTIC REGRESSION
my.log <- function(data, var) {
  set.seed(1)
  k <- 10
  n <- nrow(data)
  p <- ncol(data)
  fold <- sample(k, n, replace = TRUE)
  
  g <- length(unique(data[[var]]))
  confuse <- matrix(rep(0, g^2), nrow = g)
  pred <- as.character(rep(0, n))
  for (i in 1:k) {
    lr <- multinom(data[fold != i, var] ~ ., data[fold != i, 2:p],
                   trace = FALSE) %>% summary
    logodds <- (lr$coefficients[, 1] + lr$coefficients[, 2:p] %*%
      (data[fold == i, -1] %>% as.matrix %>% t)) %>%
      t %>% cbind(0, .)
    class <- apply(logodds, 1, which.max)
    confuse <- confuse + table(data[fold == i, var], class)
    pred[fold == i] <- class
  }
  rate <- 1 - sum(diag(confuse)) / sum(confuse)
  rates <- 1 - diag(confuse) / apply(confuse, 2, sum)
  
  return(list(confuse = confuse, rate = rate, rates = rates))
}


## Position
pos.log <- dc %>% select(-c('S/C', 'Ntnlty')) %>% my.log('Pos')
pos.log.adj <- temp.dc %>% select(-c('S/C', 'Ntnlty')) %>% my.log('Pos')


## Shoots/Catches
my.log.sc <- function(data) {
  set.seed(1)
  k <- 10
  n <- nrow(data)
  p <- ncol(data)
  fold <- sample(k, n, replace = TRUE)
  
  confuse <- matrix(rep(0, 4), nrow = 2)
  pred <- as.character(rep(0, n))
  for(i in 1:k) {
    model <- data %>% .[fold != i, ] %>%
      glm(`S/C` ~ ., data = ., family = binomial)
    logit <- model$coefficients[1] + model$coefficients[-1] %*%
      (data %>% select(-1) %>% .[fold == i, ] %>% as.matrix %>% t) %>%
      as.numeric
    confuse <- confuse + table(dc$`S/C`[fold == i], logit >= 0)
    pred[fold == i] <- logit >= 0
  }
  rate <- 1 - sum(diag(confuse)) / sum(confuse)
  rates <- 1 - diag(confuse) / apply(confuse, 2, sum)
  colnames(confuse) <- c('L', 'R')
  
  return(list(confuse = confuse, rate = rate, rates = rates))
}

sc.log <- dc %>% select(-c('Pos', 'Ntnlty')) %>% my.log.sc

save(nty.lda, pos.lda, pos.lda.adj, pos.lda.sub, pos.log, pos.log.adj,
     pos.qda, pos.qda.adj, sc.lda, sc.log, sc.qda, file = '../data/class.Rdata')


