rm( list = ls() )

# Make sure you're in the 'src' directory.
# CLASSIFICATION ANALYSIS

library(ggplot2)
library(lubridate)
library(MASS)
library(dplyr)
library(CombMSC)

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
  dplyr::select(-c(1:2, 5:7, 9:11)) %>%
  as.data.frame
dc$Pos <- as.factor(dc$Pos)
dc$Ntnlty <- as.factor(dc$Ntnlty)
dc$`S/C` <- as.factor(dc$`S/C`)



temp.dc <- dc %>% mutate(Pos = as.character(Pos))
temp.dc$Pos[temp.dc$Pos == 'L' | temp.dc$Pos == 'R'] <- 'F'
temp.dc$Pos <- as.factor(temp.dc$Pos)
data <- temp.dc %>% dplyr::select(-c(1, 3))

set.seed(1)
k <- 10
n <- dim(data)[1]
p <- dim(data)[2]
fold <- sample(k, n, replace = TRUE)
g <- length(unique(data$Pos))
prior <- tapply(data$Pos, data$Pos, function(x) length(x) / n) %>%
  as.numeric

vars <- names(data)[-1]
set <- subsets(length(vars), 2, vars)
rates <- c()

for (j in 1:(dim(set)[1])) {
  print(j)
  confuse <- matrix(rep(0, g^2), nrow = g)
  temp <- data %>% dplyr::select(c('Pos', set[j, ]))
  for (i in 1:k) {
    zcv <- lda(formula = Pos~., data = temp[fold != i,],
               prior = prior)
    ppcv <- predict(zcv, temp[fold == i, 2:3])
    confuse <- confuse + table(temp[fold == i, 'Pos'], ppcv$class)
  }
  rate <- 1 - sum(diag(confuse)) / sum(confuse)
  rates <- c(rates, rate)
}

start_vars <- set[which.min(rates), ]

m <- ncol(data) - 1
all_rates <- c(1, 1)
cv_vars <- vector(mode = 'list', length = m)
cv_vars[[1]] <- NULL; cv_vars[[2]] <- NULL

for (j in 3:m) {
  print(j)
  next_vars <- vars[!(vars %in% start_vars)]
  rates <- c()
  for (var in next_vars) {
    v <- c(start_vars, var)
    confuse <- matrix(rep(0, g^2), nrow = g)
    temp <- data %>% dplyr::select(c('Pos', v))
    for (i in 1:k) {
      zcv <- lda(formula = Pos~., data = temp[fold != i,],
                 prior = prior)
      ppcv <- predict(zcv, temp[fold == i, 2:(dim(temp)[2])])
      confuse <- confuse + table(temp[fold == i, 'Pos'], ppcv$class)
    }
    rate <- 1 - sum(diag(confuse)) / sum(confuse)
    rates <- c(rates, rate)
  }
  
  all_rates <- c(all_rates, min(rates))
  start_vars <- c(start_vars, next_vars[which.min(rates)])
  cv_vars[[j]] <- start_vars
}

print(which.min(all_rates))
print(min(all_rates))
print(cv_vars[[which.min(all_rates)]])

out_vars <- data.frame(vars = cv_vars[[which.min(all_rates)]])
out_data <- data.frame(rates = all_rates)

write.csv(out_vars, file = '../data/class-subset-vars.csv')
write.csv(out_data, file = '../data/class-subset-data.csv')

## TEST
test <- tibble(.rows = 89)
test[[1]] <- as.character(NA)
test[[2]] <- as.character(NA)
test[[2]][1:2] <- c('EV FO', 'BkS/60')
for (i in 3:length(cv_vars)) {
  test[[i]] <- as.character(NA)
  test[[i]][1:length(cv_vars[[i]])] <- cv_vars[[i]]
}

write.csv(test, file = '../data/class-subset-full.csv')




