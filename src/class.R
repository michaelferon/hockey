rm( list = ls() )

# Make sure you're in the 'src' directory.
# CLASSIFICATION ANALYSIS

library(ggplot2)
library(lubridate)
library(MASS)
library(dplyr)

# Load `df` and `ds`.
load('../data/data.Rdata')
load('../data/bio.Rdata')

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
  select(-c(1:2, 5:7, 9:11)) %>%
  as.data.frame
dc$Pos <- as.factor(dc$Pos)
dc$Ntnlty <- as.factor(dc$Ntnlty)
dc$`S/C` <- as.factor(dc$`S/C`)


my.da <- function(data, var, type = 'lda', priors = NULL) {
  set.seed(1)
  k <- 10
  n <- dim(data)[1]
  p <- dim(data)[2]
  fold <- sample(k, n, replace = TRUE)
  
  g <- length(unique(data[[var]]))
  confuse <- matrix(rep(0, g^2), nrow = g)
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
  }
  rate <- 1 - sum(diag(confuse)) / sum(confuse)
  rates <- 1 - diag(confuse) / apply(confuse, 2, sum)
  
  return(list(confuse = confuse, rate = rate, rates = rates))
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
