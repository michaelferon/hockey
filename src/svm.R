rm( list = ls() )

# Make sure you're in the 'src' directory.
# k-Nearest Neighbors analysis.

library(ggplot2)
library(lubridate)
library(MASS)
library(nnet)
library(class)
library(dplyr)
library(e1071)

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



data <- dc %>%
  select(-c('S/C', 'Ntnlty'))
my.svm <- function(data, type = 'linear') {
  cost <- log10(10^(seq(0.001, 100, length = 5)))
  params <- list(cost = cost)
  if (type == 'radial') {
    gamma = seq(0.1, 10, length = 5)
    params$gamma <- gamma
  }
  
  if (names(data)[1] == 'Pos') {
    tune.out <- tune(svm, Pos ~ ., data = data, kernel = type,
                     ranges = params)
  }
  
}


