rm( list = ls() )

# Make sure you're in the 'src' directory.

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(readxl)




summary.files <- paste('../data/summary/', list.files('../data/summary/'), sep='')
data.summary <- read_excel(summary.files[1], na = '--')
for (file in summary.files[-1]) {
  temp <- read_excel(file, na = '--')
  data.summary <- data.summary %>% bind_rows(temp)
}


faceoff.files <- paste('../data/faceoff/', list.files('../data/faceoff/'), sep='')
data.faceoff <- read_excel(faceoff.files[1], na = '--') %>%
                mutate(FO = as.numeric(gsub(',', '', FO)))
for (file in faceoff.files[-1]) {
  temp <- read_excel(file, na = '--') %>%
          mutate(FO = as.numeric(gsub(',', '', FO)),
                 `EV FO` = as.numeric(gsub(',', '', FO)),
                 FOW = as.numeric(gsub(',', '', FO)))
  data.faceoff <- data.faceoff %>% bind_rows(temp)
}

goals.files <- paste('../data/goals/', list.files('../data/goals/'), sep='')
data.goals <- read_excel(goals.files[1], na = '--')
for (file in goals.files[-1]) {
  temp <- read_excel(file, na = '--')
  data.goals <- data.goals %>% bind_rows(temp)
}

misc.files <- paste('../data/misc/', list.files('../data/misc/'), sep='')
data.misc <- read_excel(misc.files[1], na = '--')
for (file in misc.files[-1]) {
  temp <- read_excel(file, na = '--')
  data.misc <- data.misc %>% bind_rows(temp)
}

penalty.files <- paste('../data/penalty/', list.files('../data/penalty/'), sep='')
data.penalty <- read_excel(penalty.files[1], na = '--')
for (file in penalty.files[-1]) {
  temp <- read_excel(file, na = '--')
  data.penalty <- data.penalty %>% bind_rows(temp)
}

kill.files <- paste('../data/kill/', list.files('../data/kill/'), sep='')
data.kill <- read_excel(kill.files[1], na = '--')
for (file in kill.files[-1]) {
  temp <- read_excel(file, na = '--')
  data.kill <- data.kill %>% bind_rows(temp)
}

pp.files <- paste('../data/pp/', list.files('../data/pp/'), sep='')
data.pp <- read_excel(pp.files[1], na = '--')
for (file in pp.files[-1]) {
  temp <- read_excel(file, na = '--')
  data.pp <- data.pp %>% bind_rows(temp)
}




data.summary <- data.summary %>% arrange(Player, Season, Team)
data.faceoff <- data.faceoff %>% arrange(Player, Season, Team)
data.goals <- data.goals %>% arrange(Player, Season, Team)
data.misc <- data.misc %>% arrange(Player, Season, Team)
data.penalty <- data.penalty %>% arrange(Player, Season, Team)
data.kill <- data.kill %>% arrange(Player, Season, Team)
data.pp <- data.pp %>% arrange(Player, Season, Team)

del.faceoff <- c(1, 2, 3, 4, 5, 9)
del.goals <- c(1, 2, 3, 4, 5, 6, 7, 8)
del.misc <- c(1, 2, 3, 4, 5, 6, 7)
del.penalty <- c(1, 2, 3, 4, 5, 6, 7, 8)
del.kill <- c(1, 2, 3, 4, 5, 22)
del.pp <- c(1, 2, 3, 4, 5, 22)

data <- data.summary %>%
  bind_cols(select(data.faceoff, -del.faceoff)) %>%
  bind_cols(select(data.goals, -del.goals)) %>%
  bind_cols(select(data.misc, -del.misc)) %>%
  bind_cols(select(data.penalty, -del.penalty)) %>%
  bind_cols(select(data.kill, -del.kill)) %>%
  bind_cols(select(data.pp, -del.pp))












