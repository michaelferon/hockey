rm( list = ls() )

# Make sure you're in the 'src' directory.

library(dplyr)
library(readr)
library(tibble)


files <- c('../data/clean/summary.csv',
           '../data/clean/faceoff.csv',
           '../data/clean/goals.csv',
           '../data/clean/misc.csv',
           '../data/clean/penalty.csv',
           '../data/clean/kill.csv',
           '../data/clean/pp.csv')


df <- read_csv(files[1], na = c('--', '', 'NA'),
               col_types = cols('TOI/GP' = col_character())) %>%
      arrange(Player, Season, Team)
for (file in files[-1]) {
  temp <- read_csv(file, na = c('--', '', 'NA'),
                   col_types = cols('TOI/GP' = col_character())) %>%
          arrange(Player, Season, Team) %>%
          .[, !(colnames(.) %in% colnames(df))]
  df <- df %>% bind_cols(temp)
}
rm(file, files, temp)


bio <- read_csv('../data/clean/bio.csv', na = c('--', '', 'NA')) %>%
       arrange(Player, Team) %>%
       .[, !(colnames(.) %in% colnames(df)) |
             colnames(.) %in% c('Player', 'Team', 'S/C', 'Pos')]

df <- df %>% inner_join(bio, by = c('Player', 'Team', 'S/C', 'Pos'))
