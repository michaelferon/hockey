rm( list = ls() )

# Make sure you're in the 'src' directory.

library(dplyr)     # For data manipulation.
library(readr)     # For read_csv().
library(stringr)   # For str_sub().
library(lubridate) # For handling Time objects.


# Data files.
files <- c('../data/clean/summary.csv',
           '../data/clean/faceoff.csv',
           '../data/clean/goals.csv',
           '../data/clean/misc.csv',
           '../data/clean/penalty.csv',
           '../data/clean/kill.csv',
           '../data/clean/pp.csv')



# Read in data to df.
df <- read_csv(files[1], na = c('--', '', 'NA'), # -- marks NA in these files.
               col_types = cols('TOI/GP' = col_character())) %>%
      arrange(Player, Season, Team) # `TOI/GP` needs to be char.
for (file in files[-1]) {
  temp <- read_csv(file, na = c('--', '', 'NA'),
                   col_types = cols('TOI/GP' = col_character())) %>%
          arrange(Player, Season, Team) %>%
          .[, !(colnames(.) %in% colnames(df))]
  df <- df %>% bind_cols(temp)
}
rm(file, files, temp)



# Read in biographical data.
bio <- read_csv('../data/clean/bio.csv', na = c('--', '', 'NA')) %>%
       arrange(Player, Team) %>%
       .[, !(colnames(.) %in% colnames(df)) | # Removing some overlap.
             colnames(.) %in% c('Player')]

# There are 10 hockey players who share a set of 5 names. Tough to handle.
doubleNames <- (bio %>% group_by(Player) %>% count %>% filter(n > 1))$Player
df <- df %>% filter(!(Player %in% doubleNames))
df <- df %>% left_join(bio, by = c('Player'))
rm(doubleNames, bio)



# A bunch of necessary manipulation.
df <- df %>%
  mutate(Season = as.character(Season), # Change season to first year, as char.
         Season = str_sub(Season, 1, 4)) %>%
  mutate(`1st Season` = as.character(`1st Season`), # Same thing here.
         `1st Season` = str_sub(`1st Season`, 1, 4)) %>%
  mutate(`Draft Yr` = as.character(`Draft Yr`)) %>% # Change to char.
  mutate(`TOI/GP` = sapply(strsplit(df$`TOI/GP`, ':'), # char -> seconds.
                           function(y) sum(as.numeric(y) * c(60, 1)))) %>%
  # All these below convert Time to seconds.
  mutate(`PP TOI/GP` = 60*hour(`PP TOI/GP`) + minute(`PP TOI/GP`)) %>%
  mutate(`SH TOI/GP` = 60*hour(`SH TOI/GP`) + minute(`SH TOI/GP`)) %>%
  mutate(`EV TOI/GP` = 60*hour(`EV TOI/GP`) + minute(`EV TOI/GP`)) %>%
  mutate(`PIM/GP` = 60*hour(`PIM/GP`) + minute(`PIM/GP`))



## TEST
df <- df %>%
  # High NA features.
  select(-c('Draft Yr', 'Round', 'Overall', 'S/P', 'FOW%')) %>%
  filter(GP >= 20) # Removing players with low GP.

test <- apply(df, 2, function(x) mean(is.na(x)))
test <- test[test > 0] # Proportion of NAs in certain features.

