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


# Function to convert time strings to seconds.
str2sec <- function(x) {
  sapply(strsplit(x, ':'), function(y) as.numeric(y) %*% c(60, 1))
}
# A few necessary transformations.
df <- df %>%
  mutate(`Draft Yr` = as.character(`Draft Yr`)) %>%
  mutate_at(c('Season', '1st Season'),
            function(x) str_sub(as.character(x), 1, 4)) %>%
  mutate_at(c('TOI/GP', 'SH TOI', 'PP TOI'), str2sec) %>%
  mutate_at(c('PP TOI/GP', 'SH TOI/GP', 'EV TOI/GP', 'PIM/GP'),
            function(x) 60*hour(x) + minute(x))



## TEST
df <- df %>%
  # High NA features.
  select(-c('Draft Yr', 'Round', 'Overall', 'S/P', 'FOW%')) %>%
  filter(GP >= 20) # Removing players with low GP.

test <- apply(df, 2, function(x) mean(is.na(x)))
test <- test[test > 0] # Proportion of NAs in certain features.

df <- df %>% na.omit
X <- df %>% # X holds all the strictly numeric features.
  select(-c('Player', 'Season', 'Team', 'S/C', 'Pos', 'DOB', 'Birth City',
            'Ctry', 'Ntnlty', '1st Season', 'HOF'))

