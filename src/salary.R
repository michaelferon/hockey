rm(list = ls())

# Make sure you're in the 'src' directory.

library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(MASS)
library(nnet)
library(dplyr)


load('../data/data.Rdata')
rm(ds)

dep <- c('P', 'SHG', 'SHP', 'FOL', 'EV FOL', 'PP FOL', 'SH FO', 'SH FOW',
         'SH FOL', 'OZ FOL', 'NZ FOL', 'DZ FO', 'DZ FOW', 'DZ FOL',
         'On-Ice EV GD', 'ENP', 'MsS Cross', 'Net Pen', 'G Msct', 'SHA', 'SHA2',
         'PPA', 'PPA2')
df <- df %>%
  .[, !(names(df) %in% dep)] %>%
  mutate(Player = str_to_lower(Player)) %>%
  mutate(Player = str_replace_all(Player, 'alexander', 'alex')) %>%
  mutate(Player = str_replace_all(Player, 'christopher', 'chris')) %>%
  mutate(Player = str_replace_all(Player, 'matthew', 'matt')) %>%
  filter(Season == 2018) %>%
  select(-2, -5, -6, -7, -9, -10, -11)

salary <- read_excel('../data/contracts.xlsx') %>%
  select(-2, -3, -5) %>%
  rename(Player = Name, Free = `Free Agent`) %>%
  mutate(Player = str_trim(Player)) %>%
  mutate(Player = str_to_lower(Player)) %>%
  mutate(Player = str_replace_all(Player, 'alexander', 'alex')) %>%
  mutate(Player = str_replace_all(Player, 'christopher', 'chris')) %>%
  mutate(Player = str_replace_all(Player, 'matthew', 'matt')) %>%
  filter(Player != '') %>%
  arrange(Player)

badNames <- salary %>%
  filter(Free == 0) %>%
  .$Player
salary <- salary %>%
  filter(!(Player %in% badNames) | ((Player %in% badNames) & Free == 0)) %>%
  select(Player, Average) %>%
  rename(Salary = Average)


mean(df$Player %in% salary$Player)
check <- df %>%
  filter(!(Player %in% salary$Player)) %>%
  .$Player

df <- df %>%
  filter(Player %in% salary$Player)
salary <- salary %>%
  filter(Player %in% df$Player)

test <- tapply(salary$Player, salary$Player, length)
test[test > 1]
ind1 <- which(salary$Player == 'andreas johnsson' & salary$Salary == 3400000)
ind2 <- which(salary$Player == 'jason spezza' & salary$Salary == 700000)
salary <- salary %>%
  slice(-ind1, -ind2)


data <- right_join(salary, df) %>%
  mutate_at(c('S/C', 'Pos', 'Ntnlty'), as.factor)

save(data, file = '../data/newData.Rdata')
