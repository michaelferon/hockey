rm( list = ls() )

# Make sure you're in the 'src' directory.

library(dplyr)
library(ggplot2)
library(lubridate)

# Load `df` and `X` data objects.
load('../data/data.Rdata')
X %>% group_by(Season) %>% select(3:5) 



