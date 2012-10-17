library(ggplot2)
library(reshape)
library(plyr)
library(foreach)
library(fossil)
library(lubridate)

## Convert posix datetime to 24-hour hour
hourfun <- function(t){hour(t) + minute(t) / 60 + second(t) / 3600}
