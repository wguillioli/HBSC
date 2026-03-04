
require(tidyverse)

dat <- "C:/Users/wagui/Downloads/Allrounds_CSV/HBSC2018OAed1.1.csv"

d <- read_delim(dat, delim = ";")

glimpse(d)

table(d$countryno)
