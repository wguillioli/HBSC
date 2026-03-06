
require(tidyverse)

dat <- "C:/MisLocalFiles/Github/HBSC/Allrounds_CSV/HBSC2018OAed1.1.csv"

d <- read_delim(dat, delim = ";")

glimpse(d)

table(d$countryno)

write_csv(d, "C:/MisLocalFiles/Github/HBSC/HBSC2018OAed1.1.csv")
