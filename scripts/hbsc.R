
require(tidyverse)

setwd("C:/MisLocalFiles/Github/HBSC/")

hbsc2018 <- read_delim("./data/HBSC2018OAed1.1.csv", delim = ";")

glimpse(hbsc2018)

hbsc2018_slim <- hbsc2018 %>%
  select(seqno_int,
         health, #1-4, good-bad
         lifesat,#0-10, worst-best
         feellow,#1-5, daily to never
         irritable, #1-5, daily to never
         nervous, #1-5, daily to never
         starts_with("emcsocmed"),
         countryno,
         region
  )

summary(hbsc2018_slim)


hbsc2018_slim <- hbsc2018_slim %>%
  mutate(feellow_coded = recode(feellow,
    "1" = "1 About every day",
    "2" = "2 More once/week", 
    "3" = "3 About every week",
    "4" = "4 About every month",
    "5" = "5 Rarely or never"
  ))

table(hbsc2018_slim$feellow_coded, useNA = "always")

table(hbsc2018_slim$region, useNA = "always")

table(hbsc2018_slim$emcsocmed5)

