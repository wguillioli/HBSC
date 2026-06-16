# HBSC dataset prep

rm(list = ls())

setwd("C:/GitHub/HBSC/")

require(tidyverse)

hbsc2018 <- read_delim("./data/HBSC2018OAed1.1.csv",
                       delim = ";",
                       na = c("", " "))
problems(hbsc2018)
dim(hbsc2018) #244097    120
summary(hbsc2018)
glimpse(hbsc2018)

dat <- hbsc2018

# pmsu (problematic social media use) recoding
#PSMU was assessed using the Social Media Disorder Scale which assesses symptoms 
#of PSMU through nine items (preoccupation, tolerance, withdrawal, 
#persistence, escape, conflict, neglect of other activities, and difficulties 
#in other life areas) with dichotomous (No/Yes) answers [11]. 
#Internal consistency was high (Cronbach’s α = 0.75). 
#Based on prior research, respondents with a sum score of 6 to 9 were 
#identified as having PSMU. Those with a sum score between 2 and 5 were 
#considered to be at moderate risk of PSMU, while a sum score of 0 to 1 
#indicated a low risk of PSMU

# function gets a pmsu column, recodes and returns a new column
recode_pmsu <- function(col){
  # 1 No 156598 78.0%
  # 2 Yes 44267 22.0%
  # 99 Missing due to skip pattern 11259
  # Sysmiss 31973
  col_recoded <- case_when(
      col == 1 ~ 0,
      col == 2 ~ 1,
      TRUE ~ col
    )
}

vars_pmsu <- c()
vars_pmsu_r <- c()

for (i in 1:9){
  vars_pmsu <- c(vars_pmsu, paste0("emcsocmed",i))
  vars_pmsu_r <- c(vars_pmsu_r, paste0("emcsocmed",i,"_r"))
}

dat <- dat %>%
  mutate(emcsocmed1_r = recode_pmsu(emcsocmed1),
         emcsocmed2_r = recode_pmsu(emcsocmed2),
         emcsocmed3_r = recode_pmsu(emcsocmed3),
         emcsocmed4_r = recode_pmsu(emcsocmed4),
         emcsocmed5_r = recode_pmsu(emcsocmed5),
         emcsocmed6_r = recode_pmsu(emcsocmed6),
         emcsocmed7_r = recode_pmsu(emcsocmed7),
         emcsocmed8_r = recode_pmsu(emcsocmed8),
         emcsocmed9_r = recode_pmsu(emcsocmed9)
         ) 

dat <- dat %>%
  mutate(pmsu = rowSums(dat[,vars_pmsu_r]))

summary(dat[,c(vars_pmsu, vars_pmsu_r, "pmsu")])

lapply(dat[, c(vars_pmsu, vars_pmsu_r, "pmsu")], table, useNA = "always")

# for now, keep only complete observations for pmsu
dat <- dat %>%
  filter(pmsu >= 0 & pmsu <= 9)

dat <- dat %>%
  mutate(pmsu_lbl = case_when(
    between(pmsu, 0, 1) ~ "L",
    between(pmsu, 2, 5) ~ "M",
    between(pmsu, 6, 9) ~ "H",
    TRUE ~ "_ERROR"
  ))

table(dat$pmsu_lbl, useNA = "always")

#Family support was assessed using three items from the Multidimensional Scale of 
#Perceived Social Support (MSPSS) [32] tapping into emotional support: 
#“My family really tries to help me”, “I receive the emotional help and support 
#I need from my family”, and “I can discuss my problems with my family”. 
# famhelp: Family tries to help
# famsup: Get emotional help
#famtalk: Talk about problems
dat <- dat %>%
  mutate(famsup_MSPSS = famhelp
                        + famsup
                        + famtalk) %>%
  mutate(famsup_MSPSS_lbl = case_when(
    between(famsup_MSPSS, 3, 8) ~ "L",
    between(famsup_MSPSS, 9, 14) ~ "M",
    between(famsup_MSPSS, 15, 21) ~ "H",
    TRUE ~ "_ERROR"
  ))

summary(dat$famsup_MSPSS)
table(dat$famsup_MSPSS_lbl, useNA = "always")

# for now, keep only observations with complete fam support data
dat <- dat %>%
  filter(famsup_MSPSS_lbl != "_ERROR") 

#Family affluence was then categorized into three levels based on relative measures:
#the lowest 20%, the middle 60%, and the highest 20%.
## IRFAS: Family affluence scale III - continuous
# IRRELFAS_LMH: Relative family affluence categorical
#1 Lowest 20 pct 45818 19.8%
#2 Medium 60 pct 142148 61.4%
#3 Highest 20 pct
summary(dat$IRRELFAS_LMH)

# for now, keep breaks as provided. but wondering if relative to each population?
dat <- dat %>%
  mutate(IRRELFAS_LMH_lbl = case_when(
    IRRELFAS_LMH == 1 ~ "L",
    IRRELFAS_LMH == 2 ~ "M",
    IRRELFAS_LMH == 3 ~ "H",
    TRUE ~ "_ERROR"
  ))

table(dat$IRRELFAS_LMH_lbl)

# for now, keep only complete obs
dat <- dat %>%
  filter(IRRELFAS_LMH_lbl != "_ERROR") 


