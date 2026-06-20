# HBSC dataset prep

rm(list = ls())

setwd("C:/MisLocalFiles/Github/HBSC/")

require(tidyverse)

hbsc2018 <- read_delim("./data/raw/HBSC2018OAed1.1.csv",
                       delim = ";",
                       na = c("", " "))
#problems(hbsc2018)
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

# function that flags if a psycosomatic issue is frequent
is_psycosom_frequent <- function(col){
  col_recoded <- case_when(
    between(col,1,3) ~ 1,
    between(col,4,5) ~ 0,
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
    between(pmsu, 0, 1) ~ "Low",
    between(pmsu, 2, 5) ~ "Med",
    between(pmsu, 6, 9) ~ "High",
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
    between(famsup_MSPSS, 3, 8) ~ "Low",
    between(famsup_MSPSS, 9, 14) ~ "Med",
    between(famsup_MSPSS, 15, 21) ~ "High",
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
    IRRELFAS_LMH == 1 ~ "Low",
    IRRELFAS_LMH == 2 ~ "Med",
    IRRELFAS_LMH == 3 ~ "High",
    TRUE ~ "_ERROR"
  ))

table(dat$IRRELFAS_LMH_lbl)

# for now, keep only complete obs
dat <- dat %>%
  filter(IRRELFAS_LMH_lbl != "_ERROR") 

# prep Y (emotional health)
vars_mental_health <- c("headache",
                        "stomachache",
                        "backache",
                        "feellow",
                        "irritable",
                        "nervous",
                        "sleepdificulty",
                        "dizzy"
)

lapply(dat[,vars_mental_health], 
       table, useNA = "always")

dat$mental_health <- rowMeans(dat[,vars_mental_health],
                              na.rm = TRUE)

summary(dat$mental_health)

# inventing for now; hoping to find somebody doing it
dat <- dat %>%
  mutate(mental_health_lbl = case_when(
  between(mental_health,1,3.375) ~ "Low", #Q1
  between(mental_health, 3.375, 4.625) ~ "Med", #Q3
  between(mental_health, 4.625, 5) ~ "High",
  TRUE ~ "_ERROR"
  ))

table(dat$mental_health_lbl, useNA = "always")

# for now, only keep complete obs
dat <- dat %>%
  filter(mental_health_lbl != "_ERROR")

head(dat[,c(vars_mental_health, "mental_health", "mental_health_lbl")], 10)

# from Subjective health and well-being of children and adolescents 
# in Germany – Cross-sectional results of the 2017/18 HBSC study
# create Y index of wellbeing from 3 parts: health, life sat, psycosomatic

table(dat$health, useNA = "always")

table(dat$lifesat, useNA = "always")

lapply(dat[,vars_mental_health], 
       table, useNA = "always")

dat <- dat %>%
  mutate(health_r = case_when(
    health %in% c(1,2) ~ "Good",
    health %in% c(3,4) ~ "Poor",
    TRUE ~ "_ERROR" #NAs
  ),
  lifesat_r = case_when(
    between(lifesat,0,5) ~ "Low",
    between(lifesat,6,10) ~ "High",
    TRUE ~ "_ERROR" #NAs
  )
  )

lapply(dat[,vars_mental_health], 
       table, useNA = "always")

vars_mental_health_r <- paste0(vars_mental_health, "_r")

dat <- dat %>%
  mutate(
    headache_r = is_psycosom_frequent(headache), #frequent if 1-3, 
    stomachache_r = is_psycosom_frequent(stomachache),
    backache_r = is_psycosom_frequent(backache),
    feellow_r = is_psycosom_frequent(feellow),
    irritable_r = is_psycosom_frequent(irritable),
    nervous_r = is_psycosom_frequent(nervous),
    sleepdificulty_r = is_psycosom_frequent(sleepdificulty),
    dizzy_r = is_psycosom_frequent(dizzy)
  ) 

dat$num_psycosom <- rowSums(dat[,vars_mental_health_r])

dat$frequent_physocom <- ifelse(dat$num_psycosom >= 2, 1, 0) # per paper

table(dat$health_r)
table(dat$lifesat_r)
table(dat$frequent_physocom)

# clean NAs for now
dat <- dat %>%
  filter(health_r != "_ERROR") %>%
  filter(lifesat_r != "_ERROR") %>%
  filter(!is.na(frequent_physocom))

dat %>%
  group_by(health_r, lifesat_r, frequent_physocom) %>%
  tally()

dat <- dat %>% 
  mutate(good_wellbeing_index = case_when(
    health_r == "Good" & lifesat_r == "High" & frequent_physocom == 0 ~ 1,
    TRUE ~ 0
  ))

table(dat$good_wellbeing_index, useNA = "always")

write_csv(dat, 
           "./data/processed/dat_HBSC2018_20260619.csv"
           )





