# HBSC dataset prep

#backlog
#fix how age is read due to , . issue

# ---------------------------------------------------------
# project setup
# ---------------------------------------------------------

rm(list = ls())

setwd("C:/MisLocalFiles/Github/HBSC/")

require(tidyverse)

# ---------------------------------------------------------
# load dataset
# ---------------------------------------------------------

hbsc2018 <- read_delim("./data/raw/HBSC2018OAed1.1.csv",
                       delim = ";",
                       na = c("", " "))
dim(hbsc2018) #244097    120


# ---------------------------------------------------------
# prep data for modeling
# ---------------------------------------------------------

vars_health_physical <- c("headache", "stomachache", "backache", "dizzy")
vars_health_mental <- c("feellow", "irritable", "nervous", "sleepdificulty")
vars_emcsocmed <- paste0("emcsocmed", seq(1,9,1))
vars_emcsocmed_r <- paste0("emcsocmed", seq(1,9,1), "_r")
vars_family_support <- c("famhelp", "famsup", "famtalk", "famdec")

# filter to one country and select variables to use
canada <- hbsc2018 %>%
  filter(countryno == 124000) %>% #129,50
  select(# personal info
         seqno_int,
         #age,
         agecat,
         #grade,
         sex,
         # 
         lifesat,
         all_of(vars_health_physical),
         all_of(vars_health_mental),
         all_of(vars_emcsocmed),
         all_of(vars_family_support)
         )

# recode lifesat
canada <- canada %>%
  mutate(
    #lifesat as binary
    lifesat_low = case_when(
      between(lifesat, 0, 5) ~ 1,
      between(lifesat, 6, 10) ~ 0,
      TRUE ~ NA))

# recode proportions with multiple (two or more) health complaints more than once a week
canada <- canada %>%
  # 8 health complaints into binary if more than once/week
  mutate(
    headache_frequent = ifelse(headache <= 2, 1, 0),
    stomachache_frequent = ifelse(stomachache <= 2, 1, 0),
    backache_frequent = ifelse(backache <= 2, 1, 0),
    dizzy_frequent = ifelse(dizzy <= 2, 1, 0),
    feellow_frequent = ifelse(feellow <= 2, 1, 0),
    irritable_frequent = ifelse(irritable <= 2, 1, 0),
    nervous_frequent = ifelse(nervous <= 2, 1, 0),
    sleepdificulty_frequent = ifelse(sleepdificulty <= 2, 1, 0)
  ) %>%
  # add the frequency of complaints
  mutate(
    mental_complaints_sum = feellow_frequent +
                           irritable_frequent +
                           nervous_frequent +
                           sleepdificulty_frequent,
    physical_complaints_sum = headache_frequent +
                              stomachache_frequent +
                              backache_frequent + 
                              dizzy_frequent
  ) %>%
  # derive index based on sum
  mutate(
      multiple_mental_complaints = ifelse(mental_complaints_sum >=2, 1, 0),
      multiple_health_complaints = 
        ifelse((mental_complaints_sum + physical_complaints_sum) >=2, 1, 0)
      ) 

# recode invidual vars
canada <- canada %>%
  mutate(
    age_recoded = case_match(agecat,
                             1 ~ 11,
                             2 ~ 13,
                             3 ~ 15,
                             .default = NA
                             ),
    sex_recoded = case_match(sex,
                             1 ~ "boy",
                             2 ~ "girl",
                             .default = NA)
  )

# recode emcsocmed to 0/1
recode_emcsocmed <- function(old_col){
  new_col <- case_when(
    old_col == 1 ~ 0,
    old_col == 2 ~ 1,
    TRUE ~ NA #99?
  )
  return(new_col)
}

# recode pmsu into YN and LMH
canada <- canada %>%
  mutate(emcsocmed1_r = recode_emcsocmed(emcsocmed1),
         emcsocmed2_r = recode_emcsocmed(emcsocmed2),
         emcsocmed3_r = recode_emcsocmed(emcsocmed3),
         emcsocmed4_r = recode_emcsocmed(emcsocmed4),
         emcsocmed5_r = recode_emcsocmed(emcsocmed5),
         emcsocmed6_r = recode_emcsocmed(emcsocmed6),
         emcsocmed7_r = recode_emcsocmed(emcsocmed7),
         emcsocmed8_r = recode_emcsocmed(emcsocmed8),
         emcsocmed9_r = recode_emcsocmed(emcsocmed9)
         ) 

canada <- canada %>%
  mutate(emcsocmed_sum = rowSums(canada[,vars_emcsocmed_r])) %>%
  mutate(
    pmsu_yn = case_when(
      between(emcsocmed_sum,6,9) ~ 1,
      between(emcsocmed_sum,0,5) ~ 0,
      TRUE ~ NA
    ),
    pmsu_lmh = case_when(
      between(emcsocmed_sum,6,9) ~ "high",
      between(emcsocmed_sum,2,5) ~ "med",
      between(emcsocmed_sum,0,1) ~ "low",
      TRUE ~ NA
    )
  )
  
table(canada$pmsu_lmh, canada$pmsu_yn, useNA = "always")

# recode family support as yn and hml
canada <- canada %>%
  select(all_of(vars_family_support)) %>%
  mutate(family_support_avg = rowMeans(.),
         family_support_sum = rowSums(.)
         ) %>%
  mutate(family_support_high = case_when(
           family_support_avg >= 1 & family_support_avg < 5.5 ~ 0,
           family_support_avg >= 5.5 & family_support_avg <= 7 ~ 1,
           TRUE ~ NA)) %>%
  mutate(family_support_hml = case_when(
           family_support_sum >= 4 & family_support_sum <= 11 ~ "low",
           family_support_sum >= 12 & family_support_sum <= 19 ~ "med",
           family_support_sum >= 20 & family_support_sum <= 28 ~ "high",
           TRUE ~ NA))

table(canada$family_support_high, canada$family_support_hml, useNA = "always")
table(canada$family_support_high, useNA = "always")
table(canada$family_support_hml, useNA = "always")
# need to decide; won't match diff cutoffs

summary(canada)





# write_csv(canada,
#           "canada.csv")


# from before
summary(hbsc2018)
glimpse(hbsc2018)

dat <- hbsc2018


# function that flags if a psycosomatic issue is frequent
is_psycosom_frequent <- function(col){
  col_recoded <- case_when(
    between(col,1,3) ~ 1,
    between(col,4,5) ~ 0,
    TRUE ~ col
  )
}

# function gets a column and returns reversed values recoded
reverse_school <- function(col){
  recoded_col <- case_match(
    col,
    5 ~ 0,
    4 ~ 1,
    3 ~ 2,
    2 ~ 3,
    1 ~ 4,
    TRUE ~ NA
  )
  return(recoded_col)
}


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

# family support from italy paper
vars_famsup <- c("famhelp",
                 "famsup",
                 "famtalk",
                 "famdec")

dat$fam_support_avg <- rowSums(dat[,vars_famsup]) / 4
dat$fam_support_lbl <- ifelse(dat$fam_support_avg < 5.5, "Low", "High")

# peer support from italy paper
vars_peersup <- c("friendhelp",
                 "friendcounton",
                 "friendshare",
                 "friendtalk")

dat$peer_support_avg <- rowSums(dat[,vars_peersup]) / 4
dat$peer_support_lbl <- ifelse(dat$peer_support_avg < 5.5, "Low", "High")

# school support
# from teacher and classmate support

# vars_teacher <- c("teacheraccept",
#                   "teachercare",
#                   "teachertrust")

# vars_classmates <- c("studtogether",
#                      "studhelpful",
#                      "studaccept")

dat <- dat %>%
  mutate(teacheraccept_r = reverse_school(teacheraccept),
         teachercare_r = reverse_school(teachercare),
         teachertrust_r = reverse_school(teachertrust),
         studtogether_r = reverse_school(studtogether),
         studhelpful_r = reverse_school(studhelpful),
         studaccept_r = reverse_school(studaccept)
         ) %>%
  mutate(teacher_support_avg = (teacheraccept_r + teachercare_r + teachertrust_r)/3,
         stud_support_avg = (studtogether_r + studhelpful_r + studaccept_r)/3
  ) %>%
  mutate(school_support_sum = teacher_support_avg + stud_support_avg) %>%
  mutate(school_support_avg = school_support_sum / 2) %>%
  mutate(school_support_lbl = ifelse(school_support_avg < 2.5, "Low", "High")) #median or 2.5 as in paper?


# from 07 Social media threats and health among adolescents: 
# evidence from the health behaviour in school-aged children study

#pmsu as binary
dat <- dat %>%
  mutate(is_pmsu_problematic = case_when(
    pmsu >= 6 ~ 1,
    pmsu >= 0 ~ 0,
    TRUE ~ -999
  ))


write_csv(dat, 
           "./data/processed/dat_HBSC2018_20260623.csv"
           )





