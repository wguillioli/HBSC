# HBSC dataset prep
# Updated: 2026-07-17

#backlog
#fix how age is read due to , . issue
# toca borrar las vars que ya no serviran
# recode online friends for later
# # emconlpref1: Secrets, more easily online from 1 disagree to 5 strong agree
# emconlpref2: Feelings, more easily online
# emconlpref3: Concerns, more easily online


# ---------------------------------------------------------
# project setup
# ---------------------------------------------------------

rm(list = ls())

setwd("C:/MisLocalFiles/Github/HBSC/scripts")
project_folder <- "C:/MisLocalFiles/Github/HBSC/"

require(tidyverse)
library(broom)

# ---------------------------------------------------------
# load dataset
# ---------------------------------------------------------

hbsc2018_file_full_path <- paste0(project_folder,
                                  "/data/raw/HBSC2018OAed1.1.csv")
  
hbsc2018 <- read_delim(hbsc2018_file_full_path,
                       delim = ";",
                       na = c("", " "))
dim(hbsc2018) #244097    120


# ---------------------------------------------------------
# recode variables
# ---------------------------------------------------------

vars_health_physical <- c("headache", "stomachache", "backache", "dizzy")
vars_health_mental <- c("feellow", "irritable", "nervous", "sleepdificulty")
vars_emcsocmed <- paste0("emcsocmed", seq(1,9,1))
vars_emcsocmed_r <- paste0("emcsocmed", seq(1,9,1), "_r")
vars_family_support <- c("famhelp", "famsup", "famtalk", "famdec")
vars_friends_support <- c("friendhelp", "friendcounton", "friendshare", "friendtalk")
vars_school <- c("teacheraccept", "teachercare", "teachertrust",
                 "studtogether", "studhelpful", "studaccept")
vars_online_comms <- paste0("emconlfreq",seq(1,4,1))
vars_online_share <- paste0("emconlpref",seq(1,3,1))

# select variables of interest
dat <- hbsc2018 %>%
  select(countryno,
         seqno_int,
         agecat,
         sex,
         lifesat,
         all_of(vars_health_physical),
         all_of(vars_health_mental),
         all_of(vars_emcsocmed),
         all_of(vars_family_support),
         all_of(vars_friends_support),
         all_of(vars_school),
         IRRELFAS_LMH,
         talkfather,
         talkmother,
         beenbullied,
         cbeenbullied,
         all_of(vars_online_comms),
         all_of(vars_online_share)
         )

# recode country name
recode_map <- c(
  "8000"   = "Albania",
  "31000"  = "Azerbaijan",
  "40000"  = "Austria",
  "51000"  = "Armenia",
  "56001"  = "Belgium (Flemish)",
  "56002"  = "Belgium (French)",
  "100000" = "Bulgaria",
  "124000" = "Canada",
  "191000" = "Croatia",
  "203000" = "Czech Republic",
  "208000" = "Denmark",
  "233000" = "Estonia",
  "246000" = "Finland",
  "250000" = "France",
  "268000" = "Georgia",
  "276000" = "Germany",
  "300000" = "Greece",
  "304000" = "Greenland",
  "348000" = "Hungary",
  "352000" = "Iceland",
  "372000" = "Ireland",
  "376000" = "Israel",
  "380000" = "Italy",
  "398000" = "Kazakhstan",
  "428000" = "Latvia",
  "440000" = "Lithuania",
  "442000" = "Luxembourg",
  "470000" = "Malta",
  "498000" = "Republic of Moldova",
  "528000" = "Netherlands",
  "578000" = "Norway",
  "616000" = "Poland",
  "620000" = "Portugal",
  "642000" = "Romania",
  "643000" = "Russia",
  "688000" = "Serbia",
  "703000" = "Slovakia",
  "705000" = "Slovenia",
  "724000" = "Spain",
  "752000" = "Sweden",
  "756000" = "Switzerland",
  "792000" = "Turkey",
  "804000" = "Ukraine",
  "807000" = "Macedonia",
  "826001" = "England",
  "826002" = "Scotland",
  "826003" = "Wales",
  "826004" = "Northern Ireland",
  "840000" = "USA"
)

dat$country_name <- recode_map[as.character(dat$countryno)]

# recode lifesat
dat <- dat %>%
  mutate(
    lifesat_low = case_when(
      between(lifesat, 0, 5) ~ 1,
      between(lifesat, 6, 10) ~ 0,
      TRUE ~ NA))

table(dat$lifesat_low, dat$lifesat, useNA = "always")

# recode proportions with multiple (two or more) health complaints more than once a week
dat <- dat %>%
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
      mental_complaintsY = ifelse(mental_complaints_sum >=2, 1, 0),
      health_complaintsY = 
        ifelse((mental_complaints_sum + physical_complaints_sum) >=2, 1, 0)
      ) 

# recode age and sex
dat <- dat %>%
  mutate(
    age_r = case_match(agecat,
                             1 ~ 11,
                             2 ~ 13,
                             3 ~ 15,
                             .default = NA
                             ),
    sex_r = case_match(sex,
                             1 ~ "boy",
                             2 ~ "girl",
                             .default = NA)
  )

table(dat$agecat, dat$age_r, useNA = "always")
table(dat$sex, dat$sex_r, useNA = "always")

# recode emcsocmed to 0/1 from 1/2
recode_emcsocmed <- function(old_col){
  new_col <- case_when(
    old_col == 1 ~ 0,
    old_col == 2 ~ 1,
    TRUE ~ NA #99?
  )
  return(new_col)
}

# recode pmsu into YN and LMH
dat <- dat %>%
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

dat <- dat %>%
  mutate(emcsocmed_sum = rowSums(dat[,vars_emcsocmed_r])) %>%
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
  
table(dat$pmsu_lmh, dat$pmsu_yn, useNA = "always")

# recode family support as yn and hml
dat <- dat %>%
   mutate(family_support_avg = rowMeans(dat[,vars_family_support]),
          family_support_sum = rowSums(dat[,vars_family_support])
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
 
table(dat$family_support_high, dat$family_support_hml, useNA = "always")
table(dat$family_support_high, useNA = "always")
table(dat$family_support_hml, useNA = "always")

# recode friends/peer support as yn and hml
dat <- dat %>%
  mutate(friends_support_avg = rowMeans(dat[,vars_friends_support]),
         friends_support_sum = rowSums(dat[,vars_friends_support])
  ) %>%
  mutate(friends_support_high = case_when(
    friends_support_avg >= 1 & friends_support_avg < 5.5 ~ 0,
    friends_support_avg >= 5.5 & friends_support_avg <= 7 ~ 1,
    TRUE ~ NA)) %>%
  mutate(friends_support_hml = case_when(
    friends_support_sum >= 4 & friends_support_sum <= 11 ~ "low",
    friends_support_sum >= 12 & friends_support_sum <= 19 ~ "med",
    friends_support_sum >= 20 & friends_support_sum <= 28 ~ "high",
    TRUE ~ NA))

table(dat$friends_support_high, dat$friends_support_hml, useNA = "always")
table(dat$friends_support_high, useNA = "always")
table(dat$friends_support_hml, useNA = "always")

# recode school support in 3 parts: teach (3), classmates (2), belonging (2)
# Then combines all 3 and does low/high based on 2.5
# recodes from 1-5 to 0-4 (06-italy)
recode_school_vars <- function(old_values){
  
  new_values <- case_when(
    old_values == 5 ~ 1,
    old_values == 4 ~ 2,
    old_values == 3 ~ 3,
    old_values == 2 ~ 4,
    old_values == 1 ~ 5,
    TRUE ~ NA
  )
  
  return(new_values)
  
}

dat <- dat %>%
  # recode they can be added
  mutate(teacheraccept_r = recode_school_vars(teacheraccept),
         teachercare_r = recode_school_vars(teachercare),
         teachertrust_r = recode_school_vars(teachertrust),
         studtogether_r = recode_school_vars(studtogether),
         studhelpful_r = recode_school_vars(studhelpful),
         studaccept_r = recode_school_vars(studaccept)
         ) %>%
  # derive avg by teacher/student
  mutate(teacher_support_avg = (teacheraccept_r 
                               + teachercare_r
                               + teachertrust_r) / 3,
         student_support_avg = (studtogether_r
                                + studhelpful_r
                                + studaccept_r) / 3,
         # just for fun combine and try
         school_support_avg = (teacheraccept_r 
                                + teachercare_r
                                + teachertrust_r
                                + studtogether_r
                                + studhelpful_r
                                + studaccept_r) / 6,
         ) %>%
  # derive support yn based on hbsc
  mutate(
    teacher_support_high = case_when(
      teacher_support_avg >= 4 ~ 1,
      teacher_support_avg < 4 ~ 0,
      TRUE ~ NA),
      student_support_high = case_when(
        student_support_avg >= 4 ~ 1,
        student_support_avg < 4 ~ 0,
        TRUE ~ NA),
      school_support_high = case_when(
        school_support_avg >= 4 ~ 1,
        school_support_avg < 4 ~ 0,
      TRUE ~ NA)
  )
 
# family affluence
# irfas_quants <- quantile(canada$IRFAS, 
#                          probs = c(.2,.8,1), na.rm = TRUE)

dat <- dat %>%
  mutate(IRRELFAS_LMH_r = 
           case_when(IRRELFAS_LMH == 1 ~ "low20",
                     IRRELFAS_LMH == 2 ~ "med60",
                     IRRELFAS_LMH == 3 ~ "high20",
                     TRUE ~ NA)
  )

# recode talkfather and talkmother
# from 1 (very easy) to 5 (don't)
dat <- dat %>%
  mutate(talkfatherYes = 
           case_when(talkfather %in% c(1,2) ~ 1,
                     talkfather %in% c(3,4,5) ~ 0,
                     TRUE ~ NA),
         talkmotherYes = 
           case_when(talkmother %in% c(1,2) ~ 1,
                     talkmother %in% c(3,4,5) ~ 0,
                     TRUE ~ NA)
         )

table(dat$talkfatherYes, dat$talkfather, useNA = "always")
table(dat$talkmotherYes, dat$talkmother, useNA = "always")

# recode bullying vars
# cbeenbullied: Been cyber bullied
# beenbullied: Been bullied past months from 1 (no) to 5 (several/week)
dat <- dat %>%
  mutate(beenbulliedYes = 
           case_when(beenbullied == 1 ~ 0,
                     between(beenbullied, 2, 5) ~ 1,
                     TRUE ~ NA),
         cbeenbulliedYes = 
           case_when(cbeenbullied == 1 ~ 0,
                     between(cbeenbullied, 2, 5) ~ 1,
                     TRUE ~ NA)
           )

table(dat$beenbulliedYes, dat$beenbullied, useNA = "always")
table(dat$cbeenbulliedYes, dat$cbeenbullied, useNA = "always")

# online comms/friends
# emconlfreq1: Onl contact close friends from 1 never to 6 all the time()
# emconlfreq2: Onl contact larger friend group
# emconlfreq3: Onl contact online friends
# emconlfreq4: Onl contact other
# hbsc says yes if any of the 4 is always (6)
dat <- dat %>%
  mutate(emconlfreqYes = case_when(
    (emconlfreq1 == 6 | emconlfreq2 == 6 | emconlfreq3 == 6 | emconlfreq4 == 6) ~ 1,
    (!is.na(emconlfreq1) | !is.na(emconlfreq2) | !is.na(emconlfreq3) | !is.na(emconlfreq4)) ~ 0,
    TRUE ~ NA
  )) 
#still need to do something with 99

table(dat$emconlfreqYes, useNA = "always")


# -----------------------------------------------------------------------
# remove vars that are not needed anymore
# -----------------------------------------------------------------------

dat <- dat %>%
  select(-c(
    countryno,
    lifesat,
    ends_with("frequent"),
    mental_complaints_sum,
    physical_complaints_sum,
    all_of(vars_health_physical),
    all_of(vars_health_mental),
    agecat, 
    sex,
    all_of(vars_emcsocmed),
    emcsocmed_sum,
    family_support_avg,
    family_support_sum,
    friends_support_avg,
    friends_support_sum,
    all_of(vars_school),
    teacher_support_avg,
    student_support_avg,
    school_support_avg,
    IRRELFAS_LMH,
    talkfather,
    talkmother,
    beenbullied,
    cbeenbullied,
    all_of(vars_online_comms)
  ))


# -----------------------------------------------------------------------
# output data
# -----------------------------------------------------------------------

out_csv_full_name <- paste0(project_folder,
                            "data/processed/dat_",
                            Sys.Date(),
                            ".csv")
write_csv(dat,
          out_csv_full_name)


