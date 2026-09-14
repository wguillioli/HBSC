# HBSC 
# Updated: 2026-09-1

# ---------------------------------------------------
# project setup
# ---------------------------------------------------

rm(list = ls())

require(tidyverse)
require(tidymodels)
#library(knitr)
require(stringr)
library(scales)
library(corrplot)
library(psych)
require(rsample)
require(countrycode)
require(naniar) #na vis
library(janitor) #cool new plots
require(rpart.plot)
library(kernelshap)
library(shapviz)
library(vip) #needeD?
library(ggridges)

prj_fldr <- "C:/MisLocalFiles/Github/HBSC/"
#setwd(paste0(prj_fldr, "scripts/"))
setwd(prj_fldr)

options(scipen = 999)

# ---------------------------------------------------
# load data, basic eda
# ---------------------------------------------------

# the european way with ; and ,
hbsc2018 <- read_csv2(paste0(prj_fldr, "data/raw/HBSC2018OAed1.1.csv"))

dim(hbsc2018)
glimpse(hbsc2018)

# print column names in columns for ease of view
vars_hbsc2018 <- colnames(hbsc2018)
vars_hbsc2018_tbl <- matrix(vars_hbsc2018, ncol = 6, byrow = TRUE)
vars_hbsc2018_tbl


# ---------------------------------------------------
# create working copy of df with vars of interest
# ---------------------------------------------------

# list of variables to keep by "theme"
vars_groups <- list(
  "individual_info" = c(
    "seqno_int", "countryno",  
    "agecat", "sex",
    "IRFAS", "IRRELFAS_LMH", 
    "IOTF4", "MBMI", 
    #"bodyweight", "bodyheight",
    "timeexe"
  ),
  "health" = c(
    "lifesat", 
    #"headache", "stomachache", "backache", "dizzy",
    "feellow", "irritable", "nervous", "sleepdificulty" 
  ),
  "pmsu" = c(
    "emcsocmed1", "emcsocmed2", "emcsocmed3",
    "emcsocmed4", "emcsocmed5", "emcsocmed6",
    "emcsocmed7", "emcsocmed8", "emcsocmed9"
  ),
  "bullying" = c(
    #"beenbullied", 
    "cbeenbullied"
  ),
  "online" = c(
    "emconlfreq1", "emconlfreq2", "emconlfreq3", "emconlfreq4", #freq of comms
    "emconlpref1", "emconlpref2", "emconlpref3" #pref of comms
  ),
  "family_support" = c(
    "famhelp", "famsup", "famtalk", "famdec"
  ),
  "friends_support" = c(
    "friendhelp", "friendcounton", "friendshare", "friendtalk"
  ),
  "school_support" = c(
    #"likeschool", "schoolpressure", # ignore for now
    "studtogether", "studhelpful", "studaccept",
    "teacheraccept", "teachercare", "teachertrust"
  ),
  "parents" = c(
    "talkfather", "talkmother"
  )
)

# new df with cols of interest
dat <- hbsc2018 %>%
  select(all_of(unlist(vars_groups, use.names = FALSE)))

rm(hbsc2018)
#gc()

# Get an idea of missing values across data set with vars I need
dat_nas <- data.frame(
  variable = names(dat),
  n = nrow(dat),
  na_count = sapply(dat, function(x) sum(is.na(x))),
  row.names = NULL
) %>%
  mutate(na_pct = percent(
    na_count / n,
    accuracy = 1
  )
  )

dat_nas

# dat %>%
#   slice_sample(n = 10000) %>% 
#   vis_miss(warn_large_data = FALSE) +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank()
#   )


# ---------------------------------------------------
# univariate EDA and feature eng 
# ---------------------------------------------------

glimpse(dat)
#Rows: 244,097
#Columns: 54


# $ seqno_int      <dbl> 100001, 100002, 100004, 100005, 100007, 100008, 100009, 100010, 100011, 100012, 100013, …
# just an id
length(table(dat$seqno_int)) == nrow(dat)


# $ agecat         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
dat <- dat |>
  mutate(
    age = factor(
      agecat |>
       recode_values(
        1 ~ 11,
        2 ~ 13,
        3 ~ 15,
        default = NA
      ),
      levels = c(11,13,15))
  ) %>% select(-agecat)

ggplot(dat, aes(age)) + geom_bar()
#ggplot(dat, aes(age2)) + geom_bar()
#table(dat$age, dat$agecat, useNA = "always")
str(dat$age)

# $ sex            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
dat <- dat |>
  mutate(
    gender = factor(
      sex, # |>
      # recode_values(
      #   1 ~ "boy",
      #   2 ~ "girl",
        levels = c(1,2),
        labels = c("Boy", "Girl")
      )
    ) %>% select(-sex)

#table(dat$gender, dat$sex, useNA = "always")
ggplot(dat, aes(gender)) + geom_bar()


# $ IRRELFAS_LMH   <dbl> 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, NA, 2, 2, 2, NA, 1, NA, 2, NA, 2, 3, 3…
dat <- dat |>
  mutate(
    IRRELFAS = factor(
      IRRELFAS_LMH, # |>
      levels = c(1, 2, 3), 
      labels = c("Low", "Med", "High")
      #ordered = TRUE
  )) %>% select(-IRRELFAS_LMH)

#table(dat$IRRELFAS, dat$IRRELFAS_LMH, useNA = "always")
ggplot(dat, aes(IRRELFAS)) + geom_bar()


# $ IRFAS          <dbl> 6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4, NA, 5, 8, 9, NA, 1, NA, 3, NA, 5, 11, …
# discrete [0,13] and shows affluence; leave as num
table(dat$IRFAS, useNA = "always")
ggplot(dat, aes(IRFAS)) + geom_bar() 


# $ IOTF4          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# BMI group from 1(thin) to 4(obesse) 
table(dat$IOTF4, useNA = "always")

dat <- dat |>
  mutate(
    IOTF4_r = factor(
      IOTF4,
      levels = c(1, 2, 3, 4),
      labels = c("Thinness", "Normal", "Overweight", "Obesity")
    )
  ) %>% select(-IOTF4)

ggplot(dat, aes(IOTF4_r)) + geom_bar()+ coord_flip() + theme_minimal()


# $ MBMI           <dbl> 17.98167, 17.78325, 24.24392, 15.03105, 15.57093, 18.25632, 14.26873, 20.88889, 14.46759…
# Body mass index from [Range= 11.02-44.9] and 0 meaning outside overall range so make those NA.
#summary(dat$MBMI)
dat <- dat %>% 
  mutate(MBMI_r = ifelse(MBMI == 0, NA, MBMI)) %>%
  select(-MBMI)

summary(dat$MBMI_r)

ggplot(dat, aes(x = MBMI_r)) +  geom_histogram() + theme_minimal()


# $ bodyweight     <dbl> 41, 52, 59, 38, 45, 45, 30, 47, 30, 42, 34, 41, 49, 39, 75, 65, 64, NA, NA, NA, 31, 53, …
# in kilo and range from 20 to 150 so will use maybe. 
# summary(dat$bodyweight)
# ggplot(dat, aes(x = bodyweight)) +  geom_histogram() + theme_minimal()


# $ bodyheight     <dbl> 151, 171, 156, 159, 170, 157, 145, 150, 144, 158, 148, 160, 163, 165, 180, 167, 179, NA,…
# in cm and range [120,200] so will use. 
# summary(dat$bodyheight)
# ggplot(dat, aes(x = bodyheight)) +  geom_histogram() + theme_minimal()


# $ timeexe        <dbl> 3, 2, 2, 2, 6, 4, 3, 1, 3, 4, 1, 1, 1, 1, 1, 4, 2, NA, 3, NA, 4, 1, 6, 2, 2, 3, 4, 2, 2,…
# timeexe ask how often exercise in free time going from 1 (every day) to 7 never. 
# HBSC shows proportions of those that do 3+ per week (But that is 2022 data)
# in our case we do 2-3 times per week or more. And I keep timexe as is for shap test.
dat <- dat %>%
  mutate(timeexe_r = factor(case_when(
    between(timeexe, 1, 3) ~ 1,
    between(timeexe, 4, 7) ~ 0,
    TRUE ~ NA),
    levels = c(0, 1), 
    labels = c("No", "Yes"))
    ) %>% select(-timeexe)

#table(dat$timeexe_r, dat$timeexe, useNA = "ifany")
ggplot(dat, aes(timeexe_r)) + geom_bar()


# $ lifesat        <dbl> 7, 7, 10, NA, 6, 8, NA, 5, NA, NA, 9, 9, 10, 9, 10, 8, 8, NA, 9, NA, 10, 7, 8, 10, 10, 7…
# life satisfaction as ladder: 0(botton) to 10(best). I'll dichotomize and keep num for shap.

dat <- dat %>%
  mutate(
    lifesat_low = factor(case_when(
      between(lifesat, 0, 5) ~ 1,
      between(lifesat, 6, 10) ~ 0,
      TRUE ~ NA),
      levels = c(0, 1), 
      labels = c("No", "Yes"))
      )

table(dat$lifesat, dat$lifesat_low, useNA = "always")
ggplot(dat, aes(lifesat_low)) + geom_bar()


# $ headache       <dbl> 5, 5, 5, 5, 2, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 4, NA, NA, 2, 5, 5, 5, 5, 5, 5, 5, 5, 5,…
# $ stomachache    <dbl> 4, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, NA, 5, 5, NA, NA, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5…
# $ backache       <dbl> 5, 5, 5, 4, 1, 4, 5, NA, 5, 5, 5, 1, 5, 5, NA, 5, 5, NA, NA, 5, 5, 5, 5, 5, 5, 4, 1, 5, …
# $ dizzy          <dbl> 5, 5, 5, 4, 5, 5, 2, NA, 5, 5, 5, NA, 5, 5, NA, 5, 5, NA, NA, 4, 5, 5, 5, 5, 5, 5, 5, 5,…
# ignore for now the physical vars

# $ feellow        <dbl> 3, 3, 5, 5, 1, 1, 5, NA, 5, 5, 4, NA, 5, 2, NA, 2, 5, NA, NA, 2, 5, 4, 5, 5, 5, 4, 5, 5,…
# $ irritable      <dbl> 1, 4, 5, 5, 1, 2, 4, 5, 5, 4, 3, NA, 5, 5, NA, 1, 3, NA, NA, 3, 2, 4, 5, 5, 5, 4, 2, 5, …
# $ nervous        <dbl> 1, 3, 4, 5, 4, 2, 5, 5, 5, 4, 4, 5, 5, 5, NA, 2, 2, NA, NA, 3, 2, 4, 5, 5, 5, 5, 3, 5, 5…
# $ sleepdificulty <dbl> 5, 2, 4, 5, 2, 3, 5, 2, NA, 5, 5, NA, 5, 5, NA, 5, 4, NA, NA, 5, 5, 5, 5, 5, 5, 5, 1, 5,…
# Question In the last 6 months: how often have you had the following….? 
# Answers goes from 1 (About every day) to 5 (Rarely or never). 
vars_mental <- c("feellow", "irritable", "nervous", "sleepdificulty")
lapply(dat[vars_mental], table, useNA = "always")

# recode each to yn if frequent is 1 or 2 
dat <- dat %>%
  mutate(
    feellow_is = ifelse(feellow <= 2, 1, 0),
    irritable_is = ifelse(irritable <= 2, 1, 0),
    nervous_is = ifelse(nervous <= 2, 1, 0),
    sleepdificulty_is = ifelse(sleepdificulty <= 2, 1, 0)
  ) %>%
  select(-all_of(vars_mental))

# sum their frequency and issue if 2+
dat <- dat %>%
  mutate(mental_sum = feellow_is +
                      irritable_is +
                      nervous_is +
                      sleepdificulty_is
  ) %>%
  mutate(mental_issue = factor(
          ifelse(mental_sum >=2, 1, 0),
          levels = c(1, 0),
          labels = c("Yes", "No")
         )
  ) %>%
  select(-c(
    all_of(paste0(vars_mental, "_is")),
    mental_sum)
  )

#table(dat$mental_sum, dat$mental_issue, useNA = "always")
ggplot(dat, aes(mental_issue)) + geom_bar()


# $ emcsocmed1     <dbl> 2, 1, 2, 1, 1, 1, 1, 1, 1, NA, 1, 99, 1, 1, NA, 1, 1, 2, 1, 1, 1, NA, 1, 2, 2, 1, 2, 2, …
#...
# $ emcsocmed9     <dbl> 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 99, 1, 1, NA, 1, 1, 1, 1, 2, 1, NA, 1, 1, 1, 1, 1, 1, 1…
# "Problematic Social Media Use (PMSU)"
vars_pmsu <- paste0("emcsocmed", rep(1:9))
lapply(dat[vars_pmsu], table, useNA = "always")

# recode to 01
recode_emcsocmed <- function(old_col){
  new_col <- case_when(
    old_col == 1 ~ 0,
    old_col == 2 ~ 1,
    TRUE ~ NA #99?
  )
  return(new_col)
}

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

#sum and bin
vars_pmsu_r <- paste0(vars_pmsu, "_r")
dat <- dat %>%
  mutate(pmsu_s = rowSums(dat[,vars_pmsu_r])) %>%
  mutate(
    pmsu = factor(
      case_when(
        between(pmsu_s,6,9) ~ 3, 
        between(pmsu_s,2,5) ~ 2,
        between(pmsu_s,0,1) ~ 1,
        TRUE ~ NA),
      levels = c(1,2,3),
      labels = c("Low", "Medium", "High")
    )
  ) %>%
  select(-c(
    all_of(vars_pmsu),
    all_of(paste0(vars_pmsu, "_r")),
    pmsu_s
  ))

#table(dat$pmsu_s, dat$pmsu,useNA = "always")
ggplot(dat, aes(pmsu)) + geom_bar()


# $ beenbullied    <dbl> 1, 2, 1, 2, 1, 1, 1, 1, 4, 2, 1, 3, 1, 1, 1, 5, 1, 5, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1…
# $ cbeenbullied   <dbl> 3, 2, 2, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, NA, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
# There are 2 variables that measure if kid has been bullied. 
# a) beenbullied: Been bullied past months and 
# b) cbeenbullied: Been cyber bullied. 
# Answers range from 1 (Haven't) to 5 (Several times a week).  

# I will derive 2 to keep: cbeenbullied as y/n and bulliedsum as sum of both.
dat <- dat %>%
  mutate(cbullied = factor( 
           case_when(cbeenbullied == 1 ~ 0,
                     between(cbeenbullied, 2, 5) ~ 1,
                     TRUE ~ NA),
           levels = c(0,1),
           labels = c("No", "Yes")
           )#,
         #bullied_s = beenbullied + cbeenbullied
        ) %>% select(-cbeenbullied)

#table(dat$bullied_s, useNA = "always")
#table(dat$cbeenbullied, dat$cbullied, useNA = "always")
ggplot(dat, aes(cbullied)) + geom_bar()


# $ emconlfreq1    <dbl> 4, 4, 1, 4, 3, 4, 2, 6, 6, 3, NA, 1, 5, 6, 4, 3, 5, 1, 5, 6, 1, 4, 6, 3, 3, 4, 6, 3, 6, …
# ...
# $ emconlfreq4    <dbl> 5, 5, 6, 3, 6, 1, 1, 1, 2, 4, NA, 1, 5, 6, 2, 5, 6, 2, 2, 6, 6, 3, 6, 3, 3, 5, 6, 3, 5, …
# continuos online communications range from 1(NA/don't know) to 6(almost all time)
# 1 close friends, 2 larger friend group, 3 online frriends, 4 other
vars_emconlfreq <- paste0("emconlfreq", rep(1:4))
lapply(dat[vars_emconlfreq], table, useNA = "always")

# recode 2 versions - yn like HBSC and sum.
dat <- dat %>%
  mutate(
    # 1 if any of the 4 is 6
    emconlfreq = factor(case_when(
      (emconlfreq1 == 6 | emconlfreq2 == 6 | emconlfreq3 == 6 | emconlfreq4 == 6) ~ 1,
      (!is.na(emconlfreq1) & !is.na(emconlfreq2) & !is.na(emconlfreq3) & !is.na(emconlfreq4)) ~ 0,
      TRUE ~ NA # if any reply is NA, drop observation
    ),
    levels = c(0,1),
    labels = c("No", "Yes")
    ),
    # sums the 4
    #emconlfreq_s = emconlfreq1 + emconlfreq2 + emconlfreq3 + emconlfreq4
  ) %>%
  select(- all_of(vars_emconlfreq))

ggplot(dat, aes(emconlfreq)) + geom_bar() + theme_minimal()

#summary(dat$emconlfreq_s)
#ggplot(dat, aes(x=emconlfreq_s)) +  geom_bar() + theme_minimal()


# $ emconlpref1    <dbl> 2, 5, 5, 1, 1, 1, 1, 1, 2, 1, NA, 99, 1, 1, 1, 1, 3, 3, 3, 3, 1, 3, 1, 2, 2, 1, 5, 2, 1,…
# $ emconlpref2    <dbl> 1, 5, 5, 1, 1, 1, 2, 1, 2, 1, NA, 99, 1, 1, 4, 1, 2, NA, 3, 2, 2, 4, 1, 2, 2, 1, 1, 2, 1…
# $ emconlpref3    <dbl> 1, 5, 1, 1, 3, 1, NA, 1, 2, 1, NA, 99, 1, 1, 3, 1, 3, NA, 3, 3, 5, 4, 1, 2, 2, 2, 1, 2, …
# preference to communicate online on important stuff online vs real-life.
# 1 secrets, 2 feelings, 3 concerns
# The answers range from 1 (Strong disagree) to 5 (Strong agree).
vars_emconlpref <- paste0("emconlpref", rep(1:3))
lapply(dat[vars_emconlpref], table, useNA = "always")

# get rid of 99s
dat <- dat %>%
  mutate(across(all_of(vars_emconlpref), ~ na_if(.x, 99)))

#Haven't found a paper that uses them so I will replicated method of friends support below. 
#Summing them results in [3,15] and then grouped as H/M/L as L 3-6, M 7-10, H 11-15. Resulting in:
dat <- dat %>%
  mutate(# sum should give 3-15
        emconlpref_s = emconlpref1 + emconlpref2 + emconlpref3,
        #onlpref_sum = ifelse(onlpref_sum > 15, NA, onlpref_sum), #for now, 99 issue
        # create hml
        emconlpref = factor(case_when(
          between(emconlpref_s, 3, 6) ~ 1,
          between(emconlpref_s, 7, 10) ~ 2,
          between(emconlpref_s, 11, 15) ~ 3,
          TRUE ~ NA
        ),
        levels = c(1,2,3),
        labels = c("Low", "Med", "High")
        )
  ) %>%
  select(-c(
    all_of(vars_emconlpref),
    emconlpref_s
    ))
    
#table(dat$emconlpref_s, dat$emconlpref, useNA = "always")
#ggplot(dat, aes(x=emconlpref_s)) + geom_bar() + coord_flip() + theme_minimal()
ggplot(dat, aes(x=emconlpref)) + geom_bar() + coord_flip() + theme_minimal()


# $ famhelp        <dbl> 7, 7, 7, 7, 2, 7, 7, NA, 7, 7, NA, NA, 7, 7, 7, 7, 7, 1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7…
# $ famsup         <dbl> 6, 7, 7, 7, 1, 7, 7, NA, 7, 7, NA, NA, 7, 7, 7, 7, 7, 3, 7, 7, 1, 7, 7, 7, 7, 6, 7, 7, 7…
# $ famtalk        <dbl> 7, 7, 1, 7, 1, 7, 7, NA, 7, 7, NA, NA, 7, 7, 7, 7, 5, 7, 7, 7, 7, 7, 7, 7, 7, 5, 1, 7, 7…
# $ famdec         <dbl> 5, 7, 7, 7, 1, 7, 7, NA, 7, 6, NA, NA, 7, 7, 1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7…
# Family Support" 
# 4 questions going from 1 Very strongly disagree to 7 Very strongly agree.
vars_famsup <- c("famhelp", "famsup", "famtalk", "famdec")
lapply(dat[vars_famsup], table, useNA = "always")

# Summing them results in [4,28] and then grouped as H/M/L as L 4-11, M 12-19, H 20-28.
dat <- dat %>%
  mutate(famsup_s = rowSums(dat[,vars_famsup])
  ) %>%
  mutate(fam_sup = factor(case_when(
    famsup_s >= 4 & famsup_s <= 11 ~ 1,
    famsup_s >= 12 & famsup_s <= 19 ~ 2,
    famsup_s >= 20 & famsup_s <= 28 ~ 3,
    TRUE ~ NA),
    levels = c(1,2,3),
    labels = c("Low", "Medium", "High")    
    )
  ) %>%
  select(-c(all_of(vars_famsup),
           famsup_s
           )        
        )

#table(dat$famsup_s, dat$famsupp, useNA = "always")
#ggplot(dat, aes(x=factor(famsup_s))) + geom_bar() + theme_minimal()
ggplot(dat, aes(x=fam_sup)) + geom_bar() + theme_minimal()


# $ friendhelp     <dbl> 7, 5, 7, 7, 7, 3, 7, 7, 7, 7, NA, 4, 7, 7, 7, 1, 6, NA, 7, 2, 2, 7, 2, 7, 7, 7, NA, 7, 7…
# $ friendcounton  <dbl> 6, 6, 1, 7, 3, 7, 7, 1, 7, 6, NA, 2, 7, 7, 2, 1, 6, NA, 7, 3, 7, 7, 2, 3, 3, 6, NA, 3, 7…
# $ friendshare    <dbl> 7, 5, 7, 7, 5, 7, 7, 7, 7, 7, NA, 7, 7, 7, 7, 1, 7, NA, 7, 2, 7, 7, 3, 7, 7, 7, NA, 7, 7…
# $ friendtalk     <dbl> 5, 7, 1, 7, 6, 7, 7, 7, 7, 7, NA, 2, 7, 7, 7, 1, 7, NA, 7, 2, 7, 7, 7, 2, 2, 7, NA, 2, 7…
# "Friends Support"   
# 4 questions going from 1 Very strongly disagree to 7 Very strongly agree.
vars_frisup <- vars_groups[["friends_support"]]
lapply(dat[vars_frisup], table, useNA = "always")

#Summing them results in [4,28] and then grouped as H/M/L as L 4-11, M 12-19, H 20-28. Resulting in:
dat <- dat %>%
  mutate(frisup_s = rowSums(dat[,vars_frisup])
  ) %>%
  mutate(friends_sup = factor(case_when(
    frisup_s >= 4 & frisup_s <= 11 ~ 1,
    frisup_s >= 12 & frisup_s <= 19 ~ 2,
    frisup_s >= 20 & frisup_s <= 28 ~ 3,
    TRUE ~ NA),
    levels = c(1,2,3),
    labels = c("Low", "Medium", "High") 
    )
  ) %>%
  select(-c(
    all_of(vars_frisup),
    frisup_s
  )
  )

#table(dat$frisup_s, dat$frisupp, useNA = "always")
#ggplot(dat, aes(x=factor(frisup_s))) + geom_bar() + theme_minimal()
ggplot(dat, aes(x=friends_sup)) + geom_bar() + theme_minimal()


# $ studtogether   <dbl> 2, 2, 1, 1, 1, 2, 2, 1, 5, 2, 1, 3, 1, 1, 2, 3, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1, 2…
# $ studhelpful    <dbl> 1, 2, 1, 1, 3, 2, 1, 1, 2, 3, 1, 3, 2, 1, 4, 5, 2, 1, 1, 2, 1, 1, 2, 3, 3, 1, 1, 3, 1, 4…
# $ studaccept     <dbl> 1, 1, 1, 1, 3, 1, 1, 1, 2, 2, 1, 5, 1, 1, 2, 5, 1, 1, 1, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1…
# $ teacheraccept  <dbl> 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 5, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1…
# $ teachercare    <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 1, 2, 2, 1, 1, 2, 4, 5, 2, 1, 1, 2, 2, 3, 2, 1, 1, 1, 1, 1, 1, 2…
# $ teachertrust   <dbl> 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 4, 5, 1, 1, 2, 2, 2, 3, 4, 2, 2, 1, 1, 2, 1, 1…
vars_school <- vars_groups[["school_support"]]
lapply(dat[vars_school], table, useNA = "always")

#Scales are reversed so 1 is the negative and 5 is the positive.
# recodes from 1-5 to 0-4 
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
  mutate(teacheraccept_r = recode_school_vars(teacheraccept),
         teachercare_r = recode_school_vars(teachercare),
         teachertrust_r = recode_school_vars(teachertrust),
         studtogether_r = recode_school_vars(studtogether),
         studhelpful_r = recode_school_vars(studhelpful),
         studaccept_r = recode_school_vars(studaccept)
  ) 

#For stud/teach separate a binary following HBSC and a sum for shap test.
dat <- dat %>%
  # derive avg by teacher/student
  mutate(teacher_support_avg = (teacheraccept_r 
                                + teachercare_r
                                + teachertrust_r) / 3,
         student_support_avg = (studtogether_r
                                + studhelpful_r
                                + studaccept_r) / 3
  ) %>%
  # derive support yn based on hbsc
  mutate(
    teacher_sup = factor(case_when(
      teacher_support_avg >= 4 ~ 1,
      teacher_support_avg < 4 ~ 0,
      TRUE ~ NA),
      levels = c(0,1),
      labels = c("No", "Yes")
      ),
    student_sup = factor(case_when(
      student_support_avg >= 4 ~ 1,
      student_support_avg < 4 ~ 0,
      TRUE ~ NA),
      levels = c(0,1),
      labels = c("No", "Yes")
    )
  ) %>%
  # derive support sum for shap
  # mutate(teachersup_s = teacheraccept_r 
  #        + teachercare_r
  #        + teachertrust_r,
  #        studsup_s = studtogether_r
  #        + studhelpful_r
  #        + studaccept_r
  # ) %>%
  select(-c(
    all_of(vars_school),
    all_of(paste0(vars_school, "_r")),
    teacher_support_avg,
    student_support_avg
           )
  )

#table(dat$teachersup_s, dat$teachersup, useNA = "always")
#ggplot(dat, aes(x=factor(teachersup_s))) + geom_bar() + theme_minimal()
ggplot(dat, aes(x=teacher_sup)) + geom_bar() + theme_minimal()

#table(dat$studsup_s, dat$studsup,  useNA = "always")
#ggplot(dat, aes(x=factor(studsup_s))) + geom_bar() + theme_minimal()
ggplot(dat, aes(x=student_sup)) + geom_bar() + theme_minimal()


# $ talkfather     <dbl> 1, 2, 1, 1, 4, 3, 1, NA, NA, 2, NA, NA, 1, NA, 1, 1, 2, NA, 2, 3, 2, 2, 1, 1, 1, 2, 4, 1…
# $ talkmother     <dbl> 2, 1, 1, 1, 2, 1, 1, NA, NA, 1, NA, NA, 1, NA, NA, 1, 2, NA, NA, 3, 2, 2,
# Answers from 1 (Very easy) to 5 (Don't have or see)
lapply(dat[vars_groups[["parents"]]], table, useNA = "always")

# So I will dichotomize like HBSC and keep (sum of both as well for SHAP.
# improve on above by just keeping talk to a parent instead of talkf/talkm
dat <- dat %>%
  mutate(
    talkf = case_when(talkfather %in% c(1,2) ~ 1,
              talkfather %in% c(3,4,5) ~ 0),
    talkm = case_when(talkmother %in% c(1,2) ~ 1,
              talkmother %in% c(3,4,5) ~ 0),
    talk_parent = as.integer(talkf | talkm)
    #talkp = talkf + talkm
  #,
  #talkscore = talkfather + talkmother
  ) %>%
  # convert them to factors
  mutate(
    talk_parent = factor(talk_parent, levels = c(0,1), labels = c("No", "Yes")),
    #talkf = factor(talkf, levels = c(0,1), labels = c("No", "Yes")),
    #talkm = factor(talkm, levels = c(0,1), labels = c("No", "Yes"))
    #talkp = factor(talkp, levels = c("0","1","2"), ordered = TRUE)
  ) %>%
  select(-c(
    talkf,
    talkm,
    #talkp,
    talkmother,
    talkfather
  ))

#table(dat$talkp)

#table(dat$talkfather, dat$talkf, useNA = "always")
#ggplot(dat, aes(x=talkf)) + geom_bar() + theme_minimal()

#table(dat$talkmother, dat$talkm, useNA = "always")
#ggplot(dat, aes(x=talkm)) + geom_bar() + theme_minimal()

ggplot(dat, aes(x=talk_parent)) + geom_bar() + theme_minimal()

#summary(dat$talkscore)
#ggplot(dat, aes(x=factor(talkscore))) + geom_bar() + theme_minimal()


# $ countryno      <dbl> 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000, 8000…
# from code get country name
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

dat$country1 <- recode_map[as.character(dat$countryno)]

# countries <- 
#   hbsc2018_filt %>%
#   group_by(countryno, country) %>%
#   tally()
#sum(countries$n) == nrow(hbsc2018_filt) #has to be TRUE

# from name get continents info
dat <- dat %>%
  mutate(
    continent = countrycode(
      sourcevar = country1, 
      origin = "country.name", 
      destination = "continent",
      custom_match = c(
        "England"           = "Europe",
        "Scotland"          = "Europe",
        "Wales"             = "Europe"
      )
    ),
    sub_region = countrycode(
      sourcevar = country1, 
      origin = "country.name", 
      destination = "un.regionsub.name", # Changes destination to sub-regions
      custom_match = c(
        "England"  = "Northern Europe",
        "Scotland" = "Northern Europe",
        "Wales"    = "Northern Europe"
      )
    )
  ) 

# sniff test looks good
dat %>%
  group_by(continent, sub_region, country1, countryno) %>%
  tally() %>%
  print(n = Inf)




# ---------------------------------------------------------
# get rids of NAs and keep countries of interest based on Y
# and final touches before mdl
# ---------------------------------------------------------

colSums(is.na(dat)) 

## WW complete
hbsc_ww <- dat %>%
  drop_na()

table(hbsc_ww$mental_issue) / nrow(hbsc_ww) #28Y/72N

mental_issue_rates <- 
  hbsc_ww %>%
  group_by(continent, sub_region, country1, mental_issue) %>%
  tally() %>%
  pivot_wider(names_from = mental_issue,
              names_prefix = "is",
              values_from = n,
              values_fill = 0) %>%
  mutate(n = isNo + isYes,
         isYes_pct = isYes/n) 

mental_issue_rates %>% print(n = Inf)

# # won't do
# lifesat_low_rates <- 
# hbsc_ww %>%
#   #filter(continent == "continent")
#   group_by(continent, sub_region, country1, lifesat_low) %>%
#   tally() %>%
#   pivot_wider(names_from = lifesat_low,
#               names_prefix = "is",
#               values_from = n,
#               values_fill = 0) %>%
#   mutate(n = isNo + isYes,
#          isYes_pct = isYes/n)

# by area, lowest mental_health, sample of bad countries in the west
countries_to_keep <- c("Canada", 
                       "Turkey",
                       "England", "Ireland", "Scotland", "Wales",
                       "Italy",
                       "France"
)
hbsc_cmp <- hbsc_ww %>%
  filter(country1 %in% countries_to_keep) %>%
  mutate(country = as.factor(case_when(
  country1 %in% c("England", "Ireland", "Scotland", "Wales") ~ "UnitedKingdom",
  TRUE ~ country1
  ))) %>%
  select(-c(countryno))
  # maybe later remove contient, etc

table(hbsc_cmp$mental_issue) / nrow(hbsc_cmp) #34Y/66N

hbsc_cmp %>% group_by(country, mental_issue) %>% tally() %>%
  mutate(pct = n / sum(n))

# get all columns printed sorted by name
hbsc_cmp |> relocate(sort(names(hbsc_cmp))) |> glimpse()


# since these 2 are same info I will prio MBMI but leave for now
ggplot(hbsc_cmp, aes(x = IOTF4_r, y = MBMI_r, fill = IOTF4_r)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  #  geom_jitter(width = 0.2, alpha = 0.2, color = "darkgrey") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") 

# mejor aun, so quitar IOTF4_r
ggplot(hbsc_cmp, aes(x = MBMI_r, y = IOTF4_r, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.8, color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = -1) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), # Cleans up the ridge background
    axis.title.y = element_blank()
  )

# y mejor aun
ggplot(hbsc_cmp, aes(x = IRFAS, y = IRRELFAS, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.8, color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = -1) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), # Cleans up the ridge background
    axis.title.y = element_blank()
  )

# # vars any of modeling datasets will always have
# vars_all_mdls <- c("seqno_int",
#                    "mental_issue",
#                    "age",
#                    "gender",
#                    #"bodyheight",
#                    #"bodyweight",
#                    "country_",
#                    "timeexe_r"
# )
# 
# # vars for v1 mdl
# vars_mdl1 <- c(
#   "emconlfreq",
#   "emconlpref",
#   "famsupp",
#   "cbullied",
#   "frisupp",
#   "IOTF4_r",  "MBMI_r",
#   "IRRELFAS",
#   "pmsu",
#   "studsup",
#   "teachersup",
#   #"talkf",
#   #"talkm"
#   "talkp"
# )
# 
# 
# # vars for v2 mdls
# vars_mdl2 <- c(
#   "emconlfreq_s",
#   "emconlpref_s",
#   "famsup_s",
#   "frisup_s",
#   "IRFAS",
#   "pmsu_s",
#   "studsup_s",
#   "teachersup_s",
#   "talkscore",
#   "bullied_s",
#   "MBMI_r"
# )
# 
# # make v1 of dataset that we keep if shap makes sense
# # and it's consistent with papers I have seen
# d1 <- d %>%
#   select(all_of(vars_all_mdls),
#          all_of(vars_mdl1)
#          #all_of(vars_mdl2)
#          )
# 
# glimpse(d1)
# # only dbl and fct
# 
# # d2 <- d %>%
# #   select(all_of(vars_all_mdls),
# #          all_of(vars_mdl2)
# #   )

hbsc_cmp <- hbsc_cmp %>% 
  select(order(colnames(.))) %>%
  select(
    where(is.factor),
    where(is.numeric),
    everything()
  )

glimpse(hbsc_cmp)

# 1. Set the fixed target outcome variable
target_var <- "mental_issue"

# --------------------------------------------------------------------------
# check final features distributions and potential of FACTOR predictors
# --------------------------------------------------------------------------

# 2. Automatically detect all OTHER factor variables in d1
factor_vars <- hbsc_cmp %>% 
  select(where(is.factor)) %>% 
  names() %>% 
  setdiff(target_var)
factor_vars


# 4. Loop through each discovered factor variable
results_list <- list()
for (v in factor_vars) {
  
#  cat("\n==================================================\n")
  cat("\n\n\n *** ANALYSIS FOR PREDICTOR VARIABLE:", v, "\n")
#  cat("==================================================\n")
  
  # --- Step 1: Single Predictor Frequency ---
  cat("Xtab of:", v, "\n")
  hbsc_cmp %>% 
    tabyl(!!sym(v)) %>% 
    adorn_pct_formatting(digits = 0) %>% 
    print()
  
  # --- Step 2: Cross-Tabulation with Target ---
  cat("\nXtab of:", target_var, "x", v, "\n")
  
  xtab <- hbsc_cmp %>% 
    tabyl(!!sym(v), !!sym(target_var)) %>% 
    adorn_totals("col")
  
  if ("Yes" %in% names(xtab) && "Total" %in% names(xtab)) {
    xtab <- xtab %>% 
      mutate(Yes_pct = percent(Yes / Total, accuracy = 1))
  }
  print(xtab)
  
  # --- Step 3: Chi-Square Test & Components ---
  cat("\nChi-Square Test:\n")
  
  tryCatch({
    chisq <- chisq.test(hbsc_cmp[[v]], hbsc_cmp[[target_var]])
    
    print(chisq)
    cat("\nStatistic (X-squared):", chisq$statistic, "\n")
    cat("Formatted P-value:    ", format.pval(chisq$p.value, digits = 5), "\n")
    
    # NEW: Collect the row metrics into our tracking list
    results_list[[v]] <- tibble(
      variable     = v,
      x_squared    = round(chisq$statistic, 3),
      p_value_raw  = chisq$p.value,
      p_value_fmt  = format.pval(chisq$p.value, digits = 5)
    )
    
  }, error = function(e) {
    cat("Could not run Chi-Square test for", v, ":", e$message, "\n")
    
    # Log the failure case so the row isn't missing entirely
    results_list[[v]] <- tibble(
      variable     = v,
      x_squared    = NA_real_,
      p_value_raw  = NA_real_,
      p_value_fmt  = "Error/Failed"
    )
  })
  
  cat("\n")
}

# Combines all collected individual rows into a single table
chisq_results_df <- bind_rows(results_list)
print(chisq_results_df)


# --------------------------------------------------------------------------
# check final features distributions and potential of NUMBER predictors
# --------------------------------------------------------------------------

# 1. Identify all numeric variables (excluding the target variable)
numeric_vars <- hbsc_cmp %>% 
  select(where(is.numeric)) %>% 
  names() %>%
  setdiff("seqno_int")
numeric_vars

# 3. Combined Loop
ks_results_list <- list()
for (var in numeric_vars) {
  
  #--- CONSOLE HEADERS & STATS ---
  #cat("\n==================================================\n")
  cat("\n\n\n #### PROCESSING VARIABLE:", var, "\n")
  #cat("==================================================\n")
  
  cat("\n--- Summary Statistics by Group ---\n")
  print(tapply(hbsc_cmp[[var]], hbsc_cmp[[target_var]], summary))
  
  #--- STATISTICAL TESTING ---
  formula_form <- as.formula(paste(var, "~", target_var))
  ks_out <- ks.test(formula_form, data = hbsc_cmp)
  
  # Calculate medians dynamically for the output table
  medians <- hbsc_cmp %>%
    group_by(.data[[target_var]]) %>%
    summarize(med = median(.data[[var]], na.rm = TRUE), .groups = 'drop')
  
  median_no  <- medians %>% filter(.data[[target_var]] == "No")  %>% pull(med)
  median_yes <- medians %>% filter(.data[[target_var]] == "Yes") %>% pull(med)
  
  # Save metrics to our tracking list
  ks_results_list[[var]] <- data.frame(
    Variable = var,
    D_Statistic = round(ks_out$statistic, 5),
    #P_Value = ks_out$p.value,
    P_Value_Formatted = format.pval(ks_out$p.value, digits = 4),
    Median_No = ifelse(length(median_no) > 0, median_no, NA),
    Median_Yes = ifelse(length(median_yes) > 0, median_yes, NA),
    stringsAsFactors = FALSE
  )
  
  #--- PLOTTING ---
  # Boxplot
  p1 <- ggplot(hbsc_cmp, aes(x = .data[[target_var]], y = .data[[var]], fill = .data[[target_var]])) +
    geom_boxplot(alpha = 0.7, width = 0.5) +
    scale_fill_manual(values = c("No" = "#5DADE2", "Yes" = "#E74C3C")) +
    labs(title = paste("Boxplot of", var, "by", target_var), y = var, x = target_var) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p1)
  
  # Histogram
  p2 <- ggplot(hbsc_cmp, aes(x = .data[[var]], fill = .data[[target_var]])) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    scale_fill_manual(values = c("No" = "#5DADE2", "Yes" = "#E74C3C")) +
    labs(title = paste("Histogram of", var, "by", target_var), x = var) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p2)
}

# 4. Bind the tracked rows into your final dataframe after the loop ends
ks_summary_table <- bind_rows(ks_results_list)
print(ks_summary_table)


# --------------------------------------------------------------------------
# fit a good tuned tree and see in SHAP
# --------------------------------------------------------------------------

# https://parsnip.tidymodels.org/reference/decision_tree.html

#baseline is
table(hbsc_cmp$mental_issue)/nrow(hbsc_cmp) #34/66

# WARNING - pick one of this:
#d1 <- d1 %>% filter(country_ == "Turkey") %>% select(-country_)
#d1 <- d1 %>% select(-country_)

# split data
set.seed(67)
hbsc_split <- initial_split(hbsc_cmp,#, |> select(-seqno_int), 
                            strata = mental_issue)
hbsc_train <- training(hbsc_split)
hbsc_test  <- testing(hbsc_split)

# create a model specification that identifies which hyperparameters we plan to tune
# tune is a placeholder that will get values
tree_tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(), #max depth of tree
    min_n = tune() #min # of observations in node
  ) |> 
  set_engine("rpart") |> 
  set_mode("classification")

tree_tune_spec

# create grid of values to try - 25 candiates
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          min_n(),
                          levels = 5
                          ) #5^3 models

tree_grid

# create folds for cv from train dat
set.seed(67)
tree_folds <- vfold_cv(hbsc_train, v = 5, repeats = 1, strata = mental_issue)

tree_recipe <- recipe(mental_issue ~ ., 
                      data = hbsc_train) %>%
  #step_mutate(talkp = as.integer(talkf | talkm)) %>%
  step_rm(country, country, IOTF4_r, lifesat_low, IRFAS, lifesat,
          seqno_int, continent, country1, sub_region) #should probably just do before

#Tune a workflow() that bundles together a model specification and a recipe or model preprocessor.
set.seed(67)
tree_wf <- workflow() |>
  add_model(tree_tune_spec) |>
  add_recipe(tree_recipe)
  #add_formula(mental_issue ~ .)

tree_wf

#tune_grid() to fit models at all the different values we chose for each tuned hyperparameter
(Start <- Sys.time())
tree_res <- 
  tree_wf |> 
  tune_grid(
    resamples = tree_folds,
    grid = tree_grid
  )
(Sys.time() - Start) #3mins

tree_res

# see performance metrics of fitted trees
tree_res |> collect_metrics()

tree_res |> collect_metrics() |>
  filter(.metric == "roc_auc") %>%
  select(mean) %>%
  summary()

#tree_metrics #tible with 5^3 models x 3 metrics of rows

# fix if i want to use/show params combinations
# tree_res |>
#   collect_metrics() |>
#   mutate(tree_depth = factor(tree_depth)) |>
#   ggplot(aes(cost_complexity, mean, color = tree_depth)) +
#   geom_line(linewidth = 1.5, alpha = 0.6) +
#   geom_point(size = 2) +
#   facet_wrap(~ .metric, scales = "free", nrow = 2) +
#   scale_x_log10(labels = scales::label_number()) +
#   scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
#   theme_minimal()

# get top 5 models based on a metric
tree_res |> show_best(metric = "accuracy")
tree_res |> show_best(metric = "roc_auc")

# just get one
best_tree <- tree_res |> select_best(metric = "roc_auc")
best_tree

# finalize wf with best values (tuning is done)
final_wf <- tree_wf |> finalize_workflow(best_tree)
final_wf

# The last fit
# Finally, let’s fit final model to the training data and use our test data to 
# estimate the model performance we expect to see with new data. so metrics here are on test.
# The final_fit object contains a finalized, fitted workflow that you can use for 
# predicting on new data or further understanding the results. 
final_fit <- final_wf |> last_fit(hbsc_split) 
final_fit

final_fit |> collect_metrics()

# see roc
final_fit |>
  collect_predictions() |>
  roc_curve(mental_issue, .pred_Yes) |>
  autoplot() +
  theme_minimal()

# get the final tree
final_tree <- extract_workflow(final_fit)
final_tree

# 1. Define a helper function to calculate metrics for a specific dataset split
get_split_metrics <- list(
  train = hbsc_train,
  test  = hbsc_test # Make sure this matches your test set variable name
) %>% 
  purrr::map_df(function(df) {
    # Generate class and probability predictions
    predict(final_tree, new_data = df, type = "class") %>% 
      bind_cols(predict(final_tree, new_data = df, type = "prob")) %>% 
      bind_cols(df) %>% 
      # Calculate the metric set
      metric_set(accuracy, roc_auc, sens, spec)(
        truth       = mental_issue, 
        estimate    = .pred_class, 
        .pred_Yes, 
        event_level = "first" # Adjust to "second" if "Yes" is your 2nd factor level
      )
  }, .id = "dataset") # Stitches them together and tracks which is train vs test

# 2. Pivot the data into a clean, side-by-side print table
comparison_table <- get_split_metrics %>%
  select(dataset, .metric, .estimate) %>%
  tidyr::pivot_wider(names_from = dataset, values_from = .estimate) %>%
  rename(Metric = .metric, `Train Set` = train, `Test Set` = test) %>%
  mutate(Metric = toupper(Metric)) # Cleans up metric names for printing

print(comparison_table)

# shap for tree
# sample set of from test to explain, no Y
set.seed(67)
X_explain <- hbsc_test %>% 
  slice_sample(n = 100) %>% #increase?
  select(-mental_issue)

# Sample background rows (typically 100-200 rows from your training set is plenty)
bg_X <- hbsc_train %>% 
  dplyr::select(-mental_issue) %>% 
  slice_sample(n = 100) #100-500 sweetspot?

# Calculate SHAP values for all classes automatically
(start <- Sys.time())
shap_output <- kernelshap(
  final_tree, 
  X = X_explain, 
  bg_X = bg_X, 
  type = "prob"
)
Sys.time() - start #7 mins

# Convert to a shapviz object and plot
tree_sv <- shapviz(shap_output)
names(tree_sv)

sv_importance(tree_sv$.pred_Yes, kind = "bar") + theme_minimal() #var imp plot
sv_importance(tree_sv$.pred_Yes, kind = "beeswarm") + theme_minimal() #bee

# pred Y and N examples
sv_waterfall(tree_sv$.pred_Yes, row_id = 2) + theme_minimal()
sv_waterfall(tree_sv$.pred_Yes, row_id = 5) + theme_minimal()

#sv_force(sv$.pred_Yes, row_id = 7) #Yes, individual
#sv_force(sv$.pred_Yes, row_id = 9) #No, individual

# dependence plots for top 10 predictors
# single-var first and then with top interaction
top_predictors <- final_tree |> 
  extract_fit_parsnip() |> 
  vi() %>%
  tibble()

top_predictors_ <- 
top_predictors %>%
  arrange(Importance) %>% #sort like this so last predictor is top predictor
  select(Variable) %>%
  unlist(use.names = FALSE)

# dependence plots
for (pred in top_predictors_){
  #print(pred)
  print(sv_dependence(tree_sv$.pred_Yes, v = pred, color_var = NULL) + theme_minimal())
  print(sv_dependence(tree_sv$.pred_Yes, v = pred) + theme_minimal())
}

save.image(file = "hbsc_wkspace_20260914.RData")
# load here and continue

# -------------------------------------------------------------------------
# fit a tuned xgboost
# -------------------------------------------------------------------------

# https://juliasilge.com/blog/xgboost-tune-volleyball/

# optimizar todo este relajo y meter el pos wuey

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),                     
  sample_size = tune(), 
  mtry = tune(),         
  learn_rate = tune()                          
) %>%
  set_engine("xgboost", scale_pos_weight = 1.94) %>%
  set_mode("classification")

# pasar el sacle_pos asi segun ggl...
# library(tidymodels)
# xgboost_spec <- boost_tree(
#   trees = 100,
#   tree_depth = 6
# ) %>%
#   set_engine("xgboost", scale_pos_weight = 9) %>% # Pass ratio directly here
#   set_mode("classification")

xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), d1_train),
  learn_rate(),
  size = 30
)

xgb_grid

# xgboost needs numeric predictors — step_dummy() one-hot-encodes any factors
rec <- recipe(mental_issue ~ ., data = d1_train) |>
  step_dummy(all_nominal_predictors())

xgb_wf <- workflow() %>%
  add_recipe(rec) %>%
  #add_formula(mental_issue ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(67)
vb_folds <- vfold_cv(d1_train, 
                     v = 2, #change later to 10
                     repeats = 1,
                     strata = mental_issue)
vb_folds

doParallel::registerDoParallel()
(start <- Sys.time())
set.seed(67)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)
Sys.time() - start

xgb_res

collect_metrics(xgb_res)

show_best(xgb_res, metric = "accuracy")
show_best(xgb_res, metric = "roc_auc")


best_auc <- select_best(xgb_res, metric = "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb


library(vip)

final_xgb %>%
  fit(data = d1_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_xgb <- last_fit(final_xgb, d1_split)

collect_metrics(final_xgb)

final_xgb %>%
  collect_predictions() %>%
  conf_mat(Churn, .pred_class)

#adapt to me
final_res %>%
  collect_predictions() %>%
  roc_curve(mental_issue, .pred_Yes) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

# get the final tree
final_trees <- extract_workflow(final_res)
final_trees


# Calculate SHAP values for all classes automatically
#esto no es asi por las dummies
(start <- Sys.time())
shap_output <- kernelshap(
  final_trees, 
  X = X_explain, 
  bg_X = bg_X, 
  type = "prob"
)
Sys.time() - start #2 mins

# Convert to a shapviz object and plot
sv <- shapviz(shap_output)
names(sv)

sv_importance(sv$.pred_Yes, kind = "bar") + theme_minimal() #var imp plot
sv_importance(sv$.pred_Yes, kind = "beeswarm") + theme_minimal() #bee

# Yes pred
sv_waterfall(sv$.pred_Yes, row_id = 20) + theme_minimal()
sv_waterfall(sv$.pred_Yes, row_id = 7) + theme_minimal() 

# No pred
sv_waterfall(sv$.pred_Yes, row_id = 12) + theme_minimal() 
sv_waterfall(sv$.pred_Yes, row_id = 4) + theme_minimal() 

#sv_force(sv$.pred_Yes, row_id = 7) #Yes, individual
#sv_force(sv$.pred_Yes, row_id = 9) #No, individual

# dependence plots for top 10 predictors
# single-var first and then with top interaction
top_predictors <- final_trees |> 
  extract_fit_parsnip() |> 
  vi() %>%
  tibble()

top_predictors_ <- 
  top_predictors %>%
  arrange(Importance) %>% #sort like this so last predictor is top predictor
  select(Variable) %>%
  unlist(use.names = FALSE)

for (pred in top_predictors_){
  print(pred)
  
  # dependence plots
  print(sv_dependence(sv$.pred_Yes, v = pred, color_var = NULL) + theme_minimal())
  print(sv_dependence(sv$.pred_Yes, v = pred) + theme_minimal())
  
}


