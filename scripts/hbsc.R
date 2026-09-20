# HBSC 
# Updated: 2026-09-17


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
library(tinytable)

prj_fldr <- "C:/MisLocalFiles/Github/HBSC/"
#setwd(paste0(prj_fldr, "scripts/"))
setwd(prj_fldr)

options(scipen = 999)


# ---------------------------------------------------
# workspace stuff
# ---------------------------------------------------

img_file <- paste0(prj_fldr,
                  "/Rimages/hbsc_wkspace_20260917.RData")

load(file = img_file)

# save.image(file = img_file)


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

# corr plot of pmsu vars for paper

r_pmsu <- dat %>%
  select(emcsocmed1_r:emcsocmed9_r) %>%
  cor(method = "pearson", use = "pairwise.complete.obs")

corrplot(
  cor_matrix,
  method = "color",        # Fills the tiles completely with color
  type = "lower",          # Hides the upper triangle to remove duplicates
  diag = FALSE,            # Removes the diagonal completely!
  addCoef.col = "black",   # Adds numeric correlation values on top
  number.cex = 1,        # Adjusts font size of the text coefficients
  tl.col = "black",        # Color of text labels (variable names)
  tl.srt = 45,             # Rotates top/bottom labels by 45 degrees
  col = colorRampPalette(c("#b2182b", "#f7f7f7", "#2166ac"))(200) # Matches your red-white-blue theme
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
    #all_of(vars_pmsu),
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

# tbl for latex
mental_issues_country_tbl <- 
mental_issue_rates %>% 
  select(continent, sub_region, country1, n, isYes_pct)

tt_mental_issues_country_tbl <- tt(mental_issues_country_tbl, output = "latex")
tt_mental_issues_country_tbl
print(tt_mental_issues_country_tbl, "latex") #copy/paste in latex and works but long
# save_tt(tt_mental_issues_country_tbl, 
#         output = "latex/tt_mental_issues_country_tbl.tex",
#         theme = "booktabs")

# Apply the theme to the table first, then save it
tt_mental_issues_country_tbl |> 
  theme_latex(environment = "tabular") |> 
  save_tt(output = "latex/tt_mental_issues_country_tbl.tex", overwrite = TRUE)

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


# mental issues % for paper countries for latex
tbl_pct_missues <- 
hbsc_cmp %>% group_by(country, mental_issue) %>% tally() %>%
  pivot_wider(names_from = mental_issue,
              values_from = n) %>%
  mutate(n = Yes + No,
         pct_mental_issues = Yes/n)

tt_tbl_pct_missues <- tt(tbl_pct_missues, output = "latex")
tt_tbl_pct_missues

tt_tbl_pct_missues |> 
  theme_latex(environment = "tabular") |> 
  save_tt(output = "latex/tt_tbl_pct_missues.tex", overwrite = TRUE)


# get all columns printed sorted by name
hbsc_cmp |> relocate(sort(names(hbsc_cmp))) |> glimpse()

nrow(hbsc_cmp)


# since these 2 are same info I will prio MBMI but leave for now
ggplot(hbsc_cmp, aes(x = IOTF4_r, y = MBMI_r, fill = IOTF4_r)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  #  geom_jitter(width = 0.2, alpha = 0.2, color = "darkgrey") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") 

# mejor aun, iotf4 o mbi?
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
# ggplot(hbsc_cmp, aes(x = IRFAS, y = IRRELFAS, fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.8, color = "white") +
#   scale_fill_viridis_c(option = "inferno", direction = -1) + 
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     panel.grid.major.y = element_blank(), # Cleans up the ridge background
#     axis.title.y = element_blank()
#   )

#heatmap of irfas vs irrelfas
hbsc_counts <- hbsc_cmp %>%
  count(IRFAS, IRRELFAS) %>%
  filter(!is.na(IRFAS) & !is.na(IRRELFAS))

ggplot(hbsc_counts, aes(x = factor(IRFAS), y = factor(IRRELFAS), fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) + # Thin white borders perfectly separate the tiles
  scale_fill_viridis_c(
    option = "inferno", 
    direction = -1, 
    na.value = "gray95" # Distinct color for any combinations with zero data
  ) +
  labs(
    x = "IRFAS",
    y = "IRRELFAS",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Removes background grid lines that clash with tiles
    axis.text = element_text(color = "black"),
    legend.position = "right"
  ) +
  coord_fixed() # Forces tiles to be perfectly square for better symmetry



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

#temp
#factor_vars <- c("age", "pmsu")

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


# --------------------------------------------------------------
# factors eda results to latex
# --------------------------------------------------------------

# above is good, but i need a simple table with same results from above
# but for a latex table

# generate table for R EDA results
df <- hbsc_cmp %>%
  select(mental_issue, all_of(factor_vars))

df_long <- df %>%
  pivot_longer(
    cols = -mental_issue, # c(pmsu, age), 
    names_to = "Predictor", 
    values_to = "Value"
  )

df_counts <- df_long %>%
  group_by(Predictor, Value) %>%
  summarise(
    TotalCount = n(),
    YesCount = sum(mental_issue == "Yes"),
    .groups = "drop_last"
  )

# 2. NEW STEP: Calculate Chi-Square values for each predictor
chi_results <- tibble(Predictor = c(factor_vars)) %>% #c("pmsu", "age")) %>%
  rowwise() %>%
  mutate(
    # Dynamically build a contingency table and run the chi-sq test
    test = list(chisq.test(df[[Predictor]], df$mental_issue)),
    # Extract the Chi-Square statistic and format the p-value
    ChiSq = test$statistic,
    PValue = test$p.value,
    PValueFormatted = format.pval(PValue, eps = 0.00001, digits = 5)
  ) %>%
  select(Predictor, ChiSq, PValueFormatted) #, P_Value)
#format.pval(p.value, eps = 0.00001, digits = 3))

tbl_fact_eda_chi <- df_counts %>%
  mutate(
    CatPct = 100.0 * TotalCount / sum(TotalCount),
    YesPct = 100.0 * YesCount / TotalCount
  ) %>%
  ungroup() %>%
  left_join(chi_results, by = "Predictor") 

print(tbl_fact_eda_chi) #for latex

tt_tbl_fact_eda_chi <- tt(tbl_fact_eda_chi, output = "latex")

tt_tbl_fact_eda_chi |> 
  theme_latex(environment = "tabular") |> 
  save_tt(output = "latex/tt_tbl_fact_eda_chi.tex", overwrite = TRUE)


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
tree_folds <- vfold_cv(hbsc_train, 
                       v = 10, 
                       repeats = 3, 
                       strata = mental_issue)

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
(Sys.time() - Start) #17 mins 

tree_res

# see performance metrics of fitted trees
tree_res |> collect_metrics()

tree_res |> collect_metrics() |>
  filter(.metric == "roc_auc") %>%
  select(mean) %>%
  summary()

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
      metric_set(bal_accuracy, accuracy, roc_auc, sens, spec)(
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
  slice_sample(n = 5000) %>% #increase? 100-500?
  select(-mental_issue)

# Sample background rows (typically 100-200 rows from your training set is plenty)
bg_X <- hbsc_train %>% 
  dplyr::select(-mental_issue) %>% 
  slice_sample(n = 250) #100-500 sweetspot?

# Calculate SHAP values for all classes automatically
(start <- Sys.time())
shap_output <- kernelshap(
  final_tree, 
  X = X_explain, 
  bg_X = bg_X, 
  type = "prob"
)
Sys.time() - start #3.7 hrs

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

# plot the tree nicely
raw_final_tree <- extract_fit_engine(final_tree)

rpart.plot(raw_final_tree)

rpart.plot(
  raw_final_tree,
  roundint = FALSE,        # FIXES Warning 1: Tells rpart.plot not to look for integers
  tweak = 0.8,             # FIXES Warning 2: Safely shrinks font size globally
  type = 5,                # Draws crisp split labels directly on the lines
  extra = 104,             # Displays clean percentages and probability rates
  under = TRUE,            # Places node data underneath the box
  box.palette = "RdYlGn",  # Clear red/yellow/green color scheme
  fallen.leaves = TRUE     # Forces all final decision nodes to line up at the bottom
)

# unreadable so prunning to plot
pruned_tree <- prune(raw_final_tree, cp = 0.003)


png(
  filename = "plots/pruned_tree.png", 
  width = 2400,          # 7.0 inches * 300 DPI
  height = 1800,         # 5.25 inches * 300 DPI (4:3 Aspect Ratio)
  res = 300              # Standard journal publication DPI
)

rpart.plot(
  pruned_tree,
  roundint = FALSE,
#  font = 4,
  #tweak = 1.5,  
  type = 5,                    # Clear split labels directly on lines
  extra = 100,
  box.palette = list("#c0392b", "#2980b9") 
)

dev.off()



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
  #set_engine("xgboost", scale_pos_weight = 1.94) %>%
  set_engine("xgboost") %>%
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
  finalize(mtry(), hbsc_train),
  learn_rate(),
  size = 50
)

xgb_grid

# xgboost needs numeric predictors — step_dummy() one-hot-encodes any factors
xgb_recipe <- #recipe(mental_issue ~ ., data = hbsc_train) |>
  tree_recipe %>%
  step_dummy(all_nominal_predictors())

xgb_wf <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  #add_formula(mental_issue ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(67)
xgb_folds <- vfold_cv(hbsc_train, 
                      v = 10, #change later to 10
                      repeats = 3,
                      strata = mental_issue
                      )
xgb_folds

doParallel::registerDoParallel()
(start <- Sys.time())
set.seed(67)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = xgb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)
Sys.time() - start #1.5 hrs

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
  fit(data = hbsc_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_fit_xgb <- last_fit(final_xgb, hbsc_split)
final_fit_xgb


# (This gives you the actual trained model needed for SHAP!)
fitted_xgb_workflow <- fit(final_xgb, data = hbsc_train)

# 4. Get Training Accuracy
augment(fitted_xgb_workflow, new_data = hbsc_train) %>% 
  accuracy(truth = mental_issue, estimate = .pred_class)

augment(fitted_xgb_workflow, new_data = hbsc_test) %>% 
  accuracy(truth = mental_issue, estimate = .pred_class)

# shap
# 1. Extract the underlying fitted xgboost engine model
fitted_xgb <- extract_fit_engine(fitted_xgb_workflow)
fitted_xgb
class(fitted_xgb)

# 2. Extract the data processed by your recipe (bypassing the outcome variable)
# Make sure to pass your evaluation/test data or your training data here
processed_data <- extract_recipe(fitted_xgb_workflow) %>% 
  bake(new_data = hbsc_test) %>% 
  select(-mental_issue) %>% 
  as.matrix()
class(processed_data)
processed_data

# Calculate SHAP values
xgb_sv <- shapviz(fitted_xgb, X_pred = processed_data)

sv_importance(xgb_sv, kind = "bar")
sv_importance(xgb_sv, kind = "beeswarm")

sv_dependence(xgb_sv, v = "pmsu_High") #sin gracia

# 4. SHAP Waterfall Plot for an individual prediction (e.g., the 1st observation)
sv_waterfall(xgb_sv, row_id = 1)


# -------------------------------------------------------------------------
# fit logistic regression with var selection
# -------------------------------------------------------------------------

# https://www.tidymodels.org/start/case-study/

# for data use hbsc_split, hbsc_train and hbsc_test from above

# confirm baseline
table(hbsc_cmp$mental_issue) / nrow(hbsc_cmp) #34/66
table(hbsc_train$mental_issue) / nrow(hbsc_train) #34/66
table(hbsc_test$mental_issue) / nrow(hbsc_test) #34/66

# specify lasso for var selection 
lr_mod <- 
  logistic_reg(penalty = tune(), 
               mixture = 1) |> 
  set_engine("glmnet")

lr_mod

# recipe
lr_recipe <- xgb_recipe |>
  step_normalize(all_predictors())
  #step_dummy(all_nominal_predictors())

lr_recipe

lr_workflow <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(lr_recipe)

lr_workflow

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
lr_reg_grid

cv_folds <- vfold_cv(hbsc_train, 
                     v = 10)
cv_folds

# train 30 models
(start <- Sys.time())
lr_res <- 
  lr_workflow |> 
  tune_grid(cv_folds,
            #val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
Sys.time() - start #10 segs

#visualize metric roc so small penalty is best
lr_res |> 
  collect_metrics() |> 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

# get top models
lr_res |> 
  show_best(metric = "roc_auc", n = 25) |> 
  arrange(penalty) %>%
  print(n = Inf)

# 17 seems good
lr_best <- 
  lr_res |> 
  collect_metrics() |> 
  arrange(penalty) |> 
  slice(17)

#lr_best <- lr_res |> 
#  select_best(metric = "roc_auc")

lr_best

#roc 
lr_auc <- 
  lr_res |> 
  collect_predictions(parameters = lr_best) |> 
  roc_curve(mental_issue, .pred_Yes) |> 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

# 1. Lock in the best penalty parameter into your workflow
final_wf <- lr_workflow |> 
  finalize_workflow(lr_best)

my_metrics <- metric_set(accuracy, precision, recall, f_meas, bal_accuracy,
                         roc_auc)

# 2. Fit ONE last time on the entire training data and evaluate on the test split
final_res <- final_wf |> 
  last_fit(split = hbsc_split,
           metrics = my_metrics) # Pass your initial rsplit object here

# 3. View your final performance metrics on the held-out test set
final_res |> 
  collect_metrics()

# 1. Fit the finalized workflow explicitly to the training data
final_train_fit <- final_wf |> 
  fit(data = hbsc_train)

# 2. Predict on the training data and calculate metrics
final_train_fit |> 
  augment(new_data = hbsc_train) |> 
  roc_auc(truth = mental_issue, .pred_Yes) 

# Define the metrics you want to see
my_metrics <- metric_set(roc_auc, accuracy, sens, spec)

# Generate predictions and evaluate them together
final_train_fit |> 
  augment(new_data = hbsc_train) |> 
  my_metrics(truth = mental_issue, 
             estimate = .pred_class,
             .pred_Yes            # For accuracy/sens/spec
             ) # For roc_auc

# Extract and clean the final coefficients
lasso_coefs <- final_train_fit |> 
  extract_fit_parsnip() |> 
  tidy(penalty = lr_best$penalty)

print(lasso_coefs)

library(ggplot2)

lasso_coefs |>
  # Remove the intercept and any variables Lasso dropped (zeroed out)
  filter(term != "(Intercept)", estimate != 0) |>
  # Sort by the size of the impact
  mutate(term = reorder(term, estimate)) |>
  ggplot(aes(x = estimate, y = term, fill = estimate > 0)) +
  geom_col() +
  scale_fill_manual(values = c("darkred", "darkgreen"), 
                    #labels = c("Protective Factor", "Risk Factor"),
                    name = "Impact Direction") +
  labs(
    title = "Lasso Coefficient Importance for Mental Issues",
    x = "Standardized Coefficient (Log-Odds Impact per 1 SD)",
    y = NULL
  ) +
  theme_minimal()

#vip
lasso_coefs |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  mutate(abs_impact = abs(estimate)) |> 
  arrange(desc(abs_impact)) |> 
  select(Variable = term, `Standardized Coefficient` = estimate, `Absolute Impact` = abs_impact)

#relative importance
lasso_coefs |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  mutate(
    # Get absolute impact
    abs_impact = abs(estimate),
    # Scale relative to the maximum impact feature
    relative_importance = (abs_impact / max(abs_impact)) * 100
  ) |> 
  select(term, estimate, relative_importance) |> 
  arrange(desc(relative_importance))

#2. Standardized Odds Ratios (OR)
lasso_coefs |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  mutate(
    std_odds_ratio = exp(estimate)
  ) |> 
  select(term, standardized_log_odds = estimate, std_odds_ratio) |> 
  arrange(desc(std_odds_ratio))


lasso_coefs |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  mutate(
    std_or = exp(estimate),
    direction = if_else(estimate > 0, "Risk Factor (Increases Odds)", "Protective Factor (Decreases Odds)")
  ) |> 
  select(
    Variable = term, 
    `Standardized Log-Odds` = estimate, 
    `Standardized Odds Ratio` = std_or, 
    Classification = direction
  ) |> 
  arrange(desc(abs(`Standardized Log-Odds`)))

# que relajo
corrected_coefs <- final_train_fit |> 
  extract_fit_parsnip() |> 
  tidy(penalty = lr_best$penalty) |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  mutate(
    # Multiply by -1 to flip the target from 'no' to 'yes'
    corrected_log_odds = estimate * -1, 
    # Calculate the true Odds Ratio for 'yes'
    odds_ratio = exp(corrected_log_odds),
    direction = if_else(corrected_log_odds > 0, "Risk Factor (+)", "Protective Factor (-)")
  )

# View your fully corrected results
corrected_coefs |> 
  select(term, original_raw = estimate, corrected_log_odds, odds_ratio, direction) |> 
  arrange(desc(corrected_log_odds))

corrected_coefs |>
  # Remove the intercept and any variables Lasso dropped (zeroed out)
  filter(term != "(Intercept)", estimate != 0) |>
  # Sort by the size of the impact
  mutate(term = reorder(term, corrected_log_odds)) |>
  ggplot(aes(x = corrected_log_odds, y = term, fill = estimate > 0)) +
  geom_col() +
  scale_fill_manual(values = c("darkred", "darkgreen"), 
                    labels = c("Bad", "Good"),
                    name = "Impact Direction") +
  labs(
    title = "Lasso Coefficient Importance for Mental Issues",
    x = "Standardized Coefficient (Log-Odds Impact per 1 SD)",
    y = NULL
  ) +
  theme_minimal()


# refit in normal scale
# 1. Isolate the dummy/engineered feature names from your Lasso model
selected_features <- corrected_coefs |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  pull(term)
selected_features

# want age, not just age_X15
myselected_feature <- c("MBMI_r", "age", "cbullied", "emconlfreq", "emconlpref",
                        "emconlpref", "fam_sup", "friends_sup", "gender", "IRRELFAS",
                        "pmsu", "student_sup", "talk_parent", "teacher_sup", "timeexe_r")

# 2. Build a new recipe that creates the dummies, but filters down to ONLY your selected features
hybrid_recipe <- recipe(mental_issue ~ MBMI_r + age + cbullied + emconlfreq + emconlpref + fam_sup + 
                          friends_sup + gender + IRRELFAS + pmsu + student_sup + talk_parent + teacher_sup + 
                          timeexe_r, data = hbsc_train) |> 
  step_dummy(all_nominal_predictors()) #|> 
  # step_normalize() is removed here so your new p-values reflect a normal 1-unit change!
  #step_select(mental_issue, all_of(selected_features))

# 3. Define a standard, unpenalized logistic regression model
normal_lr_spec <- logistic_reg() |> 
  set_engine("glm")

# 4. Bundle them into a workflow and fit
hybrid_wf <- workflow() |> 
  add_recipe(hybrid_recipe) |> 
  add_model(normal_lr_spec)

hybrid_fit <- hybrid_wf |> fit(data = hbsc_train)
hybrid_fit

hybrid_fit |> 
  extract_fit_parsnip() |> 
  tidy(conf.int = TRUE) |> 
  # Filter out the intercept
  filter(term != "(Intercept)") |> 
  mutate(
    # 1. Flip the signs to correct the glm baseline back to predicting 'yes'
    log_odds_estimate = estimate * -1,
    
    # 2. Derive the True Odds Ratios and Confidence Intervals based on 'yes'
    odds_ratio = exp(log_odds_estimate),
    or_lower_ci = exp((conf.high) * -1), # Note the bounds flip when multiplying by -1
    or_upper_ci = exp((conf.low) * -1)
  ) -> lr_res
  # Clean up column layout for presentation

lr_res  

require(scales)

# output for paper
lr_res_tbl <- 
lr_res %>%
  #arrange(desc(term)) %>% 
  select(term, log_odds_estimate, odds_ratio, p.value,
         ) %>%
  mutate(p.value = format.pval(p.value, eps = 0.001, digits = 3))
  #mutate(p.value = p_value(p.value, accuracy = 0.001)) # Cleans the whole column

lr_res_tbl

tt_lr_res_tbl <- tt(lr_res_tbl, output = "latex")
tt_lr_res_tbl

tt_lr_res_tbl |> 
  theme_latex(environment = "tabular") |> 
  save_tt(output = "latex/tt_lr_res_tbl.tex", overwrite = TRUE)


#forest plot
library(stringr)

# 1. Clean up and format the labels for the plot
plot_data <- res |> 
  # Clean up variable names (replaces underscores with spaces, capitalizes first letter)
  mutate(
    clean_term = term,
    #clean_term = str_replace_all(term, "_", " "),
    #clean_term = str_to_sentence(clean_term),
    # Force ggplot to sort the variables by their actual log odds estimate size
    clean_term = reorder(clean_term, log_odds_estimate)
  )

# 2. Build the visual forest plot
ggplot(plot_data, aes(x = log_odds_estimate, y = clean_term)) +
  # Add a vertical dashed line at 0 (the line of no effect)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.6) +
  
  # Add the horizontal 95% Confidence Interval error bars
  geom_errorbarh(aes(xmin = or_lower_ci |> log(), xmax = or_upper_ci |> log()), 
                 height = 0.2, color = "gray30", linewidth = 0.7) +
  
  # Add the point estimates on top (colored by risk vs protective)
  geom_point(aes(color = log_odds_estimate > 0), size = 3.5) +
  
  # Define professional colors (Warm red for Risk, Soft blue for Protective)
  scale_color_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#2980b9"),
                     labels = c("TRUE" = "Risk Factor (Increases Odds)", 
                                "FALSE" = "Protective Factor (Decreases Odds)"),
                     name = NULL) +
  
  # Set clean axis titles and clear naming
  labs(
    title = "Predictors of Mental Health Issues",
    subtitle = "Standardized log-odds estimates with 95% confidence intervals",
    x = "Log-Odds Estimate (Target: Yes)",
    y = NULL
  ) +
  
  # Modern, minimal styling layout
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", color = "gray20"),
    axis.title.x = element_text(margin = margin(t = 10))
  )

# 2. Build the visual forest plot... repeat from above for odds
ggplot(plot_data, aes(x = odds_ratio, y = clean_term)) +
  # Add a vertical dashed line at 0 (the line of no effect)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray60", linewidth = 0.6) +
  
  # Add the horizontal 95% Confidence Interval error bars
  geom_errorbarh(aes(xmin = or_lower_ci, xmax = or_upper_ci), 
                 height = 0.2, color = "gray30", linewidth = 0.7) +
  
  # Add the point estimates on top (colored by risk vs protective)
  geom_point(aes(color = odds_ratio > 1), size = 3.5) +
  
  # Define professional colors (Warm red for Risk, Soft blue for Protective)
  scale_color_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#2980b9"),
                     labels = c("TRUE" = "Risk Factor (Increases Odds)", 
                                "FALSE" = "Protective Factor (Decreases Odds)"),
                     name = NULL) +
  
  # Set clean axis titles and clear naming
  labs(
    title = "Predictors of Mental Health Issues",
    subtitle = "odds_ratio estimates with 95% confidence intervals",
    x = "odds_ratio Estimate (Target: Yes)",
    y = NULL
  ) +
  
  # Modern, minimal styling layout
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", color = "gray20"),
    axis.title.x = element_text(margin = margin(t = 10))
  )
  




