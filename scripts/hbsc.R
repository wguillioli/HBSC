# HBSC 
# Updated: 2026-10-09

# ---------------------------------------------------
# project setup
# ---------------------------------------------------

rm(list = ls())

require(tidyverse)
require(tidymodels)
require(stringr)
require(kableExtra)
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
library(coefplot)

prj_fldr <- "C:/MisLocalFiles/Github/HBSC/"
setwd(prj_fldr)

options(scipen = 999)


# ---------------------------------------------------
# workspace stuff
# ---------------------------------------------------

img_file <- paste0(prj_fldr, "/Rimages/hbsc_wkspace_20261001.RData")
load(file = img_file)
#save.image(file = img_file)


# ---------------------------------------------------
# load data, basic eda
# ---------------------------------------------------

# the european way with ; and ,
hbsc <- read_csv2(paste0(prj_fldr, "data/raw/HBSC2018OAed1.1.csv"))
glimpse(hbsc)
dim(hbsc)


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
    "studtogether", "studhelpful", "studaccept",
    "teacheraccept", "teachercare", "teachertrust"
  ),
  "parents" = c(
    "talkfather", "talkmother"
  )
)

# new df with cols of interest
dat <- hbsc %>%
  select(all_of(unlist(vars_groups, use.names = FALSE)))

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


# ---------------------------------------------------
# univariate EDA and feature eng 
# ---------------------------------------------------

glimpse(dat)

# $ seqno_int      <dbl> 100001, 100002, 100004, 100005, 100007, 100008, 100009, 100010, 100011, 100012, 100013, …
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
str(dat$age)

# $ sex            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
dat <- dat |>
  mutate(
    gender = factor(
      sex,
        levels = c(1,2),
        labels = c("Boy", "Girl")
      )
    ) %>% select(-sex)

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
    IOTF4 = factor(
      IOTF4,
      levels = c(1, 2, 3, 4),
      labels = c("Thinness", "Normal", "Overweight", "Obesity")
    )
  )# %>% select(-IOTF4)

ggplot(dat, aes(IOTF4)) + geom_bar()+ coord_flip() + theme_minimal()


# $ MBMI           <dbl> 17.98167, 17.78325, 24.24392, 15.03105, 15.57093, 18.25632, 14.26873, 20.88889, 14.46759…
# Body mass index from [Range= 11.02-44.9] and 0 meaning outside overall range so make those NA.
dat <- dat %>% 
  mutate(MBMI = ifelse(MBMI == 0, NA, MBMI)) #%>%

summary(dat$MBMI)

ggplot(dat, aes(x = MBMI)) +  geom_histogram() + theme_minimal()


# $ timeexe        <dbl> 3, 2, 2, 2, 6, 4, 3, 1, 3, 4, 1, 1, 1, 1, 1, 4, 2, NA, 3, NA, 4, 1, 6, 2, 2, 3, 4, 2, 2,…
# timeexe ask how often exercise in free time going from 1 (every day) to 7 never. 
# HBSC shows proportions of those that do 3+ per week (But that is 2022 data)
# in our case we do 2-3 times per week or more. And I keep timexe as is for shap test.
dat <- dat %>%
  mutate(exercise = factor(case_when(
    between(timeexe, 1, 3) ~ 1,
    between(timeexe, 4, 7) ~ 0,
    TRUE ~ NA),
    levels = c(0, 1), 
    labels = c("No", "Yes"))
    ) %>% select(-timeexe)

ggplot(dat, aes(exercise)) + geom_bar()


# $ lifesat        <dbl> 7, 7, 10, NA, 6, 8, NA, 5, NA, NA, 9, 9, 10, 9, 10, 8, 8, NA, 9, NA, 10, 7, 8, 10, 10, 7…
# life satisfaction as ladder: 0(botton) to 10(best). I'll dichotomize and keep num for shap.
summary(dat$lifesat)
ggplot(dat, aes(factor(lifesat))) + geom_bar()

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
  mutate(mental_sum = feellow_is +
                      irritable_is +
                      nervous_is +
                      sleepdificulty_is
  ) %>%
  mutate(mentalissue = factor(
          ifelse(mental_sum >=2, 1, 0),
          levels = c(1, 0),
          labels = c("Yes", "No")
         )
  ) %>%
  select(-c(
    all_of(vars_mental), 
    all_of(paste0(vars_mental, "_is")),
    mental_sum)
  )

ggplot(dat, aes(mentalissue)) + geom_bar()


# $ emcsocmed1 to 9     <dbl> 2, 1, 2, 1, 1, 1, 1, 1, 1, NA, 1, 99, 1, 1, NA, 1, 1, 2, 1, 1, 1, NA, 1, 2, 2, 1, 2, 2, …
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
  r_pmsu,
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
      labels = c("Low", "Med", "High")
    )
  ) %>%
  select(-c(
    all_of(vars_pmsu),
    all_of(vars_pmsu_r),
    pmsu_s
  ))

#table(dat$pmsu_s, dat$pmsu,useNA = "always")
ggplot(dat, aes(pmsu)) + geom_bar()


# $ cbeenbullied   <dbl> 3, 2, 2, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, NA, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
# Answers range from 1 (Haven't) to 5 (Several times a week).  
dat <- dat %>%
  mutate(cyberbullied = factor( 
           case_when(cbeenbullied == 1 ~ 0,
                     between(cbeenbullied, 2, 5) ~ 1,
                     TRUE ~ NA),
           levels = c(0,1),
           labels = c("No", "Yes")
           )#,
        ) %>% select(-cbeenbullied)

ggplot(dat, aes(cyberbullied)) + geom_bar()


# $ emconlfreq1 to 4    <dbl> 4, 4, 1, 4, 3, 4, 2, 6, 6, 3, NA, 1, 5, 6, 4, 3, 5, 1, 5, 6, 1, 4, 6, 3, 3, 4, 6, 3, 6, …
# continuos online communications range from 1(NA/don't know) to 6(almost all time)
# 1 close friends, 2 larger friend group, 3 online frriends, 4 other
vars_emconlfreq <- paste0("emconlfreq", rep(1:4))
lapply(dat[vars_emconlfreq], table, useNA = "always")

# recode 2 versions - yn like HBSC and sum.
dat <- dat %>%
  mutate(
    # 1 if any of the 4 is 6
    onlcontcomms = factor(case_when(
      (emconlfreq1 == 6 | emconlfreq2 == 6 | emconlfreq3 == 6 | emconlfreq4 == 6) ~ 1,
      (!is.na(emconlfreq1) & !is.na(emconlfreq2) & !is.na(emconlfreq3) & !is.na(emconlfreq4)) ~ 0,
      TRUE ~ NA # if any reply is NA, drop observation
    ),
    levels = c(0,1),
    labels = c("No", "Yes")
    ),
  ) %>%
  select(- all_of(vars_emconlfreq))

ggplot(dat, aes(onlcontcomms)) + geom_bar() + theme_minimal()


# $ emconlpref1 to 3    <dbl> 2, 5, 5, 1, 1, 1, 1, 1, 2, 1, NA, 99, 1, 1, 1, 1, 3, 3, 3, 3, 1, 3, 1, 2, 2, 1, 5, 2, 1,…
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
        # create hml
        onlcommpref = factor(case_when(
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
    
ggplot(dat, aes(x=onlcommpref)) + geom_bar() + coord_flip() + theme_minimal()


# $ famhelp        <dbl> 7, 7, 7, 7, 2, 7, 7, NA, 7, 7, NA, NA, 7, 7, 7, 7, 7, 1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7…
# $ famsup         <dbl> 6, 7, 7, 7, 1, 7, 7, NA, 7, 7, NA, NA, 7, 7, 7, 7, 7, 3, 7, 7, 1, 7, 7, 7, 7, 6, 7, 7, 7…
# $ famtalk        <dbl> 7, 7, 1, 7, 1, 7, 7, NA, 7, 7, NA, NA, 7, 7, 7, 7, 5, 7, 7, 7, 7, 7, 7, 7, 7, 5, 1, 7, 7…
# $ famdec         <dbl> 5, 7, 7, 7, 1, 7, 7, NA, 7, 6, NA, NA, 7, 7, 1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7…
# 4 questions going from 1 Very strongly disagree to 7 Very strongly agree.
vars_famsup <- c("famhelp", "famsup", "famtalk", "famdec")
lapply(dat[vars_famsup], table, useNA = "always")

# plot a cool diverging bar chart for paper
# 1. Reshape, remove 4s entirely, and recalculate percentages based on the active sample
final_plot_data <- dat %>%
  select(famhelp, famsup, famtalk, famdec) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response) & Response != 4) %>% # <-- Removes missing values AND the neutral 4s
  count(Question, Response) %>%
  group_by(Question) %>%
  mutate(Percentage = (n / sum(n)) * 100) %>%
  ungroup() %>%
  # Convert responses to factors to lock the scale order 1 to 7 (skipping 4)
  mutate(Response = factor(Response, levels = c(1:3, 5:7))) %>%
  # Assign absolute directional plot values (Negative vs Positive)
  mutate(Plot_Value = case_when(
    Response %in% 1:3 ~ -Percentage,
    Response %in% 5:7 ~ Percentage
  ))

# 2. Generate the Diverging Stacked Bar Chart without Neutral 4s
ggplot(final_plot_data, aes(x = Question, y = Plot_Value, fill = Response)) +
  geom_col(width = 0.85) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) + # Perfect central split line
  scale_y_continuous(
    limits = c(-25, 100),
    breaks = seq(-25, 100, by = 25),
    labels = function(x) paste0(abs(x), "%") # Keeps labels looking like absolute positive percentages
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#b2182b", "2" = "#d6604d", "3" = "#f4a582", # Negative spectrum (Reds)
      "5" = "#92c5de", "6" = "#4393c3", "7" = "#2166ac"  # Positive spectrum (Blues)
    ),
    labels = c(
      "1" = "1 (Very strongly disagree)",
      "2" = "2 (Strongly disagree)",
      "3" = "3 (Disagree)",
      "5" = "5 (Agree)",
      "6" = "6 (Strongly agree)",
      "7" = "7 (Very strongly agree)"
    )
  ) +
  coord_flip() + 
  labs(
    x = NULL,
    y = NULL,
    fill = "Scale"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

# Summing them results in [4,28] and then grouped as H/M/L as L 4-11, M 12-19, H 20-28.
dat <- dat %>%
  mutate(famsup_s = rowSums(dat[,vars_famsup])
  ) %>%
  mutate(famsupp = factor(case_when(
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

ggplot(dat, aes(x=famsupp)) + geom_bar() + theme_minimal()


# $ friendhelp     <dbl> 7, 5, 7, 7, 7, 3, 7, 7, 7, 7, NA, 4, 7, 7, 7, 1, 6, NA, 7, 2, 2, 7, 2, 7, 7, 7, NA, 7, 7…
# $ friendcounton  <dbl> 6, 6, 1, 7, 3, 7, 7, 1, 7, 6, NA, 2, 7, 7, 2, 1, 6, NA, 7, 3, 7, 7, 2, 3, 3, 6, NA, 3, 7…
# $ friendshare    <dbl> 7, 5, 7, 7, 5, 7, 7, 7, 7, 7, NA, 7, 7, 7, 7, 1, 7, NA, 7, 2, 7, 7, 3, 7, 7, 7, NA, 7, 7…
# $ friendtalk     <dbl> 5, 7, 1, 7, 6, 7, 7, 7, 7, 7, NA, 2, 7, 7, 7, 1, 7, NA, 7, 2, 7, 7, 7, 2, 2, 7, NA, 2, 7…
# 4 questions going from 1 Very strongly disagree to 7 Very strongly agree.
vars_frisup <- vars_groups[["friends_support"]]
lapply(dat[vars_frisup], table, useNA = "always")

#Summing them results in [4,28] and then grouped as H/M/L as L 4-11, M 12-19, H 20-28. Resulting in:
dat <- dat %>%
  mutate(frisup_s = rowSums(dat[,vars_frisup])
  ) %>%
  mutate(frisupp = factor(case_when(
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

ggplot(dat, aes(x=frisupp)) + geom_bar() + theme_minimal()


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
    teachsupp = factor(case_when(
      teacher_support_avg >= 4 ~ 1, # is this right??? 
      teacher_support_avg < 4 ~ 0,
      TRUE ~ NA),
      levels = c(0,1),
      labels = c("No", "Yes")
      ),
    studsupp = factor(case_when(
      student_support_avg >= 4 ~ 1,
      student_support_avg < 4 ~ 0,
      TRUE ~ NA),
      levels = c(0,1),
      labels = c("No", "Yes")
    )
  ) %>%
  select(-c(
    all_of(vars_school),
    all_of(paste0(vars_school, "_r")),
    teacher_support_avg,
    student_support_avg
           )
  )

ggplot(dat, aes(x=teachsupp)) + geom_bar() + theme_minimal()

ggplot(dat, aes(x=studsupp)) + geom_bar() + theme_minimal()


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
    talkparent = as.integer(talkf | talkm)
  ) %>%
  mutate(
    talkparent = factor(talkparent, levels = c(0,1), labels = c("No", "Yes")),
  ) %>%
  select(-c(
    talkf,
    talkm,
    talkmother,
    talkfather
  ))

ggplot(dat, aes(x=talkparent)) + geom_bar() + theme_minimal()


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

dat$country <- recode_map[as.character(dat$countryno)]

# from name get continents info
dat <- dat %>%
  mutate(
    continent = countrycode(
      sourcevar = country, 
      origin = "country.name", 
      destination = "continent",
      custom_match = c(
        "England"           = "Europe",
        "Scotland"          = "Europe",
        "Wales"             = "Europe"
      )
    ),
    sub_region = countrycode(
      sourcevar = country, 
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
  group_by(continent, sub_region, country, countryno) %>%
  tally() %>%
  print(n = Inf)


# ---------------------------------------------------------
# get rids of NAs and keep countries of interest based on Y
# and final touches before mdl
# ---------------------------------------------------------

colSums(is.na(dat)) 

## WW complete
hbsc_ww_cmp <- dat %>%
  drop_na()

table(hbsc_ww_cmp$mentalissue) / nrow(hbsc_ww_cmp) #28Y/72N

# tbl with mental issues rate by country
tbl_mentissue_countries <- 
  hbsc_ww_cmp %>%
  group_by(continent, sub_region, country, mentalissue) %>%
  tally() %>%
  pivot_wider(names_from = mentalissue,
              names_prefix = "is",
              values_from = n,
              values_fill = 0) %>%
  mutate(n = isNo + isYes,
         isYes_pct = isYes/n) 

tbl_mentissue_countries %>% print(n = Inf)

# SE it is
hbsc_se <- hbsc_ww_cmp %>%
  filter(sub_region == "Southern Europe") %>%
  #mutate(country = factor(country1)) %>%
  select(-c(sub_region, continent, countryno)) %>%
  mutate(country = factor(country))

summary(hbsc_se)
glimpse(hbsc_se)

table(hbsc_se$mentalissue) / nrow(hbsc_se) #27/73

# SE issues tally
tbl_hbsc_se <- 
hbsc_se %>%
  group_by(country, mentalissue) %>%
  summarise(n = n_distinct(seqno_int),
            .groups = "drop") %>%
  pivot_wider(names_from = mentalissue,
              values_from = n) %>%
  mutate(n = Yes + No,
         pct_issues = Yes/n,
         pct = n / sum(n)) %>%
  arrange(desc(pct_issues)) %>%
  select(country, n, pct_issues, pct) %>%
  mutate(n = comma(n),
         pct_issues = percent(pct_issues, accuracy = 0.1),
         pct = percent(pct, accuracy = 0.1)
         )

# add total row
#total_row <- c("Total", nrow(hbsc_se),0)

tbl_hbsc_se  

# for latex
#kable(tbl_hbsc_se, format = "latex", booktabs = TRUE)
tt_tbl_hbsc_se <- tt(tbl_hbsc_se) 
save_tt(tt_tbl_hbsc_se, output = "./outputsR/tinytables/tt_tbl_hbsc_se.tex", overwrite = TRUE)

# get all columns printed sorted by name
hbsc_se |> relocate(sort(names(hbsc_se))) |> glimpse()


# ----------------------------------------------------------------------
# one more thing on bmi and fas
# ----------------------------------------------------------------------

# since these 2 are same info I will prio MBMI but leave for now
ggplot(hbsc_se, aes(x = IOTF4, y = MBMI, fill = IOTF4)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") 

# mejor aun inferno ridges
ggplot(hbsc_se, aes(x = MBMI, y = IOTF4, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.8, color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = -1) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), # Cleans up the ridge background
    axis.title.y = element_blank()
  )

# order by type
hbsc_se <- hbsc_se %>% 
  select(order(colnames(.))) %>%
  select(
    where(is.factor),
    where(is.numeric),
    everything()
  )

glimpse(hbsc_se)


# ------------------------------------------------------------
# factor vars - eda and potential of pred
# for each fact predictor get a summary table with 
# var name, n, % and % of mental issue and chi
# ------------------------------------------------------------

# get name of all factor vars (including mentalissue)
# and define a df with only those vars
factor_vars <- hbsc_se %>% 
  select(where(is.factor)) %>% 
  names()

factor_vars

hbsc_se_fct <- hbsc_se %>%
  select(all_of(factor_vars))

# tabulate for each var, n and % and % mental issues
tbl_fct <- 
hbsc_se_fct %>%
  pivot_longer(
    cols = -mentalissue, 
    names_to = "Predictor", 
    values_to = "Value"
  ) %>%
group_by(Predictor, Value) %>%
  summarise(
    n = n(),
    nmentalissue = sum(mentalissue == "Yes"),
    .groups = "drop_last"
  ) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(pctmentalissues = nmentalissue / n) %>%
  ungroup()
  
head(tbl_fct)

chi_results <- 
tibble(Predictor = c(factor_vars)) %>% 
  rowwise() %>%
  mutate(
    # Dynamically build a contingency table and run the chi-sq test
    test = list(chisq.test(hbsc_se_fct[[Predictor]], hbsc_se_fct$mentalissue)),
    # Extract the Chi-Square statistic and format the p-value
    chi = test$statistic,
    pvalue = test$p.value
  ) %>%
  select(-test) %>%
  ungroup()

tbl_fct %>% group_by(Predictor) %>% summarise(n = sum(n), 
                                              nvals = n_distinct(Value)) #triple check

tbl_fct <- inner_join(tbl_fct, chi_results, by = "Predictor")

# for latex
tt_tbl_fct <- tbl_fct %>%
  mutate(n = comma(n),
         pct = percent(pct, accuracy = 0.1),
         pctmentalissues = percent(pctmentalissues, accuracy = 0.1),
         chi = comma(chi, accuracy = 0.1),
         pvalue = round(pvalue,20)
  ) %>%
  select(-nmentalissue)

head(tt_tbl_fct)

tt_tbl_fct <- tt(tt_tbl_fct) 
save_tt(tt_tbl_fct, output = "./outputsR/tinytables/tt_tbl_fct.tex", overwrite = TRUE)


# ------------------------------------------------------------
# number vars - eda and potential of pred
# for each fact predictor get a summary table with 
# var name, n, % and % of mental issue and chi
# ------------------------------------------------------------

library(purrr)

num_vars <- c("IRFAS", "lifesat", "MBMI")

group_stats <- hbsc_se %>%
  group_by(mentalissue) %>%
  summarise(
    across(all_of(num_vars), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -mentalissue, names_to = c("Variable", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = c(mentalissue, Stat), values_from = value, names_sep = "_")

all_stats <- hbsc_se %>%
  summarise(
    across(all_of(num_vars), list(All_mean = ~ mean(.x, na.rm = TRUE), All_sd = ~ sd(.x, na.rm = TRUE)))
  ) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", "Stat"), names_pattern = "(.*)_(All_.*)") %>%
  pivot_wider(names_from = Stat, values_from = value)

desc_table <- left_join(all_stats, group_stats, by = "Variable")

# 3. Iterate over variables using a loop/map to calculate KS D-Test parameters
ks_results <- map_dfr(num_vars, function(v) {
  # Subset variables into vectors, stripping out missing data points
  vec_no  <- hbsc_se[[v]][hbsc_se$mentalissue == "No"]  %>% na.omit()
  vec_yes <- hbsc_se[[v]][hbsc_se$mentalissue == "Yes"] %>% na.omit()
  
  # Run the two-sample KS test
  test_out <- ks.test(vec_no, vec_yes)
  
  # Return data row
  tibble(
    Variable  = v,
    KS_D      = test_out$statistic,
    KS_pvalue = test_out$p.value
  )
})

# 4. Join the KS D-test columns onto your table 
final_table <- left_join(desc_table, ks_results, by = "Variable")

# 5. View the complete workflow output
print(final_table)

# for latex
tt_tbl_num <- final_table %>%
  mutate(across(
    .cols = where(is.numeric) & !c(KS_pvalue), # Numeric, but NOT the last column
    .fns = ~ round(.x, digits = 2)
  )) %>%
  mutate(KS_pvalue = round(KS_pvalue,15))

head(tt_tbl_num)

tt_tbl_num <- tt(tt_tbl_num) 
save_tt(tt_tbl_num, output = "./outputsR/tinytables/tt_tbl_num.tex", overwrite = TRUE)


# ------------------------------------------------------------
# the modeling dataset for LR
# ------------------------------------------------------------

# relevel for lr
hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")
levels(hbsc_se$mentalissue)

#split the data
set.seed(67)
hbsc_se_split  <- initial_split(hbsc_se, 
                           strata = mentalissue,
)

hbsc_se_train <- training(hbsc_se_split)
hbsc_se_test <- testing(hbsc_se_split)

# just checking
nrow(hbsc_se_train) + nrow(hbsc_se_test) == nrow(hbsc_se) #must be TRUE
round(table(hbsc_se_train$mentalissue) / nrow(hbsc_se_train),5)
round(table(hbsc_se_test$mentalissue) / nrow(hbsc_se_test), 5) #must be same


# ------------------------------------------------------------
# THE logistic regression
# ------------------------------------------------------------

#relevel, why only in lr?
#hbsc_se_train$mentalissue <- relevel(hbsc_se_train$mentalissue, ref = "No")
#hbsc_se_test$mentalissue <- relevel(hbsc_se_test$mentalissue, ref = "No")

#redoing the good old way to confirm what glmnet tidymodels did
lr1 <- glm(mentalissue ~ . 
           -IOTF4
           -IRRELFAS
           -lifesat
           -seqno_int
           -IRFAS, #not sign
           data = hbsc_se_train,
           family = binomial)

summary(lr1)

coef(lr1)
exp(coef(lr1))

cbind(
  round(coef(lr1),3),
  round(exp(coef(lr1)),3)
)

coefplot(lr1)
coefplot(lr1, trans = exp)

library(GGally)
#ggcoef_model(lr1)
png(
  filename = "outputsR/lr_coefplot.png", 
  width = 2400,          # 7.0 inches * 300 DPI
  height = 1800,         # 5.25 inches * 300 DPI (4:3 Aspect Ratio)
  res = 300              # Standard journal publication DPI
)

ggcoef_model(lr1, 
             exponentiate = TRUE,
             variable_labels = c(
               cyberbullied = "bully") #needs more work for all other long var names
             ) +
  theme_minimal()

dev.off()


# that's cool and all but seems country just confuses
lr2 <- glm(mentalissue ~ . 
           -IOTF4
           -IRRELFAS
           -lifesat
           -seqno_int
           -IRFAS #not signif
           -country,
           data = hbsc_se_train,
           family = binomial)

summary(lr2)

cbind(
  round(coef(lr2),3),
  round(exp(coef(lr2)),3)
)

#library(GGally)
ggcoef_model(lr2, exponentiate = TRUE) +
  theme_minimal() 


# lasso it
# specify model for glmnet with var sec
lr_lasso_mod <- 
  logistic_reg(penalty = tune(), 
               mixture = 1) |> 
  set_engine("glmnet")

# recipe of pre proc steps
lr_recipe <- 
  recipe(mentalissue ~ ., data = hbsc_se_train) |> 
  step_rm(IOTF4,
          IRRELFAS,
          lifesat,
          seqno_int
          ) |>
  step_dummy(all_nominal_predictors()) |> 
  step_normalize(all_predictors())

# workflow set
lr_workflow <- 
  workflow() |> 
  add_model(lr_lasso_mod) |> 
  add_recipe(lr_recipe)

# make grid for tunning
# i had this not sure where from
#lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
#summary(lr_reg_grid)

penalty_grid <- grid_regular(penalty(), levels = 100)
summary(penalty_grid)

#from islr won't work
#grid <- 10^seq(10,-2, length = 100)
#summary(grid)

# create folds for tunning
folds_10cv <- vfold_cv(hbsc_se_train, v= 10)

my_metrics <- metric_set(bal_accuracy, 
                         accuracy,
                         roc_auc,
                         brier_class,
                         j_index
)

# train and tune, <1min
lr_res <- 
  lr_workflow |> 
  tune_grid(
    resamples = folds_10cv, 
    grid = penalty_grid,
    #grid = lr_reg_grid,
    control = control_grid(save_pred = TRUE),
    metrics = my_metrics
  )

# collect all metrics
lr_res_metrics <- 
lr_res |> 
  collect_metrics()

# plot AUC values by penalty
lr_res_metrics |>
  filter(.metric == "roc_auc") |> 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("AUC ROC") +
  scale_x_log10(labels = scales::label_number()) +
  theme_minimal()

# select best model, finalize workflow and refit on whole train data
best_lr_params <- lr_res |> 
  select_best(metric = "roc_auc")

# Update workflow with these optimal parameters
final_lr_workflow <- lr_workflow |> 
  finalize_workflow(best_lr_params)

# get the last fit
final_lr_fit <- final_lr_workflow |> 
  last_fit(split = hbsc_se_split, metrics = my_metrics)

final_lr_fit

# get test and train metrics
(test_metrics <- final_lr_fit |> collect_metrics())

trained_model <- final_lr_workflow |> 
  fit(data = training(hbsc_se_split))

(train_metrics <- trained_model |> 
    augment(new_data = training(hbsc_se_split)) |> 
    my_metrics(truth = mentalissue, 
               estimate = .pred_class,
               .pred_No)  
)

# combine metrics for train/test
lr_metrics <- (train_metrics %>% 
                 select(.metric, .estimate) %>%
                 mutate(set = "train")
) %>% bind_rows(
  (test_metrics %>% 
     select(.metric, .estimate) %>%
     mutate(set = "test")
  ) 
)

lr_metrics <- 
lr_metrics %>% pivot_wider(names_from = set,
                           values_from = .estimate)

lr_metrics

# all_perf_metrics <- lr_metrics %>%
#   rename (Metric = .metric) %>%
#   mutate(Model = "Logistic Regression",
#          train = round(train,3),
#          test = round(test, 3)
#          ) %>%
#   select(Model, Metric, train, test) 
# 
# # for latex
# tt_all_perf_metrics <- tt(all_perf_metrics) 
# save_tt(tt_all_perf_metrics, output = "./outputsR/tinytables/tt_all_perf_metrics.tex", overwrite = TRUE)


# for test set, for each class (Y/N) get precision, recall, f1

lr_test_predictions <- collect_predictions(final_lr_fit)

class_metrics <- metric_set(f_meas, precision, recall)

(metrics_class1 <- class_metrics(
  lr_test_predictions, 
  truth = mentalissue, 
  estimate = .pred_class,
  event_level = "first"
) %>% 
    mutate(class = "NO")
)

(metrics_class2 <- class_metrics(
  lr_test_predictions, 
  truth = mentalissue, 
  estimate = .pred_class,
  event_level = "second"
) %>% 
    mutate(class = "YES")
)

# latex output
lr_test_metrics <- 
bind_rows(metrics_class1, metrics_class2) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric,
              values_from = .estimate
  )

lr_test_metrics %>%
  pivot_wider(names_from = class,
              values_from = c(f_meas, precision, recall)) %>%
  select(ends_with("YES"), ends_with("NO"))



# ------------------------------
# refit on normal scale using selected vars for coeff table

# # get vars selected by lasso to refit
# vars_from_glmnet <- 
# final_lr_fit |> 
#     extract_fit_parsnip() |> 
#     tidy() |>
#     filter(term != "(Intercept)") |> 
#     mutate(
#       lasso_status = if_else(estimate == 0, "Removed", "Kept")
#     ) |> 
#     select(term, estimate, lasso_status) |>
#     mutate(feature = str_split_i(term, "_", 1)) %>%
#     distinct(feature) |>
#     pull()
# 
# # now, refit a normal GLM for my coeffs and OR
# lr_recipe2 <- 
#   recipe(mentalissue ~ ., data = se_train) |> 
#   step_rm(IOTF4,
#           IRRELFAS,
#           IRFAS,
#           lifesat,
#           seqno_int
#   ) #maybe later 2check same as slected vars agove
# 
# lr_spec <- logistic_reg() %>%
#   set_engine("glm") %>%
#   set_mode("classification")
# 
# lr_workflow <- workflow() %>%
#   add_recipe(lr_recipe2) %>%
#   add_model(lr_spec)
# 
# final_fit <- last_fit(
#   lr_workflow,
#   split = se_split
# )
# 
# model_inference <- final_fit %>% 
#   extract_fit_engine() %>% 
#   tidy(exponentiate = FALSE, conf.int = TRUE) %>% 
#   mutate(
#     odds_ratio   = exp(estimate),
#     or_conf_low  = exp(conf.low),
#     or_conf_high = exp(conf.high)
#   )
# 
# lr_coeffs <- 
#     model_inference %>%
#     transmute(term,
#               coefficient = round(estimate, 3),
#               OR = round(odds_ratio,3),
#               p.value = round(p.value,10),
#               ORlow = round(or_conf_low,3),
#               ORhigh = round(or_conf_high,3)
#     )
# 
# lr_coeffs
# 
# # for latex
# tt_lr_coeffs <- tt(lr_coeffs) 
# save_tt(tt_lr_coeffs, 
#         output = "./outputsR/tinytables/tt_lr_coeffs.tex", overwrite = TRUE)
# 

# get LR shap


# ------------------------------------------------------------
# THE classification tree
# ------------------------------------------------------------

# is LR the issue?
hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "Yes")

tree_tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(), 
    min_n = tune() 
  ) |> 
  set_engine("rpart") |> 
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          min_n(),
                          levels = 5
) #5^3=125 models

set.seed(67)
folds_10cv_3x <- vfold_cv(se_train, 
                          v= 10,
#                          repeats = 3,
                          strata = mentalissue)

tree_recipe <- 
  recipe(mentalissue ~ ., data = se_train) |> 
  step_rm(IOTF4,
          IRRELFAS,
          lifesat,
          seqno_int
  ) 

#Tune a workflow() that bundles together a model specification 
# and a recipe or model preprocessor.
set.seed(67)
tree_wf <- workflow() |>
  add_model(tree_tune_spec) |>
  add_recipe(tree_recipe)

#tune_grid() to fit models at all the different values 
# we chose for each tuned hyperparameter
(Start <- Sys.time())
tree_res <- 
  tree_wf |> 
  tune_grid(
    resamples = folds_10cv_3x,
    grid = tree_grid
  )
(Sys.time() - Start) #20 mins

#tree_res |> collect_metrics()

# get the winner tree, finalize wf and get last fit and last tree
#tree_res |> show_best(metric = "roc_auc")
best_tree <- tree_res |> select_best(metric = "roc_auc")
best_tree

final_tree_wf <- tree_wf |> finalize_workflow(best_tree)

final_tree_fit <- final_tree_wf |> last_fit(se_split) 

final_tree <- extract_workflow(final_tree_fit)
final_tree

# see roc
final_tree_fit |>
  collect_predictions() |>
  roc_curve(mentalissue, .pred_No) |>
  autoplot() +
  theme_minimal()

# Calculate metrics with helper function for train/test
get_split_metrics <- list(
  train = se_train,
  test  = se_test 
) %>% 
  purrr::map_df(function(df) {
    # Generate class and probability predictions
    predict(final_tree, new_data = df, type = "class") %>% 
      bind_cols(predict(final_tree, new_data = df, type = "prob")) %>% 
      bind_cols(df) %>% 
      # Calculate the metric set
      metric_set(bal_accuracy, accuracy, j_index, roc_auc, brier_class)(
        truth       = mentalissue, 
        estimate    = .pred_class, 
        .pred_Yes, 
        event_level = "second" # Adjust to "second" if "Yes" is your 2nd factor level
      )
  }, .id = "dataset"
  )

tree_metrics <- 
get_split_metrics |>
  select(dataset, .metric, .estimate) |>
  pivot_wider(names_from = dataset,
              values_from = .estimate)# %>%

# for latex
#tt_tree_metrics <- tt(tree_metrics) 
#save_tt(tt_tree_metrics, 
#        output = "./outputsR/tinytables/tt_tree_metrics.tex", overwrite = TRUE)

#dejarme de pajaz
#models_performance combined table for latex
models_metrics <- bind_rows(
  tree_metrics %>% mutate(model = "classif tree"),
  lr_metrics %>% mutate(model = "log reg")
) %>%
  arrange(.metric)

# for latex
tt_models_metrics <- tt(models_metrics) 
save_tt(tt_models_metrics, 
        output = "./outputsR/tinytables/tt_models_metrics.tex", 
        overwrite = TRUE)

# # --------------------------------------
# # for test set, for each class (Y/N) get precision, recall, f1
 
tree_test_preds <- collect_predictions(final_tree_fit)
class_metrics <- metric_set(f_meas, precision, recall)

(metrics_class1 <- class_metrics(
  tree_test_preds,
  truth = mentalissue,
  estimate = .pred_class,
  event_level = "first"
) %>%
    mutate(class = "NO issues")
)

(metrics_class2 <- class_metrics(
  tree_test_preds,
  truth = mentalissue,
  estimate = .pred_class,
  event_level = "second"
) %>%
    mutate(class = "YES issues")
)

tree_test_metrics <-
  bind_rows(metrics_class1, metrics_class2) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric,
              values_from = .estimate
  )

# combine test metrics in one
test_metrics <- bind_rows(
  tree_test_metrics %>% mutate(model = "classifTree"),
  lr_test_metrics %>% mutate(model = "logreg")
) %>%
  arrange(class)

test_metrics

tt_test_metrics <- tt(test_metrics)
save_tt(tt_test_metrics,
        output = "./outputsR/tinytables/tt_test_metrics.tex", 
        overwrite = TRUE)

# ------------------------------------------------------
# plot the tree 
raw_final_tree <- extract_fit_engine(final_tree)
raw_final_tree

rpart.plot(
  raw_final_tree,
  roundint = FALSE,
  #  font = 4,
  tweak = 1.5,  
  type = 5,                    # Clear split labels directly on lines
  extra = 100,
  box.palette = list("#c0392b", "#2980b9") 
)

# can't read so create a massive pdf for zooming
pdf(
  file = "outputsR/giant_zoomable_tree.pdf", 
  width = 24,            # Massive 2-foot wide canvas
  height = 18,           # 1.5-foot tall canvas
  useDingbats = FALSE    # Ensures text renders perfectly across PDF readers
)

par(mar = c(0.5, 0.5, 0.5, 0.5)) 
rpart.plot(
  raw_final_tree,
  roundint = FALSE,
  tweak = 0.9,           # Dropped significantly so text scales with the 24" canvas
  type = 5,                    
  extra = 100,
  box.palette = list("#c0392b", "#2980b9") 
)
dev.off()

# after trial and error prune this so it plots and shows
pruned_tree <- prune(raw_final_tree, cp = 0.003)

png(
  filename = "outputsR/pruned_tree.png", 
  width = 2400,          # 7.0 inches * 300 DPI
  height = 1800,         # 5.25 inches * 300 DPI (4:3 Aspect Ratio)
  res = 300              # Standard journal publication DPI
)

rpart.plot(
  pruned_tree,
  roundint = FALSE,
  #  font = 4,
  tweak = 1.5,  
  type = 5,                    # Clear split labels directly on lines
  extra = 100,
  box.palette = list("#2980b9", "#c0392b") 
)
dev.off()

# tree metrics
max(rpart:::tree.depth(as.numeric(rownames(raw_final_tree$frame)))) #depth
sum(raw_final_tree$frame$var == "<leaf>") #leaves
nrow(raw_final_tree$frame) #nodes


# -------------------------------------------------------------------------
# La foresta XBG 
# -------------------------------------------------------------------------

# https://juliasilge.com/blog/xgboost-tune-volleyball/

#sanity checks
#2check level is right so its same as tree
table(se_train$mentalissue)/nrow(se_train)

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),                     
  sample_size = tune(), 
  mtry = tune(),         
  learn_rate = tune()                          
) %>%
  set_engine("xgboost", scale_pos_weight = 2.7) %>% #73/27
#  set_engine("xgboost") %>%
  set_mode("classification")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), se_train),
  learn_rate(),
  size = 25 # from 50, 5
)

# xgboost needs numeric predictors 
# step_dummy() one-hot-encodes any factors
xgb_recipe <- tree_recipe %>%
  step_dummy(all_nominal_predictors())

xgb_wf <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(xgb_spec)

set.seed(67)
xgb_folds <- vfold_cv(se_train, 
                      v = 10, #from 10 10, #change later to 10
#                      repeats = 3,
                      strata = mentalissue
)

doParallel::registerDoParallel()
(start <- Sys.time())
set.seed(67)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = xgb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)
Sys.time() - start #18 mins

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

final_fit_xgb <- last_fit(final_xgb, se_split)
final_fit_xgb

# (This gives you the actual trained model needed for SHAP!)
fitted_xgb_fit <- fit(final_xgb, data = se_train)
fitted_xgb_fit

# see roc
final_fit_xgb |>
  collect_predictions() |>
  roc_curve(mentalissue, .pred_No) |>
  autoplot() +
  theme_minimal()

# Calculate metrics with helper function for train/test
get_split_metrics <- list(
  train = se_train,
  test  = se_test 
) %>% 
  purrr::map_df(function(df) {
    # Generate class and probability predictions
    predict(fitted_xgb_fit, new_data = df, type = "class") %>% 
      bind_cols(predict(fitted_xgb_fit, new_data = df, type = "prob")) %>% 
      bind_cols(df) %>% 
      # Calculate the metric set
      metric_set(bal_accuracy, accuracy, j_index, roc_auc, brier_class)(
        truth       = mentalissue, 
        estimate    = .pred_class, 
        .pred_Yes, 
        event_level = "second" # Adjust to "second" if "Yes" is your 2nd factor level
      )
  }, .id = "dataset"
  )

xgb_metrics <- 
  get_split_metrics |>
  select(dataset, .metric, .estimate) |>
  pivot_wider(names_from = dataset,
              values_from = .estimate)# %>%
xgb_metrics


# for latex
#tt_tree_metrics <- tt(tree_metrics) 
#save_tt(tt_tree_metrics, 
#        output = "./outputsR/tinytables/tt_tree_metrics.tex", overwrite = TRUE)

#dejarme de pajaz
#models_performance combined table for latex
models_metrics <- bind_rows(
  tree_metrics %>% mutate(model = "Tree"),
  lr_metrics %>% mutate(model = "LogReg"),
  xgb_metrics %>% mutate(model = "XGB")
) %>%
  arrange(.metric) %>%
  mutate(.metric = gsub("_", "", .metric ))

models_metrics


# for latex
tt_models_metrics <- tt(models_metrics) 
save_tt(tt_models_metrics, 
        output = "./outputsR/tinytables/tt_models_metrics.tex", 
        overwrite = TRUE)

# # --------------------------------------
# # for test set, for each class (Y/N) get precision, recall, f1

xgb_test_preds <- collect_predictions(final_fit_xgb)
class_metrics <- metric_set(f_meas, precision, recall)

(metrics_class1 <- class_metrics(
  xgb_test_preds,
  truth = mentalissue,
  estimate = .pred_class,
  event_level = "first"
) %>%
    mutate(class = "NO issues")
)

(metrics_class2 <- class_metrics(
  xgb_test_preds,
  truth = mentalissue,
  estimate = .pred_class,
  event_level = "second"
) %>%
    mutate(class = "YES issues")
)

xgb_test_metrics <-
  bind_rows(metrics_class1, metrics_class2) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric,
              values_from = .estimate
  )

# combine test metrics in one
test_metrics <- bind_rows(
  tree_test_metrics %>% mutate(model = "Tree"),
  lr_test_metrics %>% mutate(model = "LogReg"),
  xgb_test_metrics %>% mutate(model = "XGB")
  ) %>%
  arrange(class)

test_metrics

tt_test_metrics <- tt(test_metrics)
save_tt(tt_test_metrics,
        output = "./outputsR/tinytables/tt_test_metrics.tex", 
        overwrite = TRUE)


# ---------------------------------------------------------------------
# let's shap for all
# ---------------------------------------------------------------------

# random explain and background datasets
set.seed(67)
X_explain <- se_test %>% 
  slice_sample(n = 100) %>% #increase? 100-500?
  select(-mentalissue)

set.seed(67)
bg_X <- se_train %>% 
  select(-mentalissue) %>% 
  slice_sample(n = 50) #100-500 sweetspot?

# log reg sv
lr_ext <- extract_workflow(final_lr_fit)

(start <- Sys.time())
shap_output <- kernelshap(
  lr_ext,
  #final_tree, 
  X = X_explain, 
  bg_X = bg_X, 
  type = "prob"
)
Sys.time() - start #30 seg

lr_sv <- shapviz(shap_output)

# tree sv
#lr_ext <- extract_workflow(final_lr_fit)
(start <- Sys.time())
shap_output <- kernelshap(
  final_tree, 
  X = X_explain, 
  bg_X = bg_X, 
  type = "prob"
)
Sys.time() - start #

tree_sv <- shapviz(shap_output)

# xgb sv
# keep for now but doesn't really work for imp cause separates factor
xgb_fitted <- extract_fit_engine(fitted_xgb_workflow)

xgb_baked_data <- extract_recipe(fitted_xgb_workflow) %>% 
  bake(new_data = se_test) %>% 
  select(-mentalissue) %>% 
  as.matrix()

xgb_sv <- shapviz(xgb_fitted, X_pred = xgb_baked_data)

sv_importance(xgb_sv, kind = "bar") + theme_minimal()
#sv_importance(xgb_sv, kind = "beeswarm") + theme_minimal()

# sv for xgb collapsed
# 1. Your existing matrix extraction code
#xgb_fitted <- extract_fit_engine(fitted_xgb_workflow)

# xgb_baked_data <- extract_recipe(fitted_xgb_workflow) %>% 
#   bake(new_data = se_test) %>% 
#   select(-mentalissue) %>% 
#   as.matrix()

# 2. DYNAMICALLY CREATE THE COLLAPSE LIST
# Find columns that were dummy encoded (containing an underscore like 'age_' or 'country_')
cols <- colnames(xgb_baked_data)
dummy_prefixes <- unique(sub("_.*", "", cols[grep("_", cols)]))

# Generate the named list grouping the dummy fields by their base factor name
collapse_list <- lapply(dummy_prefixes, function(p) cols[startsWith(cols, paste0(p, "_"))])
names(collapse_list) <- dummy_prefixes

# 3. FIXED SHAPVIZ CALL: Pass the collapse list here
xgb_sv_collapsed <- shapviz(
  xgb_fitted, 
  X_pred = xgb_baked_data, 
  X = se_test %>% select(-mentalissue),
  collapse = collapse_list
)

# var imp plots
sv_importance(tree_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("ClassTree")
sv_importance(lr_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("LogReg")
sv_importance(xgb_sv_collapsed, kind = "bar") + theme_minimal() + ggtitle("XGB Collapsed")
sv_importance(xgb_sv, kind = "bar") + theme_minimal() + ggtitle("XGB NOT Collap")


# # same plots but as files for latext
# # Define common dimensions for your LaTeX layout (in inches)
# plot_width <- 6
# plot_height <- 4
# 
# pdf("outputsR/sv_imp_classtree.pdf", width = plot_width, height = plot_height)
# sv_importance(tree_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("ClassTree")
# dev.off()
# 
# pdf("outputsR/sv_imp_logreg.pdf", width = plot_width, height = plot_height)
# sv_importance(lr_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("LogReg")
# dev.off()
# 
# pdf("outputsR/sv_imp_xgb_collapsed.pdf", width = plot_width, height = plot_height)
# sv_importance(xgb_sv_collapsed, kind = "bar") + theme_minimal() + ggtitle("XGB Collapsed")
# dev.off()

# patched pues
library(patchwork) # Handles side-by-side layout seamlessly

p1 <- sv_importance(tree_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("ClassTree")
p2 <- sv_importance(lr_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("LogReg")
p3 <- sv_importance(xgb_sv_collapsed, kind = "bar") + theme_minimal() + ggtitle("XGB Collapsed")

combined_width  <- 10 
combined_height <- 10

# 3. Save as a single combined PDF file
pdf("outputsR/sv_imp_combined.pdf", width = combined_width, height = combined_height)
# The '+' operator from patchwork places them side-by-side automatically
# 'plot_layout(nrow = 1)' forces them into a single row
combined_plot <- p1 + p2 + p3 + plot_layout(nrow = 2)
print(combined_plot)
dev.off()


# bee plots
# sv_importance(tree_sv$.pred_Yes, kind = "beeswarm") + theme_minimal() + ggtitle("ClassTree")
# sv_importance(lr_sv$.pred_Yes, kind = "beeswarm") + theme_minimal() + ggtitle("LogReg")
# sv_importance(xgb_sv_collapsed, kind = "beeswarm") + theme_minimal() + ggtitle("XGB Collapsed")
# sv_importance(xgb_sv, kind = "beeswarm") + theme_minimal() + ggtitle("XGB NOT Collap")

# waterfall plot examples to see someone with diff mental pred

(wat1 <- sv_waterfall(tree_sv$.pred_Yes, 
             max_display = 12,
             row_id = 2) + 
  theme_minimal() + ggtitle("NO Mental issue"))

(wat2 <- sv_waterfall(tree_sv$.pred_Yes, 
             max_display = 12,
             row_id = 8) + 
  theme_minimal() + ggtitle("YES Mental issue"))

combined_width  <- 10 
combined_height <- 10

# 3. Save as a single combined PDF file
pdf("outputsR/sv_waterfall_combined.pdf", width = combined_width, height = combined_height)
# The '+' operator from patchwork places them side-by-side automatically
# 'plot_layout(nrow = 1)' forces them into a single row
combined_plot <- wat1 + wat2 + plot_layout(nrow = 1)
print(combined_plot)
dev.off()





# ------------------------------------------------------------------------
# corels
# ------------------------------------------------------------------------

#install.packages("corels")
#install.packages("tidycorels")
require(corels)
#require(tidycorels)

https://systopia.cs.ubc.ca/rule_lists

https://users.cs.duke.edu/~cynthia/code.html

https://corels.cs.ubc.ca/corels/run.html

https://www.google.com/search?q=corels+in+r+for+classification+tutorial&sca_esv=fa8fc3aacbaacd94&biw=1021&bih=527&sxsrf=APpeQntTHPozQxIORdtgxHiJEJtLIumCbw%3A1791137645049&ei=YJfCaoiGOPjBkPIPg_DbQQ&uact=5&sclient=gws-wiz-serp&fbs=ABfTbFVyMZGZf1hfvX9uKjN_-G8cxpBkeIeqYwoCbfNVc4vKEyijPgk5kCEDN_PT5No9YDQ8aujKqBsqXMyy1vj2IXgmPtfV_6GrZLubApcjNsjgAG-T3pFVvhwufR6jNURhS0HPFyagDKRz8DBgNZ68OFTlp8bI3rofKcMJ4fjZZagvjCabW4z0YG6WgElKZ-ZLAn77xJB2_z0gznx7Lh2jCdSsLQFWzw&aep=10&ntc=1&mstk=AUtExfDLjJfB3KdjobHerAFo9yx5B9LXoOjrOPH3Eqg0DHMXBGyFgjfi-7sq8NN6FljS411eMRgsWbQH9cX0V0eQR3tYUcl-wND8ktrkFNl-vIQWVRO7ezaai8np0XnVgaHUOHmTvjuBxklhPztzii24J3nRXFULRFmpK92WKCmf1qL4DFC9SAAbfiBr-Dboh1XJvZm-0A82aTZ3veJ2dJpIjHSh-SenEd5Oj9PMrhzZUozdw1zWtRU3j31HG7ffL7voM_u9HBdm7-rzuKKZNxzBwFNSoBuhPFZGPC3Ds707iabDLHsPdrD6bzmbhmIq5eMzE8w4K4bttk_1XQ&aioh=3&csuir=1&cs=0&mtid=75nCavKAMqiDgLQP2e6fuAM&udm=50



https://search.r-project.org/CRAN/refmans/corels/html/corels.html
library(recipes)

# 1. Define the recipe (convert all nominal/categorical variables)
recipe_spec <- recipe(~ age + pmsu, data = se_train) %>%
  #step_rm(seqno_int, country) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# 2. Prep and bake (apply) the recipe to get the 0/1 dataframe
se_train_dummies <- prep(recipe_spec) %>% bake(new_data = NULL)

se_train_label <- se_train$mentalissue

library(corels)

logdir <- tempdir()
logdir <- "C:/temp"
rules_file <- system.file("sample_data", "compas_train.out", package="corels")
labels_file <- system.file("sample_data", "compas_train.label", package="corels")
meta_file <- system.file("sample_data", "compas_train.minor", package="corels")

stopifnot(file.exists(rules_file),
          file.exists(labels_file),
          file.exists(meta_file),
          dir.exists(logdir))

corels(rules_file, labels_file, logdir, meta_file,
       verbosity_policy = "loud",
       regularization = 0.015,
       curiosity_policy = 2,   # by lower bound
       map_type = 1) 	   # permutation map

cat("See ", logdir, " for result file.")









