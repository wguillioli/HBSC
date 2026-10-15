# HBSC 
# Updated: 2026-10-14

# ---------------------------------------------------
# project setup
# ---------------------------------------------------

rm(list = ls())

prj_fldr <- "C:/MisLocalFiles/Github/HBSC/"
setwd(prj_fldr)

options(scipen = 999)


# ---------------------------------------------------
# workspace stuff
# ---------------------------------------------------

#img_file <- paste0(prj_fldr, "/Rimages/hbsc_wkspace_20261014.RData")
#load(file = img_file)
#save.image(file = img_file)


# ---------------------------------------------------
# packages
# ---------------------------------------------------

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
# Logistic regression
# ------------------------------------------------------------

# remove the id for good
hbsc_se <- hbsc_se %>% 
  select(-c(seqno_int,
            IOTF4,
            IRRELFAS,
            lifesat)
  )

# relevel for lr
hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")
levels(hbsc_se$mentalissue)

#split the data
set.seed(67)
hbsc_se_split  <- initial_split(hbsc_se, 
                           strata = mentalissue)
hbsc_se_train <- training(hbsc_se_split)
hbsc_se_test <- testing(hbsc_se_split)

# just checking
nrow(hbsc_se_train) + nrow(hbsc_se_test) == nrow(hbsc_se) #must be TRUE
round(table(hbsc_se_train$mentalissue) / nrow(hbsc_se_train),5)
round(table(hbsc_se_test$mentalissue) / nrow(hbsc_se_test), 5) #must be same

#redoing the good old way to confirm what glmnet tidymodels did
lr1 <- glm(mentalissue ~ . 
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
           #-IOTF4
           #-IRRELFAS
           #-lifesat
           #-seqno_int
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
  #step_rm(IOTF4,
   #       IRRELFAS,
    #      lifesat,
          #country, # refit without country to see metrics
     #     seqno_int
      #    ) |>
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


#### get LR shap

#background data from train
set.seed(67)
bg_X <- hbsc_se_train %>% 
  select(-mentalissue) %>% 
  slice_sample(n = 500) #100-500 sweetspot?

#rows to explain from test
set.seed(67)
X_explain <- hbsc_se_test %>% 
  slice_sample(n = 1000) %>% #increase? 
  select(-mentalissue)

#calcualte shap with kernelshap
(start <- Sys.time())
shap_values_lr <- kernelshap(
  lr1,
  X = X_explain, 
  bg_X = bg_X, 
  type = "link" #link scale so shap values add up to log-odds
)
Sys.time() - start #10 secs (100), 3 mins(1k)

# get shapviz object
sv_lr <- shapviz(shap_values_lr)

# global plots
sv_importance(sv_lr, kind = "bar") + 
  theme_minimal() + 
  ggtitle("Global Importance LogReg")

#sv_interaction(sv_lr)

# global plot - same info but relative as %
global_importance <- colMeans(abs(sv_lr$S)) #mean abs shap of each feature
relative_shap <- data.frame(
  Feature = names(global_importance),
  Mean_Abs_SHAP = global_importance
) %>%
  mutate(
    # Normalize by dividing by the total sum of all SHAP impacts
    Relative_Importance_Pct = (Mean_Abs_SHAP / sum(Mean_Abs_SHAP)) * 100
  ) %>%
  arrange(desc(Relative_Importance_Pct))

ggplot(relative_shap, aes(x = reorder(Feature, Relative_Importance_Pct), y = Relative_Importance_Pct)) +
  geom_col(fill = "#4682B4", width = 0.6) + # Clean steel blue bars
  geom_text(aes(label = paste0(round(Relative_Importance_Pct, 1), "%")), 
            hjust = -0.2, size = 3.5) + # Adds percentage labels next to bars
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Leaves breathing room for text labels
  labs(
    x = "Survey Feature",
    y = "Relative Contribution to Model Decisions (%)",
    title = "Relative Global Feature Importance (Normalized SHAP)",
    subtitle = "Expressed as a percentage of overall absolute log-odds impact"
  ) +
  theme_minimal(base_size = 13) #+
  #theme(
  #  panel.grid.minor = element_blank(),
  #  panel.grid.major.y = element_blank()
  #)


# i'll bite and recalculate with probabilities
# 1. Re-calculate SHAP values on the "response" (probability) scale
# shap_prob <- additive_shap(
#   object = lr1, 
#   X = X_explain, 
#   type = "response" # <-- Crucial switch to probabilities
# )

# 2 mins ish
shap_values_lr_probs <- kernelshap(
  lr1,
  X = X_explain, 
  bg_X = bg_X, 
  type = "response" #probs instead of link
)

sv_prob <- shapviz(shap_values_lr_probs)

# 2. Extract the raw matrix and calculate the mean absolute probability shift
prob_importance <- colMeans(abs(sv_prob$S))

# 3. Convert to a clean data frame and multiply by 100 for percentage points
plot_data_prob <- data.frame(
  Feature = names(prob_importance),
  Mean_Abs_Prob_Shift = prob_importance * 100 # Convert to percentage points
) %>%
  arrange(desc(Mean_Abs_Prob_Shift))

# View the raw percentage data frame
print(plot_data_prob)

ggplot(plot_data_prob, aes(x = reorder(Feature, Mean_Abs_Prob_Shift), y = Mean_Abs_Prob_Shift)) +
  geom_col(fill = "blue", width = 0.6) + # Elegant teal bars
  geom_text(aes(label = paste0("+/- ", round(Mean_Abs_Prob_Shift, 1), "%")), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs(
    x = "Survey Feature",
    y = "Average Shift in Probability (Percentage Points)",
    title = "Global Feature Importance (Probability Scale)",
    subtitle = "How many percentage points a feature pushes the outcome away from the baseline average"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

sv_importance(sv_lr, kind = "beeswarm") + theme_minimal() + ggtitle("BeePlot LogReg")

sv_dependence(sv_lr, v = "pmsu") +
  theme_minimal()

sv_dependence(sv_lr, v = "country")

#might be useful latger
sv_dependence(sv_lr, 
              v = c("pmsu", "country", "age", "gender"), 
              share_y = TRUE)



#sv_dependence(sv_lr, v = "pmsu", color_var = "age")

# for (i in 1:20){
#   print(i)
#   print(sv_waterfall(sv_lr, row_id = i) + ggtitle(i))
# }

sv_waterfall(sv_lr, row_id = 42, max_display = ncol(sv_lr$X))
sv_waterfall(sv_lr, row_id = 3, max_display = ncol(sv_lr$X))

# buenos ejemplos serian
# YES issues 35, 42, 
# NO issues 13, 3

#library(shapviz)
library(patchwork)
#library(ggplot2)

# 1. Generate and save the individual waterfall plots
p1 <- sv_waterfall(sv_lr, row_id = 42, max_display = ncol(sv_lr$X)) + ggtitle("Respondent #42 Profile")
p2 <- sv_waterfall(sv_lr, row_id = 3, max_display = ncol(sv_lr$X))  + ggtitle("Respondent #3 Profile")

# 2. Glue them side by side using patchwork
combined_waterfalls <- p1 + p2

# 3. Add a global title and clean up the layout (Optional but recommended)
combined_waterfalls + 
  plot_annotation(
    title = "Comparing Individual Survey Explanations",
    subtitle = "Side-by-side log-odds breakdown of local predictions",
    theme = theme(plot_title = element_text(size = 16, face = "bold"))
  )


# ----------------------------------------------------------
#   EL ARBOL 10/14
# ------------------------------------------------------------

# Put No as base, since I want to pred Yes
hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")
levels(hbsc_se$mentalissue) #No first

# set and validate train/test sets
set.seed(67)
hbsc_se_split <- initial_split(hbsc_se, strata = mentalissue)
hbsc_se_train <- training(hbsc_se_split)
hbsc_se_test <- testing(hbsc_se_split)

nrow(hbsc_se_train) + nrow(hbsc_se_test) == nrow(hbsc_se) #has to be true
table(hbsc_se$mentalissue)/nrow(hbsc_se) #.73 vs .27
table(hbsc_se_train$mentalissue)/nrow(hbsc_se_train)
table(hbsc_se_test$mentalissue)/nrow(hbsc_se_test) 

set.seed(67)
hbsc_folds <- vfold_cv(hbsc_se_train, v= 10, strata = mentalissue)

#basic baseline tree
tree_spec <- decision_tree() |> 
  set_engine("rpart") |> 
  set_mode("classification")

final_fit <- tree_spec |> 
  fit(mentalissue ~ ., data = hbsc_se_train)

# fit a good tuned tree
# ?decision_tree confirms 3 parameters to tune
tree_spec <- 
   decision_tree(
     cost_complexity = tune(), #cp (positive number)
     tree_depth = tune(), #max depth of tree
     min_n = tune() #min # of data points in a node
   ) |> 
   set_engine("rpart") |> 
   set_mode("classification")
 
# grid_regular for even space
tree_grid <- grid_regular(cost_complexity(),
                           tree_depth(),
                           min_n(),
                           levels = 5 #5^3=125 models
) 
dim(tree_grid)
summary(tree_grid)
tree_grid

my_metrics <- metric_set(bal_accuracy, accuracy, roc_auc, brier_class, j_index)

# fit el arbol pues papa
doParallel::registerDoParallel()

set.seed(67)
system.time(
tree_rs <- tune_grid(
  tree_spec,
  mentalissue ~ .,
  resamples = hbsc_folds,
  grid = tree_grid,
  metrics = my_metrics
  )
)
#366

tree_rs
tree_rs |> collect_metrics()
autoplot(tree_rs)

# get the winner tree and last tree
tree_rs |> show_best(metric = "roc_auc")
best_tree <- tree_rs |> select_best(metric = "roc_auc")
best_tree

final_tree_spec <- tree_spec |> 
  finalize_model(best_tree)
final_tree_spec

final_fit <- final_tree_spec |> 
  fit(formula = mentalissue ~ ., data = hbsc_se_train)
final_fit

# get eval metrics on test and set
#voy con baseline metrics
# https://juliasilge.com/blog/wind-turbine/
train_results <- hbsc_se_train %>%
  select(mentalissue) %>% 
  bind_cols(
    predict(final_fit, new_data = hbsc_se_train, type = "class"),
    predict(final_fit, new_data = hbsc_se_train, type = "prob")
  )
head(train_results)

test_results <- hbsc_se_test %>%
  select(mentalissue) %>% 
  bind_cols(
    predict(final_fit, new_data = hbsc_se_test, type = "class"),
    predict(final_fit, new_data = hbsc_se_test, type = "prob")
  )
head(test_results)

# compute eval metrics for train/test
evaluation_metrics <- metric_set(accuracy, bal_accuracy, roc_auc, j_index, brier_class)

train_metrics <- train_results %>% 
  evaluation_metrics(truth = mentalissue, estimate = .pred_class, .pred_Yes, event_level = "second") %>%
  select(.metric, train_estimate = .estimate)

test_metrics <- test_results %>% 
  evaluation_metrics(truth = mentalissue, estimate = .pred_class, .pred_Yes, event_level = "second") %>%
  select(.metric, test_estimate = .estimate)

left_join(train_metrics, test_metrics, by = ".metric")


# # # --------------------------------------
# # # for test set, for each class (Y/N) get precision, recall, f1
# 
# tree_test_preds <- collect_predictions(final_tree_fit)
# class_metrics <- metric_set(f_meas, precision, recall)
# 
# (metrics_class1 <- class_metrics(
#   tree_test_preds,
#   truth = mentalissue,
#   estimate = .pred_class,
#   event_level = "first"
# ) %>%
#     mutate(class = "NO issues")
# )
# 
# (metrics_class2 <- class_metrics(
#   tree_test_preds,
#   truth = mentalissue,
#   estimate = .pred_class,
#   event_level = "second"
# ) %>%
#     mutate(class = "YES issues")
# )
# 
# tree_test_metrics <-
#   bind_rows(metrics_class1, metrics_class2) %>%
#   select(-.estimator) %>%
#   pivot_wider(names_from = .metric,
#               values_from = .estimate
#   )
# 
# # combine test metrics in one
# test_metrics <- bind_rows(
#   tree_test_metrics %>% mutate(model = "classifTree"),
#   lr_test_metrics %>% mutate(model = "logreg")
# ) %>%
#   arrange(class)
# 
# test_metrics
 
# # ------------------------------------------------------
# # plot the tree 
# raw_final_tree <- extract_fit_engine(final_tree)
# raw_final_tree
# 
# rpart.plot(
#   raw_final_tree,
#   roundint = FALSE,
#   #  font = 4,
#   tweak = 1.5,  
#   type = 5,                    # Clear split labels directly on lines
#   extra = 100,
#   box.palette = list("#c0392b", "#2980b9") 
# )
# 
# # can't read so create a massive pdf for zooming
# pdf(
#   file = "outputsR/giant_zoomable_tree.pdf", 
#   width = 24,            # Massive 2-foot wide canvas
#   height = 18,           # 1.5-foot tall canvas
#   useDingbats = FALSE    # Ensures text renders perfectly across PDF readers
# )
# 
# par(mar = c(0.5, 0.5, 0.5, 0.5)) 
# rpart.plot(
#   raw_final_tree,
#   roundint = FALSE,
#   tweak = 0.9,           # Dropped significantly so text scales with the 24" canvas
#   type = 5,                    
#   extra = 100,
#   box.palette = list("#c0392b", "#2980b9") 
# )
# dev.off()
# 
# # after trial and error prune this so it plots and shows
# pruned_tree <- prune(raw_final_tree, cp = 0.003)
# 
# png(
#   filename = "outputsR/pruned_tree.png", 
#   width = 2400,          # 7.0 inches * 300 DPI
#   height = 1800,         # 5.25 inches * 300 DPI (4:3 Aspect Ratio)
#   res = 300              # Standard journal publication DPI
# )
# 
# rpart.plot(
#   pruned_tree,
#   roundint = FALSE,
#   #  font = 4,
#   tweak = 1.5,  
#   type = 5,                    # Clear split labels directly on lines
#   extra = 100,
#   box.palette = list("#2980b9", "#c0392b") 
# )
# dev.off()
# 
# # tree metrics
# max(rpart:::tree.depth(as.numeric(rownames(raw_final_tree$frame)))) #depth
# sum(raw_final_tree$frame$var == "<leaf>") #leaves
# nrow(raw_final_tree$frame) #nodes
# 






# -------------------------------------------------------------------------
# EL Random Forest, hoy si
# -------------------------------------------------------------------------

# fit baseline with defaults and get all metrics
# compare with metrics of tuned
# ver julia vid
# address class imb

hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")
levels(hbsc_se$mentalissue)

#split the data
set.seed(67)
hbsc_se_split  <- initial_split(hbsc_se, 
                                strata = mentalissue)
hbsc_se_train <- training(hbsc_se_split)
hbsc_se_test <- testing(hbsc_se_split)

# fit a baseline RF with defaults
rf_spec <- rand_forest(trees = 500) |> 
  set_mode("classification") |> 
  set_engine("ranger", num.threads = NULL, seed = 67)

# since it's heavy overfit, fit a constrained model that google reco
rf_spec <- rand_forest(
  mtry = 3,           # Look at only 3 features at a time (de-correlates trees)
  trees = 300,        # Stable ensemble size
  min_n = 50          # Stop splitting if a node has <= 50 rows (prevents overfitting)
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# try to address class imabalance using Method 2: Internal Class Weights (Engine-Level)
# Calculate inverse class weights based on your training data
# This gives the rare class a higher mathematical "importance" flag
class_counts <- table(hbsc_se_train$mentalissue)
weights <- sum(class_counts) / (2 * class_counts)
weights

rf_spec <- rand_forest(
  mtry = 3,
  trees = 300,
  min_n = 50
) %>% 
  set_engine("ranger", class.weights = weights) %>% # Pass weights to ranger
  set_mode("classification")

xvars <- hbsc_se_train |> select(-mentalissue) |> names()
xvars

rf_wf <- workflow() |> 
  add_model(rf_spec) |>
  add_formula(reformulate(xvars, "mentalissue"))

rf_fit1 <- rf_wf |> fit(hbsc_se_train)

train_results <- hbsc_se_train %>%
  select(mentalissue) %>% 
  bind_cols(
    predict(rf_fit1, new_data = hbsc_se_train, type = "class"),
    predict(rf_fit1, new_data = hbsc_se_train, type = "prob")
  )
head(train_results)

test_results <- hbsc_se_test %>%
  select(mentalissue) %>% 
  bind_cols(
    predict(rf_fit1, new_data = hbsc_se_test, type = "class"),
    predict(rf_fit1, new_data = hbsc_se_test, type = "prob")
  )
head(test_results)

# compute eval metrics for train/test
evaluation_metrics <- metric_set(accuracy, bal_accuracy, roc_auc, j_index, brier_class)

train_metrics <- train_results %>% 
  evaluation_metrics(truth = mentalissue, estimate = .pred_class, .pred_Yes, event_level = "second") %>%
  select(.metric, train_estimate = .estimate)

test_metrics <- test_results %>% 
  evaluation_metrics(truth = mentalissue, estimate = .pred_class, .pred_Yes, event_level = "second") %>%
  select(.metric, test_estimate = .estimate)

performance_comparison <- left_join(train_metrics, test_metrics, by = ".metric")
performance_comparison







# # FIT TUNED 
# #library(tidymodels)
# library(ranger)
# library(doParallel) 
# 
# # # ------------------------------------------------------------------------
# # # 1. SETUP & DATA SPLIT
# # # ------------------------------------------------------------------------
# # set.seed(123)
# # 
# # # Split data into training and testing sets
# # data_split <- initial_split(iris, prop = 0.8, strata = Species)
# # train_data <- training(data_split)
# # test_data  <- testing(data_split)
# # 
# # # Create 5-Fold Cross-Validation resamples for tuning
# # folds <- vfold_cv(train_data, v = 5, strata = Species)
# 
# # Flag mtry, trees, and min_n for tuning
# rf_spec <- rand_forest(
#   mtry  = tune(),
#   trees = tune(),  
#   min_n = tune()
# ) %>% 
#   set_mode("classification") %>% 
#   set_engine("ranger", importance = "impurity")
# 
# rf_recipe <- recipe(mentalissue ~ ., data = hbsc_se_train)
# 
# rf_workflow <- workflow() %>% 
#   add_recipe(rf_recipe) %>% 
#   add_model(rf_spec)
# 
# rf_params <- extract_parameter_set_dials(rf_spec) %>% 
#   # Finalize ensures mtry fits the dataset dimension
#   finalize(hbsc_se_train) %>% 
#   # Explicitly limit trees to be between 100 and 1000
#   update(trees = trees(range = c(100, 1000)))
# 
# # Generate a space-filling design of 20 unique 3D combinations within boundaries
# rf_grid <- grid_latin_hypercube(rf_params, size = 20)
# 
# # Start parallel backend using available cores minus one
# cl <- makePSOCKcluster(parallel::detectCores() - 1)
# registerDoParallel(cl)
# 
# classification_metrics <- metric_set(accuracy, roc_auc, bal_accuracy)
# 
# tuning_results <- tune_grid(
#   rf_workflow,
#   resamples = folds_10cv,
#   grid = rf_grid,
#   metrics = classification_metrics #metric_set(roc_auc, accuracy)
# )
# 
# # Shutdown parallel workers safely
# stopCluster(cl)
# registerDoSEQ()
# 
# # ------------------------------------------------------------------------
# # 5. SELECT BEST MODEL & FINAL FIT
# # ------------------------------------------------------------------------
# # Pull the best combination according to ROC-AUC
# best_hyperparameters <- select_best(tuning_results, metric = "roc_auc")
# print("Winning Configuration:")
# print(best_hyperparameters)
# 
# # Finalize the workflow by embedding the winning parameters
# final_workflow <- finalize_workflow(rf_workflow, best_hyperparameters)
# 
# # 'last_fit' trains on entire training set and evaluates once on the test set
# final_fit <- last_fit(final_workflow, split = data_split)
# 
# # ------------------------------------------------------------------------
# # 6. ASSESS TEST PERFORMANCE
# # ------------------------------------------------------------------------
# print("Test Set Evaluation Metrics:")
# print(collect_metrics(final_fit))
# 
# print("Test Set Confusion Matrix:")
# final_fit %>% 
#   collect_predictions() %>% 
#   conf_mat(truth = Species, estimate = .pred_class)
# 







# ------------------------------------------------------------------------
# la rf foresta well done
# ------------------------------------------------------------------------

library(tidymodels)
library(ranger)
#library(themis)     # <--- Required for step_downsample()
library(doParallel) 

# ------------------------------------------------------------------------
# 1. SETUP & DATA SPLIT
# ------------------------------------------------------------------------

hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, "Yes", "No")
levels(hbsc_se$mentalissue) #Yes, No - huh?

set.seed(123)
data_split <- initial_split(hbsc_se, prop = 0.8, strata = mentalissue)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Folds must be stratified to preserve minority classes in each slice
rf_folds <- vfold_cv(train_data, v = 10, strata = mentalissue)

# ------------------------------------------------------------------------
# 2. MODEL SPECIFICATION
# ------------------------------------------------------------------------

rf_spec <- rand_forest(
  mtry  = tune(),
  trees = tune(),  
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger", importance = "impurity")

# --- UPDATED RECIPE ---
# We add step_downsample to dynamically balance the classes inside cross-validation
# rf_recipe <- recipe(Species ~ ., data = train_data) %>% 
#   step_downsample(Species, under_ratio = 1.0) # 1.0 means perfectly 50/50 balance

rf_recipe <- recipe(mentalissue ~ ., data = train_data) 

rf_workflow <- workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_spec)

# ------------------------------------------------------------------------
# 3. DEFINE CUSTOM PARAMETER RANGES & GRID
# ------------------------------------------------------------------------
#classification_metrics <- metric_set(accuracy, roc_auc, bal_accuracy)

rf_params <- extract_parameter_set_dials(rf_spec) %>% 
  finalize(train_data) %>% 
  update(trees = trees(range = c(100, 1000)))
# fix to limit predictors to 15 it seems

rf_grid <- grid_latin_hypercube(rf_params, size = 30)
rf_grid

# ------------------------------------------------------------------------
# 4. EXECUTE PARALLEL TUNING
# ------------------------------------------------------------------------

my_metrics <- metric_set(bal_accuracy, 
                         accuracy,
                         roc_auc,
                         brier_class,
                         j_index
)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

system.time(
tuning_results <- tune_grid(
  rf_workflow,
  resamples = rf_folds,
  grid = rf_grid,
  metrics = my_metrics,
  control = control_grid(verbose = TRUE) #works?
)
) #16 mins para grid de 5; 65 mins(30 obs grid)

stopCluster(cl)
registerDoSEQ()

# ------------------------------------------------------------------------
# 5. SELECT BEST MODEL & FINAL COUPLING
# ------------------------------------------------------------------------
# When data is imbalanced, always use bal_accuracy or roc_auc to select the winner!
best_hyperparameters <- select_best(tuning_results, metric = "roc_auc") #bal_accuracy
print("Winning Configuration:")
print(best_hyperparameters)

final_workflow <- finalize_workflow(rf_workflow, best_hyperparameters)
final_model_fit <- fit(final_workflow, data = train_data)

# ------------------------------------------------------------------------
# 6. EVALUATE TRAIN VS TEST SETS
# ------------------------------------------------------------------------
train_predictions <- augment(final_model_fit, new_data = train_data)
head(train_predictions)
test_predictions  <- augment(final_model_fit, new_data = test_data)
head(test_predictions)

cbind(
train_predictions %>% 
  classification_metrics(truth = mentalissue, estimate = .pred_class, ".pred_Yes", 
                         event_level = "first") %>%
  transmute(.metric, train = round(.estimate,3))
,
test_predictions %>% 
  classification_metrics(truth = mentalissue, estimate = .pred_class, ".pred_Yes",
                         event_level = "first") %>%
  transmute(test = round(.estimate,3))
)


# just trying to get a better RF from from selected RF
rf_spec2 <- rand_forest(mtry = 2, trees = 830, min_n = 32) %>% 
  set_mode("classification") %>% 
  # Try ExtraTrees split rule to reduce variance further
  set_engine("ranger", splitrule = "extratrees") 

rf_workflow2 <- workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_spec2)

rf_fit2 <- rf_workflow2 %>% 
  fit(data = train_data) # Replace train_data with your actual training dataset name

train_predictions2 <- augment(rf_fit2, new_data = train_data)
head(train_predictions2)
test_predictions2  <- augment(rf_fit2, new_data = test_data)

cbind(
  train_predictions2 %>% 
    classification_metrics(truth = mentalissue, estimate = .pred_class, ".pred_Yes", 
                           event_level = "first") %>%
    transmute(.metric, train = round(.estimate,3))
  ,
  test_predictions2 %>% 
    classification_metrics(truth = mentalissue, estimate = .pred_class, ".pred_Yes",
                           event_level = "first") %>%
    transmute(test = round(.estimate,3))
)



# ----------------------------------------------------
# shap the foresta baby
# ----------------------------------------------------

# functions to get prob of yes and log-odds of yes from yes
pf <- function(m, X) {
  predict(m, X, type = "prob")$.pred_Yes
}

pf_log_odds <- function(m, X) {
  prob <- predict(m, X, type = "prob")$.pred_Yes
  
  prob <- pmax(pmin(prob, 1 - 1e-15), 1e-15) 
  log_odds <- log(prob / (1 - prob))
  
  return(log_odds)
}

xvars <- train_data |> select(-mentalissue) |> names()
xvars

set.seed(67)
X_explain <- test_data[sample(1:nrow(test_data), 200), xvars] #30 to 1k (really impacts time)
X_background <- train_data[sample(1:nrow(train_data), 200), ] #50 to 200 is sweet spot (don't go over 500)

system.time(
  shap_values_rf <- kernelshap(
    rf_fit2,
    X = X_explain, 
    bg_X = X_background, 
    #pred_fun = pf,
    pred_fun = pf_log_odds
    #type = "prob" #link scale so shap values add up to log-odds
  )) #30/200 756; 50/200 1200; 100/200 2300; 200/200 4470

sv_rf <- shapviz(shap_values_rf)
sv_rf 
dim(sv_rf$S)
head(sv_rf$S)

# saveRDS(shap_values, file = "shap_values.rds")
# shap_values <- readRDS("shap_values.rds")

sv_importance(sv_rf, show_numbers = TRUE)

sv_importance(sv_rf, kind = "bee", alpha = 0.4) + theme_minimal()

# depence plots s2s

library(patchwork)

# p1 <- sv_dependence(sv_rf, v = xvars[1], color_var = NULL) + theme_minimal() #no interaction color
# p2 <- sv_dependence(sv_rf, v = xvars[1], alpha = 0.5) + theme_minimal() #color by interaction
# combined_dependence_plots <- p1 + p2
# 
# print(combined_dependence_plots)

xvars
for (i in 1:length(xvars)){
  
  print(i)
  
  p1 <- sv_dependence(sv_rf, v = xvars[i], color_var = NULL) + theme_minimal() #no interaction color
  p2 <- sv_dependence(sv_rf, v = xvars[i], alpha = 0.5) + theme_minimal() #color by interaction
  combined_dependence_plots <- p1 + p2
  
  print(combined_dependence_plots)
  
}





# # -------------------------------------------------------------------------
# # La foresta XBG 
# # -------------------------------------------------------------------------
# 
# #sanity checks
# hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "Yes")
# #3hbsc_se_train$mentalissue <- relevel(hbsc_se_train$mentalissue, ref = "Yes")
# #bsc_se_test$mentalissue <- relevel(hbsc_se_test$mentalissue, ref = "Yes")
# 
# #split the data
# set.seed(67)
# hbsc_se_split  <- initial_split(hbsc_se, 
#                                 strata = mentalissue,
# )
# 
# hbsc_se_train <- training(hbsc_se_split)
# hbsc_se_test <- testing(hbsc_se_split)
# 
# levels(hbsc_se$mentalissue)
# levels(hbsc_se_train$mentalissue)
# levels(hbsc_se_test$mentalissue)
# 
# 
# # xgb_spec <- boost_tree(
# #   #trees = 1000, 
# #   tree_depth = tune(), 
# #   min_n = tune(),
# #   loss_reduction = tune(),                     
# #   sample_size = tune(), 
# #   mtry = tune(),         
# #   learn_rate = tune()                          
# # ) %>%
# #   set_engine("xgboost", scale_pos_weight = 2.7) %>% #73/27
# #   #  set_engine("xgboost") %>%
# #   set_mode("classification")
# 
# # xgb_grid <- grid_latin_hypercube(
# #   trees(),
# #   tree_depth(),
# #   min_n(),
# #   loss_reduction(),
# #   sample_size = sample_prop(),
# #   finalize(mtry(), se_train),
# #   learn_rate(),
# #   size = 25 # from 50, 5
# # )
# 
# 
# #library(tidymodels)
# #library(xgboost)
# #library(dials)
# 
# # 1. Define the XGBoost Model Specification with Tunable Placeholders
# xgb_spec <- boost_tree(
#   trees = 1000,                       # we tune learn_rate/early stopping
#   tree_depth = tune(),                # Controls model complexity/interactions
#   min_n = tune(),                     # Prevents overfitting on noise
#   loss_reduction = tune(),            # "Gamma" - minimum loss reduction to split a node
#   sample_size = tune(),               # "Subsample" - row sampling percentage per tree
#   mtry = tune(),                      # "Colsample_bytree" - column sampling percentage
#   learn_rate = tune(),                # "Eta" - learning rate velocity
#   stop_iter = tune()                  # Early stopping rounds to prevent overfitting
# ) %>% 
#   set_engine("xgboost") %>% 
#   #set_engine("xgboost", scale_pos_weight = 2.7) %>%
#   set_mode("classification")
# 
# # 2. Extract and Customize the Parameter Object
# # We pass an approximation of the predictor dataset size to finalize mtry()
# xgb_params <- extract_parameter_set_dials(xgb_spec) %>% 
#   # Set an upper boundary for mtry based on your survey features (e.g., max 15 features)
#   finalize(x = matrix(NA, nrow = 100, ncol = 15)) %>% 
#   # Refine specific parameter ranges for optimal classification stability
#   update(
#     tree_depth = tree_depth(range = c(3L, 8L)),       # Survey data rarely needs >8 deep
#     learn_rate = learn_rate(range = c(-3, -1)),       # Explores eta values between 0.001 and 0.1
#     stop_iter  = stop_iter(range = c(10L, 30L))       # Early stopping check intervals
#   )
# 
# # 3. Generate the Comprehensive Space-Filling Grid
# xgb_grid <- grid_space_filling(
#   xgb_params,
#   size = 30,                       # 30 highly distinct profiles across 7 dimensions
#   type = "latin_hypercube"         # Ensures uniform distribution across dimensions
# )
# 
# # View the generated parameter grid
# dim(xgb_grid)
# head(xgb_grid)
# 
# # xgboost needs numeric predictors 
# # step_dummy() one-hot-encodes any factors
# xgb_recipe <- #tree_recipe %>%
#   recipe(mentalissue ~ ., data = hbsc_se_train) |> 
#   step_dummy(all_nominal_predictors())
# 
# # desesperado tratare de hack la receta para ver mejores shap plots
# xgb_recipe2 <- 
#   recipe(mentalissue ~ IRFAS + MBMI + age,
#          data = hbsc_se_train) %>%
#   step_integer(age)
# 
# xgb_wf <- workflow() %>%
#   add_recipe(xgb_recipe) %>%
#   add_model(xgb_spec)
# 
# set.seed(67)
# xgb_folds <- vfold_cv(hbsc_se_train, 
#                       v = 10, #from 10 10, #change later to 10
#                       #                      repeats = 3,
#                       strata = mentalissue
# )
# 
# doParallel::registerDoParallel()
# (start <- Sys.time())
# set.seed(67)
# xgb_res <- tune_grid(
#   xgb_wf,
#   resamples = xgb_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )
# Sys.time() - start #24 mins
# 
# xgb_res
# 
# collect_metrics(xgb_res)
# 
# show_best(xgb_res, metric = "accuracy")
# show_best(xgb_res, metric = "roc_auc")
# 
# #best <- show_best(xgb_res, metric = "roc_auc")
# #best
# 
# best_auc <- select_best(xgb_res, metric = "roc_auc")
# best_auc
# 
# final_xgb <- finalize_workflow(
#   xgb_wf,
#   best_auc
# )
# 
# final_xgb
# 
# final_fit_xgb <- last_fit(final_xgb, hbsc_se_split)
# final_fit_xgb
# 
# # (This gives you the actual trained model needed for SHAP!)
# fitted_xgb_fit <- fit(final_xgb, data = hbsc_se_train)
# fitted_xgb_fit
# 
# # see roc
# final_fit_xgb |>
#   collect_predictions() |>
#   roc_curve(mentalissue, .pred_Yes) |>
#   autoplot() +
#   theme_minimal()
# 
# # Calculate metrics with helper function for train/test
# get_split_metrics <- list(
#   train = hbsc_se_train,
#   test  = hbsc_se_test 
# ) %>% 
#   purrr::map_df(function(df) {
#     # Generate class and probability predictions
#     predict(fitted_xgb_fit, new_data = df, type = "class") %>% 
#       bind_cols(predict(fitted_xgb_fit, new_data = df, type = "prob")) %>% 
#       bind_cols(df) %>% 
#       # Calculate the metric set
#       metric_set(bal_accuracy, accuracy, j_index, roc_auc, brier_class)(
#         truth       = mentalissue, 
#         estimate    = .pred_class, 
#         .pred_Yes, 
#         event_level = "first" # Adjust to "second" if "Yes" is your 2nd factor level
#       )
#   }, .id = "dataset"
#   )
# 
# get_split_metrics |>
#   select(dataset, .metric, .estimate) |>
#   pivot_wider(names_from = dataset,
#               values_from = .estimate)# %>%
# 
# # #dejarme de pajaz
# # #models_performance combined table for latex
# # models_metrics <- bind_rows(
# #   tree_metrics %>% mutate(model = "Tree"),
# #   lr_metrics %>% mutate(model = "LogReg"),
# #   xgb_metrics %>% mutate(model = "XGB")
# # ) %>%
# #   arrange(.metric) %>%
# #   mutate(.metric = gsub("_", "", .metric ))
# # 
# # models_metrics
# 
# 
# # # for latex
# # tt_models_metrics <- tt(models_metrics) 
# # save_tt(tt_models_metrics, 
# #         output = "./outputsR/tinytables/tt_models_metrics.tex", 
# #         overwrite = TRUE)
# 
# # # --------------------------------------
# # # for test set, for each class (Y/N) get precision, recall, f1
# xgb_test_preds <- collect_predictions(final_fit_xgb)
# class_metrics <- metric_set(f_meas, precision, recall)
# 
# (metrics_class1 <- class_metrics(
#   xgb_test_preds,
#   truth = mentalissue,
#   estimate = .pred_class,
#   event_level = "second"
# ) %>%
#     mutate(class = "NO issues")
# )
# 
# (metrics_class2 <- class_metrics(
#   xgb_test_preds,
#   truth = mentalissue,
#   estimate = .pred_class,
#   event_level = "first"
# ) %>%
#     mutate(class = "YES issues")
# )
# 
# xgb_test_metrics <-
#   bind_rows(metrics_class1, metrics_class2) %>%
#   select(-.estimator) %>%
#   pivot_wider(names_from = .metric,
#               values_from = .estimate
#   )
# xgb_test_metrics
# 
# # # combine test metrics in one
# # test_metrics <- bind_rows(
# #   tree_test_metrics %>% mutate(model = "Tree"),
# #   lr_test_metrics %>% mutate(model = "LogReg"),
# #   xgb_test_metrics %>% mutate(model = "XGB")
# # ) %>%
# #   arrange(class)
# # 
# # test_metrics
# # 
# # tt_test_metrics <- tt(test_metrics)
# # save_tt(tt_test_metrics,
# #         output = "./outputsR/tinytables/tt_test_metrics.tex", 
# #         overwrite = TRUE)
# 
# # shap it
# xgb_fitted <- extract_fit_engine(fitted_xgb_fit)
# 
# xgb_baked_data <- extract_recipe(fitted_xgb_fit) %>% 
#   bake(new_data = hbsc_se_test) %>% 
#   select(-mentalissue) %>% 
#   as.matrix()
# 
# 
# # * trying with shapforxgboost
# shap_values <- shap.values(xgb_model = xgb_fitted, 
#                            X_train = xgb_baked_data)
# 
# shap_long <- shap.prep(xgb_model = xgb_fitted, 
#                        X_train = xgb_baked_data)
# 
# shap.plot.summary(shap_long)
# 
# for (v in shap.importance(shap_long, names_only = TRUE)) {
#   p <- shap.plot.dependence(shap_long, v, color_feature = "auto", 
#                             alpha = 0.5, jitter_width = 0.1) +
#     ggtitle(v)
#   print(p)
# }
# 
# 
# xgb_sv <- shapviz(xgb_fitted, X_pred = xgb_baked_data)
# 
# sv_importance(xgb_sv, kind = "bar") + theme_minimal()
# sv_importance(xgb_sv, 
#               kind = "beeswarm", 
#               max_display = 20L,
#               sort_features = FALSE,
#               alpha = 0.1) + 
#   theme_minimal()
# 
# # sv for xgb collapsed
# # 1. Your existing matrix extraction code
# #xgb_fitted <- extract_fit_engine(fitted_xgb_workflow)
# 
# # xgb_baked_data <- extract_recipe(fitted_xgb_workflow) %>% 
# #   bake(new_data = se_test) %>% 
# #   select(-mentalissue) %>% 
# #   as.matrix()
# 
# # 2. DYNAMICALLY CREATE THE COLLAPSE LIST
# # Find columns that were dummy encoded (containing an underscore like 'age_' or 'country_')
# cols <- colnames(xgb_baked_data)
# dummy_prefixes <- unique(sub("_.*", "", cols[grep("_", cols)]))
# 
# # Generate the named list grouping the dummy fields by their base factor name
# collapse_list <- lapply(dummy_prefixes, function(p) cols[startsWith(cols, paste0(p, "_"))])
# names(collapse_list) <- dummy_prefixes
# 
# # 3. FIXED SHAPVIZ CALL: Pass the collapse list here
# xgb_sv_collapsed <- shapviz(
#   xgb_fitted, 
#   X_pred = xgb_baked_data, 
#   X = hbsc_se_test %>% select(-mentalissue),
#   collapse = collapse_list
# )
# 
# sv_importance(xgb_sv_collapsed, kind = "bar") + theme_minimal() + ggtitle("XGB Collapsed")
# sv_importance(xgb_sv_collapsed, kind = "beeswarm") + theme_minimal() + ggtitle("XGB Collapsed")
# 
# sv_importance(xgb_sv, kind = "bar") + theme_minimal() + ggtitle("XGB NOT Collap")
# 
# 
# 







# # ------------------------------------------------------------------
# # and rf?
# # ------------------------------------------------------------------
# 
# # parallel proc
# cores <- parallel::detectCores()
# cores
# 
# rf_mod <- 
#   rand_forest(mtry = tune(), 
#               min_n = tune(), 
#               trees = 1000) |> 
#   set_engine("ranger", num.threads = cores) |> 
#   set_mode("classification")
# 
# rf_recipe <- 
#   recipe(mentalissue ~ ., data = hbsc_se_train)
#   #step_dummy(all_nominal_predictors()) |> 
#   
# rf_workflow <- 
#   workflow() |> 
#   add_model(rf_mod) |> 
#   add_recipe(rf_recipe)
# 
# # show what will be tuned
# extract_parameter_set_dials(rf_mod)
# 
# # We will use a space-filling design to tune, with 25 candidate models:
# splits      <- initial_validation_split(hotels, strata = children,
#                                         prop = c(0.60, 0.15))
# 
# val_set <- validation_set(splits)
# 
# set.seed(345)
# rf_res <- 
#   rf_workflow |> 
#   tune_grid(val_set,
#             grid = 25,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(roc_auc))
# 
# rf_res |> 
#   show_best(metric = "roc_auc")
# 
# autoplot(rf_res)
# 
# # get the last fit
# # the last model
# last_rf_mod <- 
#   rand_forest(mtry = 8, min_n = 7, trees = 1000) |> 
#   set_engine("ranger", num.threads = cores, importance = "impurity") |> 
#   set_mode("classification")
# 
# # the last workflow
# last_rf_workflow <- 
#   rf_workflow |> 
#   update_model(last_rf_mod)
# 
# # the last fit
# set.seed(345)
# last_rf_fit <- 
#   last_rf_workflow |> 
#   last_fit(splits)
# 
# last_rf_fit
# #> # Resampling results
# #> # Manual resampling 
# #> # A tibble: 1 × 6
# #>   splits                id             .metrics .notes   .predictions .workflow 
# #>   <list>                <chr>          <list>   <list>   <list>       <list>    
# #> 1 <split [30000/12501]> train/test sp… <tibble> <tibble> <tibble>     <workflow>
# #> 
# last_rf_fit |> 
#   collect_metrics()






























# 
# 
# 
# # ---------------------------------------------------------------------
# # let's shap for all
# # ---------------------------------------------------------------------
# 
# 
# 
# # tree sv
# #lr_ext <- extract_workflow(final_lr_fit)
# (start <- Sys.time())
# shap_output <- kernelshap(
#   final_tree, 
#   X = X_explain, 
#   bg_X = bg_X, 
#   type = "prob"
# )
# Sys.time() - start #
# 
# tree_sv <- shapviz(shap_output)
# 
# 
# # var imp plots
# sv_importance(tree_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("ClassTree")
# 
# 
# # # same plots but as files for latext
# # # Define common dimensions for your LaTeX layout (in inches)
# # plot_width <- 6
# # plot_height <- 4
# # 
# # pdf("outputsR/sv_imp_classtree.pdf", width = plot_width, height = plot_height)
# # sv_importance(tree_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("ClassTree")
# # dev.off()
# # 
# # pdf("outputsR/sv_imp_logreg.pdf", width = plot_width, height = plot_height)
# # sv_importance(lr_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("LogReg")
# # dev.off()
# # 
# # pdf("outputsR/sv_imp_xgb_collapsed.pdf", width = plot_width, height = plot_height)
# # sv_importance(xgb_sv_collapsed, kind = "bar") + theme_minimal() + ggtitle("XGB Collapsed")
# # dev.off()
# 
# # patched pues
# library(patchwork) # Handles side-by-side layout seamlessly
# 
# p1 <- sv_importance(tree_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("ClassTree")
# p2 <- sv_importance(lr_sv$.pred_Yes, kind = "bar") + theme_minimal() + ggtitle("LogReg")
# p3 <- sv_importance(xgb_sv_collapsed, kind = "bar") + theme_minimal() + ggtitle("XGB Collapsed")
# 
# combined_width  <- 10 
# combined_height <- 10
# 
# # 3. Save as a single combined PDF file
# pdf("outputsR/sv_imp_combined.pdf", width = combined_width, height = combined_height)
# # The '+' operator from patchwork places them side-by-side automatically
# # 'plot_layout(nrow = 1)' forces them into a single row
# combined_plot <- p1 + p2 + p3 + plot_layout(nrow = 2)
# print(combined_plot)
# dev.off()
# 
# 
# # bee plots
# # sv_importance(tree_sv$.pred_Yes, kind = "beeswarm") + theme_minimal() + ggtitle("ClassTree")
# # sv_importance(xgb_sv_collapsed, kind = "beeswarm") + theme_minimal() + ggtitle("XGB Collapsed")
# # sv_importance(xgb_sv, kind = "beeswarm") + theme_minimal() + ggtitle("XGB NOT Collap")
# 
# # waterfall plot examples to see someone with diff mental pred
# 
# (wat1 <- sv_waterfall(tree_sv$.pred_Yes, 
#                       max_display = 12,
#                       row_id = 2) + 
#     theme_minimal() + ggtitle("NO Mental issue"))
# 
# (wat2 <- sv_waterfall(tree_sv$.pred_Yes, 
#                       max_display = 12,
#                       row_id = 8) + 
#     theme_minimal() + ggtitle("YES Mental issue"))
# 
# combined_width  <- 10 
# combined_height <- 10
# 
# # 3. Save as a single combined PDF file
# pdf("outputsR/sv_waterfall_combined.pdf", width = combined_width, height = combined_height)
# # The '+' operator from patchwork places them side-by-side automatically
# # 'plot_layout(nrow = 1)' forces them into a single row
# combined_plot <- wat1 + wat2 + plot_layout(nrow = 1)
# print(combined_plot)
# dev.off()
# 
# 
# 
# 
# 
# # ------------------------------------------------------------------------
# # corels
# # ------------------------------------------------------------------------
# 
# #install.packages("corels")
# #install.packages("tidycorels")
# require(corels)
# #require(tidycorels)
# 
# # https://systopia.cs.ubc.ca/rule_lists
# # 
# # https://users.cs.duke.edu/~cynthia/code.html
# # 
# # https://corels.cs.ubc.ca/corels/run.html
# # 
# # https://www.google.com/search?q=corels+in+r+for+classification+tutorial&sca_esv=fa8fc3aacbaacd94&biw=1021&bih=527&sxsrf=APpeQntTHPozQxIORdtgxHiJEJtLIumCbw%3A1791137645049&ei=YJfCaoiGOPjBkPIPg_DbQQ&uact=5&sclient=gws-wiz-serp&fbs=ABfTbFVyMZGZf1hfvX9uKjN_-G8cxpBkeIeqYwoCbfNVc4vKEyijPgk5kCEDN_PT5No9YDQ8aujKqBsqXMyy1vj2IXgmPtfV_6GrZLubApcjNsjgAG-T3pFVvhwufR6jNURhS0HPFyagDKRz8DBgNZ68OFTlp8bI3rofKcMJ4fjZZagvjCabW4z0YG6WgElKZ-ZLAn77xJB2_z0gznx7Lh2jCdSsLQFWzw&aep=10&ntc=1&mstk=AUtExfDLjJfB3KdjobHerAFo9yx5B9LXoOjrOPH3Eqg0DHMXBGyFgjfi-7sq8NN6FljS411eMRgsWbQH9cX0V0eQR3tYUcl-wND8ktrkFNl-vIQWVRO7ezaai8np0XnVgaHUOHmTvjuBxklhPztzii24J3nRXFULRFmpK92WKCmf1qL4DFC9SAAbfiBr-Dboh1XJvZm-0A82aTZ3veJ2dJpIjHSh-SenEd5Oj9PMrhzZUozdw1zWtRU3j31HG7ffL7voM_u9HBdm7-rzuKKZNxzBwFNSoBuhPFZGPC3Ds707iabDLHsPdrD6bzmbhmIq5eMzE8w4K4bttk_1XQ&aioh=3&csuir=1&cs=0&mtid=75nCavKAMqiDgLQP2e6fuAM&udm=50
# # 
# # 
# # 
# # https://search.r-project.org/CRAN/refmans/corels/html/corels.html
# # library(recipes)
# 
# # 1. Define the recipe (convert all nominal/categorical variables)
# recipe_spec <- recipe(~ age + pmsu, data = se_train) %>%
#   #step_rm(seqno_int, country) %>%
#   step_dummy(all_nominal_predictors(), one_hot = TRUE)
# 
# # 2. Prep and bake (apply) the recipe to get the 0/1 dataframe
# se_train_dummies <- prep(recipe_spec) %>% bake(new_data = NULL)
# 
# se_train_label <- se_train$mentalissue
# 
# library(corels)
# 
# logdir <- tempdir()
# logdir <- "C:/temp"
# rules_file <- system.file("sample_data", "compas_train.out", package="corels")
# labels_file <- system.file("sample_data", "compas_train.label", package="corels")
# meta_file <- system.file("sample_data", "compas_train.minor", package="corels")
# 
# stopifnot(file.exists(rules_file),
#           file.exists(labels_file),
#           file.exists(meta_file),
#           dir.exists(logdir))
# 
# corels(rules_file, labels_file, logdir, meta_file,
#        verbosity_policy = "loud",
#        regularization = 0.015,
#        curiosity_policy = 2,   # by lower bound
#        map_type = 1) 	   # permutation map
# 
# cat("See ", logdir, " for result file.")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ls_ks
# 
# sv_multi <- shapviz(ls_ks)
# 
# sv_event <- sv_multi$.pred_Yes
# 
# sv_importance(sv_event) + theme_minimal() 
# 
# sv_importance(sv_event, 
#               kind = "bee", 
#               alpha = 0.6, 
#               #color_bar_2 = c("#3182bd", "#e34a33"),
#               size = 1) +
#   theme_minimal() +
#   # Use standard ggplot2 layers to control the low/high color gradient
#   scale_colour_gradient(
#     low = "#3182bd", 
#     high = "#e34a33", 
#     name = "Feature value",  # Sets the legend title
#     breaks = c(0, 1),        # Maps to the min and max values
#     labels = c("Low", "High")
#   )
# 
# #library(shapviz)
# # Generate a dependence plot for the single feature 'pmsu'
# 
# print(sv_dependence(sv_event, v = "country", color_var = NULL) + theme_minimal()) + coord_flip()
# print(sv_dependence(sv_event, v = "country") + theme_minimal()) + coord_flip()
# 
# #print(sv_dependence(tree_sv$.pred_Yes, v = pred) + theme_minimal())
# 
# 
# # https://parsnip.tidymodels.org/reference/decision_tree.html
# 
# 
# 
# 
# 
# # # dependence plots for top 10 predictors
# # # single-var first and then with top interaction
# # top_predictors <- final_tree |> 
# #   extract_fit_parsnip() |> 
# #   vi() %>%
# #   tibble()
# # 
# # top_predictors_ <- 
# #   top_predictors %>%
# #   arrange(Importance) %>% #sort like this so last predictor is top predictor
# #   select(Variable) %>%
# #   unlist(use.names = FALSE)
# # 
# # # dependence plots
# # for (pred in top_predictors_){
# #   #print(pred)
# #   print(sv_dependence(tree_sv$.pred_Yes, v = pred, color_var = NULL) + theme_minimal())
# #   print(sv_dependence(tree_sv$.pred_Yes, v = pred) + theme_minimal())
# # }
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# \section{PROC DE AQUI PARA ABAJO}
# 
# 
# 
# 
# %\input{tinytables/tt_models_metrics}
# %\input{tinytables/tt_test_metrics}
# 
# \subsection{Classification Tree}
# \begin{itemize}
# \item Use rpart classification
# \item Train/test split 75/25 con strata = mentalissue
# \item Tuned cost\_complexity, tree\_depth y min\_n via grid de 5 niveles each so 125 models
# \item Used 10CV on train while strata = mentalissue for imbalance
# \item Selected best tree based un AUC
# \item Refit best tree on all of train and tested on test
# \end{itemize}
# 
# Trees are great to visualize but quickly can be impossible to track. In our case we have a depth of 8, with 104 leaves and 207 nodes so we can't plot here. We can on zoomable pdf.
# 
# So here we plot a simple version to show it. By trial/error we pruned the tree as cp = 0.003. 
# 
# So it's not the final tree, nor correct but gives a really good idea of what is going on. 
# 
# \begin{figure}
# \centering
# \includegraphics[width=1\linewidth]{plots/pruned_tree.png}
# \caption{Pruned tree}
# \label{fig:placeholder}
# \end{figure}
# 
# \subsection{XGBoost}
# In tidyverse, used engine xgboost and mode classification.  
# 
# Train/test split 75/25 con strata = mentalissue.
# 
# Tune parameters via grid: trees = 1000, tree\_depth, min\_n, loss\_reduction, sample\_size, mtry and learn\_rate = tune().  
# 
# scale\_pos\_weight was put manually on set\_engine() to accomodate for class imbalance.  
# 
# XGB requires numbers so all factor variables were recoded using step\_dummy(all\_nominal\_predictors()).  
# 
# 10 fold CV was issued with strata = mentalissue to accomodate for class imablance. 
# 
# Best model was salected based on AUC; and then refit on all train and test on test.  
# 
# \subsection{SHAP}
# 
# Models rank top features relatively consistently.
# 
# Side to side plots:
#   
#   \begin{figure}[H]
# \centering
# \includegraphics[width=1\linewidth]{plots/sv_imp_combined.pdf}
# \caption{Imp Plots SHAP}
# \label{fig:shaps2s}
# \end{figure}
# 
# A quick average to rank top features based on all results:
#   
#   \begin{figure}[H]
# \centering
# \includegraphics[width=0.5\linewidth]{plots/image.png}
# \caption{Enter Caption}
# \label{fig:shapavg}
# \end{figure}
# 
# Example of why someone is predicted YES vs NO mental issues.
# 
# \begin{figure}[H]
# \centering
# \includegraphics[width=1\linewidth]{plots/sv_waterfall_combined.pdf}
# \caption{waterfallcomb}
# \label{fig:watf}
# \end{figure}
# 
# 
# 
# # purgatory wtf
# # hbsc purgatory / sandbox
# 
# # graph for bullied_s sucked cause it's discrete [2,10]
# # so see in discrete var
# # it's now on d2 so purgatory
# d1 %>%
#   filter(!is.na(bullied_s), !is.na(mental_issue)) %>%
#   ggplot(aes(x = factor(bullied_s), fill = mental_issue)) +
#   # position = "fill" converts counts to 100% proportional bars
#   geom_bar(position = "fill", alpha = 0.85) + 
#   scale_fill_manual(values = c("No" = "#5DADE2", "Yes" = "#E74C3C")) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal()
# 
# 
# 
# # codigo que use para calculater train metrics
# # # train metrics a pata
# # # 2. Generate predictions on the training data (both class and probabilities)
# # train_results <- predict(final_tree, new_data = d1_train, type = "class") %>% 
# #   bind_cols(predict(final_tree, new_data = d1_train, type = "prob")) %>% 
# #   bind_cols(d1_train) # Bind your original data to keep the 'truth' column
# # 
# # # 3. Calculate your performance metrics (e.g., accuracy, roc_auc)
# # # Make sure to specify event_level if your positive class is the second level
# # metric_results <- metric_set(accuracy, roc_auc, sens, spec)(
# #   train_results, 
# #   truth = mental_issue, 
# #   estimate = .pred_class, 
# #   .pred_Yes#, # Replace with your actual probability column name for the event
# #   #event_level = "second" # Change to "first" or "second" depending on your target
# # )
# # 
# # print(metric_results)
# 
# # So I will dichotomize like HBSC and keep sum of both as well for SHAP.
# # replaced when I added talkp
# dat <- dat %>%
#   mutate(talkf = factor( 
#     case_when(talkfather %in% c(1,2) ~ 1,
#               talkfather %in% c(3,4,5) ~ 0,
#               TRUE ~ NA),
#     levels = c(0,1),
#     labels = c("No", "Yes")
#   ),
#   talkm = factor(
#     case_when(talkmother %in% c(1,2) ~ 1,
#               talkmother %in% c(3,4,5) ~ 0,
#               TRUE ~ NA),
#     levels = c(0,1),
#     labels = c("No", "Yes")
#   ),
#   talkscore = talkfather + talkmother
#   )
# 
# 
# 
# 
# # see and plot the tree
# # has to be pruned but play with cp 
# final_tree_obj <- final_tree |>
#   extract_fit_engine() %>%
#   prune(final_tree_obj,
#         cp = 0.007)
# 
# # need to fix this
# rpart.plot(final_tree_obj,
#            roundint = FALSE,
#            #cex = 1.1,
#            tweak = 2,
#            #shadow.col = "gray",    # Adds a light shadow for better readability
#            box.palette = "RdBu",
#            fallen.leaves = TRUE
# )
# 
# # plot tree to pdf
# pdf("giant_readable_tree.pdf", width = 20, height = 12)
# 
# final_tree |>
#   extract_fit_engine() |>
#   rpart.plot(roundint = FALSE, tweak = 1.2) # You can scale text UP now!
# 
# dev.off()
# 
# 
# 
# 
# # xgb crap
# #adapt to me
# final_res %>%
#   collect_predictions() %>%
#   roc_curve(mental_issue, .pred_Yes) %>%
#   ggplot(aes(x = 1 - specificity, y = sensitivity)) +
#   geom_line(size = 1.5, color = "midnightblue") +
#   geom_abline(
#     lty = 2, alpha = 0.5,
#     color = "gray50",
#     size = 1.2
#   )
# 
# # get the final tree
# final_trees <- extract_workflow(final_fit_xgb)
# final_trees
# 
# 
# # Calculate SHAP values for all classes automatically
# #esto no es asi por las dummies
# (start <- Sys.time())
# shap_output <- kernelshap(
#   final_trees, 
#   X = X_explain, 
#   bg_X = bg_X, 
#   type = "prob"
# )
# Sys.time() - start #2 mins
# 
# # Convert to a shapviz object and plot
# sv <- shapviz(shap_output)
# names(sv)
# 
# sv_importance(sv$.pred_Yes, kind = "bar") + theme_minimal() #var imp plot
# sv_importance(sv$.pred_Yes, kind = "beeswarm") + theme_minimal() #bee
# 
# # Yes pred
# sv_waterfall(sv$.pred_Yes, row_id = 20) + theme_minimal()
# sv_waterfall(sv$.pred_Yes, row_id = 7) + theme_minimal() 
# 
# # No pred
# sv_waterfall(sv$.pred_Yes, row_id = 12) + theme_minimal() 
# sv_waterfall(sv$.pred_Yes, row_id = 4) + theme_minimal() 
# 
# #sv_force(sv$.pred_Yes, row_id = 7) #Yes, individual
# #sv_force(sv$.pred_Yes, row_id = 9) #No, individual
# 
# # dependence plots for top 10 predictors
# # single-var first and then with top interaction
# top_predictors <- final_trees |> 
#   extract_fit_parsnip() |> 
#   vi() %>%
#   tibble()
# 
# top_predictors_ <- 
#   top_predictors %>%
#   arrange(Importance) %>% #sort like this so last predictor is top predictor
#   select(Variable) %>%
#   unlist(use.names = FALSE)
# 
# for (pred in top_predictors_){
#   print(pred)
#   
#   # dependence plots
#   print(sv_dependence(sv$.pred_Yes, v = pred, color_var = NULL) + theme_minimal())
#   print(sv_dependence(sv$.pred_Yes, v = pred) + theme_minimal())
#   
# }
# 
# #lr ta cansado
# 
# 
# # 1. Isolate the names of the non-zero variables from your Lasso model
# selected_vars <- corrected_coefs |> 
#   filter(term != "(Intercept)", estimate != 0) |> 
#   pull(term)
# 
# # 2. Build a brand new formula using just those selected variables
# final_formula <- as.formula(
#   paste("mental_issue ~", paste(selected_vars, collapse = " + "))
# )
# 
# # 3. Fit a standard, normal logistic regression (unpenalized)
# normal_lr_spec <- logistic_reg() |> 
#   set_engine("glm")
# 
# normal_lr_fit <- normal_lr_spec |> 
#   fit(final_formula, data = hbsc_train)
# 
# # 4. View traditional coefficients, standard errors, and p-values!
# normal_lr_fit |> 
#   tidy()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
















# # retry xgb little by little forcing good plots
# library(SHAPforxgboost)
# library(xgboost)
# 
# df <- hbsc_se %>%
#   select(mentalissue,
#          #pmsu,
#          age) %>%
#   mutate(
#     age_int = recode_values(
#       age,
#       "11" ~ 11,
#       "13" ~ 13,
#       "15" ~ 15
#     ),
#     mentalissue_int = recode_values(
#       mentalissue,
#       "Yes" ~ 1,
#       "No" ~ 0
#     )) %>%
#   select(-c(age, mentalissue))
# 
# glimpse(df)
# 
# y <- "mentalissue_int"
# x <- c("age_int")
# 
# # random split
# set.seed(83454)
# ix <- sample(nrow(df), 0.8 * nrow(df))
# 
# dtrain <- xgb.DMatrix(data.matrix(df[ix, x]), 
#                       label = df[[y]][ix])
# 
# dvalid <- xgb.DMatrix(data.matrix(df[-ix, x]),
#                       label = df[-ix, y, drop = TRUE])
# 
# 
# params <- list(
#   #objective = "reg:squarederror",
#   learning_rate = 0.00915,
#   subsample = 0.840,
#   colsample_bynode = 1,
#   #reg_lambda = 2,
#   max_depth = 6
# )
# 
# fit_xgb <- xgb.train(
#   params,
#   data = dtrain,
#   evals = list(valid = dvalid),
#   early_stopping_rounds = 20,
#   print_every_n = 100,
#   nrounds = 10000 # early stopping
# )
# 
# # Step 1: Select some observations
# X <- data.matrix(df[sample(nrow(df), 1000), x])
# 
# # Step 2: Crunch SHAP values
# shap <- shap.prep(fit_xgb, X_train = X)
# 
# # Step 3: SHAP importance
# shap.plot.summary(shap)
# 
# # Step 4: Loop over dependence plots in decreasing importance
# for (v in shap.importance(shap, names_only = TRUE)) {
#   p <- shap.plot.dependence(shap, v, color_feature = "auto", 
#                             alpha = 0.5, jitter_width = 0.1) +
#     ggtitle(v)
#   print(p)
# }






# # ----------------------------------------------
# # LR de niez
# # ----------------------------------------------
# 
# # keep only the vars that will be modeled
# hbsc_se <- hbsc_se %>%
#   select(-c(IOTF4, #MBMI
#             IRRELFAS, #IRFAS
#             lifesat, #no evidence
#             seqno_int #id
#   )
#   )
# 
# glimpse(hbsc_se)
# 
# # relevel since we will predict Yes (base has to be No; in LR at least)
# hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")
# levels(hbsc_se$mentalissue)
# 
# # age podria ser #
# # gender podria ser 0/1
# # pmsu podria ser 1 2 3
# 
# # hacer lr by age, gender pmsu as is
# # repeat with dummy
# # repeat with actual #
# # ver diff en shap 
# df <- hbsc_se %>% select(age, gender, pmsu, mentalissue)
# glimpse(df)
# 
# lr1 <- glm(mentalissue ~ .,
#            data = df,
#            family = "binomial")
# summary(lr1)
# 
# xvars <- c("age", "gender", "pmsu")
# X_bg <- df[xvars] |> slice_sample(n = 100) #100 random for background dataset
# X_explain <- df[xvars] |> slice_sample(n = 100) #rows to explain
# res_shap <- kernelshap(lr1, X = X_explain, bg_X = X_bg) #shap on logit
# sv <- shapviz(res_shap) #shapviz objt
# print(get_shap_values(sv))
# sv_importance(sv, kind = "bar")
# sv_importance(sv, kind = "bee")
# 
# # repeat with features as dummies
# library(fastDummies)
# df_dummies <- dummy_cols(df, 
#                          select_columns = c("age", "gender", "pmsu"),
#                          remove_first_dummy = TRUE,
#                          remove_selected_columns = TRUE
# )
# glimpse(df_dummies)
# 
# lr2 <- glm(mentalissue ~ .,
#            data = df_dummies,
#            family = "binomial")
# summary(lr2)
# 
# xvars <- df_dummies |> select(-mentalissue) |> names()
# X_bg <- df_dummies[xvars] |> slice_sample(n = 100) #100 random for background dataset
# X_explain <- df_dummies[xvars] |> slice_sample(n = 100) #rows to explain
# res_shap <- kernelshap(lr2, X = X_explain, bg_X = X_bg) #shap on logit
# sv <- shapviz(res_shap) #shapviz objt
# print(get_shap_values(sv))
# sv_importance(sv, kind = "bar")
# sv_importance(sv, kind = "bee")
# 
# # repeat with features as numbers
# df_num <- df |>
#   mutate(
#     # 1. Factor to numeric requires converting to character first to keep literal values
#     age = as.numeric(as.character(age)),
#     
#     # 2. Convert Gender to 0/1 (Boy = 0, Girl = 1)
#     gender = if_else(gender == "Boy", 0, 1),
#     
#     # 3. Convert ordinal category to 1, 2, 3
#     pmsu = case_match(pmsu,
#                       "Low" ~ 1,
#                       "Med" ~ 2,
#                       "High" ~ 3,
#                       .default = NA)
#   )
# glimpse(df_num)
# 
# lr3 <- glm(mentalissue ~ .,
#            data = df_num,
#            family = "binomial")
# summary(lr3)
# 
# xvars <- df_num |> select(-mentalissue) |> names()
# X_bg <- df_num[xvars] |> slice_sample(n = 100) #100 random for background dataset
# X_explain <- df_num[xvars] |> slice_sample(n = 100) #rows to explain
# res_shap <- kernelshap(lr3, X = X_explain, bg_X = X_bg) #shap on logit
# sv <- shapviz(res_shap) #shapviz objt
# print(get_shap_values(sv))
# sv_importance(sv, kind = "bar")
# sv_importance(sv, kind = "bee")