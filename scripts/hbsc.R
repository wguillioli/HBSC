# HBSC 
# Updated: 2026-09-25


# ---------------------------------------------------
# workspace stuff
# ---------------------------------------------------

img_file <- paste0(prj_fldr, "/Rimages/hbsc_wkspace_20260923.RData")
 
# load(file = img_file)

save.image(file = img_file)


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

prj_fldr <- "C:/MisLocalFiles/Github/HBSC/"
setwd(prj_fldr)

options(scipen = 999)


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

tbl_hbsc_se  

# for latex
#kable(tbl_hbsc_se, format = "latex", booktabs = TRUE)
tt_tbl_hbsc_se <- tt(tbl_hbsc_se) 
save_tt(tt_tbl_hbsc_se, output = "./outputsR/tinytables/tt_tbl_hbsc_se.tex", overwrite = TRUE)




# get all columns printed sorted by name
hbsc_se |> relocate(sort(names(hbsc_se))) |> glimpse()


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
# THE logistic regression
# ------------------------------------------------------------

#relevel arg
hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")

#split the data
set.seed(67)
se_split  <- initial_split(hbsc_se, 
                            strata = mentalissue,
                            )

se_train <- training(se_split)
se_test <- testing(se_split)

nrow(se_train) + nrow(se_test) == nrow(hbsc_se) #must be TRUE

round(table(se_train$mentalissue) / nrow(se_train),3)
round(table(se_test$mentalissue) / nrow(se_test), 3) #must be same

# specify model for glmnet with var sec
lr_lasso_mod <- 
  logistic_reg(penalty = tune(), 
               mixture = 1) |> 
  set_engine("glmnet")

# recipe of pre proc steps
lr_recipe <- 
  recipe(mentalissue ~ ., data = se_train) |> 
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
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

# create folds for tunning
folds_10cv <- vfold_cv(se_train, v= 10)

my_metrics <- metric_set(bal_accuracy, 
                         accuracy,
                         roc_auc,
                         brier_class,
                         j_index
)

# train and tune
lr_res <- 
  lr_workflow |> 
  tune_grid(
    resamples = folds_10cv, 
    grid = lr_reg_grid,
    control = control_grid(save_pred = TRUE),
    metrics = my_metrics
  )

# collect all metrics
lr_res_metrics <- 
lr_res |> 
  collect_metrics()

#AUC by penalty
lr_res_metrics |>
  filter(.metric == "roc_auc") |> 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
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
  last_fit(split = se_split, metrics = my_metrics)

final_lr_fit

# get test metrics
(test_metrics <- final_lr_fit |> collect_metrics())

# get train metrics
trained_model <- final_lr_workflow |> 
  fit(data = training(se_split))

(train_metrics <- trained_model |> 
    augment(new_data = training(se_split)) |> 
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

lr_metrics %>% pivot_wider(names_from = set,
                           values_from = .estimate)

# get for test set for each class (Y/N) get precision, recall, f1
# it has to be done "manually 1x1

lr_test_predictions <- collect_predictions(final_lr_fit)

class_metrics <- metric_set(f_meas, precision, recall)

(metrics_class1 <- class_metrics(
  lr_test_predictions, 
  truth = mentalissue, 
  estimate = .pred_class,
  event_level = "first"
) %>% 
  mutate(class = "Level1_NO mental issues")
)

(metrics_class2 <- class_metrics(
  lr_test_predictions, 
  truth = mentalissue, 
  estimate = .pred_class,
  event_level = "second"
) %>% 
    mutate(class = "Level2_YES mental issues")
)

#output pues
bind_rows(metrics_class1, metrics_class2) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric,
              values_from = .estimate
              )


# get vars selected by lasso to fit a normal GLM
# get coeffs, flag kept, get var name
(lasso_coefs <- final_lr_fit |> 
  extract_fit_parsnip() |> 
  tidy())

(lasso_summary <- lasso_coefs |> 
  filter(term != "(Intercept)") |> # Exclude the baseline intercept row
  mutate(
    lasso_status = if_else(estimate == 0, "Removed", "Kept")
  ) |> 
  select(term, estimate, lasso_status)
)

(lr_vars <- 
lasso_summary %>%
  mutate(feature = str_split_i(term, "_", 1)) %>%
  distinct(feature)
)

# now, refit a normal GLM for my coeffs and OR

#splits and cv reuse above

# need a new recipe with right vars and without normalize
lr_recipe2 <- 
  recipe(mentalissue ~ ., data = se_train) |> 
  step_rm(IOTF4,
          IRRELFAS,
          IRFAS,
          lifesat,
          seqno_int
  )

# Model specification using the "glm" engine (necessary for p-values)
lr_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

lr_workflow <- workflow() %>%
  add_recipe(lr_recipe2) %>%
  add_model(lr_spec)

final_fit <- last_fit(
  lr_workflow,
  split = se_split
)

# Extract engine, fetch log-odds, and calculate odds ratios simultaneously
model_inference <- final_fit %>% 
  extract_fit_engine() %>% 
  tidy(exponentiate = FALSE, conf.int = TRUE) %>% 
  mutate(
    odds_ratio   = exp(estimate),
    or_conf_low  = exp(conf.low),
    or_conf_high = exp(conf.high)
  )

(lr_coeffs <- 
model_inference %>%
  select(term,
         coefficient = estimate,
         odds_ratio, #or_conf_low, or_conf_high,
         p.value
         )
)  


    
kable(lr_coeffs, format = "markdown") 

# ------------------------------------------------------------
# compute SHAP for LR
# ------------------------------------------------------------

# extract fitted wf for shap
fit_wf <- extract_workflow(final_fit)
fit_wf

x_vars <- c("age", "pmsu")

x_vars <- names(se_train)
x_vars <- x_vars[-10]

# step_rm(IOTF4,
#         IRRELFAS,
#         IRFAS,
#         lifesat,
#         seqno_int
# )

bg_data <- se_train[1:100, x_vars]
to_explain <- se_test[1:100, x_vars]

# calculate shap values
(start <- Sys.time())
ls_ks <- kernelshap(
  fit_wf, 
  X = to_explain, #se_test[, x_vars], 
  bg_X = bg_data, # se_train[, x_vars], 
  type = "prob"
)
(Sys.time() - start)

ls_ks

sv_multi <- shapviz(ls_ks)

sv_event <- sv_multi$.pred_Yes

sv_importance(sv_event) + theme_minimal() 

sv_importance(sv_event, 
              kind = "bee", 
              alpha = 0.6, 
              #color_bar_2 = c("#3182bd", "#e34a33"),
              size = 1) +
  theme_minimal() +
  # Use standard ggplot2 layers to control the low/high color gradient
  scale_colour_gradient(
    low = "#3182bd", 
    high = "#e34a33", 
    name = "Feature value",  # Sets the legend title
    breaks = c(0, 1),        # Maps to the min and max values
    labels = c("Low", "High")
  )

#library(shapviz)
# Generate a dependence plot for the single feature 'pmsu'

print(sv_dependence(sv_event, v = "country", color_var = NULL) + theme_minimal()) + coord_flip()
print(sv_dependence(sv_event, v = "country") + theme_minimal()) + coord_flip()

#print(sv_dependence(tree_sv$.pred_Yes, v = pred) + theme_minimal())


# FIN FOR NOW




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

# plot the tree 
raw_final_tree <- extract_fit_engine(final_tree)

rpart.plot(
  raw_final_tree,
  roundint = FALSE,
  #  font = 4,
  tweak = 1.5,  
  type = 5,                    # Clear split labels directly on lines
  extra = 100,
  box.palette = list("#c0392b", "#2980b9") 
)

# can't read so maybe in pdf
# 1. Open a massive canvas layout (dimensions are in inches)
pdf(
  file = "plots/giant_zoomable_tree.pdf", 
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

# prunning to plot
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