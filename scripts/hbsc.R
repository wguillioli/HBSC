# HBSC 
# Updated: 2026-10-30

# setup ----------------------------------------------

rm(list = ls())

prj_fldr <- "C:/GitHub/HBSC2/"
setwd(prj_fldr)

options(scipen = 999)

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
library(ggridges)
library(tinytable)
library(coefplot)
library(doParallel)

# workspace -------------------------------------

img_file <- paste0(prj_fldr, "Rimages/hbsc.RData")
#load(file = img_file)
#save.image(file = img_file)


# data -------------------------------------------

## data load -------------------------------------

# the european way with ; and ,
hbsc <- read_csv2(paste0(prj_fldr, "data/raw/HBSC2018OAed1.1.csv"))
glimpse(hbsc)
dim(hbsc)

## dat copy --------------------------------------

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


# univariate EDA -------------

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


# get rids of NAs and keep countries of interest based on Y
# and final touches before mdl

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


# one more thing on bmi and fas

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


# factors ----------------- 
# factor vars - eda and potential of pred
# for each fact predictor get a summary table with 
# var name, n, % and % of mental issue and chi

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


# number vars -------------------------------------------
# for each fact predictor get a summary table with 
# var name, n, % and % of mental issue and chi

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


# mdl data --------------------------------------

# a bit more cleaning to keep only y and predictors
hbsc_se <- hbsc_se %>% 
  select(-c(seqno_int,
            IOTF4,
            IRRELFAS,
            lifesat)
  )

glimpse(hbsc_se)

# Set NO as the reference level
hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")
levels(hbsc_se$mentalissue)

#split the data into train/test 
set.seed(67)
hbsc_se_split  <- initial_split(hbsc_se, prop = 8/10, strata = mentalissue)
train <- training(hbsc_se_split)
test <- testing(hbsc_se_split)

folds <- vfold_cv(train, v = 5, strata = mentalissue)

# just checking
nrow(train) + nrow(test) == nrow(hbsc_se)
table(train$mentalissue) / nrow(train)
table(test$mentalissue) / nrow(test)


# mdl functions -----------------------

# function takes final fit and returns metrics table for train & test
eval_mdl <- function(the_fit){
  train_preds <- train %>%
    select(mentalissue) %>% 
    bind_cols(predict(the_fit, new_data = train, type = "class"),
              predict(the_fit, new_data = train, type = "prob"))
  
  train_metrics <- train_preds %>% 
    eval_metrics(truth = mentalissue, estimate = .pred_class, .pred_Yes, event_level = "second") %>%
    select(.metric, train_estimate = .estimate)
  
  test_preds <- test %>%
    select(mentalissue) %>% 
    bind_cols(predict(the_fit, new_data = test, type = "class"),
              predict(the_fit, new_data = test, type = "prob"))
  
  test_metrics <- test_preds %>% 
    eval_metrics(truth = mentalissue, estimate = .pred_class, .pred_Yes, event_level = "second") %>%
    select(.metric, test_estimate = .estimate)
  
  return(left_join(train_metrics, test_metrics, by = ".metric"))
}


# function takes a fit and returns eval metrics for both classes of test set (F1, prec, recall)
eval_test <- function(the_fit){
  test_preds <- augment(the_fit, new_data = test)
  test_preds
  
  test_metrics <- metric_set(f_meas, precision, recall)
  
  test_metrics_tbl <- 
    bind_rows(test_preds %>% 
                test_metrics(truth = mentalissue, estimate = .pred_class, event_level = "second") %>%
                mutate(class = "mentalissue_YES"),
              test_preds %>% 
                test_metrics(truth = mentalissue, estimate = .pred_class, event_level = "first") %>%
                mutate(class = "mentalissue_NO")
    ) %>%
    select(-.estimator) %>%
    pivot_wider(names_from = .metric,
                values_from = .estimate)
  
  return(test_metrics_tbl)
}


# Log Reg --------------------------------------------------

## LR1 ------------
# dataset as is

lr_recipe <- recipe(mentalissue ~ ., data = train)

lr_spec <- logistic_reg() |>
  set_engine("glm") %>%
  set_mode("classification")

lr_workflow <- workflow() |>
  add_recipe(lr_recipe) %>%
  add_model(lr_spec)

lr_cv_results <- fit_resamples(
  lr_workflow,
  resamples = folds,
  metrics = metric_set(bal_accuracy, roc_auc),
  control = control_resamples(save_pred = TRUE)
)

lr_cv_results |> collect_metrics()
lr_cv_results |> collect_metrics(summarize = FALSE) |> arrange(.metric)

lr_final_fit1 <- last_fit(lr_workflow, hbsc_se_split)

lr_final_mdl1 <- extract_workflow(lr_final_fit1)

#lr_fit1 <- lr_spec |> 
#  fit(mentalissue ~ ., data = train)

eval_mdl(lr_final_mdl1)
eval_test(lr_final_mdl1)



## LR2 ------------
# dataset weighted

# lr_recipe <- recipe(mentalissue ~ ., data = train_weighted) #includes wts
# 
# lr_spec <- logistic_reg() |>
#   set_engine("glm") %>%
#   set_mode("classification")
# 
# lr_workflow <- workflow() |>
#   add_recipe(lr_recipe) %>%
#   add_model(lr_spec) |>
#   add_case_weights(wts) # Crucial tracks weights across resamples
# 
# lr2_cv_results <- fit_resamples(
#   lr_workflow,
#   resamples = folds_weighted,
#   metrics = metric_set(bal_accuracy, roc_auc),
#   control = control_resamples(save_pred = TRUE)
# )
# 
# lr2_cv_results |> collect_metrics()
# lr2_cv_results |> collect_metrics(summarize = FALSE) |> arrange(.metric)
# 
# lr_final_fit2 <- last_fit(lr_workflow, train_weighted)
# 
# # finalize wkflow to get final fit
# final_tree_wflow <- finalize_workflow(tree_wflow, 
#                                       select_best(tree_res2, metric = "roc_auc"))
# 
# tree_fit3_tuned_weighted <- fit(final_tree_wflow, data = train_weighted)
# 
# lr_final_mdl1 <- extract_workflow(lr_final_fit1)
# 
# eval_mdl(lr_final_mdl1)
# eval_test(lr_final_mdl1)
# 
# tree_wflow <- workflow() |> 
#   add_recipe(tree_recipe) |> 
#   add_model(tree_spec) |> # reuse the above
#   add_case_weights(wts) # Crucial tracks weights across resamples
# 
# #doParallel::registerDoParallel()
# cl <- makePSOCKcluster(5) 
# registerDoParallel(cl)
# set.seed(67)
# system.time(
#   tree_res2 <- tune_grid(
#     tree_wflow,
#     resamples = folds_weighted,
#     grid = tree_grid,
#     metrics = metric_set(bal_accuracy, accuracy, roc_auc)
#   )
# ) #1k 30 mins




# Trees ----------------------

## Tree0 --------------------
# Vanilla Tree with defaults and not even on folds

tree0_spec <- decision_tree() |> 
  set_engine("rpart") |> 
  set_mode("classification")

tree0 <- tree0_spec |> 
  fit(mentalissue ~ ., data = train)

eval_mdl(tree0)
eval_test(tree0)

# get the params
tree0_eng <- tree0 |> extract_fit_engine()
tree0_eng$control$cp #cost_complexity
tree0_eng$control$maxdepth #tree_depth
tree0_eng$control$minsplit #min_n



## Tree1 ------------------------------------------------
# Tuned with grid

tree1_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(), 
    min_n = tune() 
  ) |> 
  set_engine("rpart") |> 
  set_mode("classification")

tree1_grid <- grid_regular(cost_complexity(),
                           tree_depth(),
                           min_n(range = c(2,15)), #15 predictors on train
                           levels = 10) 
dim(tree1_grid) #1000x3
View(tree1_grid)

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

set.seed(67)
(start <- Sys.time())
tree1_res <- tune_grid(
    tree1_spec,
    mentalissue ~ .,
    resamples = folds,
    grid = tree1_grid,
    metrics = metric_set(bal_accuracy, roc_auc),
    control = control_grid(verbose = TRUE) # Shows progress bars in the console
)

stopCluster(my_cluster)
registerDoSEQ()
print(Sys.time() - start) #40 mins

tree1_metrics <- collect_metrics(tree1_res)
View(tree1_metrics)

#autoplot(tree1_res) + theme_light()
show_best(tree1_res, metric = "roc_auc", n = 10) 
my_chosen_params <- tibble(
  cost_complexity = 0.00001,
  tree_depth = 7,
  min_n = 15
)

tree1_final_spec <- tree1_spec |>
  finalize_model(my_chosen_params)

tree1 <- tree1_final_spec |>
  fit(mentalissue ~ ., data = train) 

eval_mdl(tree1)
eval_test(tree1)


## Tree2  -----------------
# Tuned on weighted data

train_weighted <- train |>
  mutate(wts = ifelse(mentalissue == "Yes", 2.70, 1.0), # is this right?
                wts = importance_weights(wts))

set.seed(67)
folds_weighted <- vfold_cv(train_weighted, v = 5, strata = mentalissue)

tree2_recipe <- recipe(mentalissue ~ ., data = train_weighted) #includes wts

tree2_wflow <- workflow() |> 
  add_recipe(tree2_recipe) |> 
  add_model(tree1_spec) |> # reuse the above
  add_case_weights(wts) # Crucial tracks weights across resamples

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

(start <- Sys.time())
set.seed(67)
tree2_res <- tune_grid(
  tree2_wflow,
  resamples = folds_weighted,
  grid = tree1_grid,
  metrics = metric_set(bal_accuracy, roc_auc),
  control = control_grid(verbose = TRUE) # Shows progress bars in the console
)

stopCluster(my_cluster)
registerDoSEQ()
print(Sys.time() - start) #40 mins

tree_metrics <- collect_metrics(tree2_res)
View(tree_metrics)

autoplot(tree2_res) + theme_light()
show_best(tree2_res, metric = "roc_auc", n=10)
my_chosen_params <- tibble(
  cost_complexity = 0.001,
  tree_depth = 7,
  min_n = 10
)

tree2_final_wflow <- finalize_workflow(tree2_wflow, 
                                       select_best(tree2_res, metric = "roc_auc")
                                       #my_chosen_params
                                      )

tree2 <- fit(tree2_final_wflow, data = train_weighted)
 
eval_mdl(tree2)
eval_test(tree2)


# SHAP ------------------------------------------

# functions to get prob of yes and log-odds of yes from yes

pf_log_odds <- function(m, X) {
  prob <- predict(m, X, type = "prob")$.pred_Yes
  
  prob <- pmax(pmin(prob, 1 - 1e-15), 1e-15) 
  log_odds <- log(prob / (1 - prob))
  
  return(log_odds)
}

# pf <- function(m, X) {
#   predict(m, X, type = "prob")$.pred_Yes
# }

## SHAP datasets -----------------------
# data to use in all SHAPs
(xvars <- train |> select(-mentalissue) |> names())
set.seed(67)
X_explain <- test[sample(1:nrow(test), 500), xvars] 
X_background <- train[sample(1:nrow(train), 200), xvars] 


## SHAP for trees --------------------------

# compute in paralel shap values for all 3 trees
num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

(start <- Sys.time())
tree0_ks <- kernelshap(
    tree0,
    X = X_explain, 
    bg_X = X_background, 
    pred_fun = pf_log_odds
  )
tree0_sv <- shapviz(tree0_ks)

tree1_ks <- kernelshap(
  tree1,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)
tree1_sv <- shapviz(tree1_ks)

tree2_ks <- kernelshap(
  tree2,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)
tree2_sv <- shapviz(tree2_ks)

print(Sys.time() - start) 

stopCluster(my_cluster)
registerDoSEQ()

sv_importance(tree0_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree0")
sv_importance(tree1_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree1")
sv_importance(tree2_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree2")

dim(tree0_sv)
head(tree0_sv)



# RF --------------------------------
# fits:
# 0) vanilla
# 1) tuned with all 3 defaults
# 2) like 2 con pesitos
# 3) add el parametro que no muy

## RF0 vanilla --------------------
rf_spec <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification")

require(ranger)

rf_fit1_vanilla <- rf_spec |> 
  fit(mentalissue ~ ., data = train)

eval_mdl(rf_fit1_vanilla)
eval_test(rf_fit1_vanilla)


## RF1 tuned -------------------
rf_spec <- rand_forest(
  mtry = tune(),    
  min_n = tune(),   
  trees = tune() 
) |> set_engine("ranger") |> 
  #set_engine("ranger", max.depth = tune()) |> 
  set_mode("classification") 

rf_recipe <- recipe(mentalissue ~ ., data = train) 

rf_wflow <- workflow() |> 
  add_recipe(rf_recipe) |> 
  add_model(rf_spec)

rf_params <- extract_parameter_set_dials(rf_wflow) |> 
  update(mtry = mtry(c(1, 15)), 
          trees = trees(c(100, 1000))) |>
  finalize(train) 

rf_grid <- grid_regular(rf_params, levels = 10)
dim(rf_grid)
summary(rf_grid)

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)
#cl <- makePSOCKcluster(5) 
#registerDoParallel(cl)

set.seed(67)
system.time(
  rf_res1 <- tune_grid(
    rf_wflow,
    resamples = folds,
    grid = rf_grid,
    metrics = metric_set(bal_accuracy, accuracy, roc_auc),
    control = control_grid(verbose = TRUE) # Shows progress bars in the console
  )
)

stopCluster(my_cluster)
#stopCluster(cl)
registerDoSEQ()

autoplot(rf_res1)

show_best(rf_res1, metric = "roc_auc")

# finalize wkflow to get final fit
final_rf_wflow <- finalize_workflow(
                        rf_wflow, 
                        select_best(rf_res1, metric = "roc_auc"))

rf_fit1_tuned <- fit(final_rf_wflow, data = train)

eval_mdl(rf_fit1_tuned)
eval_test(rf_fit1_tuned)


## RF2 tuned weighted -------------------

rf_recipe_weighted <- recipe(mentalissue ~ ., data = train_weighted) 

rf_wflow <- workflow() |> 
  add_recipe(rf_recipe_weighted) |> 
  add_model(rf_spec) # reuse rf_spec from above

rf_params <- extract_parameter_set_dials(rf_wflow) |> 
  update(mtry = mtry(c(1, 15)), 
         trees = trees(c(100, 1000))) |>
  finalize(train_weighted) 

rf_grid <- grid_regular(rf_params, levels = 8) #from 10
dim(rf_grid)
#summary(rf_grid)

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

set.seed(67)
system.time(
  rf_res2 <- tune_grid(
    rf_wflow,
    resamples = folds_weighted,
    grid = rf_grid,
    metrics = metric_set(bal_accuracy, accuracy, roc_auc),
    control = control_grid(verbose = TRUE) # Shows progress bars in the console
  )
) #13hrs

stopCluster(my_cluster)
registerDoSEQ()

autoplot(rf_res2)

show_best(rf_res2, metric = "roc_auc")

# finalize wkflow to get final fit
final_rf_wflow <- finalize_workflow(
  rf_wflow, 
  select_best(rf_res2, metric = "roc_auc"))

rf_fit2_tuned <- fit(final_rf_wflow, data = train_weighted)

eval_mdl(rf_fit2_tuned)
eval_test(rf_fit2_tuned)


## RF3 tuned+1 weighted -------------------------
rf_spec <- rand_forest(
  mtry = tune(),    
  min_n = tune(),   
  trees = tune() 
) |>
  #|> set_engine("ranger")# |> 
  set_engine("ranger", 
             max.depth = tune(),
             replace = FALSE, # recommended when tuning sample fraction
             sample.fraction = tune() 
             ) |> 
  set_mode("classification") 

#rf_recipe_weighted <- recipe(mentalissue ~ ., data = train_weighted) 
# reuse recipe from r2

rf_wflow <- workflow() |> 
  add_recipe(rf_recipe_weighted) |> 
  add_model(rf_spec) 

# Define a custom quantitative parameter for sample.fraction
sample_fraction_param <- dials::new_quant_param(
  type = "double",
  range = c(0.2, 0.8),
  inclusive = c(TRUE, TRUE),
  label = c(sample.fraction = "sample.fraction")
)

rf_params <- extract_parameter_set_dials(rf_wflow) |> 
  update(mtry = mtry(c(1, 15)), 
         trees = trees(c(100, 1000)),
         max.depth = tree_depth(c(3, 15)), #was (3,8)
         sample.fraction = sample_fraction_param
        ) |>
  finalize(train_weighted) 

#rf_grid <- grid_regular(rf_params, levels = 2) 
rf_grid <- grid_random(rf_params, size = 1000)
dim(rf_grid)

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

set.seed(67)
system.time(
  rf_res3 <- tune_grid(
    rf_wflow,
    resamples = folds_weighted,
    grid = rf_grid,
    metrics = metric_set(bal_accuracy, accuracy, roc_auc),
    control = control_grid(verbose = TRUE) # Shows progress bars in the console
  )
) 
 
stopCluster(my_cluster)
registerDoSEQ()

#autoplot(rf_res3, rf_params) #wont work

tune_metrics <- collect_metrics(rf_res3)
View(tune_metrics)
# can use ggplot to plot but won't

show_best(rf_res3, metric = "roc_auc")

# finalize wkflow to get final fit
final_rf_wflow <- finalize_workflow(
  rf_wflow, 
  select_best(rf_res3, metric = "roc_auc"))

rf_fit3_tuned <- fit(final_rf_wflow, data = train_weighted)

eval_mdl(rf_fit3_tuned)
eval_test(rf_fit3_tuned)

## RF results -------------

# train vs test
rbind(
eval_mdl(rf_fit1_vanilla) |> mutate(mdl = "0vanilla"),
eval_mdl(rf_fit1_tuned) |> mutate(mdl = "1tuned"),
eval_mdl(rf_fit2_tuned) |> mutate(mdl = "2tuned_weighted"),
eval_mdl(rf_fit3_tuned) |> mutate(mdl = "3tuned_weighted+1")
) |>
  arrange(desc(.metric))

# f1, prec, recall on test
rbind(
eval_test(rf_fit1_vanilla) |> mutate(mdl = "0vanilla"),
eval_test(rf_fit1_tuned) |> mutate(mdl = "1tuned"),
eval_test(rf_fit2_tuned) |> mutate(mdl = "2tuned_weighted"),
eval_test(rf_fit3_tuned) |> mutate(mdl = "3tuned_weighted")
) |>
  arrange(desc(class))


## SHAP for RFs --------------------------

# compute in paralel shap values for all 3 trees
num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

(start <- Sys.time())
rf0_ks <- kernelshap(
  rf_fit1_vanilla,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)
rf0_sv <- shapviz(rf0_ks)

rf1_ks <- kernelshap(
  rf_fit1_tuned,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)
rf1_sv <- shapviz(rf1_ks)

rf2_ks <- kernelshap(
  rf_fit2_tuned,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)
rf2_sv <- shapviz(rf2_ks)

rf3_ks <- kernelshap(
  rf_fit3_tuned,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)
rf3_sv <- shapviz(rf3_ks)

print(Sys.time() - start) #12 hrs

stopCluster(my_cluster)
registerDoSEQ()

sv_importance(rf0_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("rf0")
sv_importance(rf1_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("rf1")
sv_importance(rf2_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("rf2")
sv_importance(rf3_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("rf3")



# sandbox ---------------------------------

## lr ----------------------------------

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


## lasso ---------------------------------------------

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


## LR SHAP

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


## tree plot ----------------

# save full tree to pdf for reference
final_tree_engine <- extract_fit_engine(final_fit)

pdf("./outputsR/final_decision_tree.pdf", width = 10, height = 8)
rpart.plot(
  final_tree_engine, 
  type = 5,           # Split labels under the nodes
  extra = 2,        # Displays number of observations and percentages
  under = TRUE,       # Places numbers under the nodes
  tweak = 1.5,
  #fallen.leaves = TRUE, # Align leaf nodes at the bottom
  main = "Final Decision Tree Model",
  box.palette = list("#2980b9", "#c0392b"),
)
dev.off()

# plot the tree (without country)
tree_no_country <- rpart(
  formula = mentalissue ~ . -country, 
  data = hbsc_se_train,
  method = "class",                             # Ensures a classification tree
  control = rpart.control(
    cp = 0.0000000001, #was 0.0000000001
    maxdepth = 8, 
    minsplit = 40)
)

printcp(tree_no_country)
pruned_tree_no_country <- prune(tree_no_country, cp = 0.0021669493)

rpart.plot(pruned_tree_no_country,
           type = 5,
           tweak = 2,
           under = TRUE,
           fallen.leaves = TRUE,
           extra = 2,
           box.palette = list("#2980b9", "#c0392b")
)





sv_importance(sv_rf, kind = "bee", alpha = 0.4) + theme_minimal()

# plot dependence plots side 2 side - side 1 is clean; side 2 is with highest iterator
library(patchwork)

xvars
for (i in 1:length(xvars)){
  print(i)
  p1 <- sv_dependence(sv_rf, v = xvars[i], color_var = NULL) + theme_minimal() #no interaction color
  p2 <- sv_dependence(sv_rf, v = xvars[i], alpha = 0.5) + theme_minimal() #color by interaction
  combined_dependence_plots <- p1 + p2
  print(combined_dependence_plots)
}


# # waterfall plot examples to see someone with diff mental pred
# (wat1 <- sv_waterfall(tree_sv$.pred_Yes, 
#                       max_display = 12,
#                       row_id = 2) + 
#     theme_minimal() + ggtitle("NO Mental issue"))
# 


## corels ------------------

 
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

