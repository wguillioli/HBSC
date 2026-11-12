# HBSC 
# Updated: 2026-11-06

# setup ----------------------------------------------

rm(list = ls())

prj_fldr <- "C:/_GitHub/HBSC/"
setwd(prj_fldr)

options(scipen = 999)

require(tidyverse)
require(tidymodels)
require(stringr)
library(scales)
require(rsample)
require(countrycode)
require(rpart.plot)
library(kernelshap)
library(shapviz)
library(doParallel)
library(corrplot)
library(tinytable)
library(ggridges)
library(coefplot)
require(kableExtra)
require(patchwork)
#library(psych)
#require(naniar) #na vis
#library(janitor) #cool new plots



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

## train, test, folds --------------------------

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


## train weighted ------------------
# it seems not needed anymore so will comment for now; below should do
# train_weighted <- train |>
#   mutate(wts = ifelse(mentalissue == "Yes", 2.70, 1.0), # is this right?
#          wts = importance_weights(wts))
# 
# set.seed(67)
# folds_weighted <- vfold_cv(train_weighted, v = 5, strata = mentalissue)


## hbsc_se weighted ----------------

# add weight
hbsc_se_weighted <- hbsc_se |>
  mutate(wts = ifelse(mentalissue == "Yes", 2.70, 1.0), # is this right?
         wts = importance_weights(wts))

# recreate splits with weight
set.seed(67)
hbsc_se_weighted_split  <- initial_split(hbsc_se_weighted, prop = 8/10, strata = mentalissue)
train_weighted <- training(hbsc_se_weighted_split)
test_weighted <- testing(hbsc_se_weighted_split)

# recreated folds with weight
set.seed(67)
folds_weighted <- vfold_cv(train_weighted, v = 5, strata = mentalissue)
  



# mdl functions -----------------------

# function takes final fit and returns metrics table for train & test
eval_mdl <- function(the_fit){
  
  eval_metrics <- metric_set(roc_auc, bal_accuracy, accuracy)
  
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



# Trees ----------------------

## Tree0 (Vanilla) --------------------
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



## Tree1 (Tuned, imbalanced) ------------------------------------------------
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
print(Sys.time() - start) #45 mins

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





## Tree2 (Tuned, weighted)  -----------------
# Tuned on weighted data

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
  control = control_grid(verbose = TRUE) 
) 
print(Sys.time() - start) #45 mins

stopCluster(my_cluster)
registerDoSEQ()

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


## Trees ROCS -----------------
tree0_roc <- augment(tree0, new_data = test) %>% 
  roc_curve(truth = mentalissue, .pred_No) %>% 
  mutate(model = "Tree0")

tree1_roc <- augment(tree1, new_data = test) %>% 
  roc_curve(truth = mentalissue, .pred_No) %>% 
  mutate(model = "Tree1")

tree2_roc <- augment(tree2, new_data = test) %>% 
  roc_curve(truth = mentalissue, .pred_No) %>% 
  mutate(model = "Tree2")

bind_rows(tree0_roc, tree1_roc, tree2_roc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(linewidth = 1.2, alpha = 0.8) +
  geom_abline(lty = 3, color = "gray50") +  
  coord_equal() +
  labs(
    title = "ROC Curves Trees (test)",
    color = "Model"
  ) +
  theme_minimal()


# Log Reg --------------------------
# 3 versions: one with imbalanced dataset and one with dataset with weights

## LR0 (Vanilla) ------------------
# simple train and test, no folds
lr0_spec <- logistic_reg() |>   
  set_engine("glm") |>   
  set_mode("classification")

lr0_recipe <- recipe(mentalissue ~ ., data = train) |> 
  step_rm(IRFAS) #not signif

lr0_workflow <- workflow() |>   
  add_recipe(lr0_recipe) |>   
  add_model(lr0_spec)

lr0 <- fit(lr0_workflow, data = train)

eval_mdl(lr0)
eval_test(lr0)

# see fit (takes a min)
#lr0 |> extract_fit_parsnip() |> tidy(exponentiate = FALSE, conf.int = TRUE)


## LR1 (imbalanced) ----------
lr1_spec <- logistic_reg() |>
  set_engine("glm") %>%
  set_mode("classification")

lr1_recipe <- recipe(mentalissue ~ ., data = train)

lr1_workflow <- workflow() |>
  add_recipe(lr1_recipe) %>%
  add_model(lr1_spec)

lr1_cv_results <- fit_resamples(
  lr1_workflow,
  resamples = folds,
  metrics = metric_set(bal_accuracy, roc_auc),
  control = control_resamples(save_pred = TRUE)
)

# get metrics, overall and for each fold
lr1_cv_results |> collect_metrics(summarize = FALSE) |> arrange(.metric)
lr1_cv_results |> collect_metrics()

# final fit and extract the model
lr1_final_fit <- last_fit(lr1_workflow, hbsc_se_split)
lr1 <- extract_workflow(lr1_final_fit)

eval_mdl(lr1)
eval_test(lr1)

# see fit (takes a min)
#lr1 |> extract_fit_parsnip() |> tidy(exponentiate = FALSE, conf.int = TRUE)


## LR2 (weighted) --------------
lr2_spec <- logistic_reg() |>
  set_engine("glm") %>%
  set_mode("classification")

lr2_recipe <- recipe(mentalissue ~ ., data = train_weighted) # |>
 # update_role(wts, new_role = "case_weights") #ai suggestion won't work

lr2_workflow <- workflow() |>
  add_recipe(lr2_recipe) |>
  add_model(lr2_spec) |>
  add_case_weights(wts)

lr2_cv_results <- fit_resamples(
  lr2_workflow,
  resamples = folds_weighted,
  metrics = metric_set(bal_accuracy, roc_auc),
  control = control_resamples(save_pred = TRUE)
)

# get metrics, overall and for each fold
lr2_cv_results |> collect_metrics(summarize = FALSE) |> arrange(.metric)
lr2_cv_results |> collect_metrics()

# final fit and extract the model
lr2_final_fit <- last_fit(lr2_workflow, hbsc_se_weighted_split)
lr2 <- extract_workflow(lr2_final_fit)

eval_mdl(lr2)
eval_test(lr2)

# see fit (takes a min)
#lr2 |> extract_fit_parsnip() |> tidy(exponentiate = FALSE, conf.int = TRUE)


## LR ROCS -----------------
lr1_roc <- augment(lr1, new_data = test) %>% 
  roc_curve(truth = mentalissue, .pred_No) %>% 
  mutate(model = "lr1")

lr2_roc <- augment(lr2, new_data = test) %>% 
  roc_curve(truth = mentalissue, .pred_No) %>% 
  mutate(model = "lr2")

bind_rows(lr1_roc, lr2_roc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(linewidth = 1.2, alpha = 0.4) +
  geom_abline(lty = 3, color = "gray50") +  
  coord_equal() +
  labs(
    title = "LR ROCs (test)",
    color = "Model"
  ) +
  theme_minimal()


## LR Coef Tables -------------------------------

library(gtools)
library(knitr)
library(kableExtra)

### LR1 Coeffs tbl --------------------------

lr1_coef_tbl <- 
  lr1 |> 
  extract_fit_parsnip() |> 
  tidy() |>
  mutate(
    coefficient = round(estimate, 3),
    odds_ratio  = round(exp(estimate), 3),
    p.value_short = pvalue(p.value, accuracy = 0.001),
    p.value_rounded = round(p.value,5),
    stars = gtools::stars.pval(p.value),
    coefficient_star = paste0(coefficient, stars)
  ) |>
  select(term, coefficient, odds_ratio, p.value_short)

# Generate a clean, Word-friendly HTML table layout
lr1_coef_tbl |>
  kbl(
    align = c("l", "r", "r"), # Left-align terms, right-align numbers
    escape = FALSE            # Ensures stars (*, **, ***) display cleanly
  ) |>
  kable_classic(
    full_width = FALSE, 
    html_font = "Calibri" # Matches Word's standard font family
  ) |>
  row_spec(0, bold = TRUE) |> # Bold the header row
  footnote(
    general = "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
    general_title = "LR1 Coeff Table"
  )


### LR2 Coeffs tbl --------------------------

lr2_coef_tbl <- 
lr2 |> 
  extract_fit_parsnip() |> 
  tidy() |>
  mutate(
    coefficient = round(estimate, 3),
    odds_ratio  = round(exp(estimate), 3),
    p.value_short = pvalue(p.value, accuracy = 0.001),
    p.value_rounded = round(p.value,5),
    stars = gtools::stars.pval(p.value),
    coefficient_star = paste0(coefficient, stars)
  ) |>
  select(term, coefficient, odds_ratio, p.value_short)

# Generate a clean, Word-friendly HTML table layout
lr2_coef_tbl |>
  kbl(
    align = c("l", "r", "r"), # Left-align terms, right-align numbers
    escape = FALSE            # Ensures stars (*, **, ***) display cleanly
  ) |>
  kable_classic(
    full_width = FALSE, 
    html_font = "Calibri" # Matches Word's standard font family
  ) |>
  row_spec(0, bold = TRUE) |> # Bold the header row
  footnote(
    general = "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
    general_title = "LR2 Coeff Table"
  )


### LR2 Coeffs plot --------------------------
# instead of boring table, a nice coef plot?
library(GGally)
library(stats)

lr2_extracted <- extract_fit_parsnip(lr2)

ggcoef_model(lr2_extracted, 
             exponentiate = TRUE,
             add_reference_rows = FALSE,
             show_p_values = TRUE,
             signif_stars = FALSE,
             intercept = FALSE
             )



# Random Forests --------------------------------
# 4fits

require(ranger)


## RF0 vanilla --------------------
# imbalanced, fit on train, test on test
rf0_spec <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification")

rf0 <- rf0_spec |> 
  fit(mentalissue ~ ., data = train)

eval_mdl(rf0)
eval_test(rf0)


## RF1 tuned -------------------
# imbalanced, tunes the 3 from tidymodels
rf1_spec <- rand_forest(
  mtry = tune(),    
  min_n = tune(),   
  trees = tune() 
) |> set_engine("ranger") |> 
  set_mode("classification") 

rf1_recipe <- recipe(mentalissue ~ ., data = train) 

rf1_wflow <- workflow() |> 
  add_recipe(rf1_recipe) |> 
  add_model(rf1_spec)

rf1_params <- extract_parameter_set_dials(rf1_wflow) |> 
  update(mtry = mtry(c(1, 15)), 
         trees = trees(c(100, 1000))) |>
  finalize(train) 

rf1_grid <- grid_regular(rf1_params, levels = 10)
dim(rf1_grid)
summary(rf1_grid)

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

set.seed(67)
(start <- Sys.time())
rf1_res <- tune_grid(
    rf1_wflow,
    resamples = folds,
    grid = rf1_grid,
    metrics = metric_set(bal_accuracy, accuracy, roc_auc),
    control = control_grid(verbose = TRUE) # Shows progress bars in the console
)
print(Sys.time() - start) #17 hrs

stopCluster(my_cluster)
registerDoSEQ()

#autoplot(rf_res1)

show_best(rf1_res, metric = "roc_auc")

# finalize wkflow to get final fit
final_rf1_wflow <- finalize_workflow(
  rf1_wflow, 
  select_best(rf1_res, metric = "roc_auc"))

rf1 <- fit(final_rf1_wflow, data = train)

eval_mdl(rf1)
eval_test(rf1)


## RF2 tuned weighted -------------------
# same as above but on weighted dataset

rf2_recipe <- recipe(mentalissue ~ ., data = train_weighted) 

rf2_wflow <- workflow() |> 
  add_recipe(rf2_recipe) |> 
  add_model(rf1_spec) 

rf2_params <- extract_parameter_set_dials(rf2_wflow) |> 
  update(mtry = mtry(c(1, 15)), 
         trees = trees(c(100, 1000))) |>
  finalize(train_weighted) 

rf2_grid <- grid_random(rf2_params, size = 1000)
dim(rf2_grid)
#summary(rf_grid)

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

set.seed(67)
(start <- Sys.time())
rf2_res <- tune_grid(
    rf2_wflow,
    resamples = folds_weighted,
    grid = rf2_grid,
    metrics = metric_set(bal_accuracy, accuracy, roc_auc),
    control = control_grid(verbose = TRUE) # Shows progress bars in the console
  )
print(Sys.time() - start) #18 hrs

stopCluster(my_cluster)
registerDoSEQ()

#autoplot(rf_res2)

show_best(rf2_res, metric = "roc_auc")

# finalize wkflow to get final fit
final_rf2_wflow <- finalize_workflow(
  rf2_wflow, 
  select_best(rf2_res, metric = "roc_auc"))

rf2 <- fit(final_rf2_wflow, data = train_weighted)

eval_mdl(rf2)
eval_test(rf2)


## RF3 tuned+1 weighted -------------------------
# builds on rf2 adding max.depth

rf3_spec <- rand_forest(
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

rf3_wflow <- workflow() |> 
  add_recipe(rf2_recipe) |> 
  add_model(rf3_spec) 

# Define a custom quantitative parameter for sample.fraction
sample_fraction_param <- dials::new_quant_param(
  type = "double",
  range = c(0.2, 0.8),
  inclusive = c(TRUE, TRUE),
  label = c(sample.fraction = "sample.fraction")
)

rf3_params <- extract_parameter_set_dials(rf3_wflow) |> 
  update(mtry = mtry(c(1, 15)), 
         trees = trees(c(100, 1000)),
         max.depth = tree_depth(c(3, 15)), #was (3,8)
         sample.fraction = sample_fraction_param
  ) |>
  finalize(train_weighted) 

rf3_grid <- grid_random(rf3_params, size = 1000)
dim(rf3_grid)

num_cores <- detectCores() - 1 
my_cluster <- makeCluster(num_cores)
registerDoParallel(my_cluster)
print(my_cluster)

set.seed(67)
(start <- Sys.time())
rf3_res <- tune_grid(
    rf3_wflow,
    resamples = folds_weighted,
    grid = rf3_grid,
    metrics = metric_set(bal_accuracy, accuracy, roc_auc),
    control = control_grid(verbose = TRUE) # Shows progress bars in the console
)
print(Sys.time() - start) # 7.5 hrs

stopCluster(my_cluster)
registerDoSEQ()

#autoplot(rf_res3, rf_params) #wont work

tune_metrics <- collect_metrics(rf3_res)
View(tune_metrics)
# can use ggplot to plot but won't

show_best(rf3_res, metric = "roc_auc")

# finalize wkflow to get final fit
final_rf3_wflow <- finalize_workflow(
  rf3_wflow, 
  select_best(rf3_res, metric = "roc_auc"))

rf3 <- fit(final_rf3_wflow, data = train_weighted)

eval_mdl(rf3)
eval_test(rf3)




# SHAP ------------------------------------------

## SHAP functions ----------------

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

(xvars <- train |> select(-mentalissue) |> names())

set.seed(67)
X_explain <- test[sample(1:nrow(test), 500), xvars] 
X_background <- train[sample(1:nrow(train), 200), xvars] 

# get IDs of extreme cases of YES to explain with waterfall
which(X_explain$pmsu == "High" & 
        X_explain$cyberbullied == "Yes" & 
        X_explain$talkparent == "No")

# and for NO
which(X_explain$pmsu == "Low" & 
        X_explain$cyberbullied == "No" & 
        X_explain$talkparent == "Yes" &
        X_explain$frisupp == "High" &
        X_explain$famsupp == "High")


## SHAP on trees --------------------------

# parallel work on shap values for all trees
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

tree1_ks <- kernelshap(
  tree1,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)

tree2_ks <- kernelshap(
  tree2,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)

tree0_sv <- shapviz(tree0_ks)
tree1_sv <- shapviz(tree1_ks)
tree2_sv <- shapviz(tree2_ks)

stopCluster(my_cluster)
registerDoSEQ()
print(Sys.time() - start) #20 mins

# importance plots for all trees
sv_importance(tree0_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree0")
sv_importance(tree1_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree1")
sv_importance(tree2_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree2")

p0 <- sv_importance(tree0_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree0")
p1 <- sv_importance(tree1_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree1")
p2 <- sv_importance(tree2_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("tree2")

combined_sv_trees <- p0 + p1 + p2
combined_sv_trees + plot_annotation(title = "sv_importance all trees")

# bee plots for all trees
p0 <- sv_importance(tree0_sv, kind = "bee", alpha = 0.4) + theme_minimal() + ggtitle("tree0")
p1 <- sv_importance(tree1_sv, kind = "bee", alpha = 0.4) + theme_minimal() + ggtitle("tree1")
p2 <- sv_importance(tree2_sv, kind = "bee", alpha = 0.4) + theme_minimal() + ggtitle("tree2")
combined_sv_trees <- p0 + p1 + p2
combined_sv_trees + plot_annotation(title = "bee all trees")

# for best tree (2) plot dependence plots
xvars
for (i in 1:length(xvars)){
  print(i)
  p1 <- sv_dependence(tree2_sv, v = xvars[i], color_var = NULL) + theme_minimal() #no interaction color
  p2 <- sv_dependence(tree2_sv, v = xvars[i], alpha = 0.5) + theme_minimal() #color by interaction
  combined_dependence_plots <- p1 + p2
  print(combined_dependence_plots)
}

# dim(tree0_sv)
# head(tree0_sv)

# Compare a YES with all 3 trees
p0 <- sv_waterfall(tree0_sv, row_id = 277) + theme_minimal() + ggtitle("tree0")
p1 <- sv_waterfall(tree1_sv, row_id = 277) + theme_minimal() + ggtitle("tree1")
p2 <- sv_waterfall(tree2_sv, row_id = 277) + theme_minimal() + ggtitle("tree2")
combined_tree_waterfalls <- p0 + p1 + p2
combined_tree_waterfalls + plot_annotation(title = "Waterfall of same YES with all trees")


# Now with tree2 compare a YES/NO
p1 <- sv_waterfall(tree2_sv, row_id = 277, max_display = 15) + theme_minimal() + ggtitle("YES")
p2 <- sv_waterfall(tree2_sv, row_id = 14, max_display = 15) + theme_minimal() + ggtitle("NO")
combined_sv_lr <- p1 + p2
combined_sv_lr + plot_annotation(title = "Tree2 Waterfall of YES vs NO")


# redo importance plots for all trees
# to show relative % instead of a shap log-odds

# #voy, repeat for all trees
# dim(tree2_sv$S)
# #500 observations explained; 15 features
# 
# # for each feature, returns avg of absoluate value to rank
# tree2_sv_global_imp <- colMeans(abs(tree2_sv$S)) 
# tree2_sv_global_imp
# 
# # return df with feature and relative calcualted sv
# tree2_rel_sv <- data.frame(
#   feature = names(tree2_sv_global_imp),
#   avg_abs_sv = tree2_sv_global_imp) |>
#   mutate(rel_imp_pct = round(100 * avg_abs_sv / sum(avg_abs_sv),2)) |>
#   arrange(desc(rel_imp_pct)) 
# 
# tree2_rel_sv |> select(feature, rel_imp_pct) |>
#   ggplot(aes(x=rel_imp_pct, y=reorder(feature, rel_imp_pct))) +
#   geom_col() +
#   theme_minimal() +
#   labs(title = "tree2", x = "relative importance %", y = "")






## SHAP on LRs ------------------------

lr1_ks <- kernelshap(
  lr1,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds #instead of type = link?
)

lr2_ks <- kernelshap(
  lr2,
  X = X_explain, 
  bg_X = X_background, 
  pred_fun = pf_log_odds
)

lr1_sv <- shapviz(lr1_ks)
lr2_sv <- shapviz(lr2_ks)

p1 <- sv_importance(lr1_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("LR1")
p2 <- sv_importance(lr2_sv, show_numbers = TRUE) + theme_minimal() + ggtitle("LR2")
combined_sv_lr <- p1 + p2
combined_sv_lr + plot_annotation(title = "sv_importance LRs")

p1 <- sv_importance(lr1_sv, kind = "bee", alpha = 0.4) + theme_minimal() + ggtitle("LR1")
p2 <- sv_importance(lr2_sv, kind = "bee", alpha = 0.4) + theme_minimal() + ggtitle("LR2")
combined_sv_lr <- p1 + p2
combined_sv_lr + plot_annotation(title = "bees LRs")


# for best tree (2) plot dependence plots
xvars
for (i in 1:length(xvars)){
  print(i)
  p1 <- sv_dependence(lr2_sv, v = xvars[i], color_var = NULL) + theme_minimal() 
  p2 <- sv_dependence(lr2_sv, v = xvars[i], alpha = 0.5) + theme_minimal()
  combined_dependence_plots <- p1 + p2
  print(combined_dependence_plots)
}

# YES
sv_waterfall(lr2_sv, row_id = 277, max_display = 15) 
sv_waterfall(lr2_sv, row_id = 348, max_display = 15) 

# NO
sv_waterfall(lr2_sv, row_id = 14, max_display = 15) 
sv_waterfall(lr2_sv, row_id = 18, max_display = 15) 

p1 <- sv_waterfall(lr2_sv, row_id = 277, max_display = 15) + theme_minimal() + ggtitle("YES")
p2 <- sv_waterfall(lr2_sv, row_id = 14, max_display = 15) + theme_minimal() + ggtitle("NO")
combined_sv_lr <- p1 + p2
combined_sv_lr + plot_annotation(title = "LR Waterfall of YES vs NO")

p1 <- sv_waterfall(lr2_sv, row_id = 348) + theme_minimal() + ggtitle("YES")
p2 <- sv_waterfall(lr2_sv, row_id = 18) + theme_minimal() + ggtitle("NO")
combined_sv_lr <- p1 + p2
combined_sv_lr + plot_annotation(title = "LR Waterfall of YES vs NO")



#load("./RData/hbsc_20261103.RData")
save.image("./RData/hbsc_67.RData")


#img_file <- paste0(prj_fldr, "RData/hbsc_20261103.RData")
#load(file = img_file)
#save.image(file = img_file)

