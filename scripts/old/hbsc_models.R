# HBSC base log reg
# Updated: 2026-07-17

# ---------------------------------------------------------
# project setup
# ---------------------------------------------------------

rm(list = ls())

project_folder <- "C:/MisLocalFiles/Github/HBSC/"
setwd("C:/MisLocalFiles/Github/HBSC/scripts")

require(tidyverse)
require(caret)
require(broom)


# ---------------------------------------------------------
# load dataset
# ---------------------------------------------------------

dat_file_path <- paste0(project_folder, "data/processed/dat_2026-07-17.csv")
dat <- read_csv(dat_file_path)
dim(dat) #244097 x 48


# ---------------------------------------------------------
# explore vars to predict
# ---------------------------------------------------------

#lifesat_low
dat %>%
  group_by(country_name, lifesat_low) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  filter(!is.na(lifesat_low)) %>%
  mutate(is_lifesat_low = case_when(
    lifesat_low == 0 ~ "No",
    lifesat_low == 1 ~ "Yes")) %>%
  select(c(country_name, is_lifesat_low, n)) %>%
  pivot_wider(names_from = is_lifesat_low,
              values_from = n) %>%
  mutate(n = No + Yes) %>%
  mutate(life_sat_low_pct = Yes / (Yes + No)) %>%
  select(country_name, life_sat_low_pct, n) %>%
  arrange(desc(life_sat_low_pct)) %>%
  print(n = Inf)
# The worst: Turkey at 33%; NL P-2 7%

#mental_complaintsY
dat %>%
  group_by(country_name, mental_complaintsY) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  filter(!is.na(mental_complaintsY)) %>%
  pivot_wider(names_from = mental_complaintsY,
              names_prefix = "mental_complaintsY",
              values_from = n) %>%
  mutate(n = mental_complaintsY0 + mental_complaintsY1) %>%
  mutate(mental_complaintsY_pct = mental_complaintsY1 / n) %>%
  select(country_name, mental_complaintsY_pct, n) %>%
  arrange(desc(mental_complaintsY_pct)) %>%
  print(n = Inf)
# worst turkey at 52%; NL p-3 17%

# factor recoding
dat <- dat %>%
  mutate(lifesat_low = factor(lifesat_low, 
                              levels = c("0", "1")),
         pmsu_lmh = factor(pmsu_lmh, 
                           levels = c("low", "med", "high")),
         mental_complaintsY = factor(mental_complaintsY, 
                           levels = c("0", "1")),
         IRRELFAS_LMH_r = factor(IRRELFAS_LMH_r,
                                 levels = c("high20", "med60", "low20")),
         family_support_high = factor(family_support_high, 
                                     levels = c("1", "0"))
  )


# ------------------------------------------------------------------------
# Turkey base lifesat_low models
# ------------------------------------------------------------------------

turkey <- dat %>%
  filter(country_name == "Turkey") %>%
  select(seqno_int, lifesat_low, mental_complaintsY, pmsu_yn, pmsu_lmh, sex_r, IRRELFAS_LMH_r,
         family_support_high)

turkey <- na.omit(turkey)

netherlands <- dat %>%
  filter(country_name == "Netherlands") %>%
  select(seqno_int, lifesat_low, mental_complaintsY, pmsu_yn, pmsu_lmh, sex_r, IRRELFAS_LMH_r,
         family_support_high)

netherlands <- na.omit(netherlands)

# turkey
mdl <- glm(lifesat_low ~ pmsu_lmh,
           data = turkey,
           family = binomial)

mdl <- glm(mental_complaintsY ~ pmsu_lmh,
           data = turkey,
           family = binomial)

# netherlands
mdl <- glm(lifesat_low ~ pmsu_lmh,
           data = netherlands,
           family = binomial)

mdl <- glm(mental_complaintsY ~ pmsu_lmh,
           data = netherlands,
           family = binomial)

# netherlands with interactions
mdl <- glm(mental_complaintsY ~ pmsu_lmh * IRRELFAS_LMH_r,
           data = netherlands,
           family = binomial)

mdl <- glm(mental_complaintsY ~ pmsu_lmh * family_support_high,
           data = netherlands,
           family = binomial)

summary(mdl)

tidy(mdl, exponentiate = TRUE, conf.int = TRUE)

# plot NL interations pmsu_lmh * IRRELFAS_LMH_r
newdata <- expand.grid(
  pmsu_lmh = levels(netherlands$pmsu_lmh),
  IRRELFAS_LMH_r = levels(netherlands$IRRELFAS_LMH_r)
)


newdata$pred_prob <- predict(mdl, newdata = newdata, type = "response")

ggplot(newdata, aes(x = pmsu_lmh, y = pred_prob,
                    color = IRRELFAS_LMH_r, group = IRRELFAS_LMH_r)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  labs(title = "Interaction: PMSU × IRRELFAS",
       x = "PMSU Level",
       y = "Predicted Probability of Mental Complaints") +
  theme_minimal(base_size = 14)

# plot interaction pmsu_lmh * family_support_high
newdata <- expand.grid(
  pmsu_lmh = levels(netherlands$pmsu_lmh),
  family_support_high = c(0, 1)
) %>%
  mutate(family_support_high = factor(family_support_high))

newdata$pred_prob <- predict(mdl, newdata = newdata, type = "response")

ggplot(newdata, aes(x = pmsu_lmh, y = pred_prob,
                    color = factor(family_support_high),
                    group = family_support_high)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  labs(title = "Interaction: PMSU × Family Support",
       x = "Problematic Social Media Use",
       y = "Predicted Probability of Mental Complaints",
       color = "Family Support\n(0 = low, 1 = high)") +
  theme_minimal(base_size = 14)


# set.seed(93446)
# train_index <- sample(seq_len(nrow(canada_mdl)), size = 0.7 * nrow(canada_mdl))
# canada_train <- canada_mdl[train_index, ]
# canada_test  <- canada_mdl[-train_index, ]
# 

# 
# canada_train$family_support_high <- factor(canada_train$family_support_high)
# canada_train$canada_train$family_support_high <- relevel(canada_train$family_support_high, ref = "1")
# 
# #canada_train$lifesat_low <- ifelse(canada_train$lifesat_high == 1, 0, 1)
# #table(canada_train$lifesat_low, canada_train$lifesat_high)
# 
# 

# mdl1 <- glm(as.factor(multiple_mental_complaints) ~ pmsu_yn * family_support_high,
#             data = canada_train,
#             family = binomial)
# 
 


#exp(cbind(OR = coef(mdl1), confint(mdl1)))

# canada_test$pred_prob <- predict(mdl1, newdata = canada_test, type = "response")
# canada_test$pred_class <- ifelse(canada_test$pred_prob >= 0.5, 1, 0)
# 
# confusionMatrix(
#   factor(canada_test$pred_class, levels = c(0,1)),
#   factor(canada_test$lifesat_high, levels = c(0,1))
# )


