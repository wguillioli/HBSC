
rm (list = ls())

#install.packages("vip", repos = c("https://bgreenwell.r-universe.dev", "https://cloud.r-project.org"))

require(coefplot)
#require(vip)
library(ROCR)
library(tidymodels)
require(rpart.plot)
require(tidyverse)
require(randomForest)
require(ranger)
require(vip)

prj_fldr <- "C:/MisLocalFiles/Github/HBSC/"
dat_fldr <- paste0(prj_fldr,"data/")

d <- read_csv(paste0(dat_fldr, "processed/d.csv"))
glimpse(d)

#vars_list_mdls

summary(d)

# get baseline of potential y
ys <- c("lifesat_low", "mental_issue")

table_results <- map_dfr(ys, function(x) {
  d |> 
    filter(!is.na(.data[[x]])) |> 
    group_by(value = as.character(.data[[x]])) |> 
    tally() |> 
    mutate(
      p = n / sum(n),
      variable = x
    ) |> 
    relocate(variable, .before = value)
})
print(table_results) 

# let's go with mental_issue so get baseline of y by country
d %>%
  group_by(country_name, mental_issue) %>%
  tally() %>%
  pivot_wider(names_from = mental_issue,
              names_prefix = "is",
              values_from = n,
              values_fill = 0) %>%
  mutate(n = is0 + is1 + isNA,
         pis1 = round(is1 / n,2)) %>%
  arrange(pis1) %>%
  print(n = Inf) #turkey, italy worst

# deal with missing values
dc <- d[complete.cases(d),]


# coerce what needs to be coerced

vars_to_factor <- c(
  "country_name", "IOTF4", "agecat", "sex", "IRRELFAS_LMH", 
  "lifesat_low", "mental_issue", "pmsu_lmh", "cbullied", 
  "onlfreq_is", "onlpref", "famsup_hml", "frisup_hml", 
  "teachersup", "studsup", "talkf", "talkm", "timeexe_r"
)

dc <- dc |> 
  mutate(across(all_of(vars_to_factor), as.factor))

# filter to 2 countries
# and select vars of first group (fct > sum)
vars_to_keep_grp1 <- c(
  # personal
  "seqno_int",      
  "country_name",   
  "bodyheight", "bodyweight", "MBMI", "IOTF4",
  "agecat", "sex",
  "IRRELFAS_LMH", #> "IRFAS"         
  # y 
  #"lifesat_low"    
  "mental_issue", #> "mental_sum"     
  # pmsu and bad online
  "pmsu_lmh", #> "pmsu_sum"      
  "cbullied", #"bulliedsum"     
  "onlfreq_is", # > "onlfreq_sum"    
  "onlpref", #> "onlpref_sum"    
  #shields 
  "famsup_hml", # > "famsup_sum"     
  "frisup_hml", #> "frisup_sum"    
  "talkf", "talkm", #> "talkscore"      
  "teachersup", #> "teachersup_sum" 
  "studsup", #> "studsup_sum"
  "timeexe_r"# > "timeexe" 
)

dmdl1 <- dc %>%
  filter(country_name %in% c("Turkey", "Italy")) %>%
  select(all_of(vars_to_keep_grp1))

glimpse(dmdl1)

# split train/test for worst countries on mental_issue
set.seed(67)
dmdl1_split <- initial_split(dmdl1, prop = .8, strata = "mental_issue")
dmdl1_split
dmdl1_train <- training(dmdl1_split)
dim(dmdl1_train)
dmdl1_test  <- testing(dmdl1_split)
dim(dmdl1_test)

#confirm y is balanced
counts_train <- table(dmdl1_train$mental_issue)
round(counts_train["1"] / sum(counts_train),2)

counts_test <- table(dmdl1_test$mental_issue)
round(counts_test["1"] / sum(counts_test),2)

# intialize df that will hold model results for report
mdl_results <- data.frame(
  model = character(),
  test_accuracy = numeric(),
  test_auc = numeric(),
  stringsAsFactors = FALSE
)

# Create CV folds of the customers tibble
set.seed(67)
folds <- vfold_cv(dmdl1, 10)


# -----------------------------------------------------------------
# Fit a mickey mouse tree for all vars
# -----------------------------------------------------------------

# define model
tree_spec <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# train the model
tree_mdl1 <- tree_spec %>% 
  fit(mental_issue ~ . - seqno_int, 
      data = dmdl1_train)

# plot rpart object (extract first)
tree_mdl1

tree_mdl1 %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE,
             tweak = 1.3)

# Predict with model class
predictions <- predict(tree_mdl1,
                       new_data = dmdl1_test)

predictions_combined <- predictions %>% 
  mutate(true_class = dmdl1_test$mental_issue)

head(predictions_combined)

# The confusion matrix
conf_matrix1 <- conf_mat(predictions_combined,
                            estimate = .pred_class,
                            truth = true_class)

conf_matrix1

# The accuracy calculated by a function
accur1 <- accuracy(predictions_combined,
                     estimate = .pred_class,
                     truth = true_class)
accur1$.estimate

# pred probs
prob_preds <- predict(tree_mdl1, 
                      new_data = dmdl1_test,
                      type = "prob")

prob_preds_combined <- prob_preds %>%
  mutate(true_class = dmdl1_test$mental_issue)

auc <- roc_auc(prob_preds_combined, truth = true_class, .pred_0) #0 o 1???
auc$.estimate

row_mdl_results <- data.frame(
  model = "rpart_allvars_basic",
  test_accuracy = accur1$.estimate,
  test_auc = auc$.estimate)

mdl_results <- mdl_results %>%
  bind_rows(row_mdl_results)

# ROC curve
roc <- roc_curve(prob_preds_combined, 
                 truth = true_class,
                 .pred_0)
autoplot(roc)



# -----------------------------------------------------------------
# Fit a tuned tree for all vars
# -----------------------------------------------------------------

# define model
tree_spec <- decision_tree(
  tree_depth = tune(),
  cost_complexity = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# define grid
tree_grid <- tree_spec %>%
  extract_parameter_set_dials() %>%
  grid_regular(levels = 10)



# Tune along the grid
start_time <- Sys.time()
tune_results <- tune_grid(tree_spec, 
                          mental_issue ~ . -seqno_int,
                          resamples = folds,
                          grid = tree_grid,
                          metrics = metric_set(accuracy, roc_auc))
Sys.time() - start_time #3mins

collect_metrics(tune_results)
autoplot(tune_results)

# Select the parameters that perform best and fit final model
final_params <- select_best(tune_results, metric = "roc_auc")
final_params

best_spec <- finalize_model(tree_spec, final_params)

final_model <- fit(best_spec,
                    mental_issue ~ . -seqno_int,
                    dmdl1)
final_model

final_model %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE,
             tweak = 1.3)

# Predict with model class
predictions <- predict(final_model,
                        new_data = dmdl1_test)
 
predictions_combined <- predictions %>% 
   mutate(true_class = dmdl1_test$mental_issue)
 
head(predictions_combined)

# The confusion matrix
conf_matrix1 <- conf_mat(predictions_combined,
                          estimate = .pred_class,
                          truth = true_class)

conf_matrix1
 
# The accuracy calculated by a function
accur1 <- accuracy(predictions_combined,
                    estimate = .pred_class,
                    truth = true_class)
accur1$.estimate
 
# pred probs
prob_preds <- predict(final_model, 
                       new_data = dmdl1_test,
                       type = "prob")
 
prob_preds_combined <- prob_preds %>%
   mutate(true_class = dmdl1_test$mental_issue)
 
auc <- roc_auc(prob_preds_combined, truth = true_class, .pred_0) #0 o 1???
auc$.estimate

row_mdl_results <- data.frame(
   model = "rpart_allvars_tuned",
   test_accuracy = accur1$.estimate,
   test_auc = auc$.estimate)
 
mdl_results <- mdl_results %>%
   bind_rows(row_mdl_results)

mdl_results


# -----------------------------------------------------------------
# Fit a mickey mouse rf for all vars
# -----------------------------------------------------------------

rf_mdl1 <- randomForest(mental_issue ~ . - seqno_int, 
                          data = dmdl1_train, 
                          importance = TRUE)

print(rf_mdl1)

varImpPlot(rf_mdl1)

rf_probs <- predict(rf_mdl1, newdata = dmdl1_test, type = "prob")

eval_df <- data.frame(
  truth = dmdl1_test$mental_issue,
  rf_probs
)

eval_df %>% 
  roc_auc(truth = truth, X0) #0.713

# acc
rf_mdl1$confusion #train

predictions <- predict(rf_mdl1, newdata = dmdl1_test)

eval_df <- data.frame(
  truth = dmdl1_test$mental_issue,
  estimate = predictions
)
 
cm <- conf_mat(eval_df, truth = truth, estimate = estimate)
print(cm)
summary(cm)

accuracy_value <- summary(cm) %>% 
  dplyr::filter(.metric == "accuracy") %>% 
  dplyr::pull(.estimate)
                          
rf_res <- data.frame(
  model = "RF_allvars_basic",
  test_accuracy = accuracy_value,
  test_auc =  auc$.estimate
)

mdl_results <- mdl_results %>%
  bind_rows(rf_res)


# -----------------------------------------------------------------
# Fit a tuned rf for all vars
# -----------------------------------------------------------------

# 1. Specify the random forest with tune() placeholders
spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

# 2. Set up a workflow (this binds the model and formula together)
rf_wf <- workflow() %>% 
  add_model(spec) %>% 
  add_formula(mental_issue ~ . -seqno_int)

# 3. Create a regular tuning grid (e.g., 3 levels per parameter = 27 combinations)
# Note: mtry() requires seeing the training data to know the maximum number of columns available
rf_grid <- grid_regular(
  mtry(range = c(1, 10)), # Adjust '10' to match the number of predictors in your data
  trees(),
  min_n(),
  levels = 3
)
rf_grid

# 4. Run the cross-validation tuning process
# (Assumes you have already defined your cross-validation object, e.g., cv_folds)
# add time
(start <- Sys.time())
tune_results <- tune_grid(
  rf_wf,
  resamples = folds, # <-- Replace with your rsample folds object
  grid = rf_grid,
  metrics = metric_set(accuracy, roc_auc)
)
tune_results
Sys.time() - start #23 mins

# Extract the best hyperparameters based on your preferred metric
best_params <- select_best(tune_results, metric = "roc_auc")
best_params

# 2. Finalize your workflow with those optimal parameters
final_wf <- finalize_workflow(rf_wf, best_params)

# 3. Fit on train and evaluate on test using last_fit()
# (Assumes your original split object is named 'dmdl1_split')
final_fit_res <- last_fit(final_wf, split = dmdl1_split)
final_fit_res

# 4. Extract all test predictions (automatically contains truth, class, and probabilities)
test_predictions <- collect_predictions(final_fit_res)
test_predictions

# Get accuracy and ROC AUC together in a clean table
cm_metrics <- collect_metrics(final_fit_res)
cm_metrics$.estimate[1]


# Or call them individually from the predictions data frame:
# test_predictions %>% accuracy(truth = mental_issue, estimate = .pred_class)
# test_predictions %>% roc_auc(truth = mental_issue, starts_with(".pred_"))
# Generate the raw text confusion matrix
cm <- conf_mat(test_predictions, truth = mental_issue, estimate = .pred_class)
print(cm)

# Optional: Plot the matrix visually as a heatmap
autoplot(cm, type = "heatmap")
# Generate the coordinates for the ROC curve 
# (Replace .pred_1 with the probability column for your target's first level)
roc_curve_data <- test_predictions %>% 
  roc_curve(truth = mental_issue, .pred_1)

# Plot the ROC Curve visual graph instantly
autoplot(roc_curve_data)

row_mdl_results <- data.frame(
  model = "rf_allvars_tuned",
  test_accuracy = cm_metrics$.estimate[1],
  test_auc = cm_metrics$.estimate[2])

mdl_results <- mdl_results %>%
  bind_rows(row_mdl_results)

save.image(file = "my_entire_workspace.RData")
#load("my_entire_workspace.RData")





# # do forward variable selection with first choice of vars (cat vs num)
# # Fit GML model using forward subset selection
# reg0 <- glm(mental_issue ~ pmsu_lmh, data = dmdl1_train, family = binomial) 
# summary(reg0)
# reg1 <- glm(mental_issue ~ . -seqno_int, 
#             data = dmdl1_train, family = binomial)
# summary(reg1)
# best <- stats::step(reg0, scope = formula(reg1), direction = "forward", k = 2) #2 (AIC)
# summary(best)
# #best$anova
# 
# #vip::vip(best)
# 
# #coefplot(best, intercept = FALSE, title = "Log-Odds Coefficients")
# mytidy <- tidy(best, exponentiate = TRUE, conf.int = TRUE)
# ggplot(mytidy, aes(x = term, y = estimate)) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
#                 width = 0.15, color = "gray40") +
#   geom_point(size = 3, color = "blue") +
#   coord_flip() +
#   labs(
#     title = "Coefficient Estimates with 95% Confidence Intervals",
#     x = "",
#     y = "Estimate"
#   ) +
#   theme_minimal(base_size = 14)
# 
# # ROC for both
# reg0_prob <- predict(reg0, dmdl_train, type = "response")
# reg1_prob <- predict(reg1, dmdl_train, type = "response")
# 
# # Compute AUC metrics for cv_model1 and cv_model3
# perf1 <- prediction(reg0_prob, dmdl_train$mental_issue) %>%
#   performance(measure = "tpr", x.measure = "fpr")
# perf2 <- prediction(reg1_prob, dmdl_train$mental_issue) %>%
#   performance(measure = "tpr", x.measure = "fpr")
# 
# # Plot ROC curves for cv_model1 and cv_model3
# dev.off() 
# par(mar = c(4, 4, 2, 1))
# plot(perf1, col = "black", lty = 2)
# plot(perf2, add = TRUE, col = "blue")
# abline(a = 0, b = 1, col = "red", linetype = "dashed", lwd = 2)
# legend(0.8, 0.2, legend = c("r0", "r1"),
#        col = c("black", "blue"), lty = 2:1, cex = 0.6)
# 
# 
# 
# 
# # get perf
# 
# 
# 
# 
# 
# # -----------------------------------------------
# # start demo log reg con tuning
# # -----------------------------------------------
# library(tidymodels)
# 
# # Set up sample binary classification data
# set.seed(123)
# data(two_class_dat, package = "modeldata")
# 
# head(two_class_dat)
# glimpse(two_class_dat)
# str(two_class_dat)
# 
# # Split into training and testing sets
# data_split <- initial_split(two_class_dat, strata = Class)
# train_data <- training(data_split)
# test_data <- testing(data_split)
# 
# # Create cross-validation folds
# cv_folds <- vfold_cv(train_data, v = 5, strata = Class)
# 
# lr_spec <- logistic_reg(
#   penalty = tune(),
#   mixture = 1
# ) |>
#   set_engine("glmnet") |>
#   set_mode("classification")
# 
# lr_recipe <- recipe(Class ~ ., data = train_data) |>
#   step_zv(all_predictors()) |>
#   step_normalize(all_numeric_predictors())
# 
# lr_workflow <- workflow() |>
#   add_model(lr_spec) |>
#   add_recipe(lr_recipe)
# 
# # Define a grid of penalty values to test
# penalty_grid <- grid_regular(penalty(), levels = 20)
# 
# # Tune the model using ROC AUC as the metric
# tuned_results <- tune_grid(
#   lr_workflow,
#   resamples = cv_folds,
#   grid = penalty_grid,
#   metrics = metric_set(roc_auc)
# )
# 
# # Find best penalty parameter
# best_penalty <- select_best(tuned_results, metric = "roc_auc")
# best_penalty
# 
# # Finalize workflow and run last fit
# final_workflow <- finalize_workflow(lr_workflow, best_penalty)
# final_fit <- last_fit(final_workflow, data_split)
# 
# # Collect final test metrics
# collect_metrics(final_fit)
# 
# # -----------------------------------------------
# # end demo log reg con tuning
# # -----------------------------------------------

# Is google that good?

# 1. Load Libraries and Create Dummy Data ---------------------------------
library(tidymodels)
library(tidyverse)
library(xgboost)
library(ranger)

# Enable parallel processing to speed up tuning
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# Generate a dummy classification dataset for demonstration
set.seed(123)
data_dummy <- tibble(
  target = factor(sample(c("Class_A", "Class_B"), 1000, replace = TRUE)),
  num_1  = rnorm(1000),
  num_2  = runif(1000, 1, 100),
  cat_1  = factor(sample(c("Low", "Med", "High"), 1000, replace = TRUE))
)

# 2. Train / Test Split ----------------------------------------------------
set.seed(456)
data_split <- initial_split(dmdl1, prop = 0.80, strata = mental_issue)
train_data <- training(data_split)
test_data  <- testing(data_split)

# 3. Cross-Validation Setup ------------------------------------------------
set.seed(789)
cv_folds <- vfold_cv(train_data, v = 5, strata = mental_issue)

# 4. Data Preprocessing (Recipe) -------------------------------------------
model_recipe <- recipe(mental_issue ~ pmsu_lmh + talkf + MBMI, data = train_data)
  #step_novel(all_nominal_predictors()) %>% 
  #step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  #step_zv(all_predictors()) %>% 
  #step_normalize(all_numeric_predictors())

# 5. Model Specifications --------------------------------------------------

# Logistic Regression
lr_spec <- logistic_reg(
  penalty = tune(), 
  mixture = tune()
) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

# Decision Tree (rpart)
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# Random Forest
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# XGBoost
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# 6. Workflowset Creation (Bundling Models) -------------------------------
model_set <- workflow_set(
  preproc = list(base_rec = model_recipe),
  models = list(
    #logistic_reg = lr_spec,
    decision_tree = tree_spec,
    random_forest = rf_spec
    #xgboost = xgb_spec
  ),
  cross = TRUE
)

# 7. Hyperparameter Tuning via Cross-Validation ----------------------------
# Define metrics to track
cls_metrics <- metric_set(roc_auc, accuracy)

set.seed(101)
tuned_results <- model_set %>% 
  workflow_map(
    fn = "tune_grid",
    resamples = cv_folds,
    grid = 10, # 10 random combinations per model
    metrics = cls_metrics,
    control = control_grid(save_pred = TRUE, parallel_over = "resamples"),
    verbose = TRUE
  )

# Review CV performance metrics across all models
autoplot(tuned_results)
rank_results(tuned_results, rank_metric = "roc_auc", select_best = TRUE)

# 8. Evaluate Best Model on Test Set ---------------------------------------

# Extract the overall best model workflow ID (e.g., "base_rec_xgboost")
best_model_id <- tuned_results %>% 
  rank_results(rank_metric = "roc_auc", select_best = TRUE) %>% 
  slice(1) %>% 
  pull(wflow_id)

# Extract the best hyperparameter configuration for that specific model
best_params <- tuned_results %>% 
  extract_workflow_set_result(best_model_id) %>% 
  select_best(metric = "roc_auc")

# Final fit: Trains on entire training set, evaluates on the test set
final_fit <- tuned_results %>% 
  extract_workflow(best_model_id) %>% 
  finalize_workflow(best_params) %>% 
  last_fit(split = data_split, metrics = cls_metrics)

# 9. Performance Metrics & Predictions -------------------------------------

# Collect metrics on the test data
test_metrics <- collect_metrics(final_fit)
print(test_metrics)

# Extract test predictions for confusion matrix or visualization
test_predictions <- collect_predictions(final_fit)

# Confusion Matrix
test_predictions %>% 
  conf_mat(truth = mental_issue, estimate = .pred_class)

# ROC Curve
test_predictions %>% 
  roc_curve(truth = mental_issue, .pred_0) %>% 
  autoplot()

# Stop parallel processing cluster
stopCluster(cl)



















