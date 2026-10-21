



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



# -------------------------------------------------------------------------
# EL Random Forest baseline, 10/17 HOY SI
# -------------------------------------------------------------------------

#hbsc_se$mentalissue <- relevel(hbsc_se$mentalissue, ref = "No")
levels(hbsc_se$mentalissue)

# #split the data
# set.seed(67)
# hbsc_se_split  <- initial_split(hbsc_se, 
#                                 strata = mentalissue)
# hbsc_se_train <- training(hbsc_se_split)
# hbsc_se_test <- testing(hbsc_se_split)

# use data from tree hbsc_se_train

# fit a baseline RF with defaults
rf_spec <- rand_forest(trees = 500) |> 
  set_mode("classification") |> 
  set_engine("ranger", num.threads = NULL, seed = 67)

# # since it's heavy overfit, fit a constrained model that google reco
# rf_spec <- rand_forest(
#   mtry = 3,           # Look at only 3 features at a time (de-correlates trees)
#   trees = 300,        # Stable ensemble size
#   min_n = 50          # Stop splitting if a node has <= 50 rows (prevents overfitting)
# ) %>% 
#   set_engine("ranger") %>% 
#   set_mode("classification")

# try to address class imabalance using Method 2: Internal Class Weights (Engine-Level)
# Calculate inverse class weights based on your training data
# This gives the rare class a higher mathematical "importance" flag
# class_counts <- table(hbsc_se_train$mentalissue)
# weights <- sum(class_counts) / (2 * class_counts)
# weights
# 
# rf_spec <- rand_forest(
#   mtry = 3,
#   trees = 300,
#   min_n = 50
# ) %>% 
#   set_engine("ranger", class.weights = weights) %>% # Pass weights to ranger
#   set_mode("classification")

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

left_join(train_metrics, test_metrics, by = ".metric")

# ok that baseline was sad, so i will tune it without dealing with class imbalance 


# -------------------------------------------------------------------------
# EL Random Forest tuned, 10/17 HOY SI
# inspiration https://juliasilge.com/blog/sf-trees-random-tuning/
# -------------------------------------------------------------------------

library(ranger)
#library(themis)     # <--- Required for step_downsample()
library(doParallel) 

rf_spec <- rand_forest(
  mtry  = tune(),
  trees = tune(),  
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") #set_engine("ranger", importance = "impurity")

rf_recipe <- recipe(mentalissue ~ ., data = hbsc_se_train)  #   step_downsample(Species, under_ratio = 1.0) # 1.0 means perfectly 50/50 balance (ver julia)

rf_workflow <- workflow() |>
  add_recipe(rf_recipe) |> 
  add_model(rf_spec)

set.seed(67)
rf_folds <- vfold_cv(hbsc_se_train, v = 10, strata = mentalissue)

rf_params <- extract_parameter_set_dials(rf_spec) %>% 
  finalize(hbsc_se_train) %>% #ensures mtry fits the dataset dimension DOESNT SEEM TO WORK
  update(trees = trees(range = c(100, 1000))) #explicit limit

rf_grid <- grid_latin_hypercube(rf_params, size = 20) #was 20
summary(rf_grid)

# fit with paralel processing
my_metrics <- metric_set(bal_accuracy, roc_auc, brier_class, j_index, accuracy)
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
system.time(
  rf_tune_res <- tune_grid(
    rf_workflow,
    resamples = rf_folds,
    grid = rf_grid,
    metrics = my_metrics
  )
) #2460
stopCluster(cl)
registerDoSEQ()

autoplot(rf_tune_res)

# 2 winners are very different so pick 1
# auc wins since mtry is lower and trees higher
rf_best_params <- select_best(rf_tune_res, metric = "roc_auc")
#rf_best_params <- select_best(tuning_results, metric = "bal_accuracy")
rf_best_params

final_workflow <- finalize_workflow(rf_workflow, rf_best_params)
final_model_fit <- fit(final_workflow, data = hbsc_se_train)

# train/test metrics
##### voy, fix code so it gets all 5 metrics from tbl

train_predictions <- augment(final_model_fit, new_data = hbsc_se_train)
test_predictions  <- augment(final_model_fit, new_data = hbsc_se_test)

cbind(
  train_predictions %>% 
    classification_metrics(truth = mentalissue, estimate = .pred_class, ".pred_Yes", 
                           event_level = "second") %>%
    transmute(.metric, train = round(.estimate,3))
  ,
  test_predictions %>% 
    classification_metrics(truth = mentalissue, estimate = .pred_class, ".pred_Yes",
                           event_level = "second") %>%
    transmute(test = round(.estimate,3))
)








# -----------------------------------------------------------------------
# just fit a really dummy foresta for el shap 10/18
# -----------------------------------------------------------------------
# for later

# # 1. Create the preprocessing recipe layout
# dummy_recipe <- recipe(mentalissue ~ ., data = hbsc_se_train) %>% 
#   # Explicitly convert all categorical factor columns into binary dummy columns
#   step_dummy(all_nominal_predictors(), one_hot = TRUE)
# 
# # 2. Train the recipe on your data and extract the final numeric training set
# prepped_recipe <- prep(dummy_recipe, training = hbsc_se_train)
# train_processed <- bake(prepped_recipe, new_data = NULL)
# print(colnames(train_processed)) #mentalissue is still there without dummy
# 
# # 3. Specify your Random Forest structural boundaries
# rf_spec <- rand_forest(
#   trees = 500,
#   min_n = 50          # Regulate node size to control overfitting
# ) %>% 
#   set_engine("ranger", importance = "impurity") %>% 
#   set_mode("classification")
# 
# # 4. Bind the processed data and specifications into a final workflow layout
# rf_workflow <- workflow() %>% 
#   add_recipe(dummy_recipe) %>%  # Bundles the explicit dummy conversion step
#   add_model(rf_spec)
# 
# # 5. Train the final model
# final_rf_fit <- fit(rf_workflow, data = hbsc_se_train)
# 
# # 1. Strip out the target column to create a clean, numeric predictor matrix
# X_processed <- train_processed %>% 
#   select(-mentalissue)
# names(X_processed)
# 
# set.seed(67)
# X_explain <- X_processed[sample(1:nrow(X_processed), 20), ] #30 to 1k (really impacts time)
# X_background <- X_processed[sample(1:nrow(X_processed), 20), ] #50 to 200 is sweet spot (don't go over 500)
# 
# system.time(
#   shap_values_rf_tonto <- kernelshap(
#     final_rf_fit,
#     X = X_explain, 
#     bg_X = X_background, 
#     #pred_fun = pf,
#     pred_fun = pf_log_odds
#     #type = "prob" #link scale so shap values add up to log-odds
#   )
# )




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

xvars <- hbsc_se_train |> select(-mentalissue) |> names()
xvars

set.seed(67)
X_explain <- test_data[sample(1:nrow(test_data), 250), xvars] #30 to 1k (really impacts time)
X_background <- train_data[sample(1:nrow(train_data), 150), ] #50 to 200 is sweet spot (don't go over 500)

system.time(
  shap_values_rf <- kernelshap(
    final_model_fit,
    X = X_explain, 
    bg_X = X_background, 
    #pred_fun = pf,
    pred_fun = pf_log_odds
    #type = "prob" #link scale so shap values add up to log-odds
  )
) #30/200 756; 50/200 1200; 100/200 2300; 200/200 4470; 250/150 5600

sv_rf <- shapviz(shap_values_rf)
sv_rf 
dim(sv_rf$S)
head(sv_rf$S)

# saveRDS(shap_values, file = "shap_values.rds")
# shap_values <- readRDS("shap_values.rds")

sv_importance(sv_rf, show_numbers = TRUE)

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