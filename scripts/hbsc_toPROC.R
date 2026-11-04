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

