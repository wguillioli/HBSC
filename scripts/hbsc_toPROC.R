

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








# LR SHAP -------------------
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




# corels ------------------
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



# tree plot ----------------

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