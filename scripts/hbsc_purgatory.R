# hbsc purgatory / sandbox

# graph for bullied_s sucked cause it's discrete [2,10]
# so see in discrete var
# it's now on d2 so purgatory
d1 %>%
  filter(!is.na(bullied_s), !is.na(mental_issue)) %>%
  ggplot(aes(x = factor(bullied_s), fill = mental_issue)) +
  # position = "fill" converts counts to 100% proportional bars
  geom_bar(position = "fill", alpha = 0.85) + 
  scale_fill_manual(values = c("No" = "#5DADE2", "Yes" = "#E74C3C")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()



# codigo que use para calculater train metrics
# # train metrics a pata
# # 2. Generate predictions on the training data (both class and probabilities)
# train_results <- predict(final_tree, new_data = d1_train, type = "class") %>% 
#   bind_cols(predict(final_tree, new_data = d1_train, type = "prob")) %>% 
#   bind_cols(d1_train) # Bind your original data to keep the 'truth' column
# 
# # 3. Calculate your performance metrics (e.g., accuracy, roc_auc)
# # Make sure to specify event_level if your positive class is the second level
# metric_results <- metric_set(accuracy, roc_auc, sens, spec)(
#   train_results, 
#   truth = mental_issue, 
#   estimate = .pred_class, 
#   .pred_Yes#, # Replace with your actual probability column name for the event
#   #event_level = "second" # Change to "first" or "second" depending on your target
# )
# 
# print(metric_results)

# So I will dichotomize like HBSC and keep sum of both as well for SHAP.
# replaced when I added talkp
dat <- dat %>%
  mutate(talkf = factor( 
    case_when(talkfather %in% c(1,2) ~ 1,
              talkfather %in% c(3,4,5) ~ 0,
              TRUE ~ NA),
    levels = c(0,1),
    labels = c("No", "Yes")
  ),
  talkm = factor(
    case_when(talkmother %in% c(1,2) ~ 1,
              talkmother %in% c(3,4,5) ~ 0,
              TRUE ~ NA),
    levels = c(0,1),
    labels = c("No", "Yes")
  ),
  talkscore = talkfather + talkmother
  )




# see and plot the tree
# has to be pruned but play with cp 
final_tree_obj <- final_tree |>
  extract_fit_engine() %>%
  prune(final_tree_obj,
        cp = 0.007)

# need to fix this
rpart.plot(final_tree_obj,
           roundint = FALSE,
           #cex = 1.1,
           tweak = 2,
           #shadow.col = "gray",    # Adds a light shadow for better readability
           box.palette = "RdBu",
           fallen.leaves = TRUE
)

# plot tree to pdf
pdf("giant_readable_tree.pdf", width = 20, height = 12)

final_tree |>
  extract_fit_engine() |>
  rpart.plot(roundint = FALSE, tweak = 1.2) # You can scale text UP now!

dev.off()








