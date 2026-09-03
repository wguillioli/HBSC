#https://www.r-bloggers.com/2022/06/visualize-shap-values-without-tears/

library(shapviz)
library(ggplot2)
library(xgboost)

set.seed(3653)

X <- diamonds[c("carat", "cut", "color", "clarity")]
dtrain <- xgb.DMatrix(data.matrix(X), label = diamonds$price)

fit <- xgb.train(
  params = list(learning_rate = 0.1, objective = "reg:squarederror"), 
  data = dtrain,
  nrounds = 65L
)

# Explanation dataset
X_small <- X[sample(nrow(X), 2000L), ]

shp <- shapviz(fit, X_pred = data.matrix(X_small), X = X_small)

# Two types of visualizations
sv_waterfall(shp, row_id = 1)
sv_force(shp, row_id = 1)
         
# Three types of variable importance plots
sv_importance(shp)
sv_importance(shp, kind = "bar")
sv_importance(shp, kind = "both", alpha = 0.2, width = 0.2)
         
sv_dependence(shp, v = "color", "auto")
