# -*- coding: utf-8 -*-
"""
Created on Sun Aug 23 09:39:06 2026

@author: wagui
"""

import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split

df = pd.read_csv("C:/MisLocalFiles/Github/HBSC/data/processed/dat_2026-08-16.csv")

print("Original dataframe dimensions:", df.shape)

print("\nDataframe info:")
print(df.info())

print("\nSummary statistics:")
pd.set_option("display.max_columns", None)
pd.set_option("display.width", 2000)
pd.set_option("display.max_rows", None)
#print(df.describe(include='all'))
#print(df.describe())
#summary = df.describe(include='all')
#summary.to_csv("C:/MisLocalFiles/Github/HBSC/data/processed/summary_stats.csv")
#print("Summary saved to summary_stats.csv")

print("\nMissing values by variable:")
print(df.isnull().sum())

df_complete = df.dropna()
print("\nComplete-case dataframe dimensions:", df_complete.shape)

italy = df_complete[df_complete["country_name"] == "Italy"].copy()
print("\nItaly dataset dimensions:", italy.shape)

# ---------------------------------------------------------
# 7. Fit logistic regression using scikit-learn
# ---------------------------------------------------------

# List of predictors (same as your R formula)
predictors = [
    "pmsu_lmh",
    "emconlfreqYes",
    "lives_onlineYN",
    "family_support_high",
    "friends_support_high",
    "school_support_high",
    "IRRELFAS_LMH_r",
    "talkfatherYes",
    "talkmotherYes",
    "beenbulliedYes",
    "cbeenbulliedYes",
    "timeexe_r",
    "alc30d_YN",
    "age_r",
    "sex_r"
]



# Convert categorical predictors to numeric codes
for col in predictors:
    if italy[col].dtype == "object":
        italy[col] = italy[col].astype("category").cat.codes

X = italy[predictors]
y = italy["lifesat_low"]

# Scale numeric predictors
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Fit logistic regression
log_reg = LogisticRegression(max_iter=1000)
log_reg.fit(X_scaled, y)

print("\nCoefficients:")
for var, coef in zip(predictors, log_reg.coef_[0]):
    print(f"{var}: {coef:.4f}")

print("\nIntercept:", log_reg.intercept_[0])

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# ---------------------------------------------------------
# Compute odds ratios
# ---------------------------------------------------------
coeffs = log_reg.coef_[0]
odds_ratios = np.exp(coeffs)

or_df = pd.DataFrame({
    "variable": predictors,
    "coef": coeffs,
    "odds_ratio": odds_ratios
}).sort_values("odds_ratio", ascending=False)

print("\nOdds Ratios:")
print(or_df)

# ---------------------------------------------------------
# Plot odds ratios
# ---------------------------------------------------------
plt.figure(figsize=(10, 8))
sns.barplot(
    data=or_df,
    x="odds_ratio",
    y="variable",
    palette="viridis"
)

plt.axvline(1, color="red", linestyle="--", linewidth=1)
plt.title("Odds Ratios for Logistic Regression Predictors (Italy)")
plt.xlabel("Odds Ratio")
plt.ylabel("Predictor")
plt.tight_layout()
plt.show()

import shap

# ---------------------------------------------------------
# SHAP Explainer for Logistic Regression
# ---------------------------------------------------------
explainer = shap.LinearExplainer(log_reg, X_scaled, feature_names=predictors)
shap_values = explainer.shap_values(X_scaled)

# ---------------------------------------------------------
# Beeswarm plot
# ---------------------------------------------------------
shap.summary_plot(shap_values, X, feature_names=predictors)

# ---------------------------------------------------------
# Bar plot (importance)
# ---------------------------------------------------------
shap.summary_plot(shap_values, X, feature_names=predictors, plot_type="bar")

# ---------------------------------------------------------
# Example dependence plot
# ---------------------------------------------------------
shap.dependence_plot("family_support_high", shap_values, X, feature_names=predictors)

from sklearn.inspection import PartialDependenceDisplay
import matplotlib.pyplot as plt

# ---------------------------------------------------------
# Partial Dependence Plot Suite
# ---------------------------------------------------------

fig, ax = plt.subplots(4, 4, figsize=(18, 18))

PartialDependenceDisplay.from_estimator(
    log_reg,
    X_scaled,
    features=list(range(len(predictors))),   # indices of columns
    feature_names=predictors,
    ax=ax
)

plt.suptitle("Partial Dependence Plot Suite – Logistic Regression (Italy)", fontsize=18)
plt.tight_layout()
plt.show()



from sklearn.inspection import PartialDependenceDisplay
import matplotlib.pyplot as plt

# ---------------------------------------------------------
# Partial Dependence Plot Suite (FIXED)
# ---------------------------------------------------------

fig, ax = plt.subplots(4, 4, figsize=(18, 18))

# Flatten axes into a 1D list
ax_flat = ax.ravel()

PartialDependenceDisplay.from_estimator(
    log_reg,
    X_scaled,
    features=list(range(len(predictors))),   # 16 features
    feature_names=predictors,
    ax=ax_flat                                # MUST be flat list
)

plt.suptitle("Partial Dependence Plot Suite – Logistic Regression (Italy)", fontsize=18)
plt.tight_layout()
plt.show()




from sklearn.inspection import PartialDependenceDisplay
import matplotlib.pyplot as plt
import math

n_features = len(predictors)
n_cols = 4
n_rows = math.ceil(n_features / n_cols)

fig, ax = plt.subplots(n_rows, n_cols, figsize=(18, 4*n_rows))
ax_flat = ax.ravel()

PartialDependenceDisplay.from_estimator(
    log_reg,
    X_scaled,
    features=list(range(n_features)),
    feature_names=predictors,
    ax=ax_flat
)

plt.suptitle("Partial Dependence Plot Suite – Logistic Regression (Italy)", fontsize=18)
plt.tight_layout()
plt.show()



write python code that:
reads csv from C:/MisLocalFiles/Github/HBSC/data/processed/dat_2026-08-16.csv
prints dimensions of dataframe
shows an overview of the vars
shows count of missing values by variable
creates a new dataframe with only complete observations
print dimensions of new dataframe
create a new dataset called italy from countryName == "Italy"
fit a logistic regression model from this r code: reg1 <- glm(lifesat_low ~  pmsu_lmh +
              emconlfreqYes +
              lives_onlineYN +
              family_support_high +
              friends_support_high +
              school_support_high +
              IRRELFAS_LMH_r +
              talkfatherYes +
              talkmotherYes +
              beenbulliedYes +
              cbeenbulliedYes +
              timeexe_r +
              alc30d_YN +
              age_r +
              sex_r, 
            data = italy, 
            family = binomial)





# fit xgboost simple with my data and shap-it

import matplotlib.pylab as pl
import numpy as np
import xgboost
from sklearn.model_selection import train_test_split
import shap

#shap.initjs()

X, y = shap.datasets.adult()
X_display, y_display = shap.datasets.adult(display=True)

italy
italy.dtypes

y = italy["mental_complaintsY"].astype(bool).to_numpy()

X = italy[["pmsu_lmh", 
           "sex_r",
           "cbeenbulliedYes"
           #"family_support_hml"
           #, "IRRELFAS_LMH_r", 
           ]]
cat_cols = ["pmsu_lmh", 
            "sex_r"
            #"family_support_hml"
            ]
X[cat_cols] = X[cat_cols].astype("category")
X.dtypes

# create a train/test split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=7)
#d_train = xgboost.DMatrix(X_train, label=y_train)
d_train = xgboost.DMatrix(X_train, label=y_train, enable_categorical=True)
d_test = xgboost.DMatrix(X_test, label=y_test, enable_categorical=True)

params = {
    "eta": 0.01,
    "objective": "binary:logistic",
    "subsample": 0.5,
    "base_score": np.mean(y_train),
    "eval_metric": "logloss",
}
model = xgboost.train(
    params,
    d_train,
    5000,
    evals=[(d_test, "test")],
    verbose_eval=100,
    early_stopping_rounds=20,
)

xgboost.plot_importance(model)
pl.title("xgboost.plot_importance(model)")
pl.show()

xgboost.plot_importance(model, importance_type="gain")
pl.title('xgboost.plot_importance(model, importance_type="gain")')
pl.show()

explainer = shap.TreeExplainer(model)
shap_values = explainer.shap_values(X)

shap.force_plot(explainer.expected_value, shap_values[0, :], X.iloc[0, :],
                matplotlib=True)

shap.force_plot(explainer.expected_value, shap_values[4, :], X.iloc[4, :],
                matplotlib=True)


#shap.force_plot(explainer.expected_value, shap_values[:1000, :], X.iloc[:1000, :],
#                matplotlib=True)

shap.summary_plot(shap_values, X, plot_type="bar")

shap.summary_plot(shap_values, X)

    
for name in X_train.columns:
    shap.dependence_plot(name, shap_values, X, display_features=X)
    
  
    
