# hoy si

import pandas as pd
from sklearn.ensemble import RandomForestClassifier
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
import shap


# load hbsc dat
hbsc_path = "C:/MisLocalFiles/Github/HBSC/data/processed/dat_2026-08-28.csv"
hbsc = pd.read_csv(hbsc_path)
hbsc.shape

# select (some) predictors and just one country
italy = hbsc[hbsc["country_name"] == "Italy"].copy()
italy = italy[[#target vars
              "lifesat_low",
              "mental_complaintsY",
              #pmsu related
              "pmsu_lmh",
              "pmsu_yn",
              "emcsocmed_sum",
              #family support
              "family_support_sum",
              "family_support_high",	
              "family_support_hml",
              #bullying
              "cbeenbulliedYes",
              #profilers
              "IRRELFAS_LMH_r",
              "age_r",
              "sex_r"
              #ignore for now     
              #"emconlfreqYes",
              #"lives_onlineYN",
              #"friends_support_high",
              #"school_support_high",
              #"talkfatherYes",
              #"talkmotherYes",
              #"beenbulliedYes",
              #"timeexe_r",
              #"alc30d_YN",
              ]]

# remove NAs and make complete copy
italy.isna().sum() 
round(100 * italy.isna().sum() / len(italy),0)
italy_cpl = italy.copy().dropna()
italy_cpl.shape

# recode vars for modeling
italy_cpl.dtypes

order_mapping = {"Low": 0, "Med": 1, "High": 2}
italy_cpl["pmsu_lmh_enc"] = italy_cpl["pmsu_lmh"].map(order_mapping)
italy_cpl["family_support_hml_enc"] = italy_cpl["family_support_hml"].map(order_mapping)

order_mapping2 = {"Low20": 0, "Med60": 1, "High20": 2}
italy_cpl["IRRELFAS_LMH_r_enc"] = italy_cpl["IRRELFAS_LMH_r"].map(order_mapping2)

gender_mapping = {"Boy": 0, "Girl": 1}
italy_cpl["sex_r_enc"] = italy_cpl["sex_r"].map(gender_mapping)

# make a final copy that has the vars that will be in model
features_to_use1 = ['pmsu_lmh_enc',
                    'family_support_hml_enc',
                    'cbeenbulliedYes',
                    'sex_r_enc',
                    'IRRELFAS_LMH_r_enc']

# repeat from above with numerical version of the vars to include
features_to_use2 = ['emcsocmed_sum', # replace pmsu_lmh_enc
                    'family_support_sum', #family_support_hml_enc
                    'cbeenbulliedYes',
                    'sex_r_enc',
                    'IRRELFAS_LMH_r_enc']


X = italy_cpl[features_to_use2].copy()
X.nunique()

y = italy_cpl['mental_complaintsY'].copy()

X_train, X_test, y_train, y_test = train_test_split(X, y, 
                            random_state=67, test_size=0.20, stratify=y)

rf = RandomForestClassifier(random_state=67)
rf.fit(X_train, y_train)

print(rf.score(X_train, y_train))
print(rf.score(X_test, y_test))

# View which of your columns mattered most to the Random Forest
importance_df = pd.DataFrame({"Feature": X_train.columns, "Importance": rf.feature_importances_})
print(importance_df.sort_values(by="Importance", ascending=False))
plt.figure(figsize=(8, 5))
plt.barh(importance_df["Feature"], importance_df["Importance"], color="gray")
plt.xlabel("Importance Score")
plt.ylabel("Features")
plt.title("Random Forest Feature Importance")
plt.tight_layout()
plt.show()

#shap
explainer = shap.TreeExplainer(rf)
shap_values = explainer(X_test)

#bee
shap.summary_plot(shap_values[:, :, 1], X_test)

#from bee to feat imp
shap.summary_plot(shap_values[:, :, 1], X_test, plot_type="bar", show=False)
plt.tight_layout()
plt.show()

# target==0
shap.plots.waterfall(shap_values[10, :, 1])
shap.plots.force(shap_values[10, :, 1], matplotlib=True)

# target==1
shap.plots.waterfall(shap_values[67, :, 1])
shap.plots.force(shap_values[67, :, 1], matplotlib=True)
shap.plots.waterfall(shap_values[67][:, 1])

# dependence plots?
shap.plots.scatter(shap_values[:, "emcsocmed_sum"][:, 1], show=False)  #pmsu_lmh_enc
plt.tight_layout()
plt.show()

for col_name in X_test.columns:
    #plt.figure(figsize=(7, 4))
    
    shap.plots.scatter(
        shap_values[:, col_name][:, 1], 
        show=False
    )
    
    plt.title(f"SHAP Dependence Plot: {col_name}")
    plt.tight_layout()
    #plt.show()

# iter plot, but which ones?
shap.dependence_plot(
    "emcsocmed_sum",#"pmsu_lmh_enc", 
    shap_values.values[:, :, 1], 
    X_test, 
    interaction_index="family_support_sum",
    show=False
)

shap.dependence_plot(
    "cbeenbulliedYes", 
    shap_values.values[:, :, 1], 
    X_test, 
    interaction_index="pmsu_lmh_enc",
    show=False
)


#huh?
shap.plots.scatter(shap_values[:, "emcsocmed_sum", 1], color=shap_values[:, "cbeenbulliedYes", 1])


# fit another RF 


















# ---------------------------------------------------------
# 7. Fit logistic regression using scikit-learn
# ---------------------------------------------------------


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
    
    
    
    
# fit a simple model with 3-5 vars for hbsc
# catboost first
# then log reg
# train/test
# plot shaps
# then add more vars

import pandas as pd
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import OrdinalEncoder
import shap
from sklearn.ensemble import RandomForestClassifier

dat = pd.read_csv("C:/MisLocalFiles/Github/HBSC/data/processed/dat_2026-08-26.csv")
dat.info()

# keep only a few columns of interest and italy and drop nas
italy = dat.query("country_name == 'Italy'")

italy = italy[["lifesat_low",
                #"pmsu_lmh",
                "pmsu_yn",
                "cbeenbulliedYes",
                "talkfatherYes",
                "school_support_high"
]]

italy.isna().sum()
italy_complete = italy.dropna()
italy_complete.info()

# for italy, define X and y and create train/test
y_df = italy_complete["lifesat_low"]
X_df = italy_complete[["pmsu_yn", "cbeenbulliedYes", "talkfatherYes",
                    "school_support_high"]]

y = y_df.values
X = X_df.values

X_train, X_test, y_train, y_test = train_test_split(X_df, y_df, test_size=0.2, 
                                                    random_state=42, stratify=y)

# fit knn and pick best one based on neighbors
neighbors = np.arange(1, 25)
train_accuracies = {}
test_accuracies = {}

for neighbor in neighbors:
    knn = KNeighborsClassifier(n_neighbors=neighbor)
    knn.fit(X_train, y_train)
    train_accuracies[neighbor] = knn.score(X_train, y_train)
    test_accuracies[neighbor] = knn.score(X_test, y_test)

print(neighbors, '\n', train_accuracies, '\n', test_accuracies)

# Plot accuracies
plt.clf()
plt.title("KNN: Varying Number of Neighbors")
plt.plot(neighbors, train_accuracies.values(), label="Training Accuracy")
plt.plot(neighbors, test_accuracies.values(), label="Testing Accuracy")
plt.legend()
plt.xlabel("Number of Neighbors")
plt.ylabel("Accuracy")
plt.show()

# 2 is winner so will fit that
knn2 = KNeighborsClassifier(n_neighbors=2)
knn2.fit(X_train, y_train)
print(knn2.score(X_train, y_train))
print(knn2.score(X_test, y_test))

# plot feat imp for rf
rf_mdl = RandomForestClassifier(random_state=42)
rf_mdl.fit(X_train, y_train)
feature_importances = rf_mdl.feature_importances_
feature_importances
plt.bar(X_df.columns, feature_importances)
plt.show()

# shap for rf
explainer = shap.TreeExplainer(rf_mdl)
shap_values = explainer.shap_values(X)
mean_abs_shap = np.abs(shap_values[:,:,1]).mean(axis=0)
plt.bar(X_df.columns, mean_abs_shap)
plt.title('Mean Absolute SHAP Values for RandomForest')
plt.xticks(rotation=45)
plt.show()

# kernel explainer
explainer = shap.KernelExplainer(rf_mdl.predict_proba, shap.kmeans(X, 10))
shap_values = explainer.shap_values(X)
mean_abs_shap = np.abs(shap_values[:,:,1]).mean(axis=0)
plt.bar(X.columns, mean_abs_shap)
plt.title('Mean Absolute SHAP Values for MLPClassifier')
plt.xticks(rotation=45)
plt.show()

# feature imp from shap
rf_mdl = RandomForestClassifier(random_state=42)
rf_mdl.fit(X_train, y_train)
explainer = shap.TreeExplainer(rf_mdl)
shap_values = explainer.shap_values(X_train)

#feat imp
shap.summary_plot(shap_values[:, :, 1], X_train, plot_type="bar")

#bees
shap.summary_plot(shap_values[:, :, 1], X_train, plot_type="dot")



# Generate the partial dependence plot for University Rating
#shap.partial_dependence_plot('talkfatherYes', rf_mdl.predict, X_train)
#shap.partial_dependence_plot('pmsu_yn', rf_mdl.predict , X_train)

# this seems to work but i need to validate
def model_prediction_proba(x):
    return rf_mdl.predict_proba(x)[:, 1]

# 2. Pass the wrapper function instead of the hard .predict function
shap.partial_dependence_plot(
    'talkfatherYes', 
    model_prediction_proba, 
    X_train, 
    ice=False,                # Set to True if you want Individual Conditional Expectation lines
    model_expected_value=True, 
    feature_expected_value=True
)

plt.show()


# very quickly do an rf for pmsu 1 a 9 y ver
d = dat.query("country_name == 'Italy'")
d = d[["emcsocmed_sum", "lifesat_low", "talkfatherYes"]]
dc = d.dropna()
dc.info()

y = dc["lifesat_low"]
X = dc[["emcsocmed_sum", "talkfatherYes"]]

y = y.values
X = X.values

#y = y_df.values
#X = X_df.values

##X_train, X_test, y_train, y_test = train_test_split(X_df, y_df, test_size=0.2, 
  #                                                  random_state=42, stratify=y)

rf_mdl = RandomForestClassifier(random_state=42)
rf_mdl.fit(X, y)
explainer = shap.TreeExplainer(rf_mdl)
shap_values = explainer.shap_values(X)

#feat imp
shap.summary_plot(shap_values[:, :, 1], X, plot_type="bar")

#bees
shap.summary_plot(shap_values[:, :, 1], X, plot_type="dot")

# Generate the partial dependence plot for University Rating
#shap.partial_dependence_plot('talkfatherYes', rf_mdl.predict, X_train)
#shap.partial_dependence_plot('pmsu_yn', rf_mdl.predict , X_train)

# this seems to work but i need to validate
def model_prediction_proba(x):
    return rf_mdl.predict_proba(x)[:, 1]

# 2. Pass the wrapper function instead of the hard .predict function
shap.partial_dependence_plot(
    'emcsocmed_sum', 
    model_prediction_proba, 
    X, 
    ice=False,                # Set to True if you want Individual Conditional Expectation lines
    model_expected_value=True, 
    feature_expected_value=True
)

plt.show()











    
  
    
