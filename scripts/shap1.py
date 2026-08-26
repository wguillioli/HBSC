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










