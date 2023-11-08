import pandas as pd
import numpy as np
import random
import json
import pickle
from matplotlib import pyplot as plt

from sklearn.model_selection import GroupShuffleSplit, RandomizedSearchCV, GroupKFold, cross_validate, learning_curve
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error, mean_absolute_percentage_error
from sklearn.preprocessing import PowerTransformer
from sklearn.impute import SimpleImputer
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import make_pipeline
from sklearn.dummy import DummyRegressor
from sklearn.linear_model import LinearRegression, ElasticNet
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.cross_decomposition import PLSRegression
from sklearn.inspection import permutation_importance, partial_dependence

random.seed(9999)

#Role of variables
numeric_preds = ["site_elev",
                "discharge_specific", "overland", "overland_drain", "sz", "sz_drain",
                "airt", "precip", "catchment_area",
                "mean.phraetic",  "mean.dhym", "mean.dhym_slope", "mean.dhym_hand", 
                "mean.clay_a",  "mean.clay_b", "mean.clay_c",  "mean.clay_d",  
                "mean.artificial", "mean.agriculture", "mean.forest" , "mean.nature_eks_agriculture", "mean.stream", "mean.lake", 
                "mean.artificial_200m", "mean.agriculture_200m", "mean.forest_200m", "mean.nature_eks_agriculture_200m", "mean.stream_200m", "mean.lake_200m"]

target = "co2"
grouping = "co2_site_id"
other_vars = [target, grouping]

#Read, one-hot encode season and subset data
data = pd.read_parquet("data/q_points_modeling.parquet")
season_onehot = pd.get_dummies(data["season"])
data = data.drop(columns="season")
data = data[other_vars + numeric_preds]
data = data.join(season_onehot)

#Split data into train and test sets
splitter = GroupShuffleSplit(test_size=0.20, n_splits=2, random_state = 9999)
split = splitter.split(data, groups=data["co2_site_id"])
train_inds, test_inds = next(split)

train = data.iloc[train_inds]
test = data.iloc[test_inds]

X_train = train.drop(columns=other_vars)
train_groups = train["co2_site_id"]
y_train = train["co2"]

X_test = test.drop(columns=other_vars)
test_groups = test["co2_site_id"]
y_test = test["co2"]

#Preprocessing
chalk_index = X_train.columns.get_loc("mean.chalk")
numeric_var_index = [X_train.columns.get_loc(i) for i in numeric_preds]

median_impute = ColumnTransformer(
    transformers=[
        ("chalk_impute", SimpleImputer(), [chalk_index]),
    ],
    remainder='passthrough')

power_trans = ColumnTransformer(
    transformers=[
        ("power_trans", PowerTransformer(standardize=True), numeric_var_index)
    ],
    remainder='passthrough')

#Define learners and hyperparameter search grids
n_preds = len(X_train.columns)

dummy_param = None
dummy = DummyRegressor(strategy="mean")

lm_param = None
lm = LinearRegression()

knn = KNeighborsRegressor()
knn_param = {"kneighborsregressor__n_neighbors": range(1, 50), 
             "kneighborsregressor__weights": ['uniform', 'distance']}

tree_param = {"decisiontreeregressor__criterion": ["squared_error", "absolute_error"],
              "decisiontreeregressor__max_depth": [2, 3, 5, 10, 20, 40, None],
              "decisiontreeregressor__min_samples_split": [2, 5, 10], 
              "decisiontreeregressor__min_samples_leaf": [1, 2, 5, 10, 20, 40]}
tree = DecisionTreeRegressor()

plsr = PLSRegression(scale=False)
plsr_param = {'plsregression__n_components': range(1, n_preds+1)}

elastic_param = {'elasticnet__alpha': np.logspace(-5, 5, 100, endpoint=True),
                 'elasticnet__l1_ratio': np.arange(0, 1, 0.01)}
elastic = ElasticNet()

svr_param = {'svr__kernel': ['linear', 'rbf', 'poly'], 
            'svr__C': np.logspace(-4, 4, 10),
            'svr__gamma': np.logspace(-4, 4, 10),
            'svr__epsilon':[0.001, 0.01, 0.1, 0.2,0.3]}
svr = SVR()

rf_param = {'randomforestregressor__bootstrap': [True, False], 
            'randomforestregressor__max_depth': [2, 3, 5, 10, 20, 40, None],
            'randomforestregressor__max_features': range(1, n_preds+1),
            'randomforestregressor__criterion': ["squared_error", "absolute_error"],
            "randomforestregressor__min_samples_leaf": [1, 2, 5, 10, 20, 40],
            "randomforestregressor__max_leaf_nodes": [5, 10, 50, 100, None],
            'randomforestregressor__n_estimators': [100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000]}
rf = RandomForestRegressor(n_jobs=5)

learner_list = [{"name": "dummy", "model": dummy, "hparams": dummy_param},
                {"name": "linear_model", "model": lm, "hparams": lm_param},
                {"name": "k_nearest_neighbors", "model": knn, "hparams": knn_param},
                {"name": "regression_tree", "model": tree, "hparams": tree_param},
                {"name": "partial_least_squares_regression", "model": plsr, "hparams": plsr_param},
                {"name": "elastic_net", "model": elastic, "hparams": elastic_param},
                {"name": "support_vector_regression", "model": svr, "hparams": svr_param},
                {"name": "random_forest", "model": rf, "hparams": rf_param}]

#Bencmark models using nested cross-validation
random_iters = 50
inner_cv = GroupKFold(n_splits=5)
outer_cv = GroupKFold(n_splits=5)

metrics = ("r2", "neg_mean_absolute_error", "neg_root_mean_squared_error", "neg_mean_absolute_percentage_error")

benchmark_results = []

for i in learner_list:

    print(i["name"])

    pipeline = make_pipeline(
        median_impute,
        power_trans,
        i["model"])

    if i["hparams"] is not None:
        pipeline_wrap = RandomizedSearchCV(pipeline, param_distributions=i["hparams"], n_iter=random_iters, cv=inner_cv, scoring="r2", refit=True, random_state=9999)
        cv_scores = cross_validate(pipeline_wrap, X_train, y_train, scoring=metrics, cv=outer_cv, groups = train_groups, fit_params={'groups': train_groups}, n_jobs=5)
    else:
        cv_scores = cross_validate(pipeline, X_train, y_train, scoring=metrics, cv=outer_cv, groups = train_groups)
    
    cv_scores["name"] = i["name"]

    benchmark_results.append(cv_scores)

benchmark_df = pd.concat([pd.DataFrame(i) for i in benchmark_results])
benchmark_df.to_csv("data/modeling/model_benchmark.csv", index=False)

#Train best model on entire training set
median_impute_all = ColumnTransformer(
    transformers=[
        ("all_impute", SimpleImputer(), numeric_var_index),
    ],
    remainder='passthrough'
)

best_model = RandomForestRegressor(n_jobs=5)
best_model_param = rf_param

pipeline = make_pipeline(
    median_impute_all,
    power_trans,
    best_model)

pipeline_wrap = RandomizedSearchCV(pipeline, param_distributions=best_model_param, n_iter=random_iters, cv=inner_cv, refit=True, random_state=9999)

#Fit model
pipeline_wrap.fit(X_train, y_train, groups=train_groups)

#Save model
with open('data/modeling/best_model.pkl','wb') as dst:
    pickle.dump(pipeline_wrap, dst)

##load
#with open('data/best_model.pkl', 'rb') as src:
#    pipeline_wrap = pickle.load(src)

#Predict for test set
yhat_test = pipeline_wrap.predict(X_test)

#Metrics on test set
r2 = r2_score(y_test, yhat_test)
mae = mean_absolute_error(y_test, yhat_test)
rmse = mean_squared_error(y_test, yhat_test, squared=False)
mape = mean_absolute_percentage_error(y_test, yhat_test)
pearson = np.corrcoef(y_test, yhat_test)[0, 1]

#Test metrics
test_metrics = {'r2': r2, 'mae': mae, 'rmse': rmse, "mape": mape, "pearson": pearson}

with open('data/modeling/test_metrics.json', 'w') as json_file:
  json.dump(test_metrics, json_file)

obs_pred_df = pd.DataFrame({"y_test": y_test, "yhat_test": yhat_test})
obs_pred_df.to_csv("data/modeling/test_obs_pred.csv", index=False)

#Predict for all qpoints
qpoints_raw = pd.read_parquet("data/q_points_features.parquet")

qpoints = qpoints_raw.copy()
season_onehot = pd.get_dummies(qpoints["season"])
qpoints = qpoints.drop(columns="season")
qpoints = qpoints[numeric_preds]
qpoints = qpoints.join(season_onehot)

co2_pred = pipeline_wrap.predict(qpoints)

qpoints_raw["co2_pred"] = co2_pred
qpoints_raw.to_parquet("data/q_points_predictions.parquet")

#Variable importance
#https://scikit-learn.org/stable/auto_examples/ensemble/plot_forest_importances.html
importance = permutation_importance(pipeline_wrap, X_test, y_test, n_repeats=25, random_state=9999, n_jobs=5)

importance_df = pd.DataFrame({"variable": X_test.columns,
                            "importance_mean": importance["importances_mean"], 
                            "importance_std": importance["importances_std"]})
importance_df.to_csv("data/modeling/variable_importance.csv", index=False)

#PDP plots
top_4_predictors = importance_df.sort_values("importance_mean", ascending=False)["variable"][:4].tolist()

pdp_results = []
for i in top_4_predictors:
    i_index = X_test.columns.get_loc(i)
    i_pdp = partial_dependence(pipeline_wrap, X_test, [i_index], kind="average")

    i_df = pd.DataFrame({"variable": i,
                        "response": i_pdp["average"][0], 
                        "x": i_pdp["values"][0]})
    pdp_results.append(i_df)

pdp_df = pd.concat(pdp_results)
pdp_df.to_csv("data/modeling/partial_dependence.csv", index=False)

#Learning curve
pipeline = make_pipeline(
        median_impute,
        power_trans,
        RandomForestRegressor(n_jobs=5))

pipeline_wrap = RandomizedSearchCV(pipeline, param_distributions=rf_param, n_iter=random_iters, cv=inner_cv, refit=True, random_state=9999)

train_size_props = np.array([0.1, 0.3, 0.5, 0.7, 0.9, 1])
train_size_abs, train_scores, test_scores = learning_curve(pipeline_wrap, X_train, y_train, groups = train_groups, train_sizes=train_size_props,
                        cv=outer_cv, n_jobs=5, scoring="r2", fit_params={'groups': train_groups}) 

learning_df = pd.DataFrame({"train_size_abs": train_size_abs,
                            "train_size_props": train_size_props,
                            "train_mean": train_scores.mean(axis=1),
                            "train_sd": train_scores.std(axis=1),
                            "test_mean": test_scores.mean(axis=1),
                            "test_sd": test_scores.std(axis=1)})
learning_df.to_csv("data/modeling/learning_curve.csv", index=False)
