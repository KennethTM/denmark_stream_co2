import pandas as pd
import numpy as np
import random
from sklearn.model_selection import GroupShuffleSplit, RandomizedSearchCV, GroupKFold
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error
from sklearn.compose import TransformedTargetRegressor, ColumnTransformer
from sklearn.preprocessing import FunctionTransformer, MinMaxScaler, PowerTransformer
from sklearn.pipeline import make_pipeline

random.seed(9999)

data = pd.read_parquet("data/model_data.parquet")

splitter = GroupShuffleSplit(test_size=0.20, n_splits=2, random_state = 7)
split = splitter.split(data, groups=data["reachno"])
train_inds, test_inds = next(split)

train = data.iloc[train_inds]
test = data.iloc[test_inds]

X_train = train.drop(columns=["reachno", "co2"])
train_groups = train["reachno"]
y_train = train["co2"]

X_test = test.drop(columns=["reachno", "co2"])
test_groups = test["reachno"]
y_test = test["co2"]

#Preprocessing
#https://scikit-learn.org/stable/auto_examples/applications/plot_cyclical_feature_engineering.html
def sin_transformer(period):
    return FunctionTransformer(lambda x: np.sin(x / period * 2 * np.pi))

def cos_transformer(period):
    return FunctionTransformer(lambda x: np.cos(x / period * 2 * np.pi))

preprocessor = ColumnTransformer(
    transformers=[
        ("doy_sin", sin_transformer(365), ["doy"]),
        ("doy_cos", cos_transformer(365), ["doy"]),
    ],
    remainder=PowerTransformer(),
)

#Group CV
group_cv = GroupKFold(n_splits=5).split(X_train, y_train, train_groups)

#Random forest hyperparameters
rf_param = {'transformedtargetregressor__regressor__bootstrap': [True, False], 
            'transformedtargetregressor__regressor__max_depth': [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, None],
            'transformedtargetregressor__regressor__max_features': ['auto', 'sqrt'],
            'transformedtargetregressor__regressor__min_samples_leaf': [1, 2, 4],
            'transformedtargetregressor__regressor__min_samples_split': [2, 5, 10],
            'transformedtargetregressor__regressor__n_estimators': [50, 100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000]}

rf = RandomForestRegressor(n_jobs=-1, criterion="squared_error")

rf_trans = TransformedTargetRegressor(
    regressor=rf, 
    func=np.log, inverse_func=np.exp
    )

pipeline = make_pipeline(
    preprocessor,
    rf_trans,
)

pipeline_wrap = RandomizedSearchCV(pipeline, param_distributions=rf_param, 
                             n_iter=20, scoring="r2", 
                             cv=group_cv, refit=True, random_state=9999)

#Fit model
pipeline_wrap.fit(X_train, y_train)

#Predict for validation set
yhat_val = pipeline_wrap.predict(X_test)

#Metrics on validation set
r2 = r2_score(y_test, yhat_val)
mae = mean_absolute_error(y_test, yhat_val)
rmse = mean_squared_error(y_test, yhat_val, squared=False)

print("Rsq = {:.3f}, MAE = {:.3f}, RMSE = {:.3f}".format(r2, mae, rmse))

#Rsq = 0.113, MAE = 59.276, RMSE = 111.491
#Rsq = 0.135, MAE = 50.024, RMSE = 110.086


'''
#https://github.com/microsoft/FLAML/blob/main/notebook/automl_lightgbm.ipynb

from flaml import AutoML
lgbm = AutoML()

settings = {
    "time_budget": 300,  # total running time in seconds
    "metric": 'r2',  # primary metrics for regression can be chosen from: ['mae','mse','r2','rmse','mape']
    "estimator_list": ['lgbm'],  # list of ML learners; we tune lightgbm in this example
    "task": 'regression',  # task type
    "seed": 9999,    # random seed
}

lgbm.fit(X_train=X_train, y_train=y_train, **settings)

#Predict for validation set
yhat_val = lgbm.predict(X_test)

#Metrics on validation set
r2 = r2_score(y_test, yhat_val)
mae = mean_absolute_error(y_test, yhat_val)
rmse = mean_squared_error(y_test, yhat_val, squared=False)

print("Rsq = {:.3f}, MAE = {:.3f}, RMSE = {:.3f}".format(r2, mae, rmse))

#Rsq = 0.137, MAE = 55.976, RMSE = 109.929

'''
