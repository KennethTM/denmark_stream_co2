import pandas as pd
import numpy as np
import random

random.seed(9999)

from sklearn.model_selection import GroupShuffleSplit, RandomizedSearchCV, GroupKFold
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error

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

#Random forest hyperparameters
rf_param = {'bootstrap': [True, False], 
            'max_depth': [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, None],
            'max_features': ['auto', 'sqrt'],
            'min_samples_leaf': [1, 2, 4],
            'min_samples_split': [2, 5, 10],
            'n_estimators': [50, 100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000]}

group_cv = GroupKFold(n_splits=4).split(X_train, y_train, train_groups)

rf = RandomForestRegressor(n_jobs=-1, criterion="squared_error")
rf_wrap = RandomizedSearchCV(rf, param_distributions=rf_param, 
                             n_iter=20, scoring="r2", 
                             cv=group_cv, refit=True, random_state=9999)

#Fit model
rf_wrap.fit(X_train, y_train)

#Predict for validation set
yhat_val = rf_wrap.predict(X_test)

#Metrics on validation set
r2 = r2_score(y_test, yhat_val)
mae = mean_absolute_error(y_test, yhat_val)
rmse = mean_squared_error(y_test, yhat_val, squared=False)

print("Rsq = {:.3f}, MAE = {:.3f}, RMSE = {:.3f}".format(r2, mae, rmse))

#Rsq = 0.113, MAE = 59.276, RMSE = 111.491