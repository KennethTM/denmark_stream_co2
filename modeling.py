import pandas as pd
import numpy as np
import sklearn
from sklearn.model_selection import GroupShuffleSplit 
from flaml import AutoML

data = pd.read_parquet("data/model_data.parquet")

splitter = GroupShuffleSplit(test_size=0.20, n_splits=2, random_state = 7)
split = splitter.split(data, groups=data["reachno"])
train_inds, test_inds = next(split)

train = data.iloc[train_inds]
test = data.iloc[test_inds]

X_train = train.drop(columns=["reachno", "co2"])
y_train = train["co2"]

X_test = test.drop(columns=["reachno", "co2"])
y_test = test["co2"]

#https://github.com/microsoft/FLAML/blob/main/notebook/automl_lightgbm.ipynb

settings = {
    "time_budget": 600,  # total running time in seconds
    "metric": 'r2',  # primary metrics for regression can be chosen from: ['mae','mse','r2','rmse','mape']
    "task": 'regression',  # task type    
    "seed": 7654321,    # random seed
}

automl = AutoML()
automl.fit(X_train, y_train, **settings)

#retrieve best config
print('Best hyperparmeter config:', automl.best_config)
print('Best r2 on validation data: {0:.4g}'.format(1-automl.best_loss))
print('Training duration of best run: {0:.4g} s'.format(automl.best_config_train_time))

#compute predictions of testing dataset
y_pred = automl.predict(X_test)
print('Predicted labels', y_pred)
print('True labels', y_test)

#compute different metric values on testing dataset
from flaml.ml import sklearn_metric_loss_score

print('r2', '=', 1 - sklearn_metric_loss_score('r2', y_pred, y_test))
print('mse', '=', sklearn_metric_loss_score('mse', y_pred, y_test))
print('mae', '=', sklearn_metric_loss_score('mae', y_pred, y_test))