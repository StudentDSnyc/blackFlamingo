# To support both python 2 and python 3
from __future__ import division, print_function, unicode_literals

# Common imports
import numpy as np
import os
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd

# to make this notebook's output stable across runs
np.random.seed(42)

# Load data
houses_train = pd.read_csv('../Data/encoded.houses.train.csv')
houses_test = pd.read_csv('../Data/encoded.houses.test.csv')

print("houses_train dimensions: {}".format(houses_train.shape))
print("houses_test dimensions: {}".format(houses_test.shape))

# print(houses_train.head(3))
# print(houses_train.info())


# Delete first column
houses_train.drop('Unnamed: 0', axis=1, inplace=True, errors='raise')
houses_test.drop('Unnamed: 0', axis=1, inplace=True, errors='raise')

# Create private training & test set

from sklearn.model_selection import train_test_split

seed = 10
test_ratio = 0.2

X = houses_train.loc[:, houses_train.columns != "SalePrice"]
y = houses_train.loc[:, houses_train.columns == "SalePrice"]

# Take log of SalePrice
y = np.log(y + 1)


X_pr_train, X_pr_test, y_pr_train, y_pr_test = train_test_split(X, y, test_size=test_ratio, random_state=seed)

# Fit Model

from xgboost import XGBClassifier

model = XGBClassifier()
model.fit(X_pr_train, y_pr_train.ravel())

# make predictions for test data
y_pr_pred = model.predict(X_pr_test)
predictions = [round(value) for value in y_pr_pred]

# Evaluate predictions

from sklearn.metrics import mean_squared_error

mse = mean_squared_error(y_pr_test, predictions)
rmse = np.sqrt(final_mse)