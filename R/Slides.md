Slides
========================================================
author: Black Flamingos
date: November 2017
autosize: true

Cleaning/Imputing/Releveling
========================================================

- Straight mode imputing for most; other-column-dependent mode imputing otherwise
- 

Feature Engineering
========================================================

- Garage interaction: GarageQual * GarageCars
- Total bathrooms: FullBath + HalfBath + BsmtFullBath + BsmtHalfBath
- Average room size: GrLivArea / TotRmsAbvGrd
- Bathroom to room ratio: (FullBath + HalfBath) / BedroomAbvGr
- Comparative size of living area: GrLivArea / mean(GrLivArea)
- Landscape-ability interaction: LotShape, LotContour



Splitting and Encoding
========================================================

- Split Kaggle train data into 'private train' and 'private test' (80/20)
- Encoding: Label-count encoding for tree models; one-hot encoding for linear, ridge/lasso, elastic net

Modeling Process (individual models)
========================================================

- Grid search to tune hyper parameters (if applicable)
- Fit model to private train set with tuned parameters
- Predict on private test set; if okay, proceed...

- Fit new model to Kaggle train set with private train hyperparameters
- Cross-validate on Kaggle train set; if okay, proceed...

- Predict on Kaggle test set
- Submit to Kaggle; deal with feelings of mediocrity


Models Trained
========================================================
(In R and Python)

- Multiple linear, spline
- Ridge, LASSO, elastic net
- Decision tree (basic)
- Random Forest
- Gradient boosted forest
- XGBoosted forest


Stacking
========================================================

Following Zeyu:

- Spline 
- Random Forest
- 2 GBM
- 1 XGBoost

Meta model: GBM

KAGGLE
========================================================
