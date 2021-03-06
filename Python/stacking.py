import numpy as np
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error, mean_squared_log_error
from sklearn.base import clone
import pandas as pd


def transformer(y, func=None):
    """Transforms target variable and prediction"""
    if func is None:
        return y
    else:
        return func(y)

def append_meta_features(S_train, S_test, x_train, x_test=None):
    new_train = np.append(S_train, x_train, 1)
    if x_test is not None:
        new_test = np.append(S_test, x_test, 1)
    else:
        new_test = S_test

    return new_train, new_test

def stacking_regression(models, default_X_train, default_y_train, default_X_test,
             default_transform_target=None, default_transform_pred=None,
             default_metric=None, n_folds=3, average_fold=True,
             shuffle=False, random_state=0, verbose=1):
    '''
    Function 'stacking' takes train data, test data, list of 1-st level
    models and returns stacked_features for train data and test data for
    for feeding into 2-nd level metal model.

    Parameters
    ----------
    models : list of models dictionaries {'model', 'X_train', 'y_train', 'X_test'}
        List of 1-st level models. You can use any models that follow sklearn
        convention i.e. accept numpy arrays and have methods 'fit' and 'predict'.

    meta_model: model
        2-nd level model. You can use any model that follow sklearn convention
x`
    X_train : numpy array or sparse matrix of shape [n_train_samples, n_features]
        Training data

    y_train : numpy 1d array
        Target values

    X_test : numpy array or sparse matrix of shape [n_test_samples, n_features]
        Test data


    transform_target : callable, default None
        Function to transform target variable.
        If None - transformation is not used.
        For example, for regression task (if target variable is skewed)
            you can use transformation like numpy.log.
            Set transform_target = numpy.log
        Usually you want to use respective backward transformation
            for prediction like numpy.exp.
            Set transform_pred = numpy.exp
        Caution! Some transformations may give inapplicable results.
            For example, if target variable contains zeros, numpy.log
            gives you -inf. In such case you can use appropriate
            transformation like numpy.log1p and respective
            backward transformation like numpy.expm1

    transform_pred : callable, default None
        Function to transform prediction.
        If None - transformation is not used.
        If you use transformation for target variable (transform_target)
            like numpy.log, then using transform_pred you can specify
            respective backward transformation like numpy.exp.
        Look at description of parameter transform_target

    metric : callable, default None
        Evaluation metric (score function) which is used to calculate
        results of cross-validation.
        If None, then by default:
            sklearn.metrics.mean_absolute_error - for regression

    n_folds : int, default 3
        Number of folds in cross-validation

    average_fold: boolean, default True
        Whether to take the average of the predictions on test set from each fold.
        Refit the model using the whole training set and predict test set if False

    shuffle : boolean, default False
        Whether to perform a shuffle before cross-validation split

    random_state : int, default 0
        Random seed for shuffle

    verbose : int, default 1
        Level of verbosity.
        0 - show no messages
        1 - for each model show single mean score
        2 - for each model show score for each fold and mean score

        Caution. To calculate MEAN score across all folds
        full train set prediction and full true target are used.
        So for some metrics (e.g. rmse) this value may not be equal
        to mean of score values calculated for each fold.

    Returns
    -------
    stacking_prediction : numpy array of shape n_test_samples
        Stacking prediction
    '''

    # Specify default metric for cross-validation
    if default_metric is None:
        default_metric = mean_squared_error

    # Print metric
    if verbose > 0:
        print('metric: [%s]\n' % default_metric.__name__)

    # Split indices to get folds
    kf = KFold(n_splits = n_folds, shuffle = shuffle, random_state = random_state)

    if default_X_train.__class__.__name__ == "DataFrame":
    	default_X_train = default_X_train.as_matrix()
    	default_X_test = default_X_test.as_matrix()

    # Create empty numpy arrays for stacking features
    S_train = np.zeros((default_X_train.shape[0], len(models)))
    S_test = np.zeros((default_X_test.shape[0], len(models)))

    # Loop across models
    for model_counter, model in enumerate(models):
        # check if model has a custom X_train, y_train, X_test
        if(type(model) is dict):
            X_train = model['X_train']
            y_train = model['y_train']
            X_test = model['X_test']
            
            if 'metric' in model:
                metric = model['metric']
            else:
                metric = default_metric

            if 'transform_target' in model:
                transform_target = model['transform_target']
            else:
                transform_target = default_transform_target

            if 'transform_pred' in model:
                transform_pred = model['transform_pred']
            else:
                transform_pred = default_transform_pred

            model = model['model']

            print('model has own dataset')
        else:
            X_train = default_X_train
            y_train = default_y_train
            X_test = default_X_test
            metric = default_metric
            transform_target = default_transform_target
            transform_pred = default_transform_pred
            print('using default dataset')

        if verbose > 0:
            print('model %d: [%s]' % (model_counter, model.__class__.__name__))

        # Create empty numpy array, which will contain temporary predictions for test set made in each fold
        S_test_temp = np.zeros((X_test.shape[0], n_folds))
        # Loop across folds
        for fold_counter, (tr_index, te_index) in enumerate(kf.split(X_train, y_train)):
            X_tr = X_train[tr_index]
            y_tr = y_train[tr_index]
            X_te = X_train[te_index]
            y_te = y_train[te_index]
            # Clone the model because fit will mutate the model.
            instance = clone(model)
            # Fit 1-st level model
            instance.fit(X_tr, transformer(y_tr, func = transform_target))
            # Predict out-of-fold part of train set
            S_train[te_index, model_counter] = transformer(instance.predict(X_te), func = transform_pred)
            # Predict full test set
            S_test_temp[:, fold_counter] = transformer(instance.predict(X_test), func = transform_pred)

            # Delete temperatory model
            del instance

            if verbose > 1:
                print('    fold %d: [%.8f]' % (fold_counter, metric(y_te, S_train[te_index, model_counter])))

        # Compute mean or mode of predictions for test set
        if average_fold:
            S_test[:, model_counter] = np.mean(S_test_temp, axis = 1)
        else:
            model.fit(X_train, transformer(y_train, func = transform_target))
            S_test[:, model_counter] = transformer(model.predict(X_test), func = transform_pred)

        if verbose > 0:
            print('    ----')
            print('    MEAN:   [%.8f]\n' % (metric(y_train, S_train[:, model_counter])))

    # Fit our second layer meta model
    #meta_model.fit(X=S_train, y=transformer(y_train, func = transform_target))
    # Make our final prediction
    #stacking_prediction = transformer(meta_model.predict(S_test), func = transform_pred)

    # print(S_train.shape)
    # print(S_train[:,0])
    # print(mean())
    # submission_test = pd.DataFrame({"Id": range(1461, 2920),
    #                            "SalePrice": S_train[:,0]})
    # submission_test.submission.to_csv("./Ensemble Submissions/submission_test {}.csv".format(time), sep=',', index = False)

    return S_train, S_test
    #return stacking_prediction
