import numpy as np

class DecisionStump():
    def __update_parameter(self, h, feature_index, threshold, direction, err_value):
        self.__min_err_value = err_value
        self.__feature_index = feature_index
        self.__threshold = threshold
        self.__direction = direction

    def __select_direction(self, feature_index, threshold, X, y, w):
        for direction in ['greater', 'less']:
            h = np.ones_like(y)

            if direction == 'greater':
                h[np.flatnonzero(X[:, feature_index] < threshold)] = -1
            else:
                h[np.flatnonzero(X[:, feature_index] >= threshold)] = -1

            err_value = np.sum(w[np.flatnonzero(h != y)])
            if err_value < self.__min_err_value:
                self.__update_parameter(h, feature_index, threshold, 'greater', err_value)

    def __select_threshold(self, feature_index, X, y, w):
        n_samples = X.shape[0]

        for i in range(n_samples):
            self.__select_direction(feature_index, X[i, feature_index], X, y, w)

    def __select_feature(self, X, y, w):
        n_features = X.shape[1]

        for i in range(n_features):
            self.__select_threshold(i, X, y, w)

    def fit(self, X, y, w):
        self.__feature_index = None
        self.__threshold = None
        self.__direction = None
        self.__min_err_value = np.inf

        self.__select_feature(X, y, w)

    def predict(self, X):
        n_samples = X.shape[0]

        y_pred = np.ones(n_samples)
        if self.__direction == 'greater':
            y_pred[np.flatnonzero(X[:, self.__feature_index] < self.__threshold)] = -1
        else:
            y_pred[np.flatnonzero(X[:, self.__feature_index] >= self.__threshold)] = -1

        return y_pred