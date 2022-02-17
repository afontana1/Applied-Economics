import numpy as np
import cvxopt

class SVM:
    def __qp(self, X, y, kernel, C):       
        n_samples = X.shape[0]

        P = np.outer(y, y) * kernel

        q = np.full((n_samples, 1), -1.0)

        G = np.vstack((-np.eye(n_samples), np.eye(n_samples)))

        h = np.hstack((np.zeros(n_samples), np.full(n_samples, C)))

        A = y.reshape((1, -1))

        b = np.zeros(1)

        res = cvxopt.solvers.qp(cvxopt.matrix(P), cvxopt.matrix(q), cvxopt.matrix(G), cvxopt.matrix(h), cvxopt.matrix(A), cvxopt.matrix(b))
        alpha = np.array(res['x']).ravel()

        support_items = np.flatnonzero(np.isclose(alpha, 0) == False)
        self.__X_support = X[support_items]
        self.__y_support = y[support_items]
        self.__a_support = alpha[support_items]

        free_items = np.flatnonzero(self.__a_support < C)
        y_free = y[free_items]

        self.__bias = y_free[0] - (self.__a_support * self.__y_support).T.dot(kernel[support_items, free_items[0]])

    def __smo(self, X, y, kernel, C, epochs):
        n_samples = X.shape[0]

        alpha = np.zeros(n_samples)
        self.__bias = 0
        e = -y

        for _ in range(epochs):
            for i in range(n_samples):
                hi = kernel[i].dot(alpha * y) + self.__bias
                if (y[i] * hi < 1 and alpha[i] < C) or (y[i] * hi > 1 and alpha[i] > 0):
                    j = np.argmax(np.abs(e - e[i]))

                    if y[i] == y[j]:
                        L = max(0, alpha[i] + alpha[j] - C)
                        H = min(C, alpha[i] + alpha[j])
                    else:
                        L = max(0, alpha[j] - alpha[i])
                        H = min(C, C + alpha[j] - alpha[i])
                    if L == H:
                        continue

                    eta = kernel[i, i] + kernel[j, j] - 2 * kernel[i, j]
                    if eta <= 0:
                        continue

                    alpha_j = alpha[j] + y[j] * (e[i] - e[j]) / eta

                    if alpha_j > H:
                        alpha_j = H
                    elif alpha_j < L:
                        alpha_j = L

                    alpha_i = alpha[i] + y[i] * y[j] * (alpha[j] - alpha_j)

                    bi = self.__bias - e[i] - y[i] * kernel[i, i] * (alpha_i - alpha[i]) - y[j] * kernel[i, j] * (alpha_j - alpha[j])
                    bj = self.__bias - e[j] - y[i] * kernel[i, j] * (alpha_i - alpha[i]) - y[j] * kernel[j, j] * (alpha_j - alpha[j])

                    if 0 < alpha_i and alpha_i < C:
                        self.__bias = bi
                    elif 0 < alpha_j and alpha_j < C:
                        self.__bias = bj
                    else:
                        self.__bias = (bi + bj) / 2

                    alpha[i] = alpha_i
                    alpha[j] = alpha_j

                    e[i] = kernel[i].dot(alpha * y) + self.__bias - y[i]
                    e[j] = kernel[j].dot(alpha * y) + self.__bias - y[j]
                
        support_items = np.flatnonzero(alpha > 1e-6)
        self.__X_support = X[support_items]
        self.__y_support = y[support_items]
        self.__a_support = alpha[support_items]

    def fit(self, X, y, kernel_func, C, solver, sigma=1, epochs=0):
        '''
        Parameters
        ----------
        X : shape (n_samples, n_features)
            Training data
        y : One-hot encoder, shape (n_samples,)
            Target values, 1 or -1
        kernel_func : kernel algorithm see also kernel.py
        C : Penalty parameter C of the error term
        solver : Solve algorithm
        sigma : Parameter for rbf kernel
        epochs : The number of epochs
        '''
        self.__sigma = sigma
        self.__kernel_func = kernel_func

        kernel = self.__kernel_func(X, X, self.__sigma)
        
        if solver == 'qp':
            self.__qp(X, y, kernel, C)
        elif solver == 'smo':
            self.__smo(X, y, kernel, C, epochs)
        
    def predict(self, X):
        '''
        Parameters
        ----------
        X : shape (n_samples, n_features)
            Predicting data

        Returns
        -------
        y : shape (n_samples,)
            Predicted class label per sample, 1 or -1
        '''
        return np.sign(self.score(X))

    def score(self, X):
        '''
        Parameters
        ----------
        X : shape (n_samples, n_features)
            Predicting data

        Returns
        -------
        y : shape (n_samples,)
            Predicted score of class per sample.
        '''
        kernel = self.__kernel_func(X, self.__X_support, self.__sigma)
        return (self.__a_support * self.__y_support).dot(kernel) + self.__bias