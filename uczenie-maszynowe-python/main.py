import pandas as pd
import numpy as np
from sklearn.grid_search import RandomizedSearchCV
from sklearn.ensemble import RandomForestClassifier
from scipy.stats import randint as sp_randint
from time import time
from sklearn.externals import joblib


if __name__ == "__main__":

    dane = pd.read_csv("all_summary.txt", sep=";", na_values=["nan"], keep_default_na=False, dtype={'res_id': object})
    grouped_res_name = pd.read_csv("grouped_res_name.txt", sep=",", na_values=["nan"], keep_default_na=False)
    dane_test = pd.read_csv("test_data.txt", sep=",", na_values=["nan"], keep_default_na=False)

    dane = dane[~dane["res_name"].isin(["DA", "DC", "DT", "DU", "DG", "DI", "UNK", "UNX", "UNL", "PR", "PD",
                                        "Y1", "EU", "N", "15P", "UQ", "PX4", "NAN"])]
    dane = dane.drop_duplicates(subset=["pdb_code", "res_name"], keep='first')
    dane = dane[dane.groupby('res_name').res_id.transform(len) >= 5]

    res_name = dane['res_name']
    res_name_group = grouped_res_name['res_name_group']

    cols = ["part_0[0-9]_.*", "TwoFoFc_.*", "FoFc_.*", "Fc_.*", "Fo_.*", "local_volume", "local_electrons",
            "local_mean", "local_std", "local_min", "local_max", "local_skewness", "local_parts", "resolution",
            "solvent_mask_count", "void_mask_count", "modeled_mask_count", "solvent_ratio"]
    colsReg = "^(" + '|'.join(cols) + ")$"
    dane = dane.filter(regex=colsReg)

    dane = dane.fillna(0)
    dane_test = dane_test.fillna(0)

    corr_matrix = dane.corr()
    corr_matrix.loc[:, :] = np.tril(corr_matrix, k=-1)
    already_in = set()
    result = []
    for col in corr_matrix:
        perfect_corr = corr_matrix[col][abs(corr_matrix[col]) > 0.9].index.tolist()
        if perfect_corr and col not in already_in:
            already_in.update(set(perfect_corr))
            perfect_corr.append(col)
            result.append(perfect_corr)
    to_delete = []
    for group in result:
        for c in group[1:]:
            to_delete.append(c)
    dane = dane.drop(to_delete, 1)

    # ograniczenie danych testowych do tych samych kolumn
    dane_test = dane_test[list(dane.columns.values)]

    print dane.shape
    
    param_dist = {"max_depth": [20, None],
                  "max_features": sp_randint(1, 40),
                  "min_samples_split": sp_randint(1, 40),
                  "min_samples_leaf": sp_randint(1, 40),
                  "bootstrap": [True, False],
                  "criterion": ["gini", "entropy"]}
    n_iter_search = 25

    print '---------- klasyfikacja res_name'
    clf = RandomForestClassifier(n_estimators=25)
    random_search = RandomizedSearchCV(clf, param_distributions=param_dist, cv=5, n_iter=n_iter_search,
                                       scoring='recall_weighted', n_jobs=4)
    start = time()
    random_search = random_search.fit(dane, res_name)
    print "Czas: %.2f sec" % (time() - start)
    print 'best params:', random_search.best_params_
    print 'best score:', random_search.best_score_

    clf_res_name = random_search.best_estimator_
    joblib.dump(clf_res_name, 'clf_res_name.pkl', compress=1)

    print '---------- klasyfikacja res_name_group'
    clf = RandomForestClassifier(n_estimators=25)
    random_search = RandomizedSearchCV(clf, param_distributions=param_dist, cv=5, n_iter=n_iter_search,
                                       scoring='recall_weighted', n_jobs=4)
    start = time()
    random_search = random_search.fit(dane, res_name_group)
    print "Czas: %.2f sec" % (time() - start)
    print 'best params:', random_search.best_params_
    print 'best score:', random_search.best_score_

    clf_res_name_group = random_search.best_estimator_
    joblib.dump(clf_res_name_group, 'clf_res_name_group.pkl', compress=1)

    print '---------- predykcja res_name'
    predicted = clf_res_name.predict(dane_test)
    text_file = open("predicted_res_name.txt", "w")
    for p in predicted:
        text_file.write("%s\n" % p)
    text_file.close()

    print '---------- predykcja res_name_group'
    predicted_group = clf_res_name_group.predict(dane_test)
    text_file = open("predicted_res_name_group.txt", "w")
    for p in predicted_group:
        text_file.write("%s\n" % p)
    text_file.close()
