
# Projekt z uczenia maszynowego

## Tomasz Kaik 106646
## Arkadiusz Rusin 106644

### Wczytane biblioteki
Do przetwarzania danych i budowy klasyfikatora oraz pomiaru czasu wykorzystaliśmy biblioteki: 
- pandas
- sklearn
- numpy


```python
import pandas as pd
import numpy as np
from sklearn.grid_search import RandomizedSearchCV
from sklearn.ensemble import RandomForestClassifier
from scipy.stats import randint as sp_randint
from time import time
from sklearn.externals import joblib
```

### Wczytywanie danych treningowych i testowych
Do wczytania danych wykorzystana została funkcja read_csv(), gdzie wskazane zostały wartoścu NA w naszym zbiorze (na_values='nan'), oraz ustawiona została maska (keep_default_na=False), gdyż biblioteka pandas domyślnie traktował wartości NA jako puste (a w analizowanym zbiorze 'NA' jest poprawną wartością atrybutu res_name)


```python
dane = pd.read_csv("all_summary.txt", sep=";", na_values=["nan"], keep_default_na=False, dtype={'res_id': object})
grouped_res_name = pd.read_csv("grouped_res_name.txt", sep=",", na_values=["nan"], keep_default_na=False)

dane_test = pd.read_csv("test_data.txt", sep=",", na_values=["nan"], keep_default_na=False)
```

### Filtrowanie danych
Filtrowanie danych przebiegało w 5 etapach
1. Pozbycie się wartości ["DA", "DC", "DT", "DU", "DG", "DI", "UNK", "UNX", "UNL", "PR", "PD", "Y1", "EU", "N", "15P", "UQ", "PX4", "NAN"] z kolumny "res_name"
2. Pozbycie się duplikatów ("pdb_code", "res_name")
3. Pozbycie się wierszy, których liczność klasy "res_name" w ogólnym zbiorze danych nie przekraczała 5 instancji
4. Pozostawienie odpowiednich kolumn do klasyfikacji
5. Zamiana wartości pustych (nan) na wartości równe 0


```python
dane = dane[~dane["res_name"].isin(["DA", "DC", "DT", "DU", "DG", "DI", "UNK", "UNX", "UNL", "PR", "PD", 
                                    "Y1", "EU", "N", "15P", "UQ", "PX4", "NAN"])]
```


```python
dane = dane.drop_duplicates(subset=["pdb_code", "res_name"], keep='first')
```


```python
dane = dane[dane.groupby('res_name').res_id.transform(len) >= 5]
```


```python
res_name = dane['res_name']
res_name_group = grouped_res_name['res_name_group']

cols = ["part_0[0-9]_.*", "TwoFoFc_.*", "FoFc_.*", "Fc_.*", "Fo_.*", "local_volume", "local_electrons",
        "local_mean", "local_std", "local_min", "local_max", "local_skewness", "local_parts", "resolution",
        "solvent_mask_count", "void_mask_count", "modeled_mask_count", "solvent_ratio"]
colsReg = "^(" + '|'.join(cols) + ")$"
dane = dane.filter(regex=colsReg)
```


```python
dane = dane.fillna(0)
dane_test = dane_test.fillna(0)
```

### Obliczanie korelacji
Treningowy zbiór danych składał się z:
- 11005 instancji
- 795 atrybutów

Dlatego, aby usprawnić klasyfikację i budowanie modelu, obliczona została korelacja pomiędzy wszystkimi kolumnami. Następnie kolumny, których wartość bezwzględna korelacji przekraczała zadany próg (0.9) zostały usunięte.
Ostatecznie zbiór treningowy składał się z:
- 11005 instancji
- 257 atrybutów


```python
corr_matrix = dane.corr()
corr_matrix.loc[:, :] = np.tril(corr_matrix, k=-1)
already_in = set()
result = []
for col in corr_matrix:
    perfect_corr = corr_matrix[col][abs(corr_matrix[col]) > threshold].index.tolist()
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
dane_test = dane_test[list(data.columns.values)]
```

### Uczenie klasyfikatorów
Do budowy modelu klasyfikacji wykorzystany został algorytm Random Forest


```python
clf = RandomForestClassifier(n_estimators=30)
```

Zdefiniowano następującą przestrzeń parametrów


```python
param_dist = {"max_depth": [20, None],
              "max_features": sp_randint(1, 40),
              "min_samples_split": sp_randint(1, 40),
              "min_samples_leaf": sp_randint(1, 40),
              "bootstrap": [True, False],
              "criterion": ["gini", "entropy"]}
```

Najlepsze klasyfikatory zostały wygenerowane metodą Randomized Search CV, która w testach okazał się szybszy od Grid Search CV. Użyto dodatkowo 5-krotnej walidacji krzyżowej (cv=5). Do optymalizacji wykorzystana została miara "recall".


```python
n_iter_search = 25

random_search = RandomizedSearchCV(clf, param_distributions=param_dist, cv=5, n_iter=n_iter_search, 
                                   scoring='recall_weighted', n_jobs=4)
start = time()
random_search.fit(dane, res_name)
print "Czas: %.2f sec" % (time() - start)
```

### Najlepsze znalezione klasyfikatory 

Uczenie klasyfikatora zostało wykonane zarówno dla oryginalnego zbioru etykiet (res_name), jak i pogrupowanych etykiet (res_name_group). 

Poniżej wyświetlone zostały parametry, dla których uzyskano najlepsze klasyfikatory, oraz wartość optymalizowanej miary (best score - recall) osiągniętą dla tych klasyfikatorów:


```python
print 'best params:', random_search.best_params_
print 'best score:', random_search.best_score_
```


```python
# dla res_name:
best params: {'bootstrap': False, 'min_samples_leaf': 5, 'min_samples_split': 20, 
              'criterion': 'gini', 'max_features': 36, 'max_depth': 20}
best score: 0.397

# dla res_name_group:
best params: {'bootstrap': False, 'min_samples_leaf': 5, 'min_samples_split': 11, 
              'criterion': 'entropy', 'max_features': 33, 'max_depth': 20}
best score: 0.449
```

Najlepsze znalezione klasyfikatory zostały zapisane za pomocą biblioteki joblib do plików: 
* clf_res_name.plk
* clf_res_name_group.plk


```python
clf_res_name = random_search.best_estimator_
joblib.dump(random_search.best_estimator_, 'clf_res_name.pkl', compress=1)
```

### Klasyfikacja danych testowych

Najlepsze znalezione klasyfikatory zostały uruchomione dla danych testowych.


```python
predicted = clf_res_name.predict(dane_test)

text_file = open("predicted_res_name.txt", "w")
for p in predicted:
    text_file.write("%s\n" % p)
text_file.close()
```

Uzyskane predykcje (dla obu typów klasyfikatorów) zostały załączone odpowiednio w plikach:
* predicted_res_name.txt
* predicted_res_name_group.txt
