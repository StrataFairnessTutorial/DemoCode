## code from https://blog.fastforwardlabs.com/2017/03/09/fairml-auditing-black-box-predictive-models.html
## only lightly modified

import fairml
import sklearn
import pandas as pd

from sklearn.linear_model import LogisticRegression
from fairml import audit_model

df = pd.read_csv("~/Desktop/german_credit.csv")
y = df['class']
df.drop('class', 1, inplace = True)
df.drop('Unnamed: 0', 1, inplace = True)

clf = LogisticRegression(penalty='l2', C=0.01)
clf.fit(df.values, y)

importances, _ = audit_model( clf.predict, df)
print(importances)
