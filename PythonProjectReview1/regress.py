# Dependencies
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

plt.style.use('ggplot')

import statsmodels.api as sm
from statsmodels.compat import lzip
import statsmodels.stats.api as sms
from statsmodels.formula.api import ols
from scipy.stats import zscore
from statsmodels.stats.stattools import durbin_watson
from sklearn.model_selection import train_test_split, KFold
from statsmodels.stats.outliers_influence import variance_inflation_factor
from sklearn.metrics import mean_squared_error
from sklearn.feature_selection import RFECV
from mlxtend.feature_selection import SequentialFeatureSelector as sfs
from mlxtend.plotting import plot_sequential_feature_selection as plot_sfs
from sklearn.linear_model import LinearRegression, RidgeCV, LassoCV, ElasticNetCV

# Objective
target = 'area'

# Load and describe data
path = r'Datasets\forestfires.csv'
df = pd.read_csv(path)
print(df.shape)
print(df.dtypes)
print(df.describe().T)

# Missing Value Treatment
print(df.isna().sum().sum())

# Exploratory Data Analysis
plt.rcParams["figure.figsize"] = 9, 5

# Univariate
plt.figure(figsize=(16, 5))
print("Skew: {}".format(df[target].skew()))
print("Kurtosis: {}".format(df[target].kurtosis()))
ax = sns.kdeplot(df[target], shade=True, color='g')
plt.xticks([i for i in range(0, 1200, 50)])
plt.show()
ax = sns.boxplot(df[target])

# Outlier points
y_outliers = df[abs(zscore(df[target])) >= 3]
print(y_outliers)

# Independent Columns
dfa = df.drop(columns=target)
cat_columns = dfa.select_dtypes(include='object').columns.tolist()
num_columns = dfa.select_dtypes(exclude='object').columns.tolist()
print(cat_columns)
print(num_columns)

# analyzing categorical columns
plt.figure(figsize=(16, 10))
for i, col in enumerate(cat_columns, 1):
    plt.subplot(2, 2, i)
    sns.countplot(data=dfa, y=col)
    plt.subplot(2, 2, i + 2)
    df[col].value_counts(normalize=True).plot.bar()
    plt.ylabel(col)
    plt.xlabel('% distribution per category')
plt.tight_layout()
plt.show()

# analyzing numerical columns
plt.figure(figsize=(18, 40))
for i, col in enumerate(num_columns, 1):
    plt.subplot(8, 4, i)
    sns.kdeplot(df[col], color='g', shade=True)
    plt.subplot(8, 4, i + 10)
    df[col].plot.box()
plt.tight_layout()
plt.show()
num_data = df[num_columns]
print(pd.DataFrame(data=[num_data.skew(), num_data.kurtosis()], index=['skewness', 'kurtosis']))

# Bivariate
print(df['area'].describe(),'\n')
print(y_outliers)

# a categorical variable based on forest fire area damage
# No damage, low, moderate, high, very high
def area_cat(area):
    if area == 0.0:
        return "No damage"
    elif area <= 1:
        return "low"
    elif area <= 25:
        return "moderate"
    elif area <= 100:
        return "high"
    else:
        return "very high"

df['damage_category'] = df['area'].apply(area_cat)
print(df.head())

# Categorical Analysis
for col in cat_columns:
    cross = pd.crosstab(index=df['damage_category'],columns=df[col],normalize='index')
    cross.plot.barh(stacked=True,rot=40,cmap='hot')
    plt.xlabel('% distribution per category')
    plt.xticks(np.arange(0,1.1,0.1))
    plt.title("Forestfire damage each {}".format(col))
plt.show()

# Numerical Analysis
plt.figure(figsize=(20,40))
for i,col in enumerate(num_columns,1):
    plt.subplot(10,1,i)
    if col in ['X','Y']:
        sns.swarmplot(data=df,x=col,y=target,hue='damage_category')
    else:
        sns.scatterplot(data=df,x=col,y=target,hue='damage_category')
plt.show()

# Multivariate
selected_features = df.drop(columns=['damage_category','day','month']).columns
print(selected_features)
sns.pairplot(df,hue='damage_category',vars=selected_features)
plt.show()

# Outlier Treatment
out_columns = ['area','FFMC','ISI','rain']

# Data Modelling

# Encoding Categorically
df = pd.get_dummies(df,columns=['day','month'],drop_first=True)

# Data Transformation
print(df[out_columns].describe())
print(np.log1p(df[out_columns]).skew()) 
print(np.log1p(df[out_columns]).kurtosis())

# FFMC and rain are still having high skew and kurtosis values, 
# since we will be using Linear regression model we cannot operate with such high values
# so for FFMC we can remove the outliers in them using z-score method
mask = df.loc[:,['FFMC']].apply(zscore).abs() < 3

# Since most of the values in rain are 0.0, we can convert it as a categorical column
df['rain'] = df['rain'].apply(lambda x: int(x > 0.0))

df = df[mask.values]
print(df.shape)

out_columns.remove('rain')
df[out_columns] = np.log1p(df[out_columns])
print(df[out_columns].skew())

# we will use this dataframe for building our ML model
df_ml = df.drop(columns=['damage_category']).copy()