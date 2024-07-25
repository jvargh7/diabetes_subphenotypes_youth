# The purpose of this python file is to run k means on five variables: age of diagnosis, bmi, HbA1c, fasting insulin, and fasting glucose
# load libraries
import pandas as pd 
import numpy as np
import os as os

import sklearn as sklearn

import matplotlib.pyplot as plt
import seaborn as sns
from kneed import KneeLocator
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import StandardScaler


#path = "C:/Users/JGUO258/Documents/JGUO/diabetes_subphenotypes_youth/analysis/dsy01_complete cases df.csv"
path = "C:/Users/JGUO258/Documents/JGUO/diabetes_subphenotypes_youth/analysis/dsy02_kmeans imputation.csv"

analytic_dataset = pd.read_csv(path) 

# cluster variables: "bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc"
study = analytic_dataset['study']
study_id = analytic_dataset['study_id']
dmduration_category = analytic_dataset['dmduration_category']
female = analytic_dataset['female']
age_category = analytic_dataset['age_category']
totalc = analytic_dataset['totalc']

analytic_dataset = analytic_dataset.drop(columns = ['study_id', 'age_category', 'dmduration_category', 'female','totalc','study'])
analytic_dataset.shape

#check if any missing values
analytic_dataset.isnull().sum()
#check variable types
analytic_dataset.dtypes

#run kmeans clustering to create the TRUE labels
#standardize the data
scaler = StandardScaler()
data_scaled = scaler.fit_transform(analytic_dataset)

data_scaled = pd.DataFrame(data_scaled, columns=analytic_dataset.columns)

data_scaled.head()

# select variables to cluster
var = ["bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc"]
cluster_var = data_scaled[var]

kmeans = KMeans(init="random", n_clusters=5, n_init=10, max_iter=300, random_state=57)
kmeans.fit(cluster_var)

# summarize the cluster labels to the original dataset
analytic_dataset_cluster = analytic_dataset.copy()
analytic_dataset_cluster['cluster'] = kmeans.labels_
analytic_dataset_cluster.groupby('cluster').mean()

# relabel the cluster labels 
# analytic_dataset_cluster['cluster'] = analytic_dataset_cluster['cluster'].replace({0:'SIDD', 1:'MARD', 2:'SIRD', 3:'MOD'})
# analytic_dataset_cluster['cluster'].value_counts()

# add study, race, and female back to the dataset
analytic_dataset_cluster['study'] = study
analytic_dataset_cluster['study_id'] = study_id
analytic_dataset_cluster['dmduration_category'] = dmduration_category
analytic_dataset_cluster['female'] = female
analytic_dataset_cluster['age_category'] = age_category
analytic_dataset_cluster['totalc'] = totalc

analytic_dataset_cluster['cluster'].value_counts()
 
 
# plot the clusters
# add the cluster labels to the copy of the scaled data

data_scaled_cluster = data_scaled.copy()
data_scaled_cluster['cluster'] = kmeans.labels_
# relabel the cluster labels
#data_scaled_cluster['cluster'] = data_scaled_cluster['cluster'].replace({0:'SIDD', 1:'MARD', 2:'SIRD', 3:'MOD'})

import seaborn as sns
# Create a new DataFrame with the cluster assignments and variables
data_clustered = pd.concat([data_scaled_cluster['cluster'], data_scaled_cluster[["bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc"]]], axis=1)

# Melt the DataFrame to convert it into long format
data_melted = data_clustered.melt(id_vars='cluster', var_name='Variable', value_name='Value')

# Create the boxplot
plt.figure(figsize=(10, 6))
sns.boxplot(x='cluster', y='Value', hue='Variable', data=data_melted)
plt.title('Variables by Clusters')
plt.xlabel('Cluster')
plt.ylabel('Value')
plt.xticks(rotation=45)
plt.show()

analytic_dataset_cluster.head()
