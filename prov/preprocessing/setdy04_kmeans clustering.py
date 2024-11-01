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

if os.getlogin()=="JVARGH7":
    path_diabetes_subphenotypes_youth_folder = "C:/Cloud/OneDrive - Emory University/Papers/Subphenotypes in Youth-onset T2DM"
if os.getlogin()=='JGUO258':
    path_diabetes_subphenotypes_youth_folder = "C:/Users/JGUO258/OneDrive - Emory/Subphenotypes in Youth-onset T2DM"


analytic_dataset = pd.read_csv(path_diabetes_subphenotypes_youth_folder + '/working/cleaned/prov/setdy03_knn imputation add residuals.csv') 

#----------------------------------------------------------------------------------------------------------------------------------
# cluster variables: "bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc"

study = analytic_dataset['study']
study_id = analytic_dataset['study_id']
dmduration_category = analytic_dataset['dmduration_category']
race_eth = analytic_dataset['race_eth']
female = analytic_dataset['female']
age_category = analytic_dataset['age_category']
totalc = analytic_dataset['totalc']
insulin = analytic_dataset['insulin']
insulinf = analytic_dataset['insulinf']
metformin = analytic_dataset['metformin']
tgl = analytic_dataset['tgl']
glucosef = analytic_dataset['glucosef']
bmipct = analytic_dataset['bmipct']
dmfamilyhistory = analytic_dataset['dmfamilyhistory']

retinopathy_lefteye = analytic_dataset['retinopathy_lefteye']
retinopathy_righteye = analytic_dataset['retinopathy_righteye']
retinopathy = analytic_dataset['retinopathy']
retinopathy_tx = analytic_dataset['retinopathy_tx']
dkd = analytic_dataset['dkd']
nephropathy_prescription = analytic_dataset['nephropathy_prescription']
nephropathy_tx = analytic_dataset['nephropathy_tx']
nephropathy_diag = analytic_dataset['nephropathy_diag']


analytic_dataset = analytic_dataset.drop(columns = ['study_id', 'study', 'age_category', 'dmduration_category', 'race_eth',
                                                  'female','tgl','glucosef','insulinf','totalc','insulin','metformin','bmipct',
                                                  "retinopathy_lefteye","retinopathy_righteye","retinopathy","retinopathy_tx",
                                                  "dkd","nephropathy_prescription","nephropathy_tx","nephropathy_diag",'dmfamilyhistory'])
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
var = ["bmi_residual","hba1c_residual","cpeptidef_residual", "sbp_residual","dbp_residual","ldlc_residual","hdlc_residual"]
cluster_var = data_scaled[var]

kmeans = KMeans(init="random", n_clusters=3, n_init=10, max_iter=300, random_state=57)
kmeans.fit(cluster_var)

# summarize the cluster labels to the original dataset
analytic_dataset_cluster = analytic_dataset.copy()
analytic_dataset_cluster['cluster'] = kmeans.labels_
analytic_dataset_cluster.groupby('cluster').mean()

# relabel the cluster labels 
analytic_dataset_cluster['cluster'] = analytic_dataset_cluster['cluster'].replace({0:'yMOD', 1:'ySIRD', 2:'ySIDD'})
analytic_dataset_cluster['cluster'].value_counts()

# add study, race, and female back to the dataset
analytic_dataset_cluster['study'] = study
analytic_dataset_cluster['study_id'] = study_id
analytic_dataset_cluster['dmduration_category'] = dmduration_category
analytic_dataset_cluster['race_eth'] = race_eth
analytic_dataset_cluster['female'] = female
analytic_dataset_cluster['age_category'] = age_category
analytic_dataset_cluster['totalc'] = totalc
analytic_dataset_cluster['insulin'] = insulin
analytic_dataset_cluster['insulinf'] = insulinf
analytic_dataset_cluster['metformin'] = metformin
analytic_dataset_cluster['tgl'] = tgl
analytic_dataset_cluster['glucosef'] = glucosef
analytic_dataset_cluster['bmipct'] = bmipct
analytic_dataset_cluster['dmfamilyhistory'] = dmfamilyhistory

analytic_dataset_cluster['retinopathy_lefteye'] = retinopathy_lefteye
analytic_dataset_cluster['retinopathy_righteye'] = retinopathy_righteye
analytic_dataset_cluster['retinopathy'] = retinopathy
analytic_dataset_cluster['retinopathy_tx'] = retinopathy_tx
analytic_dataset_cluster['dkd'] = dkd
analytic_dataset_cluster['nephropathy_prescription'] = nephropathy_prescription
analytic_dataset_cluster['nephropathy_tx'] = nephropathy_tx
analytic_dataset_cluster['nephropathy_diag'] = nephropathy_diag


analytic_dataset_cluster['cluster'].value_counts()
analytic_dataset_cluster.to_csv(path_diabetes_subphenotypes_youth_folder + '/working/cleaned/prov/setdy04_kmeans clustering.csv', index=False)
 
 
 
#---------------------------------------------------------------------------------------------------------------------------------------------- 
# plot the clusters
# add the cluster labels to the copy of the scaled data

data_scaled_cluster = data_scaled.copy()
data_scaled_cluster['cluster'] = kmeans.labels_
# relabel the cluster labels
data_scaled_cluster['cluster'] = data_scaled_cluster['cluster'].replace({0:'yMOD', 1:'ySIRD', 2:'ySIDD'})



import seaborn as sns
import matplotlib.pyplot as plt

# Create a new DataFrame with the cluster assignments and variables
data_clustered = pd.concat([data_scaled_cluster['cluster'], data_scaled_cluster[["bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc"]]], axis=1)

# Melt the DataFrame to convert it into long format
data_melted = data_clustered.melt(id_vars='cluster', var_name='Variable', value_name='Value')

# Create the boxplot
plt.figure(figsize=(9.5, 6))  # Adjusted figure size for better layout
sns.boxplot(x='cluster', y='Value', hue='Variable', data=data_melted)
plt.title('Variables by Clusters')
plt.xlabel('Cluster')
plt.ylabel('Value')
plt.xticks(rotation=45)

# Move the legend to the right of the plot
plt.legend(title='Variable', bbox_to_anchor=(1.01, 1), loc=2, borderaxespad=0.)

plt.tight_layout()  # Adjust the layout to make room for the legend
plt.show()
 
