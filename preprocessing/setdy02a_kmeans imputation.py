import numpy as np
import pandas as pd
from sklearn.impute import KNNImputer
import os


if os.getlogin()=="JVARGH7":
    path_diabetes_subphenotypes_youth_folder = "C:/Cloud/OneDrive - Emory University/Papers/Subphenotypes in Youth-onset T2DM"
if os.getlogin()=='JGUO258':
    path_diabetes_subphenotypes_youth_folder = "C:/Users/JGUO258/OneDrive - Emory/Subphenotypes in Youth-onset T2DM"


data_mi = pd.read_csv(path_diabetes_subphenotypes_youth_folder + '/working/cleaned/setdy01a_analytic sample.csv')

#select variables 
selected_variables = ["study_id","study","age_category","dmduration_category","race_eth",
                      "female","bmi","hba1c","cpeptidef","tgl","glucosef","insulinf",
                      "sbp","dbp","totalc","ldlc","hdlc","insulin","metformin"]

#drop missing values in the selected variables
data_mi = data_mi[selected_variables]
data_mi.shape

############### Do KNN Imputation with K = 5 by Study Sites #####################
columns_to_impute = ["bmi","hba1c","cpeptidef","sbp","dbp","ldlc","hdlc"]

# Function to impute data for one study site
def impute_study_data(data, n_neighbors=5):
    imputer = KNNImputer(n_neighbors=n_neighbors)
    data[columns_to_impute] = imputer.fit_transform(data[columns_to_impute])
    return data
# Impute data for each study site
study_sites = data_mi['study'].unique()
imputed_datasets = []

for site in study_sites:
    site_data = data_mi[data_mi['study'] == site].copy()
    imputed_data = impute_study_data(site_data)
    imputed_datasets.append(imputed_data)
    
# Merge all imputed datasets back to one
imputed_data_merged = pd.concat(imputed_datasets)


# Check if there are any missing values
print(imputed_data_merged.isnull().sum())
print(data_mi.isnull().sum())
# compare the imputed data with the original data
data_mi.describe()
imputed_data_merged.describe()

# save the imputed data
imputed_data_merged.to_csv(path_diabetes_subphenotypes_youth_folder + '/working/cleaned/setdy02_kmeans imputation.csv', index=False)



## elbow plot - find the optimal # of cluster
selected_variables = ["bmi","hba1c","cpeptidef","sbp","dbp","ldlc","hdlc","tgl"]
data_to_cluster = imputed_data_merged[selected_variables]


kmeans_kwargs = { # set the parameters for the kmeans algorithm
    "init": "random",
    "n_init": 10,
    "max_iter": 300,
    "random_state": 57,
}

# A list holds the SSE values for each k
sse = [] #initiate an empty list to store the sum of squared errors 
for k in range(1, 11):
    kmeans = KMeans(n_clusters=k, **kmeans_kwargs)
    kmeans.fit(data_to_cluster)
    sse.append(kmeans.inertia_)

plt.figure(figsize=(4, 2))
plt.style.use("fivethirtyeight")
plt.plot(range(1, 11), sse)
plt.xticks(range(1, 11))
plt.yticks(np.linspace(min(sse), max(sse), 10), ['{:.2e}'.format(x) for x in np.linspace(min(sse), max(sse), 10)])  # Format y-ticks as scientific notation
plt.xlabel("Number of Clusters")
plt.ylabel("SSE")
plt.show()
