# diabetes_subphenotypes_youth
Subphenotypes of Youth-onset T2DM, associations with complications (neuropathy, nephropathy)

`preprocessing/`
  - Extract variables from two studies (SEARCH, TODAY) and created subsets.

`etiologic/`
  - Identified diabetes cases based on etiologic evidence (T2: DAA negative & insulin     
    resistant, or Other/Unknown forms
  
`etiologic/preprocessing`
  - created analytic sample
  - applied KNN imputation on 7 variables from clustering
  - calculated residuals (linear regression: 7 variables ~ age)
  - K-Means clustering (k: 2-10) using 7 vars, identified 3 clusters

`etiologic/analysis`
  - combined MNSI data
  - descriptive analysis
  - neuropathy/nephropathy:
    multiple imputation on DSPN indicators/nephropathy indicator
    cross-sectional analysis generating PRs (crude vs adjusted)
  - calculated HOMA-B, HOMA-IR on TODAY

`etiologic/paper`
  - created final tables and figures for manuscript

`etiologic/sensitivity analysis`
  - `\complete cases`
    exclude obs with missing MNSI scores
    cross-sectional analysis generating PRs (crude vs adjusted)
  - `\search only`
    created subset with SEARCH only from original dataset with existing clusters
    MI on MNSI values; cross-sectional analysis


`factorial/`
  - Identified T2DM based on etiologic evidence as:
  Diabetes-Associated Autoantibodies (DAA) negative and insulin sensitive (≥8.15), 
  DAA negative and insulin resistant (<8.15), 
  Missing DAA and Insulin Resistant (<8.15), 
  Missing DAA and Insulin Sensitive (≥8.15), 
  or Missing Both DAA and Insulin Sensitivity Score (ISS) 

`prov/`
  - Identified diabetes cases based on the initial provider, we included Type 2 and      
    Other/Unknown forms.
  