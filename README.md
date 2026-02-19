# Depressive symptoms trajectories

**Paper**: 

This page contains the R analysis code from the above paper. The files are described below:

- **1. setup.R**: Loads ALSPAC data, create sex-specific datasets with the analysis sample and saves the data for later use.

- **2. face.R**: Fits FPCA and calcuates predictions and saves the output data.

- **3. lme.R**: Fits mixed effects models and calcuates predictions and saves the output data.

- **4. rmse.R**: Calcuates RMSE from each fitted model from the previous 2 scripts.

- **5. get_peak.R**: Calcautes the peak and age of depressive symptoms score and depressive symptoms velocity. Combines the estimates and saves the new dataset.

- **6. exp_peak**: Performs linear regression analyses to examine the association between early life risk factors and the derived peak features.
  
- **7. tables.R**: Generates tables for the main paper and supplement.

- **8. figures.R**: Generates figures for the main paper and supplement.
