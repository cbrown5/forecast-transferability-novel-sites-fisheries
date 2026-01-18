# Transferability of models to novel site conditions: comparing abundance forecasts between different exploitation regimes

Code to support the research study. 
Code by Leigha Aitken and CJ Brown

## Summary

Models are increasingly used to make forecasts to support fisheries management by translating ecological theory into quantitative predictions. However, forecasts for fisheries species are often constrained by limited data from unfished reference conditions. No-take marine reserves (NTMRs) can provide insight into unfished population dynamics, potentially improving models’ predictive performance. This study asked whether incorporating NTMR data improves the accuracy of annual abundance forecasts over eight years. We compared the predictive performance of first-order autoregressive models that were trained on abundance data from either NTMR sites, fished sites, or a mix of both site types. Each model was then tested by predicting to sites it was not trained on (‘transferred’). We tested the forecasts for both fished and unfished species with 23 years of time-series abundance data from the Maria Island (Tasmania) NTMR and nearby control sites. We found a general decrease in the performance of fished and NTMR models when forecasts were transferred to the opposite site type. Model transferability was species-specific, with the NTMR model reducing forecasting errors by approximately 10% at fished sites for one sustainably fished species. In contrast, for other species, the fished model reduced forecast errors by approximately 4% and 56% at NTMR sites. Additionally, we found that models trained on a mix of site types consistently underperformed. These findings highlight the importance of accounting for site- and species-specific dynamics. While NTMR data offers valuable insights into unfished conditions, further work is required to create models that transfer well to any exploitation regime.

## Directory Overview

Below is an overview of the main sections and folders in this repository. Each section includes a brief explanation of its contents and purpose.

### Top-level Files

-   Readme.md: This file. Project overview and documentation.

### Data

Raw fish, inverts, environmental and environmental-surface (10m) data. Original data should be downloaded from: https://portal.aodn.org.au/ with appropriate attribution to authors listed on that site according to each datasets license. So we have not provided duplicated copies here. 
Processed data files used in the analysis can be provided directly by the authors on request. Please email c.j.brown@utas.edu.au

### Scripts

Main folder for all model scripts and outputs. - enviro-PCA: principal components analysis for environmental covariates. Subfolders include:

#### Scripts/Fish/

-   Fish-clean.R: Cleaning fish data.
-   Fish-model-comparison.R: Comparing enviro, no-enviro and lagged models.
-   Fish-model-elaboration.R: Fitting model.
-   Fish-plots: Calculate MASE and plots.
-   Workspace: Chosen fitted model workspaces - to load into plots script.

#### Scripts/Inverts/

-   Inverts-clean.R: Cleaning invert data.
-   Inverts-model-comparison.R: Comparing enviro, no-enviro and lagged models.
-   Inverts-model-elaboration.R: Fitting model.
-   Inverts-plots: Calculate MASE and plots.
-   Workspace: Chosen fitted model workspaces - to load into plots script.

#### Scripts/summary-data

-   .csv file outputs from/for scripts in Fish/ and Inverts/ folders.

### Plots

Figures for Fish/ and Inverts/ including:

#### /[species]

-   [species]\_mega-plot_enviro-10_50: Model forecasts of abundance trends across 50-split tests for environmental (10m) model.
-   [species]\_mega-plot_enviro-10_2_50: Plot of all 50 split-test groups for environmental (10m) model on 1 axis.
-   [species]\_mega-plot_ASE_enviro-10_50: Plot of all 50 split-test groups ASE values for environmental (10m) model.
-   [species]\_mega-plot_enviro-10_site-splitID: Plot of 1 split-test group
-   [species]\_mega-plot_enviro-10_site-splitID_2: Plot of 2 split-test groups
-   [species]\_mega-plot_enviro-10_site-splitID_combined / Figure_3: Plot of multiple split-test groups / Figure 3 in manuscript
-   [species]\_mega-plot_MASE_enviro-10_50 / Figure_4: MASE values for each site, grouped by split-test group / Figure 4 in manuscript
-   [species]\_Figure_5 / 6: MASE plots - Figures 5 and 6 in manuscript.

#### /pca-results

-   bioplot_10m: PCA plot for environmental (10m) data - Figure A1 in manuscript.
-   biplot_all: PCA plot for all environmental data.

## Workflow

-   Order: enviro-PCA, Fish/inverts-clean, Fish/Inverts-model-comparison, Fish/Inverts-model-elaboration, Inverts-plots
-   Inverts/Fish-plots script: load data using appropriate workspace as defined in Inverts/Fish-model-elaboration.R
-   Read in the data and filter for desired species.
-   Save plots for each species in new folders.
