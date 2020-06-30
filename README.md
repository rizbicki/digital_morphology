# digital_morphology


[!(https://github.com/rizbicki/digital_morphology/releases/tag/v1.1)](https://zenodo.org/badge/235828767.svg)

This is the content of each of the folders:


# R 

## Measurement precision and accuracy

- measurement_precision_accuracy.R # plots and tests for evaluating measurement precision and accuracy

## Accuracy of image-based datasets

- compte_pca_cor.R # creates RDS files with correlation between principal components
- compte_pca_sim.R # creates RDS files with PCA similarity
- plot_pca.R # plots the figures based on the RDS files created with the code above
- correlation_missing_accuracy.R # correlation between how much missing data there is and the accuracy in estimating the principal components

## Descriptive analysis

- compute_statistics.R # computes missing data statistics

## RDS 

Contains the RDS files generated by the scripts compte_pca_cor.R and compte_pca_sim.R.
These are used by plot_pca.R to plot the figures based on the RDS files created with the code above


## Data 

- image-variable-check (copy).xlsx  -> which variables from the 'original datasets' could be measured in images
- measurements-clean.xls  ->  the 'measurement dataset'
- variable-unit.xlsx  -> units of each of the 'original datasets'
- measurements_sampling_vouchers.txt -> information on measurements, sampling, and vouchers.

## Data_missing 

All 'original datasets' of morphometric studies of different plant groups.


## additional_figures_tables.pdf

Contains additional figures and tables 

