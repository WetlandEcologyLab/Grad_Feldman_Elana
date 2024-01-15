All the code related to Chapter III of my thesis - Field experiments at both Farmington Bay and Utah Lake in both the 2022 and 2023 growing seasons

Raw-Data - Copy of the original data collected for this experiment as it was originally entered into Google Sheets
Cleaned-Data - includes all the cleaned data to be used for analysis, cleaned in a separate sheet on Google Sheets
Code - all the code used for my analyses of this data in R
clean_dfs.RData, wells.RData, fb23.RData - data object that includes all the dataframes needed for analysis; always loaded in the beginning of my R scripts
Field2022.Rproj - The R project that uses all my R scripts in the Code folder
renv, renv.lock - renv library of all the packages as they were when these analyses were completed

Overview of data cleaning process:
-0s were entered as needed in the Measurement columns
-Plot values were separated into Group and Density columns
-More forms of cleaning were completed in the DataCleaning.R file: values were changed to mid-points, 0s for cover estimates were nudged to a trace value, column names were fixed 

