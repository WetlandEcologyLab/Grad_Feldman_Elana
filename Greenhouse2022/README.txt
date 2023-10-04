Most the code related to the first greenhouse experiment in Chapter 1 of my thesis. All code related to the process error model of growth rate is in the folder Process_Error_Model. 

Raw-Data - Copy of the original data collected for this experiment; another copy is also on my Google Drive
Cleaned-Data - includes all the cleaned data to be used for analysis; another copy is also on my Google Drive
Graphs  - includes all figures I have created in R
Code - all the code used for my analyses of this data in R
main_dfs.RData - data object that includes all the dataframes needed for analysis; always loaded in the beginning of my R scripts
Greenhouse2022.Rproj - The R project that uses all my R scripts in the Code folder

Overview of data cleaning process:
-u (for unknown) and x (for not relevant) were changed to NAs
-Tub column was split into Species, Density, and Phrag_Presence columns
-More forms of cleaning were completed in the DataCleaning.R file: values were changed to mid-points, 0s for cover estimates were nudged to a trace value, column names were fixed 
