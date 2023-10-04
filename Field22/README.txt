All the code related to Chapter 2 of my thesis - Field experiments at both Farmington Bay and Utah Lake in both the 2022 and 2023 growing seasons

Year-1-Figures/ - includes all figures I have created in R using the field data from 2022
Year-2-Figures/ - includes all figures I have created in R using the field data from 2023
Raw-Data - Copy of the original data collected for this experiment; another copy is also on my Google Drive
Cleaned-Data - includes all the cleaned data to be used for analysis; another copy is also on my Google Drive
Code - all the code used for my analyses of this data in R
clean_dfs.RData, wells.RData, fb23.RData - data object that includes all the dataframes needed for analysis; always loaded in the beginning of my R scripts
Field2022.Rproj - The R project that uses all my R scripts in the Code folder
Class_SQL_Bookdown - all additional files related to my Reproducible Data Science Coursework associated with this research

Overview of data cleaning process:
-0s were entered as needed in the Measurement columns
-Plot values were separated into Group and Density columns
-More forms of cleaning were completed in the DataCleaning.R file: values were changed to mid-points, 0s for cover estimates were nudged to a trace value, column names were fixed 

