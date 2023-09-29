Goals/Inputs/Outputs:
Goals - determine if Phragmites australis cover changes with native seed mix composition and density
Inputs - values for native seed mix composition, native seed mix density, and end of year Phragmites cover from the field; I also have cover data on about 20 other species 
Outputs - graphs of the final Phragmites cover (and other invasives) by plot (seed mix composition x density) and site (Utah Lake or Great Salt Lake); statistical analysis determining whether seed mix had an outcome; graphs of which seeded species were present in which plots 



Figures/ - includes all figures I have created in R using the field data
Raw-Data - Copy the original data collected for this experiment; another copy is also on my Google Drive
Cleaned-Data - includes all the cleaned data to be used for analysis; another copy is also on my Google Drive
Code - all the code used for my analyses of this data in R
clean_dfs.RData - data object that includes all the dataframes needed for analysis; always loaded in the beginning of my R scripts
Field2022.Rproj - The R project that uses all my R scripts in the Code folder

.gitignore is set to ignore all jpeg and docx files. It is also set to ignore the files Figures/ because that includes large files

***However, I can see that it is not reading my .gitignore and instead it is uploading all the files to my github anyway. How do I fix this??

Overview of data cleaning process:
-0s were entered as needed in the Measurement columns
-Plot values were separated into Group and Density columns
-More forms of cleaning were completed in the DataCleaning.R file

