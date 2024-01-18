All code related to the first greenhouse experiment in Chapter II of my thesis. 

Raw-Data - Copy of the original data collected for this experiment; another copy is also on my Google Drive
Cleaned-Data - includes all the cleaned data to be used for analysis; another copy is also on my Google Drive
Code - all the code used for my analyses of this data in R
main_dfs.RData - data object that includes all the dataframes needed for analysis; always loaded in the beginning of my R scripts
r_values - CSV of all the r values calculated in Growth_Rates.r
Greenhouse2022.Rproj - The R project that uses all my R scripts in the Code folder
renv;renv.lock - renv library of all the package versions used in the scripts

Overview of data cleaning process:
-u (for unknown) and x (for not relevant) were changed to NAs
-Tub column was split into Species, Density, and Phrag_Presence columns
-More forms of cleaning were completed in the DataCleaning.R file: values were changed to mid-points, 0s for cover estimates were nudged to a trace value, blank values were made NA

Species codes used in the scripts: 
BICE = Bidens cernua
BOMA = Bolboschoenus maritimus
DISP = Distichlis spicata
EPCI = Epilobium ciliatum
EUMA = Eutrochium maculatum
EUOC = Euthamia occidentalis
HENU = Helianthus nuttallii 
JUAR = Juncus arcticus
JUGE = Juncus gerardii
JUTO = Juncus torreyi
MUAS = Muhlenbergia asperifolia
PHAU = Phragmites australis
PUNU = Puccinellia nuttalliana
RUMA = Rumex maritimus
SCAC = Schoenoplectus acutus
SCAM = Schoenoplectus americanus
SCPU = Schoenoplectus pungens
SOCA = Solidago canadensis
SYCI = Symphyotrichum ciliatum