All the code related to the second greenhouse experiment in Chapter II of my thesis

Raw-Data - Copy of the original data collected for this experiment; another copy is also on my Google Drive
Code - all the code used for my analyses of this data in R
main_dfs.Rdata - data object that includes all the dataframes needed for analysis; always loaded in the beginning of my R scripts
Greenhouse2023.Rproj - The R project that uses all my R scripts in the Code folder
renv; renv.lock - renv library of all package version used in the scripts

Overview of data cleaning process:
-All cleaning performed in the DataCleaning.R file: values were changed to mid-points, cover estimates of 0 were nudged to a trace value, cover values of >0.99 were nudged to 0.995, tub column was split into mix, density, and phrag_presence columns, biomass values of "T" were changes to a trace value of 0.5

Species codes used in code:
BOMA = Bolboschoenus maritimus
DISP = Distichlis spicata
EUMA = Eutrochium maculatum
EUOC = Euthamia occidentalis
MUAS = Muhlenbergia asperifolia
PHAU = Phragmites australis
PUNU = Puccinellia nuttalliana
SCAC = Schoenoplectus acutus
SCAM = Schoenoplectus americanus
SOCA = Solidago canadensis