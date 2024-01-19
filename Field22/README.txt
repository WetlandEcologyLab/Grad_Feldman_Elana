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
-More forms of cleaning were completed in the DataCleaning.R file: values were changed to mid-points and made decimals, trace values were added to the 0s for the beta distribution, values greater than 99% were made .995 for the beta distribution, some column names were changed

Species codes used in scripts:
ALPR = Alopecurus pratensis 
ASIN = Asclepias incarnata 
BASC = Bassia scoparia 
BICE = Bidens cernua 
BOMA = Bolboschoenus maritimus 
BY = Echinochloa crus-galli 
Cheno = Chenopodium sp. 
CYDA = Cynodon dactylon 
CYER = Cyperus erythrorhizos 
DISP = Distichlis spicata 
EUMA = Eutrochium maculatum 
EUOC = Euthamia occidentalis 
JUAR = Juncus arcticus 
JUGE = Juncus gerardii 
JUTO = Juncus torreyi 
LASE = Lactuca serriola 
LEFY = Leptochloa fusca 
MUAS = Muhlenbergia asperifolia 
PHAU = Phragmites australis 
POFR = Populus fremontii 
POPE = Polygonum persicaria 
PUNU = Puccinellia nuttalliana 
RACY = Ranunculus cymbalaria 
RUST = Rumex stenophyllus 
RUMA = Rumex maritimus 
SAAM = Salix amygdaloides 
SARU = Salicornia rubra 
SCAC = Schoenoplectus acutus 
SCAM = Schoenoplectus americanus 
SCPU = Schoenoplectus pungens 
SOCA = Solidago canadensis 
SYCI = Symphyotrichum ciliatum 
Tamarisk = Tamarix chinensis 
TYPHA = Typha sp. 