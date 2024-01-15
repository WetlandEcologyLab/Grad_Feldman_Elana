#import data
cover_dat <- read.csv("Raw_Data/2023_greenhouse_data_raw.csv")
biomass_dat <- read.csv("Raw_Data/2023_greenhouse_biomass_data_raw.csv")

#All package version saved in renv.lock 
#renv::init, renv::restore

library(tidyverse)

# Fix cover data ####
glimpse(cover_dat)

#change all values to the midpoint and make numeric
#0s are nudged to a trace value and 100s are nudged to .995 so we can use the beta distribution

#TOTAL
unique(cover_dat$Total)
cover_dat$Total[cover_dat$Total == "<1"] <- 0.005
cover_dat$Total[cover_dat$Total == "1"] <- .05
cover_dat$Total[cover_dat$Total == "10"] <- .15
cover_dat$Total[cover_dat$Total == "20"] <- .25
cover_dat$Total[cover_dat$Total == "30"] <- .35
cover_dat$Total[cover_dat$Total == "40"] <- .45
cover_dat$Total[cover_dat$Total == "50"] <- .55
cover_dat$Total[cover_dat$Total == "60"] <- .65
cover_dat$Total[cover_dat$Total == "70"] <- .75
cover_dat$Total[cover_dat$Total == "80"] <- .85
cover_dat$Total[cover_dat$Total == "90"] <- .95
cover_dat$Total[cover_dat$Total == ">99"] <- .995
cover_dat$Total <- as.numeric(cover_dat$Total)
unique(cover_dat$Total)

#PHRAG
unique(cover_dat$Phrag)
cover_dat$Phrag[cover_dat$Phrag == "<1"] <- 0.005
cover_dat$Phrag[cover_dat$Phrag == "1"] <- .05
cover_dat$Phrag[cover_dat$Phrag == "10"] <- .15
cover_dat$Phrag[cover_dat$Phrag == "20"] <- .25
cover_dat$Phrag[cover_dat$Phrag == "30"] <- .35
cover_dat$Phrag[cover_dat$Phrag == "40"] <- .45
cover_dat$Phrag[cover_dat$Phrag == "50"] <- .55
cover_dat$Phrag[cover_dat$Phrag == "60"] <- .65
cover_dat$Phrag[cover_dat$Phrag == "70"] <- .75
cover_dat$Phrag[cover_dat$Phrag == "80"] <- .85
cover_dat$Phrag[cover_dat$Phrag == "90"] <- .95
cover_dat$Phrag[cover_dat$Phrag == ">99"] <- .995
cover_dat$Phrag <- as.numeric(cover_dat$Phrag)
unique(cover_dat$Phrag)

#EUMA
unique(cover_dat$EUMA)
cover_dat$EUMA[cover_dat$EUMA == "<1"] <- 0.005
cover_dat$EUMA[cover_dat$EUMA == "1"] <- .05
cover_dat$EUMA[cover_dat$EUMA == "10"] <- .15
cover_dat$EUMA[cover_dat$EUMA == "20"] <- .25
cover_dat$EUMA[cover_dat$EUMA == "30"] <- .35
cover_dat$EUMA[cover_dat$EUMA == "40"] <- .45
cover_dat$EUMA[cover_dat$EUMA == "50"] <- .55
cover_dat$EUMA[cover_dat$EUMA == "60"] <- .65
cover_dat$EUMA[cover_dat$EUMA == "70"] <- .75
cover_dat$EUMA[cover_dat$EUMA == "80"] <- .85
cover_dat$EUMA[cover_dat$EUMA == "90"] <- .95
cover_dat$EUMA[cover_dat$EUMA == ">99"] <- .995
cover_dat$EUMA <- as.numeric(cover_dat$EUMA)
unique(cover_dat$EUMA)

#SOCA
unique(cover_dat$SOCA)
cover_dat$SOCA[cover_dat$SOCA == "<1"] <- 0.005
cover_dat$SOCA[cover_dat$SOCA == "1"] <- .05
cover_dat$SOCA[cover_dat$SOCA == "10"] <- .15
cover_dat$SOCA[cover_dat$SOCA == "20"] <- .25
cover_dat$SOCA[cover_dat$SOCA == "30"] <- .35
cover_dat$SOCA[cover_dat$SOCA == "40"] <- .45
cover_dat$SOCA[cover_dat$SOCA == "50"] <- .55
cover_dat$SOCA[cover_dat$SOCA == "60"] <- .65
cover_dat$SOCA[cover_dat$SOCA == "70"] <- .75
cover_dat$SOCA[cover_dat$SOCA == "80"] <- .85
cover_dat$SOCA[cover_dat$SOCA == "90"] <- .95
cover_dat$SOCA[cover_dat$SOCA == ">99"] <- .995
cover_dat$SOCA <- as.numeric(cover_dat$SOCA)
unique(cover_dat$SOCA)

#EUOC
unique(cover_dat$EUOC)
cover_dat$EUOC[cover_dat$EUOC == "<1"] <- 0.005
cover_dat$EUOC[cover_dat$EUOC == "1"] <- .05
cover_dat$EUOC[cover_dat$EUOC == "10"] <- .15
cover_dat$EUOC[cover_dat$EUOC == "20"] <- .25
cover_dat$EUOC[cover_dat$EUOC == "30"] <- .35
cover_dat$EUOC[cover_dat$EUOC == "40"] <- .45
cover_dat$EUOC[cover_dat$EUOC == "50"] <- .55
cover_dat$EUOC[cover_dat$EUOC == "60"] <- .65
cover_dat$EUOC[cover_dat$EUOC == "70"] <- .75
cover_dat$EUOC[cover_dat$EUOC == "80"] <- .85
cover_dat$EUOC[cover_dat$EUOC == "90"] <- .95
cover_dat$EUOC[cover_dat$EUOC == ">99"] <- .995
cover_dat$EUOC <- as.numeric(cover_dat$EUOC)
unique(cover_dat$EUOC)

#BOMA
unique(cover_dat$BOMA)
cover_dat$BOMA[cover_dat$BOMA == "<1"] <- 0.005
cover_dat$BOMA[cover_dat$BOMA == "1"] <- .05
cover_dat$BOMA[cover_dat$BOMA == "10"] <- .15
cover_dat$BOMA[cover_dat$BOMA == "20"] <- .25
cover_dat$BOMA[cover_dat$BOMA == "30"] <- .35
cover_dat$BOMA[cover_dat$BOMA == "40"] <- .45
cover_dat$BOMA[cover_dat$BOMA == "50"] <- .55
cover_dat$BOMA[cover_dat$BOMA == "60"] <- .65
cover_dat$BOMA[cover_dat$BOMA == "70"] <- .75
cover_dat$BOMA[cover_dat$BOMA == "80"] <- .85
cover_dat$BOMA[cover_dat$BOMA == "90"] <- .95
cover_dat$BOMA[cover_dat$BOMA == ">99"] <- .995
cover_dat$BOMA <- as.numeric(cover_dat$BOMA)
unique(cover_dat$BOMA)

#SCAC
unique(cover_dat$SCAC)
cover_dat$SCAC[cover_dat$SCAC == "<1"] <- 0.005
cover_dat$SCAC[cover_dat$SCAC == "1"] <- .05
cover_dat$SCAC[cover_dat$SCAC == "10"] <- .15
cover_dat$SCAC[cover_dat$SCAC == "20"] <- .25
cover_dat$SCAC[cover_dat$SCAC == "30"] <- .35
cover_dat$SCAC[cover_dat$SCAC == "40"] <- .45
cover_dat$SCAC[cover_dat$SCAC == "50"] <- .55
cover_dat$SCAC[cover_dat$SCAC == "60"] <- .65
cover_dat$SCAC[cover_dat$SCAC == "70"] <- .75
cover_dat$SCAC[cover_dat$SCAC == "80"] <- .85
cover_dat$SCAC[cover_dat$SCAC == "90"] <- .95
cover_dat$SCAC[cover_dat$SCAC == ">99"] <- .995
cover_dat$SCAC[cover_dat$SCAC == " <1"] <- 0.05
cover_dat$SCAC <- as.numeric(cover_dat$SCAC)
unique(cover_dat$SCAC)

#SCAM
unique(cover_dat$SCAM)
cover_dat$SCAM[cover_dat$SCAM == "<1"] <- 0.005
cover_dat$SCAM[cover_dat$SCAM == "1"] <- .05
cover_dat$SCAM[cover_dat$SCAM == "10"] <- .15
cover_dat$SCAM[cover_dat$SCAM == "20"] <- .25
cover_dat$SCAM[cover_dat$SCAM == "30"] <- .35
cover_dat$SCAM[cover_dat$SCAM == "40"] <- .45
cover_dat$SCAM[cover_dat$SCAM == "50"] <- .55
cover_dat$SCAM[cover_dat$SCAM == "60"] <- .65
cover_dat$SCAM[cover_dat$SCAM == "70"] <- .75
cover_dat$SCAM[cover_dat$SCAM == "80"] <- .85
cover_dat$SCAM[cover_dat$SCAM == "90"] <- .95
cover_dat$SCAM[cover_dat$SCAM == ">99"] <- .995
cover_dat$SCAM[cover_dat$SCAM == " <1"] <- 0.05
cover_dat$SCAM <- as.numeric(cover_dat$SCAM)
unique(cover_dat$SCAM)

#DISP
unique(cover_dat$DISP)
cover_dat$DISP[cover_dat$DISP == "<1"] <- 0.005
cover_dat$DISP[cover_dat$DISP == "1"] <- .05
cover_dat$DISP[cover_dat$DISP == "10"] <- .15
cover_dat$DISP[cover_dat$DISP == "20"] <- .25
cover_dat$DISP[cover_dat$DISP == "30"] <- .35
cover_dat$DISP[cover_dat$DISP == "40"] <- .45
cover_dat$DISP[cover_dat$DISP == "50"] <- .55
cover_dat$DISP[cover_dat$DISP == "60"] <- .65
cover_dat$DISP[cover_dat$DISP == "70"] <- .75
cover_dat$DISP[cover_dat$DISP == "80"] <- .85
cover_dat$DISP[cover_dat$DISP == "90"] <- .95
cover_dat$DISP[cover_dat$DISP == ">99"] <- .995
cover_dat$DISP[cover_dat$DISP == " <1"] <- 0.05
cover_dat$DISP <- as.numeric(cover_dat$DISP)
unique(cover_dat$DISP)

#MUAS
unique(cover_dat$MUAS)
cover_dat$MUAS[cover_dat$MUAS == "<1"] <- 0.005
cover_dat$MUAS[cover_dat$MUAS == "1"] <- .05
cover_dat$MUAS[cover_dat$MUAS == "10"] <- .15
cover_dat$MUAS[cover_dat$MUAS == "20"] <- .25
cover_dat$MUAS[cover_dat$MUAS == "30"] <- .35
cover_dat$MUAS[cover_dat$MUAS == "40"] <- .45
cover_dat$MUAS[cover_dat$MUAS == "50"] <- .55
cover_dat$MUAS[cover_dat$MUAS == "60"] <- .65
cover_dat$MUAS[cover_dat$MUAS == "70"] <- .75
cover_dat$MUAS[cover_dat$MUAS == "80"] <- .85
cover_dat$MUAS[cover_dat$MUAS == "90"] <- .95
cover_dat$MUAS[cover_dat$MUAS == ">99"] <- .995
cover_dat$MUAS[cover_dat$MUAS == " <1"] <- 0.05
cover_dat$MUAS <- as.numeric(cover_dat$MUAS)
unique(cover_dat$MUAS)

#PUNU
unique(cover_dat$PUNU)
cover_dat$PUNU[cover_dat$PUNU == "<1"] <- 0.005
cover_dat$PUNU[cover_dat$PUNU == "1"] <- .05
cover_dat$PUNU[cover_dat$PUNU == "10"] <- .15
cover_dat$PUNU[cover_dat$PUNU == "20"] <- .25
cover_dat$PUNU[cover_dat$PUNU == "30"] <- .35
cover_dat$PUNU[cover_dat$PUNU == "40"] <- .45
cover_dat$PUNU[cover_dat$PUNU == "50"] <- .55
cover_dat$PUNU[cover_dat$PUNU == "60"] <- .65
cover_dat$PUNU[cover_dat$PUNU == "70"] <- .75
cover_dat$PUNU[cover_dat$PUNU == "80"] <- .85
cover_dat$PUNU[cover_dat$PUNU == "90"] <- .95
cover_dat$PUNU[cover_dat$PUNU == ">99"] <- .995
cover_dat$PUNU[cover_dat$PUNU == " <1"] <- 0.05
cover_dat$PUNU <- as.numeric(cover_dat$PUNU)
unique(cover_dat$PUNU)

#make the date a date
cover_dat$Date <- lubridate::mdy(cover_dat$Date)

#Split tub column into mix, density, and phrag presence
cover_dat <- cover_dat %>% 
  separate(col = "Tub", into = c("Mix", "Other"))

cover_dat <- cover_dat %>% 
  separate(col = "Other", into = c("Density", "Phrag_Presence"), sep=1)


#check it
unique(cover_dat$Mix)
unique(cover_dat$Density)
unique(cover_dat$Phrag_Presence)

#Fix phrag column so it is equal to the total column
#When filling out the spreadsheet, I would put the total cover for all the PHAU in the 
#PHAU control plots under Total instead of Phrag
cover_dat <- cover_dat %>% 
  mutate(Phrag = ifelse(Mix == "PHAU", Total, Phrag))

#Name the mixes
cover_dat$Mix[cover_dat$Mix == 1] <- "Forb"
cover_dat$Mix[cover_dat$Mix == 2] <- "Grass"
cover_dat$Mix[cover_dat$Mix == 3] <- "Bulrush"
cover_dat$Mix[cover_dat$Mix == 4] <- "Equal"

#Make everything factors
cover_dat$Mix <- as.factor(cover_dat$Mix)
cover_dat$Density <- as.factor(cover_dat$Density)
cover_dat$Phrag_Presence <- as.factor(cover_dat$Phrag_Presence)

# Fix biomass data ####
glimpse(biomass_dat)

#change column name
colnames(biomass_dat)[4] <- "Weight"


#add a trace value to my "T" values and then make numeric
biomass_dat$Weight[biomass_dat$Weight == "T"] <- 0.5
biomass_dat$Weight <- as.numeric(biomass_dat$Weight)

#get rid of the unnecessary space after some species ("PHAU ")
biomass_dat$Species[biomass_dat$Species == "PHAU "] <- "PHAU"

#split the tub column into mix, density, and phrag presence
biomass_dat <- biomass_dat %>% 
  separate(col = "Tub", into = c("Mix", "Other"))

biomass_dat <- biomass_dat %>% 
  separate(col = "Other", into = c("Density", "Phrag_Presence"), sep=1)

# make wide so each species is a different column
biomass_dat <- biomass_dat %>% pivot_wider(
  names_from = Species,
  values_from = Weight,
  id_cols = c(Mix, Density, Phrag_Presence, Replicate)
)

#Any tub that was seeded with PHAU but has PHAU cover listed as NA should have that changed to 0
biomass_dat %>% 
  filter(Phrag_Presence == "W" & is.na(PHAU))

biomass_dat[60, 5] <- 0 #should be a 

# Name the mixes
biomass_dat$Mix[biomass_dat$Mix == 1] <- "Forb"
biomass_dat$Mix[biomass_dat$Mix == 2] <- "Grass"
biomass_dat$Mix[biomass_dat$Mix == 3] <- "Bulrush"
biomass_dat$Mix[biomass_dat$Mix == 4] <- "Equal"

#make everything a factor
biomass_dat$Mix <- as.factor(biomass_dat$Mix)
biomass_dat$Density <- as.factor(biomass_dat$Density)
biomass_dat$Phrag_Presence <- as.factor(biomass_dat$Phrag_Presence)

glimpse(biomass_dat)

# Save everything ####

save(biomass_dat, cover_dat, file = "main_dfs.RData")
