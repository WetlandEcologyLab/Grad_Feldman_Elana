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
cover_dat <- cover_dat %>% 
  mutate(Total = case_when(
    Total == "<1" ~ 0.005,
    Total == ">99" ~ .995,
    Total == "1" ~ .05,
    Total == "10" ~ .15,
    Total == "20" ~ .25,
    Total == "30" ~ .35,
    Total == "40" ~ .45,
    Total == "50" ~ .55,
    Total == "60" ~ .65,
    Total == "70" ~ .75,
    Total == "80" ~ .85,
    Total == "90" ~ .95,
    Total == "0" ~ 0)) %>% 
  mutate(Total = as.double(Total))

#PHRAG
cover_dat <- cover_dat %>% 
  mutate(Phrag = case_when(
    Phrag == "<1" ~ 0.005,
    Phrag == ">99" ~ .995,
    Phrag == "1" ~ .05,
    Phrag == "10" ~ .15,
    Phrag == "20" ~ .25,
    Phrag == "30" ~ .35,
    Phrag == "40" ~ .45,
    Phrag == "50" ~ .55,
    Phrag == "60" ~ .65,
    Phrag == "70" ~ .75,
    Phrag == "80" ~ .85,
    Phrag == "90" ~ .95,
    Phrag == "0" ~ 0)) %>% 
  mutate(Phrag = as.double(Phrag))

#EUMA
cover_dat <- cover_dat %>% 
  mutate(EUMA = case_when(
    EUMA == "<1" ~ 0.005,
    EUMA == ">99" ~ .995,
    EUMA == "1" ~ .05,
    EUMA == "10" ~ .15,
    EUMA == "20" ~ .25,
    EUMA == "30" ~ .35,
    EUMA == "40" ~ .45,
    EUMA == "50" ~ .55,
    EUMA == "60" ~ .65,
    EUMA == "70" ~ .75,
    EUMA == "80" ~ .85,
    EUMA == "90" ~ .95,
    EUMA == "0" ~ 0)) %>% 
  mutate(EUMA = as.double(EUMA))

#SOCA
cover_dat <- cover_dat %>% 
  mutate(SOCA = case_when(
    SOCA == "<1" ~ 0.005,
    SOCA == ">99" ~ .995,
    SOCA == "1" ~ .05,
    SOCA == "10" ~ .15,
    SOCA == "20" ~ .25,
    SOCA == "30" ~ .35,
    SOCA == "40" ~ .45,
    SOCA == "50" ~ .55,
    SOCA == "60" ~ .65,
    SOCA == "70" ~ .75,
    SOCA == "80" ~ .85,
    SOCA == "90" ~ .95,
    SOCA == "0" ~ 0)) %>% 
  mutate(SOCA = as.double(SOCA))

#EUOC
cover_dat <- cover_dat %>% 
  mutate(EUOC = case_when(
    EUOC == "<1" ~ 0.005,
    EUOC == ">99" ~ .995,
    EUOC == "1" ~ .05,
    EUOC == "10" ~ .15,
    EUOC == "20" ~ .25,
    EUOC == "30" ~ .35,
    EUOC == "40" ~ .45,
    EUOC == "50" ~ .55,
    EUOC == "60" ~ .65,
    EUOC == "70" ~ .75,
    EUOC == "80" ~ .85,
    EUOC == "90" ~ .95,
    EUOC == "0" ~ 0)) %>% 
  mutate(EUOC = as.double(EUOC))

#BOMA
cover_dat <- cover_dat %>% 
  mutate(BOMA = case_when(
    BOMA == "<1" ~ 0.005,
    BOMA == ">99" ~ .995,
    BOMA == "1" ~ .05,
    BOMA == "10" ~ .15,
    BOMA == "20" ~ .25,
    BOMA == "30" ~ .35,
    BOMA == "40" ~ .45,
    BOMA == "50" ~ .55,
    BOMA == "60" ~ .65,
    BOMA == "70" ~ .75,
    BOMA == "80" ~ .85,
    BOMA == "90" ~ .95,
    BOMA == "0" ~ 0)) %>% 
  mutate(BOMA = as.double(BOMA))

#SCAC
cover_dat$SCAC[cover_dat$SCAC == " <1"] <- "<1"
cover_dat <- cover_dat %>% 
  mutate(SCAC = case_when(
    SCAC == "<1" ~ 0.005,
    SCAC == ">99" ~ .995,
    SCAC == "1" ~ .05,
    SCAC == "10" ~ .15,
    SCAC == "20" ~ .25,
    SCAC == "30" ~ .35,
    SCAC == "40" ~ .45,
    SCAC == "50" ~ .55,
    SCAC == "60" ~ .65,
    SCAC == "70" ~ .75,
    SCAC == "80" ~ .85,
    SCAC == "90" ~ .95,
    SCAC == "0" ~ 0)) %>% 
  mutate(SCAC = as.double(SCAC))

#SCAM
cover_dat <- cover_dat %>% 
  mutate(SCAM = case_when(
    SCAM == "<1" ~ 0.005,
    SCAM == ">99" ~ .995,
    SCAM == "1" ~ .05,
    SCAM == "10" ~ .15,
    SCAM == "20" ~ .25,
    SCAM == "30" ~ .35,
    SCAM == "40" ~ .45,
    SCAM == "50" ~ .55,
    SCAM == "60" ~ .65,
    SCAM == "70" ~ .75,
    SCAM == "80" ~ .85,
    SCAM == "90" ~ .95,
    SCAM == "0" ~ 0)) %>% 
  mutate(SCAM = as.double(SCAM))

#DISP
cover_dat <- cover_dat %>% 
  mutate(DISP = case_when(
    DISP == "<1" ~ 0.005,
    DISP == ">99" ~ .995,
    DISP == "1" ~ .05,
    DISP == "10" ~ .15,
    DISP == "20" ~ .25,
    DISP == "30" ~ .35,
    DISP == "40" ~ .45,
    DISP == "50" ~ .55,
    DISP == "60" ~ .65,
    DISP == "70" ~ .75,
    DISP == "80" ~ .85,
    DISP == "90" ~ .95,
    DISP == "0" ~ 0)) %>% 
  mutate(DISP = as.double(DISP))

#MUAS
cover_dat$MUAS[cover_dat$MUAS == " <1"] <- "<1"
cover_dat <- cover_dat %>% 
  mutate(MUAS = case_when(
    MUAS == "<1" ~ 0.005,
    MUAS == ">99" ~ .995,
    MUAS == "1" ~ .05,
    MUAS == "10" ~ .15,
    MUAS == "20" ~ .25,
    MUAS == "30" ~ .35,
    MUAS == "40" ~ .45,
    MUAS == "50" ~ .55,
    MUAS == "60" ~ .65,
    MUAS == "70" ~ .75,
    MUAS == "80" ~ .85,
    MUAS == "90" ~ .95,
    MUAS == "0" ~ 0)) %>% 
  mutate(MUAS = as.double(MUAS))

#PUNU
cover_dat <- cover_dat %>% 
  mutate(PUNU = case_when(
    PUNU == "<1" ~ 0.005,
    PUNU == ">99" ~ .995,
    PUNU == "1" ~ .05,
    PUNU == "10" ~ .15,
    PUNU == "20" ~ .25,
    PUNU == "30" ~ .35,
    PUNU == "40" ~ .45,
    PUNU == "50" ~ .55,
    PUNU == "60" ~ .65,
    PUNU == "70" ~ .75,
    PUNU == "80" ~ .85,
    PUNU == "90" ~ .95,
    PUNU == "0" ~ 0)) %>% 
  mutate(PUNU = as.double(PUNU))

#make the date a date
cover_dat$Date <- lubridate::mdy(cover_dat$Date)

#Split tub column into mix, density, and phrag presence
cover_dat <- cover_dat %>% 
  separate(col = "Tub", into = c("Mix", "Other")) %>% 
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
cover_dat <- cover_dat %>% 
  mutate(Mix = case_when(
    Mix == 1 ~ "Forb",
    Mix == 2 ~ "Grass",
    Mix == 3 ~ "Bulrush",
    Mix == 4 ~ "Equal",
    Mix == "PHAU" ~ "PHAU"))

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
  separate(col = "Tub", into = c("Mix", "Other")) %>% 
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
biomass_dat <- biomass_dat %>% 
  mutate(Mix = case_when(
    Mix == 1 ~ "Forb",
    Mix == 2 ~ "Grass",
    Mix == 3 ~ "Bulrush",
    Mix == 4 ~ "Equal",
    Mix == "PHAU" ~ "PHAU"))

#make everything a factor
biomass_dat$Mix <- as.factor(biomass_dat$Mix)
biomass_dat$Density <- as.factor(biomass_dat$Density)
biomass_dat$Phrag_Presence <- as.factor(biomass_dat$Phrag_Presence)

glimpse(biomass_dat)

# Save everything ####

save(biomass_dat, cover_dat, file = "main_dfs.RData")
