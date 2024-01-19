#Import everything ####
greenhouse <- read.csv("Cleaned_Data/Greenhouse2022_clean.csv")
biomass <- read.csv("Cleaned_Data/Biomass_Cleaned.csv")

#All package version saved in renv.lock 
#renv::init, renv::restore
library(dplyr)
library(magrittr)
library(lubridate)

#Clean cover data ####
glimpse(greenhouse)

greenhouse[greenhouse==""] <-NA #make the blanks in the cover columns NA

#make sure categories look about right
unique(greenhouse$Species)
unique(greenhouse$Density)
unique(greenhouse$Phrag_Presence)

#change all cover values to midpoint, make double, make a decimal
unique(greenhouse$Cover.Native)
greenhouse <- greenhouse %>% 
  mutate(Cover.Native = case_when(
    Cover.Native == "<1" ~ 0.5,
    Cover.Native == ">99" ~ 99.5,
    Cover.Native == "1" ~ 5,
    Cover.Native == "10" ~ 15,
    Cover.Native == "20" ~ 25,
    Cover.Native == "30" ~ 35,
    Cover.Native == "40" ~ 45,
    Cover.Native == "50" ~ 55,
    Cover.Native == "60" ~ 65,
    Cover.Native == "70" ~ 75,
    Cover.Native == "80" ~ 85,
    Cover.Native == "90" ~ 95,
    Cover.Native == "0" ~ 0.5)) %>% #use a trace amount so we can use the beta
  mutate(Cover.Native = as.double(Cover.Native)) %>% 
  mutate(Cover.Native = Cover.Native/100)

unique(greenhouse$Cover.Phrag)
greenhouse <- greenhouse %>% 
  mutate(Cover.Phrag = case_when(
    Cover.Phrag == "1" ~ 5,
    Cover.Phrag == "10" ~ 15,
    Cover.Phrag == "20" ~ 25,
    Cover.Phrag == "30" ~ 35,
    Cover.Phrag == "40" ~ 45)) %>% 
  mutate(Cover.Phrag = as.double(Cover.Phrag)) %>% 
  mutate(Cover.Phrag = Cover.Phrag/100)

#convert the dates
greenhouse <- greenhouse %>% 
  mutate(Date = lubridate::mdy(greenhouse$Date), 
         Date_Cleaned = lubridate::mdy(greenhouse$Date_Cleaned))

#make the everything a factor and relabel
glimpse(greenhouse)
greenhouse$Block <- as.factor(greenhouse$Block)
greenhouse$Species <- as.factor(greenhouse$Species)
greenhouse$Density <- factor(greenhouse$Density, levels = c("L", "H"),
                             labels = c("Low", "High"))
greenhouse$Phrag_Presence <- factor(greenhouse$Phrag_Presence, levels = c("WO", "W"),
                                    labels = c("Absent", "Present"))

#Clean the biomass data ####

glimpse(biomass)

#make block a factor
biomass$Block <- as.factor(biomass$Block)
biomass$Species <- as.factor(biomass$Species)
biomass$Phrag_Presence <- factor(biomass$Phrag_Presence, levels = c("WO", "W"),
                                 labels = c("Absent", "Present"))
biomass$Density <- factor(biomass$Density, levels = c("L", "H"),
                          labels = c("Low", "High"))

#double check that the numbers make sense
max(biomass$Native.Biomass, na.rm = TRUE)
min(biomass$Native.Biomass, na.rm = TRUE)

max(biomass$Phrag.Biomass, na.rm = TRUE)
min(biomass$Phrag.Biomass, na.rm = TRUE)

#Save the objects ####
save(biomass, greenhouse, file = "main_dfs.RData")