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
#unique(greenhouse$Cover.Native)
greenhouse$Cover.Native[greenhouse$Cover.Native == "<1"] <- "0.5"
greenhouse$Cover.Native[greenhouse$Cover.Native == ">99"] <- "99.5"
greenhouse$Cover.Native[greenhouse$Cover.Native == 1.00] <- 5.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 10.00] <- 15.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 20.00] <- 25.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 30.00] <- 35.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 40.00] <- 45.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 50.00] <- 55.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 60.00] <- 65.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 70.00] <- 75.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 80.00] <- 85.00
greenhouse$Cover.Native[greenhouse$Cover.Native == 90.00] <- 95.00
greenhouse$Cover.Native <- as.double(greenhouse$Cover.Native)
greenhouse$Cover.Native <- greenhouse$Cover.Native/100

#unique(greenhouse$Cover.Phrag)
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 1.00] <- 5.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 10.00] <- 15.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 20.00] <- 25.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 30.00] <- 35.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 40.00] <- 45.00
greenhouse$Cover.Phrag <- as.double(greenhouse$Cover.Phrag)
greenhouse$Cover.Phrag <- greenhouse$Cover.Phrag/100

#make all 0s a trace amount so we can use the beta
greenhouse$Cover.Native[greenhouse$Cover.Native == 0] <- 0.005 

#convert the dates
greenhouse$Date <- lubridate::mdy(greenhouse$Date)
greenhouse$Date_Cleaned <- lubridate::mdy(greenhouse$Date_Cleaned)

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