#set working directory

path.wd <- "/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Greenhouse2022/Cleaned_Data/"
setwd(path.wd)

greenhouse <- read.csv("Greenhouse2022_clean.csv")

library(dplyr)
library(magrittr)

#dplyr::glimpse(greenhouse)

#clean data to look at it
greenhouse[greenhouse==""] <-NA #make the blanks in the cover columns NA

#make sure categories came out right for tubs
#unique(greenhouse$Species)
#unique(greenhouse$Density)
#unique(greenhouse$Phrag_Presence)

#check that everything makes sense
#max(greenhouse$Height.Native.1, na.rm = TRUE)
#min(greenhouse$Height.Native.1, na.rm = TRUE)

#max(greenhouse$Height.Native.2, na.rm = TRUE)
#min(greenhouse$Height.Native.2, na.rm = TRUE)

#max(greenhouse$Height.Native.3, na.rm = TRUE)
#min(greenhouse$Height.Native.3, na.rm = TRUE)

#max(greenhouse$Height.Phrag.1, na.rm = TRUE)
#min(greenhouse$Height.Phrag.1, na.rm = TRUE)

#max(greenhouse$Height.Phrag.2, na.rm = TRUE)
#min(greenhouse$Height.Phrag.2, na.rm = TRUE)

#max(greenhouse$Height.Phrag.3, na.rm = TRUE)
#min(greenhouse$Height.Phrag.3, na.rm = TRUE)

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
#unique(greenhouse$Cover.Native)

greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 1.00] <- 5.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 10.00] <- 15.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 20.00] <- 25.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 30.00] <- 35.00
greenhouse$Cover.Phrag[greenhouse$Cover.Phrag == 40.00] <- 45.00
greenhouse$Cover.Phrag <- as.double(greenhouse$Cover.Phrag)
greenhouse$Cover.Phrag <- greenhouse$Cover.Phrag/100
#unique(greenhouse$Cover.Phrag)

#convert the dates
library(lubridate)
greenhouse$Date <- lubridate::mdy(greenhouse$Date)
greenhouse$Date_Cleaned <- lubridate::mdy(greenhouse$Date_Cleaned)

#make the Block into a factor
greenhouse$Block <- as.factor(greenhouse$Block)
#glimpse(greenhouse)

#average all the heights together
library(tidyverse)
df <- greenhouse %>% 
  select(Height.Native.1, Height.Native.2, Height.Native.3)  %>%              
  mutate(Height.Native = rowMeans(.,na.rm = T))

greenhouse$Height.Native <- df$Height.Native 

df1 <- greenhouse %>% 
  select(Height.Phrag.1, Height.Phrag.2, Height.Phrag.3)  %>%              
  mutate(Height.Phrag = rowMeans(.,na.rm = T))

greenhouse$Height.Phrag <- df1$Height.Phrag

#now the biomass data sheet
biomass <- read.csv("Biomass_Cleaned.csv")
#glimpse(biomass)

biomass$Block <- as.factor(biomass$Block)

#max(biomass$Native.Biomass, na.rm = TRUE)
#min(biomass$Native.Biomass, na.rm = TRUE)

#max(biomass$Phrag.Biomass, na.rm = TRUE)
#min(biomass$Phrag.Biomass, na.rm = TRUE)

#now the grouping sheet
func_grp <- read.csv("Groups_Final.csv")

#Save the objects

save(biomass, greenhouse, func_grp, file = "main_dfs.RData")
