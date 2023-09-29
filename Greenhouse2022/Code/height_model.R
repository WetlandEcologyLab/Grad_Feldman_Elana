#set working directory

path.wd <- "~/Desktop/Greenhouse2022"
setwd(path.wd)

greenhouse <- read.csv("Greenhouse2022_clean.csv")

library(dplyr)
library(magrittr)

dplyr::glimpse(greenhouse)

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

greenhouse$Cover.Native[greenhouse$Cover.Native == "<1"] <- "0.1"
greenhouse$Cover.Native[greenhouse$Cover.Native == ">99"] <- "99.99"
greenhouse$Cover.Native <- as.double(greenhouse$Cover.Native)
#unique(greenhouse$Cover.Native)

greenhouse$Cover.Phrag <- as.integer(greenhouse$Cover.Phrag)
#unique(greenhouse$Cover.Phrag)

#convert the dates
library(lubridate)
greenhouse$Date <- lubridate::mdy(greenhouse$Date)

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


##Modeling
species <- greenhouse %>%
  filter(Species == "EPCI", !is.na(Density),
         Date == "2022-05-16" | Date == "2022-05-17")

#use glmmtmb because will eventually use beta 
#library(glmmTMB)
#library(DHARMa)
#library(emmeans)
species.m1 <- glmmTMB(Height.Native ~ Phrag_Presence * Density #* for interaction
                      + (1|Block),
                      data = species
)


summary(species.m1)
simulateResiduals(species.m1, plot = T)
plotResiduals(species.m1, form= species$Phrag_Presence) 

#library(car)
Anova(species.m1) 
emmip(species.m1, Phrag_Presence~Density, CIs = T) 
