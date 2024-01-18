#All package version saved in renv.lock 
#renv::init, renv::restore
library(tidyverse)
library(patchwork)

#Load data
load("main_dfs.RData")

#Graph set up####
#refactor the species so that PHAU is first in the cover graphs and rest are alphabetical
greenhouse$Species <- factor(greenhouse$Species,
                             levels = c('PHAU',"BICE", 'BOMA', 'DISP', 'EPCI', 'EUMA',
                                        'EUOC', 'HENU', 'JUAR', 'JUGE', 'JUTO',
                                        'MUAS', 'PUNU', 'RUMA', 'SCAC', 'SCAM',
                                        'SCPU', 'SOCA', 'SYCI'))

#change the NAs in Density to Control since they are all the control tubs
greenhouse$Density <- as.character(greenhouse$Density)
greenhouse$Density[is.na(greenhouse$Density)] <- "Control"
greenhouse$Density <- factor(greenhouse$Density, levels = c("Control", "Low", "High"),
                             labels = c("Control","Low", "High"))

biomass$Density <- as.character(biomass$Density)
biomass$Density[is.na(biomass$Density)] <- "Control"
biomass$Density <- factor(biomass$Density, levels = c("Control", "Low", "High"),
                          labels = c("Control","Low", "High"))

#Native species graphs####
## Native cover over time by species, phrag presence, and density ####

cover_native <- greenhouse %>% 
  filter(Species != "PHAU") %>%
  ggplot(aes(x = Date_Cleaned, y = Cover.Native, shape = Phrag_Presence, color = Density)) +
  #using the means of the blocks
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2) +
  #error bars added
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = 0),
               fun.data = mean_se, geom = "errorbar") +
  #add a line to connect the dates
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "line") +
  facet_wrap(~Species) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.title =ggtext::element_markdown(size = 10),
        legend.text = ggtext::element_markdown(size = 9)) +
  labs(x = "Date", y = "Proportional Native Cover", color = "Density", shape = "*P.australis* Presence") +
  scale_color_manual(values = c("darkblue", "red3")) + #change the legend labels
  scale_shape(labels = c("Present", "Absent"))

cover_native

## Native biomass by species, density, and phrag presence####
#manually reoder biomass graphs so that species are in order from smallest to largest
biomass %>% 
  group_by(Species) %>% 
  mutate(mean = mean(Native.Biomass, na.rm = TRUE)) %>% 
  arrange(mean) 

biomass <- biomass %>% 
  mutate(Species = factor(Species,
                          levels = c("SCAM", "JUTO", "BOMA",
                                     "JUGE", "SCPU", "SCAC", "JUAR",
                                     "SOCA", "DISP", "EUOC",
                                     "PUNU", "EUMA", "SYCI", "MUAS",
                                     "EPCI", "HENU", 'RUMA', 'BICE', 'PHAU')))
biomass_native <- biomass %>%
  filter(Species != "PHAU") %>%
  ggplot(aes(x = Species,
             y = Native.Biomass, color = Density)) +
  #using the means of the blocks
  stat_summary(aes(group = interaction(Species, Density)),
               fun = mean, geom = "point", size = 2, position = position_dodge(width = .5)) +
  #error bars added
  stat_summary(aes(group = interaction(Species, Density), width = 0),
               fun.data = mean_se, geom = "errorbar", position = position_dodge(width = .5)) +
  labs(x = "Native Species Identity", y = "Native Biomass (g)", color = "Density") +
  facet_wrap(~Phrag_Presence) +
  scale_color_manual(values = c("darkblue", "red3")) + #change the legend labels
  theme(legend.title = ggtext::element_markdown(),
        legend.text = element_text(),#change legend size
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "bottom") 

biomass_native

#Phragmites graphs####

## Phrag cover over time by density ####
cover_phrag <- greenhouse %>%
  ggplot(aes(x = Date_Cleaned, y = Cover.Phrag, color= Density)) +
  #using the means of the blocks
  stat_summary(aes(group = Density),
               fun = mean, geom = "point") +
  #add a line to connect the dates
  stat_summary(aes(group = Density),
               fun = mean, geom = "line") +
  #error bars added
  stat_summary(aes(group = Density, width = 0),
               fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Species) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Date", y = "*P.australis* Proportional Cover", color = "Native Seeding Density") +
  scale_color_manual(values = c("#7D7D7D","darkblue", "red3")) +
  theme(axis.title.y = ggtext::element_markdown())+
  coord_cartesian(ylim = c(0, 0.4))

cover_phrag

#Reduction Graphs####

##Cover Reduction####
#make a separate dataset of the control values to use later
control.values <- greenhouse %>%
  dplyr::select(Tub, Species, Block, Date_Cleaned, Cover.Phrag) %>% 
  filter(Date_Cleaned == "2022-05-16",
         Species == "PHAU")

#now calculate the data we want
final.cover.red <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16", #only need the last date of sampling
         Phrag_Presence == "Present") %>% #only need tubs with PHAU
  dplyr::select(Tub, Species, Density, Block, Phrag_Presence,
                Date_Cleaned, Cover.Phrag) %>% 
  group_by(Block) %>% #because we need to calculate on the block level
  mutate(Phrag.Control = case_when(
    Block == 1 ~ control.values$Cover.Phrag[1], #add a new column of the PHAU cover in control
    Block == 2 ~ control.values$Cover.Phrag[2],
    Block == 3 ~ control.values$Cover.Phrag[3]
  )) %>% 
  mutate(P.Cover.Red = ((Cover.Phrag - Phrag.Control)/Phrag.Control)*-1) #calculate percent reduction
  #multiply by -1 to make it positive for graphing

cover.red <- final.cover.red %>% 
  filter(Species != "PHAU") %>% #only the native tubs
  mutate(Species = factor(Species, #arrange species so axes match the biomass graph
                          levels = c("SCAM", "JUAR", "SCAC",
                                     "JUGE", "SYCI", "MUAS", "PUNU",
                                     "BOMA", "SOCA", "SCPU", "EUOC", "JUTO",
                                     "DISP", "EUMA", "BICE", "EPCI",
                                     "RUMA", "HENU"))) %>% 
  ggplot(aes(x = Species, y = P.Cover.Red, color = Density)) +
  stat_summary(aes(group = Density),
               fun = mean, geom = "point", 
               position = position_dodge(0.95)) +
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown()) +
  labs(y = "Proportion Reduction in *P.australis* Cover", x = "Native Species Identity") +
  scale_color_manual(values = c("darkblue", "red3")) + #change the legend labels 
  ylim(c(-1, 1))

cover.red

##Biomass Reduction####
#make a separate dataset of the control values to use later
control.values2 <- biomass %>%
  dplyr::select(Tub, Species, Block, Phrag.Biomass) %>% 
  filter(Species == "PHAU")

#now calculate the data we want
final.biomass.red <- biomass %>% #only need the last date of sampling 
  filter(Phrag_Presence == "Present") %>% #only need tubs with PHAU
  dplyr::select(Tub, Species, Density, Block, Phrag_Presence,
                Phrag.Biomass) %>% 
  group_by(Block) %>% #because we need to calculate on the block level
  mutate(Phrag.Control = case_when(
    Block == 1 ~ control.values2$Phrag.Biomass[1],#add a new column of the PHAU cover in control
    Block == 2 ~ control.values2$Phrag.Biomass[2],
    Block == 3 ~ control.values2$Phrag.Biomass[3]
  )) %>% 
  mutate(P.Biomass.Red = ((Phrag.Biomass - Phrag.Control)/Phrag.Control)*-1) #calculate percent reduction
#multiply by -1 to make it positive for graphing

biomass.red <- final.biomass.red %>%
  filter(Species != "PHAU") %>% #only the native tubs
  ggplot(aes(x = reorder(Species, P.Biomass.Red), y = P.Biomass.Red, color = Density)) +
  stat_summary(aes(group = Density),
               fun = mean, geom = "point", 
               position = position_dodge(0.95)) +
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown()) +
  labs(y = "Proportion Reduction in *P.australis* Biomass", x = "Native Species Identity") +
  scale_color_manual(values = c("darkblue", "red3"))  #change the legend labels

biomass.red

# Graph of cover versus biomass ####

#make a new dataframe that joins ending cover and biomass 
cover_final <- greenhouse %>% 
  filter(Date_Cleaned == "2022-05-16", #only need last sampling date
         Species != "PHAU") %>%  #only the native species 
  select(Species, Density, Phrag_Presence, Block, Cover.Native)

final_data <- biomass %>%  #left join so they match up biomass and cover
  select(Species, Density, Phrag_Presence, Block, Native.Biomass) %>% 
  left_join(cover_final, by = c("Species", "Density", "Phrag_Presence", "Block"))

plot(final_data$Native.Biomass ~ final_data$Cover.Native)

#correlation
cor(final_data$Native.Biomass, final_data$Cover.Native, use = "complete.obs")
