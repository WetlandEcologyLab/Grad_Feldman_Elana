#Load objects
load("main_dfs.RData")

#Load packages
#All package version saved in renv.lock 
#renv::init, renv::restore
library(tidyverse)
library(glmmTMB) 
library(DHARMa)
library(emmeans)
library(car)
library(patchwork)
library(multcompView)
library(gridExtra)

options(contrasts = c("contr.sum", "contr.poly")) #in case we want to run a Type III Anova

# Global model that includes all species ####
# colors to use for graphing
color1 <- c("orange", "purple4")
color2 <- c("darkblue", "red3")

##Native cover~Phrag Presence * Density * Species####
mdf <- greenhouse %>%
  dplyr::filter(!is.na(Density), #removes all the PHAU controls
                Date_Cleaned == "2022-05-16", #only need the last date of sampling
                Species != "JUTO" & Species != "JUGE"  & Species != "SCAM" & Species != "BOMA"& Species != "SYCI")
                #removed these 5 species from analyses because not enough replicates


#Run the model 
mdf.m1<- glmmTMB(Cover.Native ~ Phrag_Presence * Density * Species
                 + (1|Block),
                 data = mdf,
                 family = beta_family)

summary(mdf.m1)
mdf.m1_res <- simulateResiduals(mdf.m1, plot = T)

Anova(mdf.m1, type = 3) 
emmip(mdf.m1, Species~Density|Phrag_Presence, CIs = T)
emmip(mdf.m1, Phrag_Presence~Density|Species, CIs = T)

###Graph of models means for phrag presence####
emm <- emmeans(mdf.m1, pairwise ~ Phrag_Presence, adjust = "tukey", type = "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)

#remove the spaces in the group label so easier to graph
str_1 <- data1$.group
str_2 <- gsub(" ", "", str_1)

ggplot(data = data1, aes(x = Phrag_Presence, y = response,
                         color = Phrag_Presence)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Presence of *Phragmites*", y = "Model Predicted Proportional Native Cover",
       title = "(a)") +
  geom_text(aes(label = str_2,  y = response),
            nudge_x = 0.1, color = "black") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = color1)

###Graph of model means for Density * Species####
emm <- emmeans(mdf.m1, pairwise ~ Species * Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)

#remove the spaces in the group label so easier to graph
str_3 <- data2$.group
str_4 <- gsub(" ", "", str_3)

ggplot(data = data2, aes(x = reorder(Species,response), y = response, color = Density)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Species", y = "Model Predicted Proportional Native Cover",
       title = '(b)') +
  geom_text(aes(label = str_4,
                vjust = .9, hjust = "left"),
            nudge_x = .15,
            check_overlap = TRUE,
            color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9)) +
  scale_color_manual(values = color2)

##Native biomass~Phrag Presence * Density * Species####
mdf <- biomass %>%
  dplyr::filter(!is.na(Density), #remove the phau controls
                Species != "JUTO" & Species != "JUGE"  & Species != "SCAM" & Species != "BOMA" & Species != "SYCI") %>%
                #remove these 5 species from analyses because not enough replicates
  dplyr::mutate(Species = factor(Species)) #need to remove the factor levels (species) that were removed

#use this to fix the modeling problems due to an extra replicate in BICE
table(mdf$Species)
with(mdf, table(Species, Density, Phrag_Presence, useNA = "ifany"))
mdf_avg <- mdf %>% 
  dplyr::group_by(Block, Phrag_Presence, Density, Species) %>%
  dplyr::summarize(Native.Biomass = mean(Native.Biomass), 
                   nobs = dplyr::n()) %>%#to average the BICE where there is an extra observation
  dplyr::ungroup() 
table(mdf_avg$nobs) #double check to make sure BICE is the only one with 2 observations
summary(mdf_avg$Native.Biomass) 

#Now run the model
mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density * Species 
                  + (1|Block),
                  data = mdf_avg
)

summary(mdf.m1)
mdf.m1_res <- simulateResiduals(mdf.m1, plot = T)
#sqrt fit better than log or nothing

useData <- drop_na(mdf_avg) #need to get rid of NAs to use plotResiduals function
plotResiduals(mdf.m1_res, form= useData$Phrag_Presence)
plotResiduals(mdf.m1_res, form= useData$Density)
plotResiduals(mdf.m1_res, form= useData$Species)

Anova(mdf.m1, type = 3) 
#there is evidence of a significant 3-way interaction - other interactions not interpretable because of 3 way
emmip(mdf.m1, Species~Density|Phrag_Presence, CIs = T)
emmip(mdf.m1, Phrag_Presence~Density|Species, CIs = T)

## Phrag cover~Species * Density####
mdf <- greenhouse %>%
  filter(Species != "PHAU" , #remove PHAU control
         Phrag_Presence == "Present",  #only look at tubs with PHAU included
         Date_Cleaned == "2022-05-16") #only need the last date of sampling 

#Use this to remove all NAs so plotResiduals function works 
mdf %>% 
  select(Cover.Phrag, Species, Density, Block) %>%
  summarise_all(list(~sum(is.na(.))))
#drop observations with NA Cover.Phrag and refactor so that level is dropped
with(mdf, table(Species, useNA = "ifany")) #no observations for PHAU
mdf <- mdf %>%
  drop_na(Cover.Phrag) %>%
  mutate(Species = factor(Species)) #so it also drops those factor levels
with(mdf, table(Species, useNA = "ifany")) #note absence of Phrag level

#Run the model
mdf.m5 <- glmmTMB(Cover.Phrag ~ Species * Density
                  + (1|Block),
                  data = mdf,
                  family = beta_family 
)

summary(mdf.m5)
simulateResiduals(mdf.m5, plot = T) 
plotResiduals(mdf.m5, form= mdf$Species) 

car::Anova(mdf.m5) 
emmip(mdf.m5, Species~Density, CIs = T)

###Graph of model means for species####
emm <- emmeans(mdf.m5, pairwise ~ Species, adjust = "tukey", type = "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)

ggplot(data = data1, aes(x = reorder(Species, response), y = response)) +
  geom_point(size=2) +
  ylim(c(0, .5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Native Species Identity", y = "",
       title = "(b)") +
  geom_text(aes(label = .group,  y = response),
            nudge_y = .05, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9))

###Graph of model means for density####
emm <- emmeans(mdf.m5, pairwise ~ Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)

ggplot(data = data2, aes(x = Density, y = response, color = Density)) +
  ylim(c(0, .3)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Density", y = "Model Predicted <br> Proportional *P. australis* Cover",
       title = "(a)") +
  geom_text(aes(label = .group,  y = response),
            nudge_x = .2, color = "black") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = color2) 

##Phrag biomass ~ Species * Density####
mdf <- biomass %>%
  filter(Species != "PHAU", #remove PHAU control
         Phrag_Presence == "Present") #only include tubs with PHAU present

#run the model
mdf.m6 <- glmmTMB(sqrt(Phrag.Biomass) ~ Species * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

summary(mdf.m6)
simulateResiduals(mdf.m6, plot = T) 
plotResiduals(mdf.m6, form= mdf$Density)

Anova(mdf.m6) 
emmip(mdf.m6, Species~Density, CIs = T)

###Model means graph of species####
emm <- emmeans(mdf.m6, pairwise ~ Species, adjust = "tukey", type = "response")
data3 <- multcomp::cld(emm$emmeans, alpha = 0.05, Letters = letters)

ggplot(data = data3, aes(x = reorder(Species, response), y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Native Species Identity", y = "",
       title = "(d)") +
  geom_text(aes(label = .group,  y = response),
            nudge_y = 3.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9))

###Model means graph of density####
emm <- emmeans(mdf.m6, pairwise ~ Density, adjust = "tukey", type = "response")
data4 <- multcomp::cld(emm$emmeans, alpha = 0.05, Letters = letters)

ggplot(data = data4, aes(x = Density, y = response, color = Density)) +
  ylim(c(0, 20)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Density", y = "Model Predicted <br> *P. australis* Biomass (g)",
       title = "(c)") +
  geom_text(aes(label = .group,  y = response),
            nudge_x = .2, color = "black") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = color2)

#Individual species models to run to help with interpretation####
#Need to update the species name for each species of interest and run individually for each

##Cover of species X ~ Phrag Presence * Density####
mdf <- greenhouse %>%
  filter(Species == "SOCA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
                  )


summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

Anova(mdf.m1, type = 2) #type 2 or 3 depending on the results
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)
emmip(mdf.m1, Density ~ Phrag_Presence, CIs = T)

#emmeans(mdf.m1, pairwise ~ Density|Phrag_Presence) #if anything significant
#emmeans(mdf.m1, pairwise ~ Phrag_Presence|Density)

##Biomass of species X ~ Phrag Presence * Density#### 
mdf <- biomass %>%
  filter(Species == "PUNU", !is.na(Density))

mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

Anova(mdf.m1, type = 3) #switch between type 2 and type 3 depending on whether there are interactions 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)

#emmeans(mdf.m1, pairwise ~ Density|Phrag_Presence) #if anything significant
#emmeans(mdf.m1, pairwise ~ Phrag_Presence|Density)

##Graphing ####
###Cover ####
####BICE####
mdf <- greenhouse %>%
  filter(Species == "BICE", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m1)

emm <- emmeans(mdf.m1, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data1$Density <- factor(data1$Density,
                              levels = c("L", "H"),
                              labels = c("Low", "High"))
data1$Phrag_Presence <- factor(data1$Phrag_Presence,
                               levels = c("WO", "W"),
                               labels = c("Absent", "Present"))

data1

((BICE <- ggplot(data = data1, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_dodge(width = 0.5)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response, group = Density), 
            position = position_dodge(width = 0.5),
            color = "black", vjust = 1.5) +
  ggtitle("BICE") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("darkblue", "red3")) +
  ylim(0, 1.2)
))
####DISP####
mdf <- greenhouse %>%
  filter(Species == "DISP", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m2 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m2)

emm <- emmeans(mdf.m2, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data2$Density <- factor(data2$Density,
                        levels = c("L", "H"),
                        labels = c("Low", "High"))
data2$Phrag_Presence <- factor(data2$Phrag_Presence,
                               levels = c("WO", "W"),
                               labels = c("Absent", "Present"))
data2
str_2 <- gsub(" ", "", data2$.group)

((DISP <- ggplot(data = data2, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = 1.2, vjust = -1) +
    ggtitle("DISP") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####EPCI####
mdf <- greenhouse %>%
  filter(Species == "EPCI", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m3 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m3)

emm <- emmeans(mdf.m3, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data3 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data3

((EPCI <- ggplot(data = data3, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_dodge(width = 0.5)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response, group = Density), 
            position = position_dodge(width = 0.5),
            color = "black", vjust = 1.5) +
  ggtitle("EPCI") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("darkblue", "red3")) +
  ylim(0, 1.2)
))
####EUOC####
mdf <- greenhouse %>%
  filter(Species == "EUOC", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m4 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m4)

emm <- emmeans(mdf.m4, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data4 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data4
str_4 <- gsub(" ", "", data4$.group)

((EUOC <- ggplot(data = data4, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = 1.5) +
    ggtitle("EUOC") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####EUMA####
mdf <- greenhouse %>%
  filter(Species == "EUMA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m5 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m5)

emm <- emmeans(mdf.m5, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data5 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data5

((EUMA <- ggplot(data = data5, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = 1.5) +
    ggtitle("EUMA") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####HENU####
mdf <- greenhouse %>%
  filter(Species == "HENU", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m6 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m6)

emm <- emmeans(mdf.m6, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data6<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data6
str_6 <- gsub(" ", "", data6$.group)

((HENU <- ggplot(data = data6, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = 1.5) +
    ggtitle("HENU") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####JUAR####
mdf <- greenhouse %>%
  filter(Species == "JUAR", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m7 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m7)

emm <- emmeans(mdf.m7, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data7<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data7
str_7 <- gsub(" ", "", data7$.group)

((JUAR <- ggplot(data = data7, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = -1.5) +
    ggtitle("JUAR") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####MUAS####
mdf <- greenhouse %>%
  filter(Species == "MUAS", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m8 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m8)

emm <- emmeans(mdf.m8, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data8<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data8

((MUAS <- ggplot(data = data8, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = 1.5, vjust = -1) +
    ggtitle("MUAS") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####PUNU####
mdf <- greenhouse %>%
  filter(Species == "PUNU", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m9 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m9)

emm <- emmeans(mdf.m9, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data9<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data9
str_9 <- sub(" ", "", data9$.group)

((PUNU <- ggplot(data = data9, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = 1.5) +
    ggtitle("PUNU") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####RUMA####
mdf <- greenhouse %>%
  filter(Species == "RUMA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m10 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m10)

emm <- emmeans(mdf.m10, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data10<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data10

((RUMA <- ggplot(data = data10, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = 1.5) +
    ggtitle("RUMA") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####SCAC####
mdf <- greenhouse %>%
  filter(Species == "SCAC", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m11 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m11)

emm <- emmeans(mdf.m11, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data11<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data11
str_11 <- gsub(" ", "", data11$.group)

((SCAC <- ggplot(data = data11, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = -1.5) +
    ggtitle("SCAC") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####SCPU####
mdf <- greenhouse %>%
  filter(Species == "SCPU", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m12 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m12)

emm <- emmeans(mdf.m12, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data12 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data12

((SCPU <- ggplot(data = data12, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = -1.5) +
    ggtitle("SCPU") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####SOCA####
mdf <- greenhouse %>%
  filter(Species == "SOCA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m13 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                   + (1|Block),
                   data = mdf,
                   family = beta_family #change the family to beta
)
Anova(mdf.m13)

emm <- emmeans(mdf.m13, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data13 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data13
str_13 <- gsub(" ", "", data13$.group)

((SOCA <- ggplot(data = data13, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = 1.5) +
    ggtitle("SOCA") +
    theme(axis.title.x = ggtext::element_markdown(),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "right") +
    scale_color_manual(values = c("darkblue", "red3")) +
    ylim(0, 1.2)
))
####ALL TOGETHER####
BICE + DISP + EPCI + EUOC + EUMA + HENU + 
  JUAR + MUAS + PUNU + RUMA + SCAC + SCPU + SOCA +
  guide_area() +
  plot_layout(guides = "collect")

###Biomass####
####BICE####
mdf <- biomass %>%
  filter(Species == "BICE", !is.na(Density))

mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

emm <- emmeans(mdf.m1, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type= "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)

((BICE_b <- ggplot(data = data1, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_dodge(width = 0.5)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response, group = Density), 
            position = position_dodge(width = 0.5),
            color = "black", hjust = -.1, vjust = -.1) +
  ggtitle("BICE") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("darkblue", "red3"))+
  ylim(0, 120)
))
####DISP####
mdf <- biomass %>%
  filter(Species == "DISP", !is.na(Density))

mdf.m2 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

emm <- emmeans(mdf.m2, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)

((DISP_b <- ggplot(data = data2, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = -.1, vjust = -1) +
    ggtitle("DISP") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####EPCI####
mdf <- biomass %>%
  filter(Species == "EPCI", !is.na(Density))

mdf.m3 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

emm <- emmeans(mdf.m3, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data3 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
str_3 <- gsub(" ", "", data3$.group)

((EPCI_b <- ggplot(data = data3, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_dodge(width = 0.5)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response, group = Density), 
            position = position_dodge(width = 0.5),
            color = "black", hjust = -.1, vjust = .7) +
  ggtitle("EPCI") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("darkblue", "red3"))+
  ylim(0, 120)
))
####EUOC####
mdf <- biomass %>%
  filter(Species == "EUOC", !is.na(Density))

mdf.m4 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m4)

emm <- emmeans(mdf.m4, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data4 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data4
str_4 <- gsub(" ", "", data4$.group)

((EUOC_b <- ggplot(data = data4, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = -.7) +
    ggtitle("EUOC") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####EUMA####
mdf <- biomass %>%
  filter(Species == "EUMA", !is.na(Density))

mdf.m5 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m5)

emm <- emmeans(mdf.m5, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data5 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data5

((EUMA_b <- ggplot(data = data5, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = -.1, vjust = .7) +
    ggtitle("EUMA") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####HENU####
mdf <- biomass %>%
  filter(Species == "HENU", !is.na(Density))

mdf.m6 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m6)

emm <- emmeans(mdf.m6, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data6<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data6

((HENU_b <- ggplot(data = data6, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = -.1, vjust = 1) +
    ggtitle("HENU") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####JUAR####
mdf <- biomass %>%
  filter(Species == "JUAR", !is.na(Density))

mdf.m7 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m7)

emm <- emmeans(mdf.m7, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data7<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data7

((JUAR_b <- ggplot(data = data7, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = -.1, vjust = -.7) +
    ggtitle("JUAR") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####MUAS####
mdf <- biomass %>%
  filter(Species == "MUAS", !is.na(Density))

mdf.m8 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m8)

emm <- emmeans(mdf.m8, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data8<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data8

((MUAS_b <- ggplot(data = data8, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = .1, vjust = -.7) +
    ggtitle("MUAS") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####PUNU####
mdf <- biomass %>%
  filter(Species == "PUNU", !is.na(Density))

mdf.m9 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m9)

emm <- emmeans(mdf.m9, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data9<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data9
str_9 <- gsub(" ", "", data9$.group)

((PUNU_b <- ggplot(data = data9, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = .1, vjust = -.7) +
    ggtitle("PUNU") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####RUMA####
mdf <- biomass %>%
  filter(Species == "RUMA", !is.na(Density))

mdf.m10<- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m10)

emm <- emmeans(mdf.m10, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data10<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data10

((RUMA_b <- ggplot(data = data10, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = -.1, vjust = .7) +
    ggtitle("RUMA") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####SCAC####
mdf <- biomass %>%
  filter(Species == "SCAC", !is.na(Density))

mdf.m11 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m11)

emm <- emmeans(mdf.m11, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data11<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data11

((SCAC_b <- ggplot(data = data11, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = -.1, vjust = -.7) +
    ggtitle("SCAC") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####SCPU####
mdf <- biomass %>%
  filter(Species == "SCPU", !is.na(Density))

mdf.m12 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m12)

emm <- emmeans(mdf.m12, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data12 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data12

((SCPU_b <- ggplot(data = data12, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", hjust = -.1, vjust = -.7) +
    ggtitle("SCPU") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####SOCA####
mdf <- biomass %>%
  filter(Species == "SOCA", !is.na(Density))

mdf.m13 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m13)

emm <- emmeans(mdf.m13, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data13 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data13
dat_list <- c("a", " ab", " ab", "b")

((SOCA_b <- ggplot(data = data13, aes(x = Phrag_Presence, y = response, color = Density)) +
    geom_point(size=2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5, position = position_dodge(width = 0.5)) +
    labs(x="", y = "", color = "Density") +
    geom_text(aes(label = .group,  y = response, group = Density), 
              position = position_dodge(width = 0.5),
              color = "black", vjust = -.7) +
    ggtitle("SOCA") +
    theme(axis.title.x = ggtext::element_markdown(),
          plot.title = element_text(size = 9),
          axis.text.x = element_text(angle = 25, hjust = 0.9),
          legend.position = "left")  +
    scale_color_manual(values = c("darkblue", "red3"))+
    ylim(0, 120)
))
####ALL TOGETHER####
BICE_b + DISP_b + EPCI_b + EUOC_b + EUMA_b + HENU_b + 
  JUAR_b + MUAS_b + PUNU_b + RUMA_b + SCAC_b + SCPU_b + SOCA_b +
  guide_area() +
  plot_layout(guides = "collect") & theme(plot.margin = margin(t = 0,
                                                              r = 0,
                                                              b = 0, 
                                                              l = 0))

