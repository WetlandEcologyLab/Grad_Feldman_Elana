#Load objects
load("main_dfs.RData")
library(tidyverse)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)
library(multcompView)
library(gridExtra)
library(patchwork)

greenhouse$Density <- as.factor(greenhouse$Density)
greenhouse$Phrag_Presence <- as.factor(greenhouse$Phrag_Presence)
greenhouse$Species <- as.factor(greenhouse$Species)

options(contrasts = c("contr.sum", "contr.poly"))

#for graphing
color1 <- c("orange", "purple4")
color2 <- c("darkblue", "red3")

#Native cover ####
mdf <- greenhouse %>%
  dplyr::filter(!is.na(Density),
         Date_Cleaned == "2022-05-16",
         Species != "JUTO" & Species != "JUGE"  & Species != "SCAM" & Species != "BOMA"& Species != "SYCI")


mdf$Cover.Native[mdf$Cover.Native == 0] <- 0.005 #make 0s a trace amount - could be half the smallest amount
mdf$Phrag_Presence <- as.character(mdf$Phrag_Presence)
mdf$Phrag_Presence[mdf$Phrag_Presence == "W"] <- "Present"
mdf$Phrag_Presence[mdf$Phrag_Presence == "WO"] <- "Absent"
mdf$Phrag_Presence <- as.factor(mdf$Phrag_Presence)


mdf$Density <- as.character(mdf$Density)
mdf$Density[mdf$Density == "H"] <- "High"
mdf$Density[mdf$Density == "L"] <- "Low"
mdf$Density <- as.factor(mdf$Density)

mdf.m1<- glmmTMB(Cover.Native ~ Phrag_Presence * Density * Species  #* for interaction
                 + (1|Block),
                 data = mdf,
                 family = beta_family)

summary(mdf.m1)
#everything is being compared to the reference species (BICE) with the default contrast but we changed it to sum
#intercept now represents the mean across all the species - re-read email about the contrasts
#the standard errors should be the same if everything is balanced

mdf.m1_res <- simulateResiduals(mdf.m1, plot = T)
#sqrt did not seem to help, residuals very bad 

Anova(mdf.m1, type = 3) 
emmip(mdf.m1, Species~Density|Phrag_Presence, CIs = T)
emmip(mdf.m1, Phrag_Presence~Density|Species, CIs = T)

##phrag presence ####
emm <- emmeans(mdf.m1, pairwise ~ Phrag_Presence, adjust = "tukey", type = "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data1
str_1 <- data1$.group
str_2 <- gsub(" ", "", str_1)

ggplot(data = data1, aes(x = Phrag_Presence, y = response * 100,
                         color = Phrag_Presence)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Presence of *Phragmites*", y = "Model Predicted Native Cover (%)",
       title = "(a)") +
  geom_text(aes(label = str_2,  y = response * 100),
            nudge_x = 0.1, color = "black") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = color1)

ggsave("native_cover_presence_model_means.jpeg")

##species by density ####
emm <- emmeans(mdf.m1, pairwise ~ Species * Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data2
str_3 <- data2$.group
str_4 <- gsub(" ", "", str_3)

ggplot(data = data2, aes(x = reorder(Species,response), y = response * 100, color = Density)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Species", y = "Model Predicted Native Cover (%)",
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

ggsave("native_cover_species_model_means.jpeg")

#Native biomass ####
table(biomass$Species)
mdf <- biomass %>%
  dplyr::filter(!is.na(Density),
         Species != "JUTO" & Species != "JUGE"  & Species != "SCAM" & Species != "BOMA" & Species != "SYCI") %>%
  dplyr::mutate(Species = factor(Species)) #need to remove the factor levels (species) that were removed
table(mdf$Species)
with(mdf, table(Species, Density, Phrag_Presence, useNA = "ifany"))
mdf_avg <- mdf %>% 
  dplyr::group_by(Block, Phrag_Presence, Density, Species) %>%
  dplyr::summarize(Native.Biomass = mean(Native.Biomass), 
            nobs = dplyr::n()) %>%#to average the BICE where there is an extra observation
  dplyr::ungroup() 
table(mdf_avg$nobs) #double check to make sure BICE is the only one with 2 observations
summary(mdf_avg$Native.Biomass) #no 0s so log shouldnt be a problem


mdf_avg$Phrag_Presence <- as.character(mdf_avg$Phrag_Presence)
mdf_avg$Phrag_Presence[mdf_avg$Phrag_Presence == "W"] <- "Present"
mdf_avg$Phrag_Presence[mdf_avg$Phrag_Presence == "WO"] <- "Absent"
mdf_avg$Phrag_Presence <- as.factor(mdf_avg$Phrag_Presence)


mdf_avg$Density <- as.character(mdf_avg$Density)
mdf_avg$Density[mdf_avg$Density == "H"] <- "High"
mdf_avg$Density[mdf_avg$Density == "L"] <- "Low"
mdf_avg$Density <- as.factor(mdf_avg$Density)

mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density * Species #* for interaction
                  + (1|Block),
                  data = mdf_avg
)

summary(mdf.m1)

mdf.m1_res <- simulateResiduals(mdf.m1, plot = T)
#log was too much, sqrt not quite enough but better than nothing

useData <- drop_na(mdf_avg) #need to get rid of NAs
plotResiduals(mdf.m1_res, form= useData$Phrag_Presence)
#warning message because Phrag_Presence is a character, but it runs
#slight difference but good enough
plotResiduals(mdf.m1_res, form= useData$Density)
#variance slightly less in low density but again good enough
plotResiduals(mdf.m1_res, form= useData$Species)
#this is messier, fewer observations for each, some boxplots bigger than others, but only 12 observations
#species have different variances that the transformation doesn't stabilize
#library(car)
Anova(mdf.m1, type = 3) 
#there is evidence of a significant 3-way interaction - other interactions not interpretable because of 3 way
emmip(mdf.m1, Species~Density|Phrag_Presence, CIs = T)
emmip(mdf.m1, Phrag_Presence~Density|Species, CIs = T)
#a lot of species that phrag presence doesn't matter for, for some a pattern of WO being on top
#probably no interaction between density and phrag if the lines are parallel
#there is an interaction between density and phrag if the lines are not parallel
#three way interaction because only some species have the density x phrag_presence interaction

#now we test for all the pieces
#we want to know which species have an interaction and which don't, and which might have one effect
#then we want to know whether the interaction pattern is the same for each species
mdf.m1.emm <- emmeans(mdf.m1, ~Species * Density * Phrag_Presence)
pairs(mdf.m1.emm, by = c("Density", "Phrag_Presence"))
pairs(mdf.m1.emm, simple = "Species")

#a good way to check the contrast coding would be how each species looks individually compared to the contrast in the full model 
#the interaction effect is the phrag difference for high - the phrag difference for low - if we can get these, then we can compare - a contrast of contrasts
#contrast vignettes and interaction vignettes 

##if I do just look at the species specific models, it should be pretty similar but not exactly because
#it is now using contrasts only for that species instead of all of them

## Three way ####
emm <- emmeans(mdf.m1, pairwise ~ Species * Density * Phrag_Presence, adjust = "tukey", type = "response")
data3 <- multcomp::cld(emm$emmeans, alpha = 0.05, Letters = letters)
data3

#I actually don't think the letters make sense for this
ggplot(data = data3, aes(x = reorder(Species,response), y = response, shape = Density)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Species", y = "Model Predicted Native Biomass (g)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown()) +
  facet_grid(~Phrag_Presence)

ggsave("native_biomass_three-way_model_means.jpeg")

# Phrag cover ####
mdf <- greenhouse %>%
  filter(Species != "PHAU", Phrag_Presence == "W",
         Date_Cleaned == "2022-05-16")


##plotResiduals wasn't working so Susan sent me this code to figure out why
##You cannot have any missing values or 0 observations
#see what variables have missing values
mdf %>% 
  select(Cover.Phrag, Species, Density, Block) %>%
  summarise_all(list(~sum(is.na(.))))

#drop observations with NA Cover.Phrag and refactor so that level is dropped
with(mdf, table(Species, useNA = "ifany")) #no observations for PHAU

mdf <- mdf %>%
  drop_na(Cover.Phrag) %>%
  mutate(Species = factor(Species))

with(mdf, table(Species, useNA = "ifany")) #note absence of Phrag level

mdf$Phrag_Presence <- as.character(mdf$Phrag_Presence)
mdf$Phrag_Presence[mdf$Phrag_Presence == "W"] <- "Present"
mdf$Phrag_Presence[mdf$Phrag_Presence == "WO"] <- "Absent"
mdf$Phrag_Presence <- as.factor(mdf$Phrag_Presence)


mdf$Density <- as.character(mdf$Density)
mdf$Density[mdf$Density == "H"] <- "High"
mdf$Density[mdf$Density == "L"] <- "Low"
mdf$Density <- as.factor(mdf$Density)

#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("Low", "High"),
                      labels = c("Low", "High")
)

#back to model
mdf.m5 <- glmmTMB(Cover.Phrag ~ Species * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)

summary(mdf.m5)

simulateResiduals(mdf.m5, plot = T) 
plotResiduals(mdf.m5, form= mdf$Species) 

#library(car)
car::Anova(mdf.m5) 
emmip(mdf.m5, Species~Density, CIs = T)

##Species ####
emm <- emmeans(mdf.m5, pairwise ~ Species, adjust = "tukey", type = "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data1

ggplot(data = data1, aes(x = reorder(Species, response), y = response * 100)) +
  geom_point(size=2) +
  ylim(c(0, 40)) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Species", y = "Model Predicted *Phragmites* Cover (%)") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_y = 4, nudge_x = .3, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown())

ggsave("phrag_cover_model_means.jpeg")

##Density ####
emm <- emmeans(mdf.m5, pairwise ~ Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data2

ggplot(data = data2, aes(x = Density, y = response * 100, color = Density)) +
  ylim(c(0, 30)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Density", y = "Model Predicted *Phragmites* Cover (%)") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = .2, color = "black") +
  theme(axis.title.y = ggtext::element_markdown()) +
  scale_color_manual(values = color2)

ggsave("phrag_cover_density_model_means.jpeg")

# Phrag biomass ####
mdf <- biomass %>%
  filter(Species != "PHAU", Phrag_Presence == "W")

mdf$Phrag_Presence <- as.character(mdf$Phrag_Presence)
mdf$Phrag_Presence[mdf$Phrag_Presence == "W"] <- "Present"
mdf$Phrag_Presence[mdf$Phrag_Presence == "WO"] <- "Absent"
mdf$Phrag_Presence <- as.factor(mdf$Phrag_Presence)


mdf$Density <- as.character(mdf$Density)
mdf$Density[mdf$Density == "H"] <- "High"
mdf$Density[mdf$Density == "L"] <- "Low"
mdf$Density <- as.factor(mdf$Density)

#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("Low", "High"),
                      labels = c("Low", "High")
)


mdf.m6 <- glmmTMB(sqrt(Phrag.Biomass) ~ Species * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

summary(mdf.m6)
#model specification probably okay because 12 obs and 3 blocks

simulateResiduals(mdf.m6, plot = T) 
plotResiduals(mdf.m6, form= mdf$Density)

#library(car)
Anova(mdf.m6) 
emmip(mdf.m6, Species~Density, CIs = T)

##Species ####
emm <- emmeans(mdf.m6, pairwise ~ Species, adjust = "tukey", type = "response")
data3 <- multcomp::cld(emm$emmeans, alpha = 0.05, Letters = letters)
data3

ggplot(data = data3, aes(x = reorder(Species, response), y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Species", y = "Model Predicted *Phragmites* Biomass (g)") +
  geom_text(aes(label = .group,  y = response),
            nudge_y = 3.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown())

ggsave("phrag_biomass_model_means.jpeg")

##Density ####
emm <- emmeans(mdf.m6, pairwise ~ Density, adjust = "tukey", type = "response")
data4 <- multcomp::cld(emm$emmeans, alpha = 0.05, Letters = letters)
data4

ggplot(data = data4, aes(x = Density, y = response)) +
  ylim(c(0, 20)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Density", y = "Model Predicted *Phragmites* Biomass (g)") +
  geom_text(aes(label = .group,  y = response),
            nudge_x = .2) +
  theme(axis.title.y = ggtext::element_markdown())

ggsave("phrag_biomass_density_model_means.jpeg")


# Putting graphs side by side ####
##Phrag Density ####
a <- ggplot(data = data2, aes(x = Density, y = response * 100, color = Density)) +
  ylim(c(0, 30)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Density", y = "Model Predicted Proportional *P.australis* Cover",
       title = "(a)") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = .2, color = "black") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = color2) 


b <- ggplot(data = data4, aes(x = Density, y = response, color = Density)) +
  ylim(c(0, 20)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Density", y = "Model Predicted *P.australis* Biomass (g)",
       title = "(b)") +
  geom_text(aes(label = .group,  y = response),
            nudge_x = .2, color = "black") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = color2)


a + b
ggsave("phrag_cover_biomass_model_means_tog.jpeg", width =800, height = 1050, units = c("px"))

## Phrag species ####
c <- ggplot(data = data1, aes(x = reorder(Species, response), y = response)) +
  geom_point(size=2) +
  ylim(c(0, .5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Native Species Identity", y = "Model Predicted <br> Proportional *P.australis* Cover",
       title = "(a)") +
  geom_text(aes(label = .group,  y = response),
            nudge_y = .05, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9))

d <- ggplot(data = data3, aes(x = reorder(Species, response), y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Native Species Identity", y = "Model Predicted <br> *P.australis* Biomass (g)",
       title = "(b)") +
  geom_text(aes(label = .group,  y = response),
            nudge_y = 3.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9))

c / d
ggsave("phrag_cover_biomass_model_means_species.jpeg")

##Native cover graphs ####
e <- ggplot(data = data1, aes(x = Phrag_Presence, y = response,
                              color = Phrag_Presence)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Presence of <br> *P.australis*", y = "Model Predicted Proportional Native Cover",
       title = "(a)") +
  ylim(0, 1) +
  geom_text(aes(label = str_2,  y = response),
            nudge_x = 0.2, color = "black", size = 3) +
  theme(axis.title.x = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = color1)


f <- ggplot(data = data2, aes(x = reorder(Species,response), y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=1)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed=1)) +
  labs(x="Native Species Identity", y = "Model Predicted Proportional Native Cover",
       title = '(b)') +
  ylim(0, 1) +
  geom_text(aes(label = str_4,
                vjust = 1.7, hjust = -.1),
            check_overlap = TRUE,
            color = "black", position = position_jitter(seed=1),
            size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9),
        legend.position = "bottom") +
  scale_color_manual(values = color2)


(e + f) + plot_layout(widths = c(1, 3))
ggsave("native_cover_model_means_both.jpeg")
