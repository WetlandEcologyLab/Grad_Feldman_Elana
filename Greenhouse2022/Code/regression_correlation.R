#Call objects
load("main_dfs.RData")
func_group <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Greenhouse2022/Cleaned_Data/Groups_Final.csv")

##Graphing
library(ggplot2)
library(magrittr)
library(dplyr)
library(patchwork)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)
options(contrasts = c("contr.sum", "contr.poly"))

#Relationships between traits and phrag cover ####

#using the full dataset
final.traits <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16" & Phrag_Presence == "W") %>%
  dplyr::select(Species, Block, Density, Cover.Native, Height.Native, Cover.Phrag)

final.biomass <- biomass %>%
  filter(Phrag_Presence == "W") %>%
  dplyr::select(Species, Block, Density, Native.Biomass, Phrag.Biomass)

final <- left_join(final.traits, final.biomass, by = c("Species", "Density", "Block"))
final.all <- left_join(final, func_group, by = c("Species"))

#Regressions ####

# lm_cover_height <- lm(Cover.Phrag ~ Height.Native + Density, data = final.all)
# summary(lm_cover_height)

# lm_biomass_height <- lm(Phrag.Biomass ~ Height.Native + Density, data = final.all)
# summary(lm_biomass_height)

lm_cover_biomass <- lm(Cover.Phrag ~ Native.Biomass + Density + Group, data = final.all)
summary(lm_cover_biomass)
#Only biomass significant but Susan says to leave in things that are not 
#significant because we can then show that they weren’t significant
#In fact, we can see that there might be some trends with my groups
#can use ANOVA function to see the group level as a whole


lm_cover_cover <- lm(Cover.Phrag ~ Cover.Native + Density + Group, data = final.all)
summary(lm_cover_cover)


lm_biomass_biomass <- lm(Phrag.Biomass ~ Native.Biomass + Density + Group, data = final.all)
summary(lm_biomass_biomass)
#group seems to be significant for this one but only this one - bulrush, rush (make sense because they are 0), and grass (idk...)
emmeans(lm_biomass_biomass, pairwise~Group)
#shows the contrasts, adjusted for type 1 error - talk to Maddie about adjusting
#default is the Tukey method

#Might be worth standardizing - she thought it could be good, she will send me a paper 
#easy to do with the scale function in R

## correlation ####
#might be better if i just want to show a relationship between the two things
#scale centers and scales by default
lm_bb_correlation <- lm(scale(Phrag.Biomass) ~ scale(Native.Biomass), data = final.all)
cor(final.all$Phrag.Biomass, final.all$Native.Biomass, method = "spearman", use = "complete.obs")
cor(final.all$Phrag.Biomass, final.all$Native.Biomass, method = "pearson", use = "complete.obs")
#cor different from scale because top is scaling across the missing values but cor is using a slightly different
#dataset by removing the NAs - so really we should make a different complete dataset and then scale 
#recommended to report the spearman or the pearson, line 534 is problematic because of the NAs
#if the NAs are removed from 534 then we should get the pearson value, but easier to use the other two 
#tells us that when standardized, we get the correlation as the slope
#spearman is a pearson correlation on the ranked data

cor.test(final.all$Phrag.Biomass, final.all$Native.Biomass, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#gives you the conifidence interval, only for the pearson
#might be other functions that can give you for the spearman
#r(100) = -0.56, p = 7.052e-10

cor.test(final.all$Cover.Phrag, final.all$Native.Biomass, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(100) = -0.61, p = 1.352e-11

cor.test(final.all$Cover.Phrag, final.all$Cover.Native, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(106) = -0.60, p = 9.93e-12

cor.test(final.all$Phrag.Biomass, final.all$Cover.Native, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(108) = -0.60, p = 5.097e-12

lm_biomass_cover <- lm(Phrag.Biomass ~ Cover.Native + Density + Group, data = final.all)
summary(lm_biomass_cover)

#Graphs ####
#when graphing, we want to see whether the rule is the same for all the groups
#if not, that would suggest an interaction

b_b <- ggplot(final.all, aes(x = Native.Biomass, y = Phrag.Biomass)) +
  geom_point(aes(color = Group), size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Biomass", y = "*Phragmites* Biomass", color = "Functional Group") +
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE) +
  ylim(0, 50) +
  theme(legend.position = "none")
b_b

ggsave(filename = "lm_native-biomass_phrag-biomass.jpeg", 
       device = "jpeg")

b_c <- ggplot(final.all, aes(x = Cover.Native, y = Phrag.Biomass)) +
  geom_point(aes(color = Group), size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Cover", y = "*Phragmites* Biomass", color = "Functional Group") +
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE) +
  ylim(0, 50)+
  theme(legend.position = "none")
b_c

ggsave(filename = "lm_native-cover_phrag-biomass.jpeg", 
       device = "jpeg")


# b_h <- ggplot(traits, aes(x = final.height, y = phrag.biomass)) +
#   geom_point() +
#   labs(x = "Native Height", y = "P. australis Biomass")

c_b <- ggplot(final.all, aes(x = Native.Biomass, y = Cover.Phrag, group = Group, color = Group)) +
  geom_point( size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Biomass", y = "*Phragmites* Cover",color = "Functional Group")+
  geom_smooth(se=FALSE, fullrange = TRUE, span = 1.5)+
  theme(legend.position = "none") 
#facet_wrap(~Group, scales = "free_x")
c_b

#negative relationship between forbs and phrag but not the rushes - looks a little positive
#but probably not significant
#we can't tell with the rushes because they grew so little
#the two forbs seem to have the same relationship
#the grass we don't know because they didn't have high enough biomass, but because of interactions
#probably don't need the interaction in this model but could add it and check 

ggsave(filename = "lm_native-biomass_phrag-cover.jpeg", 
       device = "jpeg")


c_c <- ggplot(final.all, aes(x = Cover.Native, y = Cover.Phrag)) +
  geom_point(aes(color = Group), size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Cover", y = "*Phragmites* Cover", color = "Functional Group")+
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE)+
  theme(legend.position = "none")
c_c

ggsave(filename = "lm_native-cover_phrag-cover.jpeg", 
       device = "jpeg")


(c_b + c_c) / (b_b + b_c)
ggsave(filename = "phrag_native_biomass_cover_graphs_all.jpeg",
       device = "jpeg")


# Redo as GLMM ####

#need to remove the species with NAs as I did before
final.all <- final.all %>% 
  filter(Species != "JUTO" & Species != "JUGE"  & Species != "SCAM" & 
           Species != "BOMA"& Species != "SYCI")

final.all$Density <- as.factor(final.all$Density)
final.all$Group <- as.factor(final.all$Group)
final.all$Species <- as.factor(final.all$Species)
                  
##Invasive ~ Native biomass ####
mdf.m1 <- glmmTMB(sqrt(Phrag.Biomass) ~ Native.Biomass * Group * Density
                  + (1|Block),
                  data = final.all,
                  family = gaussian
)
summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= final.all$Density) 
#looks good

Anova(mdf.m1) 
#no three way interaction
#interaction between biomass & group and group & density

emmip(mdf.m1, Native.Biomass~Group, CIs = T, cov.reduce = range)
emmeans(mdf.m1, pairwise~Native.Biomass|Group, adjust = "tukey", cov.reduce = range)
#significant difference between the max and min for bulrush and perennial forb
#marginally significant for rush
#phrag biomass higher when native biomass at the max level in Bulrush and Rush
#phrag biomass lower when native biomass at the max level for Perennial Forb

emm1 <- emmeans(mdf.m1, pairwise~Native.Biomass|Group, adjust = "tukey", cov.reduce = range)
data1 <- multcomp::cld(emm1, alpha = 0.1, Letters = letters)

ggplot(data = data1, aes(x = Group, y = emmean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (emmean - SE),
                    ymax = (emmean+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model predicted mean (Phragmites biomass)") +
  geom_text(aes(label = .group,  y = emmean),
            nudge_x = 0.2) +
  facet_grid(~Native.Biomass)


emmip(mdf.m1, Density~Group, CIs = T)
emmeans(mdf.m1, pairwise~Density|Group, type = "response", adjust = "tukey")
#Only an effect of density on Bulrush
#Phrag biomass higher in the low density than the high density

emm2 <- emmeans(mdf.m1, pairwise~Density|Group, type = "response", adjust = "tukey")
data2 <- multcomp::cld(emm2, alpha = 0.1, Letters = letters)

ggplot(data = data2, aes(x = Group, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model predicted Phragmites biomass") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = 0.2)

## Native ~ Invasive biomass ####
mdf.m4 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag.Biomass * Density * Group
                  + (1|Block),
                  data = final.all,
                  family = gaussian
)
summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T) 
plotResiduals(mdf.m4, form= final.all$Density) 
#looks pretty good

Anova(mdf.m4)
#looks like Density and group are significant with an interaction between Phrag.Biomass and Group

emmip(mdf.m4, Density~Group, CIs = T)
emmeans(mdf.m4, pairwise~Density, type = "response", adjust = "tukey")
#Can I still interpret Density because not part of the two-way interaction?
#If so, high density seems to have higher native biomass, but only marginally 

emm3 <- emmeans(mdf.m4, pairwise~Density, type = "response", adjust = "tukey")
data3 <- multcomp::cld(emm3, alpha = 0.1, Letters = letters)

ggplot(data = data3, aes(x = Density, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Seeding density", y = "Model predicted native cover") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = 0.2)

emmip(mdf.m4, Phrag.Biomass~Group, CIs = T)
emmeans(mdf.m4, pairwise~Phrag.Biomass|Group, type = "response", adjust = "tukey", cov.reduce = range)
#Significant difference in native biomass between min and max phrag biomass for 
#Annual forb (p = 0.0778) and Perennial forb (p = 0.0029)

emm4 <- emmeans(mdf.m4, pairwise~Phrag.Biomass|Group, type = "response", adjust = "tukey", cov.reduce = range)
data4 <- multcomp::cld(emm4, alpha = 0.1, Letters = letters)

ggplot(data = data4, aes(x = Group, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model predicted native cover") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = 0.2) +
  facet_grid(~Phrag.Biomass)

##Invasive ~ Native cover ####
mdf.m2 <- glmmTMB(Cover.Phrag ~ Cover.Native + Density + Group
                  + (1|Block),
                  data = final.all,
                  family = beta_family(),
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)
summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= final.all$Density) 
#So this doesn't work with the interactions, however the interactions are not significant individually so maybe okay to just do +?
#model fit actually pretty good 

Anova(mdf.m2)
#Only native cover is significant

emmip(mdf.m2, Cover.Native ~ Group, CIs = T, cov.reduce= range)
emmeans(mdf.m2, pairwise~Cover.Native, cov.reduce = range)
#mean is lower at max native cover compared to min native cover
#so the phrag cover seems to decrease as native cover increases? 

##Native ~ Invasive Cover ####
mdf.m3 <- glmmTMB(Cover.Native ~ Cover.Phrag * Group + Density
                  + (1|Block),
                  data = final.all,
                  family = beta_family()
)
summary(mdf.m3)
simulateResiduals(mdf.m3, plot = T) 
#this fit is really bad but this is best out of all combinations of */+
plotResiduals(mdf.m3, form= final.all$Density) 

Anova(mdf.m3)
#Cover, Group, and Density all significant, with an interaction between cover and group
emmip(mdf.m3, Density~Group, CIs = T)
#high density seems higher, although honestly not that much higher?
emmip(mdf.m3, Cover.Phrag~Group, CIs = T, cov.reduce= range)
#looks like native cover has a bigger difference between the max and min of the phrag cover for perennial forb but maybe not the others

emmeans(mdf.m3, pairwise~Density, type = "response", adjust = "tukey")
#High density has higher mean than low density
#So native cover is higher in the high density

emmeans(mdf.m3, pairwise~Group|Cover.Phrag, cov.reduce = range)
#at the lowest phrag cover, annual forb is different from everything except perennial forb
#also perennial forb is different from bulrush and grass
#at the highest phrag cover, the only thing different is bulrush and perennial forb 
#So I guess everything is pretty similar when there is high phrag cover 
#(except rush is a little lower and perennial is a little higher)
#but at the low phrag cover, annual forb and perennial forb do pretty well 
#Annual forb does the best, perennial is the best except a little similar to rush 
