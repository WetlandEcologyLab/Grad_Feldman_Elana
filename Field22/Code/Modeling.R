load("clean_dfs.RData")
library(tidyverse)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)
library(multcompView)
library(gridExtra)
library(multcomp)
library(patchwork)
options(contrasts = c("contr.sum", "contr.poly"))

fb$Group <- as.factor(fb$Group)
fb$Density <- as.factor(fb$Density)

ul$Group <- as.factor(ul$Group)
ul$Density <- as.factor(ul$Density)

#FB ####
# These first 2 are good examples to follow

#only need the last date
mdf <- fb %>%
  filter(Date == "2022-09-16") %>%
  dplyr::select(Block, Group, Density, Date, Invasive.Cover, Native.Cover)

useData <- filter(mdf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

##FB Native ####
mdf.m1 <- glmmTMB(Native.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T) #great!
plotResiduals(mdf.m1, form= useData$Density) #must have data for every factor level

emmip(mdf.m1, Group~Density, CIs = T) #looks significant without CI but the CI shows it isn't
car::Anova(mdf.m1) #no interaction but at least one group different from another
#seems like 1 + 2 are same, 3+4 are the same, and they are different from each other, 5 probably in the middle 

emmip(mdf.m1, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m1, pairwise~Group, type = "response", adjust = 'tukey') #not significant when adjusted for the tukey test
emmeans(mdf.m1, pairwise~Group, adjust = "none") #a more liberal test shows what we expected above with the groups
#there is some suggestion that there might be some differences but the study fails to support these differences - suggestion without support

##FB native trt vs ctl ####
mdf$gd <- factor(mdf$Group:mdf$Density)
mdf.m3 <- glmmTMB(Native.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m3) #don't use this summary
simulateResiduals(mdf.m3, plot = T)  #pretty good!
plotResiduals(mdf.m3, form= mdf$gd)

emmeans(mdf.m3, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences between treatment and control 

##FB Invasive trt vs ctl ####
mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(Invasive.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #pretty good! 
plotResiduals(mdf.m2, form= mdf$gd)

#Dunnetts test - type 1 error control, comparing the control to each of 10 other means
#We have inflated type 1 error because we use the control mean 10 times
#We could use a tukeys but that would control for 55 tests and we only need 10 - we will lose all power
#Allows us to just look at control against every other treatment - only the comparisons we want
emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#4L is the closest to being different, but nothing else
#4L is the largest mean and 10C is pretty small, so that's why they are different

#so nothing is different from the control so doesn't necessarily seem like any differences are related to treatment

##FB Invasive ####
mdf.m4 <- glmmTMB(Invasive.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T)  #great!
plotResiduals(mdf.m4, form= useData$Density) 


emmip(mdf.m4, Group~Density, CIs = T) #looks significant without CI but the CI shows it isn't
car::Anova(mdf.m4) #no interaction but densities different and at least one group different

emmip(mdf.m4, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m4, pairwise~Group, type = "response") #only 4 and 5 different when adjusted for tukey
emmeans(mdf.m4, pairwise~Group, adjust = "none", type = "response") #same as with the tukey
#seems like trt 4 had higher cover overall compared to trt 5 but neither are different from the control so idk

emmeans(mdf.m4, pairwise~Density, type = "response") #not significant but almost significant when adjusted for with tukey
emmeans(mdf.m4, pairwise~Density, adjust = "none") #same p-value with the more liberal test
#also, low density seems to have higher invasive cover than high density, but again neither different from the control 

emm4a <- emmeans(mdf.m4, pairwise~Group, type = "response", adjust = 'tukey')
data4a <- multcomp::cld(emm4a, alpha = 0.1, Letters = letters)

data4a$Group <- factor(data4a$Group,
         levels = c(5, 4, 3, 2, 1),
         labels = c("Annual Forb", "Bulrush", "Grass",
                    "Rush", "Perennial Forb"))

mix <- ggplot(data = data4a, aes(x = Group, y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.4) +
  labs(x="Seed Mix", y = "Model Predicted <br> Proportional Invasive Cover",
       title = "(a)") +
  geom_text(aes(label = .group,  y = response),
            nudge_x = 0.3) +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, 1))

emm4b <- emmeans(mdf.m4, pairwise~Density, type = "response", adjust = 'tukey')
data4b <- multcomp::cld(emm4b, alpha = 0.1, Letters = letters)

data4b$Density <- factor(data4b$Density,
                         levels = c("L", "H"),
                         labels = c("Low", "High"))

density <- ggplot(data = data4b, aes(x = Density, y = response, color= Density)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model Predicted <br> Proportional Invasive Cover",
       title = "(b)") +
  scale_color_manual(values = c("darkblue", "red3"))+
  geom_text(aes(label = .group,  y = response),
            nudge_x = 0.2, color = "black") +
  theme(axis.title.y = ggtext::element_markdown(),
        legend.position = "blank",
        plot.title = element_text(size = 9))+
  coord_cartesian(ylim = c(0, 1))


mix + density + plot_layout(width = c(2, 1))
ggsave("model_means_fb_invasive_mix_density.jpeg")

#UL ####
mdf1 <- ul %>%
  filter(Date == "2022-09-16")

useData <- filter(mdf1, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

##UL Invasive trt vs ctl ####
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf.m5 <- glmmTMB(Invasive.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf1,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m5) #don't use this summary 
simulateResiduals(mdf.m5, plot = T)  #pretty good!
plotResiduals(mdf.m5, form= mdf$gd)

emmeans(mdf.m5, specs = trt.vs.ctrlk~gd,ref = 3) #reference group is the third option (10:C)
#no significant differences, just a slight different with 4H


##UL Invasive ####
mdf.m6 <- glmmTMB(Invasive.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m6)
simulateResiduals(mdf.m6, plot = T) 
plotResiduals(mdf.m6, form= useData$Density)

emmip(mdf.m6, Group~Density, CIs = T) 
car::Anova(mdf.m6) #nothing significant

## UL Native ####
#compare a model where you nudge the 100s down to one where you just do a log normal and see if there is a difference

###Log normal ####
#this log normal one is good enough and shows nothing significant 
mdf.m7 <- glmmTMB(log(Native.Cover) ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)


summary(mdf.m7)
simulateResiduals(mdf.m7, plot = T) #pretty good!
plotResiduals(mdf.m7, form= useData$Density)
car::Anova(mdf.m7) #nothing significant

emmip(mdf.m7, Group~Density, CIs = T)

###Beta with nudged values ####
useData$Native.Cover[useData$Native.Cover >= 1] <- .999
mdf.m8 <- glmmTMB(Native.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m8)
simulateResiduals(mdf.m8, plot = T) #residuals look a little worse so I am inclined to use the log normal
plotResiduals(mdf.m8, form= useData$Density)

emmip(mdf.m8, Group~Density, CIs = T)
car::Anova(mdf.m8) #however, this does show a significant interaction

emmip(mdf.m8, Group~Density, CIs = T, type = "response") #shows on the level of the response

mdf.m8.emm <- emmeans(mdf.m8, ~Group * Density)
pairs(mdf.m8.emm, simple = "Density") #looks like Group 3 has the interaction, higher native cover in the low density

##UL Native trt vs ctl ####
### Dunnetts gaussian ####
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf.m9 <- glmmTMB(log(Native.Cover) ~ gd #* for interaction
                  + (1|Block),
                  data = mdf1,
                  family = gaussian,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m9) #don't use this summary 
simulateResiduals(mdf.m9, plot = T) #pretty good!
plotResiduals(mdf.m9, form= mdf1$gd)

emmeans(mdf.m9, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences, 5H and 2H seem kind of different 

###Dunnetts beta ####
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf1$Native.Cover[mdf1$Native.Cover >= 1] <- .999

mdf.m10 <- glmmTMB(Native.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf1,
                  family = beta_family,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m10) #don't use this summary 
simulateResiduals(mdf.m10, plot = T)#residuals look pretty bad, don't use
plotResiduals(mdf.m10, form= mdf$gd)

emmeans(mdf.m10, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences

# Modeling only seeded species ####

##Fb ####

### Perennial forbs ####
fb_pf <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, EUOC, EUMA) %>% 
  rowwise() %>% 
  mutate(cover_pf = sum(EUOC, EUMA))

#nudge 0s into a trace amount
fb_pf$cover_pf[fb_pf$cover_pf == 0] <- 0.0025 #one half the smallest amount recorded

####trts####
useData <- filter(fb_pf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_pf ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #terrible with both beta and log normal
plotResiduals(mdf.m1, form= useData$Density)#not good - I think not enough data

#can't get to fit, don't use

####Dunnetts####
fb_pf$gd <- factor(fb_pf$Group:fb_pf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_pf ~ gd #* for interaction
                  + (1|Block),
                  data = fb_pf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #not great
plotResiduals(mdf.m2, form= fb_pf$gd) #fine

#can't get to fit, don't use

### Annual forbs ####
fb_af <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, SYCI, BICE, RUMA, BIFR) %>% 
  rowwise() %>% 
  mutate(cover_af = sum(SYCI, BICE, RUMA, BIFR))

#nudge 0s into a trace amount
fb_af$cover_af[fb_af$cover_af == 0] <- 0.0025 #one half the smallest amount recorded

####trts####
useData <- filter(fb_af, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_af ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #terrible both ways
plotResiduals(mdf.m1, form= useData$Density) 

#cant get to fit, don't use

####Dunnetts####
fb_af$gd <- factor(fb_af$Group:fb_af$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_af ~ gd #* for interaction
                  + (1|Block),
                  data = fb_af,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #not good, better with beta
plotResiduals(mdf.m2, form= fb_pf$gd) 
#cant get to fit, don't use

### Bulrushes ####
fb_b <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, BOMA, SCAC, SCAM) %>% 
  rowwise() %>% 
  mutate(cover_b = sum(BOMA, SCAC, SCAM))

#nudge 0s into a trace amount
fb_b$cover_b[fb_b$cover_b == 0] <- 0.0025 #one half the smallest amount recorded

####trts####
useData <- filter(fb_b, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_b ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #great!
plotResiduals(mdf.m1, form= useData$Density) 

emmip(mdf.m1, Group~Density, CIs = T) 
car::Anova(mdf.m1) #at least one group is significantly different from another

emmip(mdf.m1, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m1, pairwise~Group, type = "response")
#tukey test shows us a significant difference between Group1/4, Group2/4, Group3/4, and Group4/5
#So looks like group 4 had a lot more cover than the others groups
#This is actually really important because that was my bulrush group!!

emm1a <- emmeans(mdf.m1, pairwise~Group, type = "response", adjust = 'tukey')
data1a <- multcomp::cld(emm1a, alpha = 0.1, Letters = letters)

data1a$Group <- factor(data1a$Group,
                       levels = c(5, 4, 3, 2, 1),
                       labels = c("Annual Forb", "Bulrush", "Grass",
                                  "Rush", "Perennial Forb"))

a <- ggplot(data = data1a, aes(x = Group, y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model Predicted <br> Proportional Bulrush Cover",
       title = "(a)") +
  geom_text(aes(label = .group,  y = response),
            color = "black",
            hjust = 0.05) +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, 1))

#ggsave("model_means_bulrush_fb.jpeg")

####Dunnetts####
fb_b$gd <- factor(fb_b$Group:fb_b$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_b ~ gd #* for interaction
                  + (1|Block),
                  data = fb_b,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #looks fine!
plotResiduals(mdf.m2, form= fb_b$gd) 


emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#the only one significantly higher than the control is 4H and 4L!!!

emm2a <- emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
data2a <- multcomp::cld(emm2a, alpha = 0.1, Letters = letters)

data2a$gd <- factor(data2a$gd,
                       levels = c("10:C","5:H", "5:L", "4:H", "4:L", 
                                  "3:H", "3:L", "2:H", "2:L", 
                                  "1:H", "1:L"),
                       labels = c("Control","Annual Forb High", "Annual Forb Low", 
                                  "Bulrush High", "Bulrush Low",
                                  "Grass High", "Grass Low",
                                  "Rush High", "Rush Low", 
                                  "Perennial Forb High", "Perennial Forb Low"))

b <- ggplot(data = data2a, aes(x = gd, y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "",
       title = "(b)") +
  geom_text(aes(label = .group,  y = response),
            color = "black",
            hjust = 0.05, vjust = .1) +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, 1))

#ggsave("dunnetts_fb_bulrush.jpeg")

a + b +plot_layout(width = c(1, 2))
ggsave("fb_bulrush_both.jpeg")

### Grasses ####
fb_g <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, MUAS, DISP) %>% 
  rowwise() %>% 
  mutate(cover_g = sum(MUAS, DISP))

#nudge 0s into a trace amount
fb_g$cover_g[fb_g$cover_g == 0] <- 0.0025

####trts####
useData <- filter(fb_g, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(log(cover_g) ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #fine - better with log normal
plotResiduals(mdf.m1, form= useData$Density) #still not great

emmip(mdf.m1, Group~Density, CIs = T) 
car::Anova(mdf.m1) #interaction between group and density!

emmip(mdf.m1, Group~Density, CIs = T, type = "response") #shows on the level of the response
#from this, looks like the interaction with group 3 H vs L

emmeans(mdf.m1, pairwise~Group*Density, type = "response")
#kind of weird results, only things that are marginally significant
#2H / 3H, 3H / 4H, 3H / 5H, 3H / 5L - 3H being higher than the others
#a lot of grass grew in 3H but not 3L, so there's an interaction
#shows that we need high density for grasses?

emm1a <- emmeans(mdf.m1, pairwise~Group*Density, type = "response", adjust = 'tukey')
data1a <- multcomp::cld(emm1a, alpha = 0.1, Letters = letters)

data1a$Group <- factor(data1a$Group,
                       levels = c(5, 4, 3, 2, 1),
                       labels = c("Annual Forb", "Bulrush", "Grass",
                                  "Rush", "Perennial Forb"))
data1a$Density <- factor(data1a$Density,
                         levels = c("L", "H"),
                         labels = c("Low", "High"))

ggplot(data = data1a, aes(x = Group, y = response, color = Density)) +
  geom_point(size=2, position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5,
                position = position_dodge(width = .5)) +
  labs(x="Seed Mix", y = "Model Predicted <br> Proportional Grass Cover") +
  geom_text(aes(label = .group,  y = response, group = Density),
            position = position_dodge(width = .5),
           color = "black",
             vjust = -1) +
  scale_color_manual(values = c("darkblue", "red3")) +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  coord_cartesian(ylim = c(0,1))

ggsave("model_means_grass_fb.jpeg")

####Dunnetts####
fb_g$gd <- factor(fb_g$Group:fb_g$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(log(cover_g) ~ gd #* for interaction
                  + (1|Block),
                  data = fb_g,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #fine - better with log normal
plotResiduals(mdf.m2, form= fb_g$gd) #looks fine!


emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#none of them significantly different from control, sad
#but 3H definitely the closest

##Ul ####

### Annual forbs ####
ul_af <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, EUOC) %>% 
  mutate(cover_af = EUOC)

#nudge 0s into a trace amount
ul_af$cover_af[ul_af$cover_af == 0] <- 0.0025 #one half the smallest amount recorded

####trts####
useData <- filter(ul_af, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_af ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #fine
plotResiduals(mdf.m1, form= useData$Density)#fine

emmip(mdf.m1, Group~Density, CIs = T) 
car::Anova(mdf.m1) #at least one group significantly different

emmeans(mdf.m1, pairwise~Group, type = "response")
#Group1/3, Group3/4, Group3/5 - with three much higher than the others
#doesn't make sense because isn't my forb group
#maybe the grass helped the forbs grow somehow?

emm1a <- emmeans(mdf.m1, pairwise~Group, type = "response", adjust = 'tukey')
data1a <- multcomp::cld(emm1a, alpha = 0.1, Letters = letters)

data1a$Group <- factor(data1a$Group,
                       levels = c(5, 4, 3, 2, 1),
                       labels = c("Annual Forb", "Bulrush", "Grass",
                                  "Rush", "Perennial Forb"))

ggplot(data = data1a, aes(x = Group, y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model Predicted <br> Proportional Annual Forb Cover") +
  geom_text(aes(label = .group,  y = response),
            color = "black",
            hjust = 0.05) +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 45, hjust = 0.9))

ggsave("model_means_annual_ul.jpeg")

####Dunnetts####
ul_af$gd <- factor(ul_af$Group:ul_af$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(log(cover_af) ~ gd #* for interaction
                  + (1|Block),
                  data = ul_af,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #much better log normal
plotResiduals(mdf.m2, form= ul_af$gd)


emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#differences between 2L, 3H, 3L, 4L
#this doesn't make any sense because those aren't my forbs, so must just be random in the field
#with C much lower

### Perennial forbs ####
ul_pf <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, SYCI, BICE, RUMA) %>% 
  rowwise() %>% 
  mutate(cover_pf = sum(SYCI, BICE, RUMA))

#nudge 0s into a trace amount
ul_pf$cover_pf[ul_pf$cover_pf == 0] <- 0.0025 #one half the smallest amount recorded

####trts####
useData <- filter(ul_pf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_pf ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #fine
plotResiduals(mdf.m1, form= useData$Density) 

emmip(mdf.m1, Group~Density, CIs = T) 
car::Anova(mdf.m1) #at least one group significant

emmip(mdf.m1, Group~Density, CIs = T, type = "response")
#kind of just looks like everything was 0 except 5H was higher 

emmeans(mdf.m1, pairwise~Group, type = "response")
#only between 3/5 - which doesn't make any sense - with 5 higher 

emm1a <- emmeans(mdf.m1, pairwise~Group, type = "response", adjust = 'tukey')
data1a <- multcomp::cld(emm1a, alpha = 0.1, Letters = letters)

data1a$Group <- factor(data1a$Group,
                       levels = c(5, 4, 3, 2, 1),
                       labels = c("Annual Forb", "Bulrush", "Grass",
                                  "Rush", "Perennial Forb"))

ggplot(data = data1a, aes(x = Group, y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model Predicted <br> Proportional Perennial Forb Cover") +
  geom_text(aes(label = .group,  y = response),
            color = "black",
            hjust = 0.05) +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  coord_cartesian(ylim = c(0, 1))

ggsave("model_means_perennial_ul.jpeg")

####Dunnetts####
ul_pf$gd <- factor(ul_pf$Group:ul_pf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_pf ~ gd #* for interaction
                  + (1|Block),
                  data = ul_pf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #fine
plotResiduals(mdf.m2, form= ul_pf$gd) 


emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#no significant differences

### Bulrushes ####
ul_b <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, BOMA, SCAC, SCAM) %>% 
  rowwise() %>% 
  mutate(cover_b = sum(BOMA, SCAC, SCAM))

#nudge 0s into a trace amount
ul_b$cover_b[ul_b$cover_b == 0] <- 0.0025 #one half the smallest amount recorded

####trts####
useData <- filter(ul_b, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_b ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #fine
plotResiduals(mdf.m1, form= useData$Density) 

emmip(mdf.m1, Group~Density, CIs = T) 
car::Anova(mdf.m1) #at least one group is significantly different from another

emmip(mdf.m1, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m1, pairwise~Group, type = "response")
#only between 3/4 - with 3 much lower, don't know what that means

emm1a <- emmeans(mdf.m1, pairwise~Group, type = "response", adjust = 'tukey')
data1a <- multcomp::cld(emm1a, alpha = 0.1, Letters = letters)

data1a$Group <- factor(data1a$Group,
                       levels = c(5, 4, 3, 2, 1),
                       labels = c("Annual Forb", "Bulrush", "Grass",
                                  "Rush", "Perennial Forb"))

ggplot(data = data1a, aes(x = Group, y = response)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model Predicted <br> Proportional Bulrush Cover") +
  geom_text(aes(label = .group,  y = response),
            color = "black",
            hjust = 0.05) +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  coord_cartesian(ylim = c(0, 1))

ggsave("model_means_bulrush_ul.jpeg", width = 3, height = 4, units = "in")

####Dunnetts####
ul_b$gd <- factor(ul_b$Group:ul_b$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_b ~ gd #* for interaction
                  + (1|Block),
                  data = ul_b,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #looks fine!
plotResiduals(mdf.m2, form= ul_b$gd) 


emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#no significant differences

### Grasses ####
ul_g <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, DISP) %>% 
  rowwise() %>% 
  mutate(cover_g = DISP)

#nudge 0s into a trace amount
ul_g$cover_g[ul_g$cover_g == 0] <- 0.0025

####trts####
useData <- filter(ul_g, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_g ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #terrible with both 
plotResiduals(mdf.m1, form= useData$Density)

#can't get to fit, don't use

####Dunnetts####
ul_g$gd <- factor(ul_g$Group:ul_g$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_g ~ gd #* for interaction
                  + (1|Block),
                  data = ul_g,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #terrible with both
plotResiduals(mdf.m2, form= ul_g$gd) 

#can't get to fit, don't use

mdf2 <- dplyr::select(mdf1, c(Plot, EUOC))

mdf2 %>% 
  ggplot(aes(x = Plot, y = EUOC)) + #x is plot, y is cover
  stat_summary(aes(group = Plot), #calculate means of the total cover
               fun = mean, geom = "bar", size = 1) +
  stat_summary(aes(group = Plot, width = 0), #calculate error bars
               fun.data = mean_se, geom = "errorbar", size = .5) 

# 2023 ####
fb23$Group <- as.factor(fb23$Group)
fb23$Density <- as.factor(fb23$Density)

#only need the last date
mdf <- fb23 %>%
  filter(Date == "2023-09-11") %>%
  dplyr::select(Block, Group, Density, Date, Invasive.Cover, Native.Cover)

useData <- filter(mdf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

##FB Native ####
mdf.m1 <- glmmTMB(Native.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T) #not good but better than log normal
plotResiduals(mdf.m1, form= useData$Density) #fine

emmip(mdf.m1, Group~Density, CIs = T) #nothing looks significant
car::Anova(mdf.m1) #nothing significant
#no effect of the presence of natives

##FB native trt vs ctl ####
mdf$gd <- factor(mdf$Group:mdf$Density)
mdf.m3 <- glmmTMB(log(Native.Cover) ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m3) #don't use this summary
simulateResiduals(mdf.m3, plot = T)  #log normal much better
plotResiduals(mdf.m3, form= mdf$gd)

emmeans(mdf.m3, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences between treatment and control 

##FB Invasive ####
mdf.m4 <- glmmTMB(Invasive.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T)  #great!
plotResiduals(mdf.m4, form= useData$Density) 


emmip(mdf.m4, Group~Density, CIs = T)
car::Anova(mdf.m4) #nothing significant


##FB Invasive trt vs ctl ####
mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(Invasive.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #pretty good! 
plotResiduals(mdf.m2, form= mdf$gd)

emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #nothing significant
#nothing different from the control

##2023 seeded ####

### Annual forbs ####
fb23_af <- fb23 %>% 
  filter(Date == "2023-09-11") %>% 
  dplyr::select(Block:Density, RUMA) %>% 
  rowwise() %>% 
  mutate(cover_af = sum(RUMA))

####trts####
useData <- filter(fb23_af, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_af ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #terrible both ways
plotResiduals(mdf.m1, form= useData$Density) 

#cant get to fit, don't use

####Dunnetts####
fb23_af$gd <- factor(fb23_af$Group:fb23_af$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_af ~ gd #* for interaction
                  + (1|Block),
                  data = fb23_af,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #terrible both ways
plotResiduals(mdf.m2, form= fb23_af$gd) 
#cant get to fit, don't use

### Bulrushes ####
fb23_b <- fb23 %>% 
  filter(Date == "2023-09-11") %>% 
  dplyr::select(Block:Density, BOMA, SCAC, SCAM) %>% 
  rowwise() %>% 
  mutate(cover_b = sum(BOMA, SCAC, SCAM))

####trts####
useData <- filter(fb23_b, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_b ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  #bad both ways
plotResiduals(mdf.m1, form= useData$Density) 

#cannot get to fit, don't use

####Dunnetts####
fb23_b$gd <- factor(fb23_b$Group:fb23_b$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_b ~ gd #* for interaction
                  + (1|Block),
                  data = fb23_b,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #terrible both ways
plotResiduals(mdf.m2, form= fb_b$gd) 

#cant get to fit, don't use

## Grasses ####
fb23_g <- fb23 %>% 
  filter(Date == "2023-09-11") %>% 
  dplyr::select(Block:Density, DISP) %>% 
  rowwise() %>% 
  mutate(cover_g = sum(DISP))

####trts####
useData <- filter(fb23_g, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_g ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T)  ##bad both ways
plotResiduals(mdf.m1, form= useData$Density) #still not great

#cannot fit, do not use

####Dunnetts####
fb23_g$gd <- factor(fb23_g$Group:fb23_g$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_g ~ gd #* for interaction
                  + (1|Block),
                  data = fb23_g,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) #terrible both ways 
plotResiduals(mdf.m2, form= fb23_g$gd) 

#does not fit, do not use