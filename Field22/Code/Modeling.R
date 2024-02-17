#Load data and packages
load("clean_dfs.RData")

#All package version saved in renv.lock 
#renv::init, renv::restore
library(tidyverse)
library(glmmTMB) 
library(DHARMa)
library(emmeans)
library(car)
library(multcompView)
library(gridExtra)
library(multcomp)
library(patchwork)

#Run these options in case we decide to do a type-iii ANOVA
options(contrasts = c("contr.sum", "contr.poly"))

#make everything a factor that needs to be
glimpse(fb)
glimpse(ul)
fb$Group <- as.factor(fb$Group)
fb$Density <- as.factor(fb$Density)

ul$Group <- as.factor(ul$Group)
ul$Density <- as.factor(ul$Density)

#Farmington Bay 2022 Models####
##Models of total native and invasive cover ####
#only need the last date
mdf <- fb %>%
  filter(Date == "2022-09-16") %>%
  dplyr::select(Block, Group, Density, Date, Invasive.Cover, Native.Cover)

useData <- filter(mdf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

###FB Native Cover ~ Functional Group x Density####
mdf.m1 <- glmmTMB(Native.Cover ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density)

car::Anova(mdf.m1) #no interaction but at least one group different from another

emmip(mdf.m1, Group~Density, CIs = T, type = "response")

emmeans(mdf.m1, pairwise~Group, type = "response", adjust = 'tukey') #not significant when adjusted for the tukey test

###FB Native Cover Dunnett's Test ####
mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m3 <- glmmTMB(Native.Cover ~ gd 
                  + (1|Block),
                  data = mdf,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m3) 
simulateResiduals(mdf.m3, plot = T)  
plotResiduals(mdf.m3, form= mdf$gd)

emmeans(mdf.m3, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences between treatment and control 

###FB Invasive Cover ~ Functional Group * Density ####
mdf.m4 <- glmmTMB(Invasive.Cover ~ Group * Density
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T) 
plotResiduals(mdf.m4, form= useData$Density) 


car::Anova(mdf.m4) #no interaction but densities different and at least one group different

emmip(mdf.m4, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m4, pairwise~Group, type = "response") #only 4 and 5 different when adjusted for tukey
emmeans(mdf.m4, pairwise~Density, type = "response") #marginally significant when adjusted for with tukey

####Graph of the model means####
#Graph of the functional groups
emm4a <- emmeans(mdf.m4, pairwise~Group, type = "response", adjust = 'tukey')
data4a <- multcomp::cld(emm4a, alpha = 0.1, Letters = letters)

#re-factor everything to help with graphing
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

#graph of the densities
emm4b <- emmeans(mdf.m4, pairwise~Density, type = "response", adjust = 'tukey')
data4b <- multcomp::cld(emm4b, alpha = 0.1, Letters = letters)

#re-factor everything to help with the graphing 
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

#make a compoint figure 
mix + density + plot_layout(width = c(2, 1))
#ggsave("model_means_fb_invasive_mix_density.jpeg")

###FB Invasive Cover Dunnett's Test ####
mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(Invasive.Cover ~ gd 
                  + (1|Block),
                  data = mdf,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) 
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= mdf$gd)

emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#nothing significant

##Models of only seeded species####

### Perennial forbs ####
#select only the perennial forbs and only the last date
fb_pf <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, EUOC, EUMA) %>% 
  rowwise() %>% 
  mutate(cover_pf = sum(EUOC, EUMA))

#nudge 0s into a trace amount so we can use the beta distribution
fb_pf$cover_pf[fb_pf$cover_pf == 0] <- 0.0025 #one half the smallest amount recorded

####Perennial forbs ~ Functional Group * Density####
useData <- filter(fb_pf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_pf ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density)

#Could not get to fit with either the beta or the log normal - Do not use

####Dunnett's test####
fb_pf$gd <- factor(fb_pf$Group:fb_pf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_pf ~ gd 
                  + (1|Block),
                  data = fb_pf,
                  family = beta_family,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= fb_pf$gd) 

#Could not get to fit - Do not use

### Annual forbs ####
#select only the annual forbs and only the last sampling date
fb_af <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, SYCI, BICE, RUMA, BIFR) %>% 
  rowwise() %>% 
  mutate(cover_af = sum(SYCI, BICE, RUMA, BIFR))

#nudge 0s into a trace amount so we can use the beta distribution
fb_af$cover_af[fb_af$cover_af == 0] <- 0.0025 #one half the smallest amount recorded

####Annual forbs ~ Functional Group * Density####
useData <- filter(fb_af, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_af ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T)  
plotResiduals(mdf.m1, form= useData$Density) 

#Could not get to fit model - Do not use

####Dunnett's test####
fb_af$gd <- factor(fb_af$Group:fb_af$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_af ~ gd
                  + (1|Block),
                  data = fb_af,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) 
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= fb_pf$gd) 
#Could not get to fit - Do not use

### Bulrushes ####
#select only the bulrushes and the last sampling date
fb_b <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, BOMA, SCAC, SCAM) %>% 
  rowwise() %>% 
  mutate(cover_b = sum(BOMA, SCAC, SCAM))

#nudge 0s into a trace amount so we can use the beta distribution
fb_b$cover_b[fb_b$cover_b == 0] <- 0.0025 #one half the smallest amount recorded

####Bulrushes ~ Functional Group * Density####
useData <- filter(fb_b, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_b ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density) 


car::Anova(mdf.m1) #at least one group is significantly different from another

emmip(mdf.m1, Group~Density, CIs = T, type = "response") 

emmeans(mdf.m1, pairwise~Group, type = "response")
#tukey test shows us a significant difference between Group1/4, Group2/4, Group3/4, and Group4/5

#####Graph of the model means####
emm1a <- emmeans(mdf.m1, pairwise~Group, type = "response", adjust = 'tukey')
data1a <- multcomp::cld(emm1a, alpha = 0.1, Letters = letters)

#re-factor everything to help with graphing
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

####Dunnett's Test####
fb_b$gd <- factor(fb_b$Group:fb_b$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_b ~ gd 
                  + (1|Block),
                  data = fb_b,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= fb_b$gd) 


emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#the only one significantly higher than the control is 4H and 4L

#####Graph of the model means####
emm2a <- emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
data2a <- multcomp::cld(emm2a, alpha = 0.1, Letters = letters)

#re-factor everything to help with graphing
data2a$gd <- factor(data2a$gd,
                    levels = c("10:C","5:L", "5:H", "4:L", "4:H", 
                               "3:L", "3:H", "2:L", "2:H", 
                               "1:L", "1:H"),
                    labels = c("Control","Annual Forb Low", "Annual Forb High", 
                               "Bulrush Low", "Bulrush High",
                               "Grass Low", "Grass High",
                               "Rush Low", "Rush High", 
                               "Perennial Forb Low", "Perennial Forb High"))

data2a$density <- c("Control", "Low", "High", "High", 
                    "Low", "Low", "High", "High", "Low",
                    "Low", "High")

b <- ggplot(data = data2a, aes(x = gd, y = response, color = density)) +
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
        plot.title = element_text(size = 9),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(values = c("Control" = "black",
                                "Low" = "darkblue", 
                                "High" = "red3"))

a + b +plot_layout(width = c(1, 2))

### Grasses ####
#select only the bulrushes and the last sampling date
fb_g <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, MUAS, DISP) %>% 
  rowwise() %>% 
  mutate(cover_g = sum(MUAS, DISP))

#nudge 0s into a trace amount so we can use the beta distribution
fb_g$cover_g[fb_g$cover_g == 0] <- 0.0025

####Grasses ~ Functional Group * Density####
useData <- filter(fb_g, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(log(cover_g) ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = gaussian, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T)  
plotResiduals(mdf.m1, form= useData$Density) 
#log normal distribution fit better than the beta distribution


car::Anova(mdf.m1, type = 3) #interaction between group and density

emmip(mdf.m1, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m1, pairwise~Group*Density, type = "response")
#2H / 3H, 3H / 4H, 3H / 5H, 3H / 5L - 3H being higher than the others, marginally significant

####Dunnett's Test####
fb_g$gd <- factor(fb_g$Group:fb_g$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(log(cover_g) ~ gd 
                  + (1|Block),
                  data = fb_g,
                  family = gaussian, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) 
simulateResiduals(mdf.m2, plot = T)
plotResiduals(mdf.m2, form= fb_g$gd)
#log normal fits better than the beta

emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#none of them significantly different from control

#Utah Lake 2022 Models####
##Models of total native and invasive cover ####
#only need data from the final date of sampling
mdf1 <- ul %>%
  filter(Date == "2022-09-16")

useData <- filter(mdf1, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)



###UL Native Cover ~ Functional Group * Density####
#I tested both the log normal and the beta, and log normal fit better
mdf.m7 <- glmmTMB(log(Native.Cover) ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = gaussian, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)


summary(mdf.m7)
simulateResiduals(mdf.m7, plot = T)
plotResiduals(mdf.m7, form= useData$Density)
car::Anova(mdf.m7) #nothing significant

emmip(mdf.m7, Group~Density, CIs = T)

###UL Native Cover Dunnett's Test ####
####Dunnetts gaussian ####
#I tried the log normal and the beta but the log normal fit better
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf.m9 <- glmmTMB(log(Native.Cover) ~ gd #* for interaction
                  + (1|Block),
                  data = mdf1,
                  family = gaussian,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m9) 
simulateResiduals(mdf.m9, plot = T) 
plotResiduals(mdf.m9, form= mdf1$gd)

emmeans(mdf.m9, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences

###UL Invasive Cover ~ Functional Group * Density ####
mdf.m6 <- glmmTMB(Invasive.Cover ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m6)
simulateResiduals(mdf.m6, plot = T) 
plotResiduals(mdf.m6, form= useData$Density)

emmip(mdf.m6, Group~Density, CIs = T) 
car::Anova(mdf.m6) #nothing significant

###UL Invasive Dunnett's Test ####
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf.m5 <- glmmTMB(Invasive.Cover ~ gd 
                  + (1|Block),
                  data = mdf1,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m5) 
simulateResiduals(mdf.m5, plot = T)  
plotResiduals(mdf.m5, form= mdf$gd)

emmeans(mdf.m5, specs = trt.vs.ctrlk~gd,ref = 3) #reference group is the third option (10:C)
#no significant differences

##Models of only seeded species####
### Annual forbs ####
#only select annual forbs and the last date of sampling
ul_af <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, EUOC) %>% 
  mutate(cover_af = EUOC)

#nudge 0s into a trace amount so we can use the beta
ul_af$cover_af[ul_af$cover_af == 0] <- 0.0025 #one half the smallest amount recorded

####Annual forbs ~ Functional Group * Density####
useData <- filter(ul_af, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_af ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T)
plotResiduals(mdf.m1, form= useData$Density)#fine

emmip(mdf.m1, Group~Density, CIs = T) 
car::Anova(mdf.m1) #at least one group significantly different

emmeans(mdf.m1, pairwise~Group, type = "response")
#Group1/3, Group3/4, Group3/5 - with three much higher than the others
#These results do not make sense with the treatments. Acknowledge in results

####Dunnett's Test####
ul_af$gd <- factor(ul_af$Group:ul_af$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(log(cover_af) ~ gd 
                  + (1|Block),
                  data = ul_af,
                  family = gaussian, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) 
simulateResiduals(mdf.m2, plot = T)
plotResiduals(mdf.m2, form= ul_af$gd)
#log normal fits better than the beta

emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#differences between 2L, 3H, 3L, 4L
#This does not make sense with the treatments. Acknowledge in the results

### Perennial forbs ####
#only select perennial forbs and last date of sampling
ul_pf <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, SYCI, BICE, RUMA) %>% 
  rowwise() %>% 
  mutate(cover_pf = sum(SYCI, BICE, RUMA))

#nudge 0s into a trace amount so we can use the beta
ul_pf$cover_pf[ul_pf$cover_pf == 0] <- 0.0025 #one half the smallest amount recorded

####Perennial forbs ~ Functional Group * Density####
useData <- filter(ul_pf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_pf ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density) 


car::Anova(mdf.m1) #at least one group different

emmip(mdf.m1, Group~Density, CIs = T, type = "response")

emmeans(mdf.m1, pairwise~Group, type = "response")
#only between 3/5
#Results do not make sense with the treatments. Acknowledge in results

####Dunnett's test####
ul_pf$gd <- factor(ul_pf$Group:ul_pf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_pf ~ gd 
                  + (1|Block),
                  data = ul_pf,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= ul_pf$gd) 

emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#no significant differences

### Bulrushes ####
#only select bulrushes and the last date of sampling
ul_b <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, BOMA, SCAC, SCAM) %>% 
  rowwise() %>% 
  mutate(cover_b = sum(BOMA, SCAC, SCAM))

#nudge 0s into a trace amount so we can use the beta
ul_b$cover_b[ul_b$cover_b == 0] <- 0.0025 #one half the smallest amount recorded

####Bulrushes ~ Functional Group * Density####
useData <- filter(ul_b, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_b ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T)  
plotResiduals(mdf.m1, form= useData$Density) 


car::Anova(mdf.m1) #at least one group is significantly different from another

emmip(mdf.m1, Group~Density, CIs = T, type = "response")

emmeans(mdf.m1, pairwise~Group, type = "response")
#only between 3/4 - with 3 much lower

####Dunnett's test####
ul_b$gd <- factor(ul_b$Group:ul_b$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_b ~ gd 
                  + (1|Block),
                  data = ul_b,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) 
simulateResiduals(mdf.m2, plot = T)
plotResiduals(mdf.m2, form= ul_b$gd) 


emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") 
#no significant differences

### Grasses ####
#only select grasses and the last sampling date
ul_g <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block:Density, DISP) %>% 
  rowwise() %>% 
  mutate(cover_g = DISP)

#nudge 0s into a trace amount so we can use the beta
ul_g$cover_g[ul_g$cover_g == 0] <- 0.0025

####Grasses ~ Functional Group * Density####
useData <- filter(ul_g, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_g ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density)

#Cannot get model to fit - Do not use

####Dunnett's test####
ul_g$gd <- factor(ul_g$Group:ul_g$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_g ~ gd
                  + (1|Block),
                  data = ul_g,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) 
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= ul_g$gd) 

#Cannot get model to fit - Do not use

# Farmington Bay 2023 Models ####
##Models of total native and invasive cover ####
fb23$Group <- as.factor(fb23$Group)
fb23$Density <- as.factor(fb23$Density)

#only need the last date
mdf <- fb23 %>%
  filter(Date == "2023-09-11") %>%
  dplyr::select(Block, Group, Density, Date, Invasive.Cover, Native.Cover)

useData <- filter(mdf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

###FB Native Cover ~ Functional Group * Density ####
mdf.m1 <- glmmTMB(Native.Cover ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T)
plotResiduals(mdf.m1, form= useData$Density) 
#beta distribution fit better than the log normal

emmip(mdf.m1, Group~Density, CIs = T)
car::Anova(mdf.m1) #nothing significant

###FB Native Cover Dunnett's Test ####
mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m3 <- glmmTMB(log(Native.Cover) ~ gd 
                  + (1|Block),
                  data = mdf,
                  family = gaussian, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m3)
simulateResiduals(mdf.m3, plot = T)
plotResiduals(mdf.m3, form= mdf$gd)
#log normal fits better than the beta

emmeans(mdf.m3, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences between treatment and control 

###FB Invasive Cover ~ Functional Group * Density ####
mdf.m4 <- glmmTMB(Invasive.Cover ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T) 
plotResiduals(mdf.m4, form= useData$Density) 

emmip(mdf.m4, Group~Density, CIs = T)
car::Anova(mdf.m4) #nothing significant

###FB Invasive Cover Dunnett's Test####
mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(Invasive.Cover ~ gd
                  + (1|Block),
                  data = mdf,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T)
plotResiduals(mdf.m2, form= mdf$gd)

emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #nothing significant

##Models of only seeded species####

### Annual forbs ####
#select only annual forbs and the last date of sampling
fb23_af <- fb23 %>% 
  filter(Date == "2023-09-11") %>% 
  dplyr::select(Block:Density, RUMA) %>% 
  rowwise() %>% 
  mutate(cover_af = sum(RUMA))

####Annual forbs ~ Functional Group * Density####
useData <- filter(fb23_af, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_af ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density) 

#Cannot get to fit - Do not use

####Dunnett's test####
fb23_af$gd <- factor(fb23_af$Group:fb23_af$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_af ~ gd 
                  + (1|Block),
                  data = fb23_af,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) 
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= fb23_af$gd) 
#Cannot get to fit - Do not use

### Bulrushes ####
#select only bulrushes and the last date of sampling
fb23_b <- fb23 %>% 
  filter(Date == "2023-09-11") %>% 
  dplyr::select(Block:Density, BOMA, SCAC, SCAM) %>% 
  rowwise() %>% 
  mutate(cover_b = sum(BOMA, SCAC, SCAM))

####Bulrushes ~ Functional Group * Density####
useData <- filter(fb23_b, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_b ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T)  
plotResiduals(mdf.m1, form= useData$Density) 

#Cannot get to fit - Do not use

####Dunnett's Test####
fb23_b$gd <- factor(fb23_b$Group:fb23_b$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_b ~ gd 
                  + (1|Block),
                  data = fb23_b,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T)
plotResiduals(mdf.m2, form= fb_b$gd) 

#Cannot get to fit - Do not use

## Grasses ####
#only select grasses and last date of sampling
fb23_g <- fb23 %>% 
  filter(Date == "2023-09-11") %>% 
  dplyr::select(Block:Density, DISP) %>% 
  rowwise() %>% 
  mutate(cover_g = sum(DISP))

####Grasses ~ Functional Group * Density####
useData <- filter(fb23_g, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(cover_g ~ Group * Density 
                  + (1|Block),
                  data = useData,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) 
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density) 

#Cannot get to fit - Do not use

####Dunnett's test####
fb23_g$gd <- factor(fb23_g$Group:fb23_g$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(cover_g ~ gd 
                  + (1|Block),
                  data = fb23_g,
                  family = beta_family, 
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= fb23_g$gd) 

#Cannot get to fit - Do not use