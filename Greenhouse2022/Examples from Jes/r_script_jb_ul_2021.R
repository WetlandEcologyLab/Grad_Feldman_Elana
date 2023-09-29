#species over the summer
library(tidyverse)
library(lubridate)
library(ggplot2)

path.wd <- "~/Desktop/Greenhouse2022"
setwd(path.wd)
x <- read.csv("utah_lake_2021_seed_cleaned.csv") %>%
  as_tibble()

x <-  x %>%
  mutate(date = mdy(date))
x

x2 <- x %>%
  pivot_longer(cols = 5:54) %>%
  rename(cover = value)

##mutate data to change na's to zeros

x2 <- x2 %>% 
  replace(is.na(.), 0)
x2
 
##seed treatments over time with error bars----

x2 %>%
  ggplot(aes(x = date, y = cover, color = seed_treatments)) +
  stat_summary(aes(group = seed_treatments, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 8)) +
  stat_summary(aes(group = seed_treatments, width = .5),
               fun = mean, geom = "point", 
               position = position_dodge(width = 8)) +
  theme_bw()

##now again but separated over block
x2 %>%
  ggplot(aes(x = date, y = cover, color = seed_treatments)) +
  stat_summary(aes(group = seed_treatments, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 8)) +
  stat_summary(aes(group = seed_treatments, width = .5),
               fun = mean, geom = "point", 
               position = position_dodge(width = 8)) +
  facet_wrap(~B)
  theme_bw()


##stacked column graph with all species and raw data----
##also removed some columns

x2 %>%
  filter(!(name %in% c("bare_ground", "litter", "total_cover", "unknown_forb_1", 
                       "unknown_grass_1"))) %>%
  filter(month(date) == 8) %>%
  ggplot() +
  geom_col(aes(x = seed_treatments, y = cover, fill = name))


###create additional columns for data: native, invasive, seeded---

native <- c("BICE", "BOMA", "cyperus_spp","DISP","Eleocharis_spp", "DISP",
            "EUOC","Helianthus_spp", "JUTO", "JUAR", "POPE", "populus_spp", 
            "rumex_spp.", "ELPA", "SCAC", "SOAS", "SYCI", "ASIN",	"ASSP",	"CANE",	
            "CAPR",	"EPCI",	"EUMA",	"HEAU",	"SCAM",	"SEHY",	"VEHA")
invasive <- c("barnyard_grass","bermuda_grass", "chenopode_spp.", "HOJU",
              "LASE", "PHAU", "tamarisk", "typha" )
seeded <- c("BICE","BOMA","cyperus_spp", "DISP", "Helianthus_spp", 
            "rumex_spp.", "SCAC", "ASIN",	"ASSP",	"CANE",	
            "CAPR","ELPA","EPCI",	"EUMA",	"HEAU", "JUAR",	"SCAM",	"SEHY",	"VEHA")

plant_group<-c(native,invasive,seeded)

x2 <- x2 %>%
  mutate(native = case_when(name %in% native ~ TRUE,
                            name %in% invasive ~ FALSE)) %>%
  mutate(seeded = ifelse(name %in% seeded, TRUE, FALSE))

##stacked graph with native vs invasive (end of season / august) but raw data----
x2 %>%
  filter(!is.na(native)) %>%
  mutate(native = ifelse(native, "native", "invasive")) %>%
  filter(month(date) == 8) %>%
  ggplot() +
  geom_col(aes(x = seed_treatments, y = cover, fill = native)) +
  scale_fill_viridis_d()

##end of season cover----
x2 %>%
  filter(!is.na(native)) %>%
  mutate(native = ifelse(native, "native", "invasive")) %>%
  filter(month(date) == 9) %>%
  ggplot(aes(x = seed_treatments, y = cover, color = native)) +
  stat_summary(aes(group = native, width = .5),
               fun.data = mean_se, geom = "errorbar") +
  stat_summary(aes(group = native, width = .5),
               fun = mean, geom = "point") +
  facet_wrap(~B)
  theme_bw()
  
##end of season cover with Karin edits---
blocks.labels <- c("Closest to Water", "Further from Water", 
                   "Further from Upland", "Closest to Upland")

x2 %>%
    filter(!is.na(native)) %>%
    mutate(native = ifelse(native, "native", "invasive")) %>%
    filter(month(date) == 9) %>%
    mutate(seed_treatments = fct_relevel(seed_treatments, "control", "1x", "5x")) %>%  
    ggplot(aes(x = seed_treatments, y = cover, color = native)) +
    stat_summary(aes(group = native, width = 0),
                 fun.data = mean_se, geom = "errorbar", size = 2) +
    stat_summary(aes(group = native, width = 2),
                 fun = mean, geom = "point", size = 3) +
    facet_wrap(~B, nrow=1)+
    xlab("Seeding Densities")+
    ylab("Percent Cover")+
  scale_color_discrete(name = "Plant Group")
## cover of seed treatments over season ---
x2 %>%
  ggplot(aes(x = date, y = cover, col = seed_treatments)) +
  stat_summary(aes(group = seed_treatments),
               fun = "mean", geom = "line")+
  facet_wrap(~plant_group)


## now with se grey shadow
x2 %>%
  filter(native) %>%
  ggplot(aes(x = date, y = cover, col = name)) +
  stat_summary(aes(group = name),
               fun = "mean", geom = "line")

## just looking at what was seeded
x2 %>%
  filter(seeded) %>%
  ggplot(aes(x = date, y = cover, col = name)) +
  stat_summary(aes(group = name),
               fun = "mean", geom = "line") +
  facet_wrap(~seed_treatments)

##moving control first in the order
x_new<- x2 # Replicate data
x_new$seed_treatments <- factor(x_new$seed_treatments,# Reordering group factor levels
                         levels = c("control", "1x", "5x"))
x_new %>%
  filter(seeded) %>%
  ggplot(aes(x = date, y = cover, col = name)) +
  stat_summary(aes(group = name),
               fun = "mean", geom = "line") +
  facet_wrap(~seed_treatments)+
  xlab("")+
  ylab("Percent Cover")

##now modifying the labels
x2 %>%
  filter(seeded) %>%
  ggplot(aes(x = date, y = cover, col = name)) +
  stat_summary(aes(group = name),
               fun = "mean", geom = "line") +
  xlab("") + ylab("Percent Cover") +
  facet_wrap(~seed_treatments)

## and with se shading
x2 %>%
  filter(seeded) %>%
  ggplot(aes(x = date, y = cover, col = name)) +
  stat_summary(aes(group= name),
               geom = "ribbon",
               alpha = 0.5)+
  stat_summary(aes(group = name),
               fun = "mean", geom = "line") +
  facet_wrap(~seed_treatments)

######Okay now for plug plots####################
##########3
y <- read.csv("utah_lake_2021_plugs_cleaned.csv") %>%
  as_tibble()

y <-  y %>%
  mutate(date = mdy(date))

###filtering out late summer planting blocks (neg 1 and 2)
y2 <- y %>%
  pivot_longer(cols = 5:44) %>%
  rename(cover = value) %>%
  filter(!(B %in% c("neg_1", "neg_2")))

## plot in stacked columns raw values
y2 %>%
  ggplot() +
  geom_col(aes(x = plug_arrangment, y = cover)) +
  scale_fill_viridis_d()
y2 %>% 
  ggplot(aes(x = date, y = cover, color = B)) +
  stat_summary(aes(group = B, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 8)) +
  stat_summary(aes(group = B, width = .5),
               fun = mean, geom = "point", 
               position = position_dodge(width = 8)) +
  theme_bw()

## now separated by clumped vs dispersed WITH DISP
##With new edits from Karin meeting

y2 %>% 
  filter(month(date) == 9) %>%
  filter(day(date) == 16) %>% 
  filter(name == "DISP") %>% 
  filter(!(plug_arrangment %in% c("clumped_SCAC", "dispersed_SCAC"))) %>%
  mutate(plug_arrangment = fct_relevel(plug_arrangment, "control", 
                                       "clumped_DISP", "dispersed_DISP")) %>%  
  ggplot(aes(x = date, y = cover, color = B)) +
  stat_summary(aes(group = B, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 8)) +
  stat_summary(aes(group = B, width = .5),
               fun = mean, geom = "point",
               size = 3,
               position = position_dodge(width = 8)) +
  facet_wrap(~plug_arrangment, nrow=1)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("September 16th, 2021")+
  ylab("Percent Cover of DISP")

##now separated by clumped vs dispersed WITH SCAC

y2 %>% 
  filter(month(date) == 9) %>%
  filter(day(date) == 16) %>% 
  filter(name == "SCAC") %>% 
  filter(!(plug_arrangment %in% c("clumped_DISP", "dispersed_DISP"))) %>%
  mutate(plug_arrangment = fct_relevel(plug_arrangment, "control", 
                                       "clumped_SCAC", "dispersed_SCAC")) %>%  
  ggplot(aes(x = date, y = cover, color = B)) +
  stat_summary(aes(group = B, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 8)) +
  stat_summary(aes(group = B, width = 5),
               fun = mean, geom = "point",
               size =3,
               position = position_dodge(width = 8)) +
  facet_wrap(~plug_arrangment, nrow=1)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("September 16th, 2021")+
  ylab("Percent Cover of SCAC")

###Now over whole season with DISP

y2 %>% 
  filter(name == "DISP") %>% 
  filter(!(plug_arrangment %in% c("clumped_SCAC", "dispersed_SCAC"))) %>%
  mutate(plug_arrangment = fct_relevel(plug_arrangment, "control", 
                                       "clumped_DISP", "dispersed_DISP")) %>%  
  ggplot(aes(x = date, y = cover, color = B)) +
  stat_summary(aes(group = B),
               fun = "mean", geom = "line") +
  facet_wrap(~plug_arrangment, nrow=1) +
  xlab("")+
  ylab("Percent Cover of DISP")

###Now over whole season with SCAC
y2 %>% 
  filter(name == "SCAC") %>% 
  filter(!(plug_arrangment %in% c("clumped_DISP", "dispersed_DISP"))) %>%
  mutate(plug_arrangment = fct_relevel(plug_arrangment, "control", 
                                       "clumped_SCAC", "dispersed_SCAC")) %>%  
  ggplot(aes(x = date, y = cover, color = B)) +
  stat_summary(aes(group = B),
               fun = "mean", geom = "line") +
  facet_wrap(~plug_arrangment, nrow=1) +
  xlab("")+
  ylab("Percent Cover of SCAC")

  
  
##label code
 c("Closest to Water", 
    "Further from Water",
    "Further from upland",
    "Closest to Upland")

 