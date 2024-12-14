#import data
fb <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/clean_fb.csv")
ul <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/clean_ul.csv")
fb23 <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/FB2023-CLEAN.csv")

#All package version saved in renv.lock 
#renv::init, renv::restore
library(dplyr)
library(lubridate)

View(fb)
View(ul)

#Clean Farmington Bay 2022 Data####
glimpse(fb)

#rename block and make  a factor
names(fb)[1] <- "Block"
fb$Block <- as.factor(fb$Block)

#change the C in block to 10 so it is easier to keep track of, then make a factor
fb$Group[fb$Group == "C"] <- 10
fb$Group <- as.factor(fb$Group)

#double check density and group and then make a factor
unique(fb$Group)
fb$Group <- as.factor(fb$Group)

unique(fb$Density)
fb$Density <- as.factor(fb$Density)

#fix the date
fb$Date <- lubridate::mdy(fb$Date)

#change cover values to the midpoints, make numeric, and make a decimal
fb <- fb %>% 
  mutate(Total.Cover = case_when(
    Total.Cover == "<1" ~ 0.5,
    Total.Cover == ">99" ~ 99.5,
    Total.Cover == "1" ~ 5,
    Total.Cover == "10" ~ 15,
    Total.Cover == "20" ~ 25,
    Total.Cover == "30" ~ 35,
    Total.Cover == "40" ~ 45,
    Total.Cover == "50" ~ 55,
    Total.Cover == "60" ~ 65,
    Total.Cover == "70" ~ 75,
    Total.Cover == "80" ~ 85,
    Total.Cover == "90" ~ 95,
    Total.Cover == "0" ~ 0,
    Total.Cover == "" ~ 0,
    is.na(Total.Cover) ~ 0)) %>% 
  mutate(Total.Cover = as.double(Total.Cover)) %>% 
  mutate(Total.Cover = Total.Cover/100)

#now do it for all the other columns
fb <- fb %>% 
  mutate(PHAU = case_when(
    PHAU == "<1" ~ 0.5,
    PHAU == ">99" ~ 99.5,
    PHAU == "1" ~ 5,
    PHAU == "10" ~ 15,
    PHAU == "20" ~ 25,
    PHAU == "30" ~ 35,
    PHAU == "40" ~ 45,
    PHAU == "50" ~ 55,
    PHAU == "60" ~ 65,
    PHAU == "70" ~ 75,
    PHAU == "80" ~ 85,
    PHAU == "90" ~ 95,
    PHAU == "0" ~ 0,
    PHAU == "" ~ 0,
    is.na(PHAU) ~ 0)) %>% 
  mutate(PHAU = as.double(PHAU)) %>% 
  mutate(PHAU = PHAU/100)

fb <- fb %>% 
  mutate(Cheno = case_when(
    Cheno == "<1" ~ 0.5,
    Cheno == ">99" ~ 99.5,
    Cheno == "1" ~ 5,
    Cheno == "10" ~ 15,
    Cheno == "20" ~ 25,
    Cheno == "30" ~ 35,
    Cheno == "40" ~ 45,
    Cheno == "50" ~ 55,
    Cheno == "60" ~ 65,
    Cheno == "70" ~ 75,
    Cheno == "80" ~ 85,
    Cheno == "90" ~ 95,
    Cheno == "0" ~ 0,
    Cheno == "" ~ 0,
    is.na(Cheno) ~ 0)) %>% 
  mutate(Cheno = as.double(Cheno)) %>% 
  mutate(Cheno = Cheno/100)

fb <- fb %>% 
  mutate(Typha = case_when(
    Typha == "<1" ~ 0.5,
    Typha == ">99" ~ 99.5,
    Typha == "1" ~ 5,
    Typha == "10" ~ 15,
    Typha == "20" ~ 25,
    Typha == "30" ~ 35,
    Typha == "40" ~ 45,
    Typha == "50" ~ 55,
    Typha == "60" ~ 65,
    Typha == "70" ~ 75,
    Typha == "80" ~ 85,
    Typha == "90" ~ 95,
    Typha == "0" ~ 0,
    Typha == "" ~ 0,
    is.na(Typha) ~ 0)) %>% 
  mutate(Typha = as.double(Typha)) %>% 
  mutate(Typha = Typha/100)

fb <- fb %>% 
  mutate(BOMA = case_when(
    BOMA == "<1" ~ 0.5,
    BOMA == ">99" ~ 99.5,
    BOMA == "1" ~ 5,
    BOMA == "10" ~ 15,
    BOMA == "20" ~ 25,
    BOMA == "30" ~ 35,
    BOMA == "40" ~ 45,
    BOMA == "50" ~ 55,
    BOMA == "60" ~ 65,
    BOMA == "70" ~ 75,
    BOMA == "80" ~ 85,
    BOMA == "90" ~ 95,
    BOMA == "0" ~ 0,
    BOMA == "" ~ 0,
    is.na(BOMA) ~ 0)) %>% 
  mutate(BOMA = as.double(BOMA)) %>% 
  mutate(BOMA = BOMA/100)

fb <- fb %>% 
  mutate(DISP = case_when(
    DISP == "<1" ~ 0.5,
    DISP == ">99" ~ 99.5,
    DISP == "1" ~ 5,
    DISP == "10" ~ 15,
    DISP == "20" ~ 25,
    DISP == "30" ~ 35,
    DISP == "40" ~ 45,
    DISP == "50" ~ 55,
    DISP == "60" ~ 65,
    DISP == "70" ~ 75,
    DISP == "80" ~ 85,
    DISP == "90" ~ 95,
    DISP == "0" ~ 0,
    DISP == "" ~ 0,
    is.na(DISP) ~ 0)) %>% 
  mutate(DISP = as.double(DISP)) %>% 
  mutate(DISP = DISP/100)

fb <- fb %>% 
  mutate(EUMA = case_when(
    EUMA == "<1" ~ 0.5,
    EUMA == ">99" ~ 99.5,
    EUMA == "1" ~ 5,
    EUMA == "10" ~ 15,
    EUMA == "20" ~ 25,
    EUMA == "30" ~ 35,
    EUMA == "40" ~ 45,
    EUMA == "50" ~ 55,
    EUMA == "60" ~ 65,
    EUMA == "70" ~ 75,
    EUMA == "80" ~ 85,
    EUMA == "90" ~ 95,
    EUMA == "0" ~ 0,
    EUMA == "" ~ 0,
    is.na(EUMA) ~ 0)) %>%
  mutate(EUMA = as.double(EUMA)) %>% 
  mutate(EUMA = EUMA/100)

fb <- fb %>% 
  mutate(SYCI = case_when(
    SYCI == "<1" ~ 0.5,
    SYCI == ">99" ~ 99.5,
    SYCI == "1" ~ 5,
    SYCI == "10" ~ 15,
    SYCI == "20" ~ 25,
    SYCI == "30" ~ 35,
    SYCI == "40" ~ 45,
    SYCI == "50" ~ 55,
    SYCI == "60" ~ 65,
    SYCI == "70" ~ 75,
    SYCI == "80" ~ 85,
    SYCI == "90" ~ 95,
    SYCI == "0" ~ 0,
    SYCI == "" ~ 0,
    is.na(SYCI) ~ 0)) %>% 
  mutate(SYCI = as.double(SYCI)) %>% 
  mutate(SYCI = SYCI/100)

fb <- fb %>% 
  mutate(LEFA = case_when(
    LEFA == "<1" ~ 0.5,
    LEFA == ">99" ~ 99.5,
    LEFA == "1" ~ 5,
    LEFA == "10" ~ 15,
    LEFA == "20" ~ 25,
    LEFA == "30" ~ 35,
    LEFA == "40" ~ 45,
    LEFA == "50" ~ 55,
    LEFA == "60" ~ 65,
    LEFA == "70" ~ 75,
    LEFA == "80" ~ 85,
    LEFA == "90" ~ 95,
    LEFA == "0" ~ 0,
    LEFA == "" ~ 0,
    is.na(LEFA) ~ 0)) %>% 
  mutate(LEFA = as.double(LEFA)) %>% 
  mutate(LEFA = LEFA/100)

fb <- fb %>% 
  mutate(SCAC = case_when(
    SCAC == "<1" ~ 0.5,
    SCAC == ">99" ~ 99.5,
    SCAC == "1" ~ 5,
    SCAC == "10" ~ 15,
    SCAC == "20" ~ 25,
    SCAC == "30" ~ 35,
    SCAC == "40" ~ 45,
    SCAC == "50" ~ 55,
    SCAC == "60" ~ 65,
    SCAC == "70" ~ 75,
    SCAC == "80" ~ 85,
    SCAC == "90" ~ 95,
    SCAC == "0" ~ 0,
    SCAC == "" ~ 0,
    is.na(SCAC) ~ 0)) %>% 
  mutate(SCAC = as.double(SCAC)) %>% 
  mutate(SCAC = SCAC/100)

fb <- fb %>% 
  mutate(BICE = case_when(
    BICE == "<1" ~ 0.5,
    BICE == ">99" ~ 99.5,
    BICE == "1" ~ 5,
    BICE == "10" ~ 15,
    BICE == "20" ~ 25,
    BICE == "30" ~ 35,
    BICE == "40" ~ 45,
    BICE == "50" ~ 55,
    BICE == "60" ~ 65,
    BICE == "70" ~ 75,
    BICE == "80" ~ 85,
    BICE == "90" ~ 95,
    BICE == "0" ~ 0,
    BICE == "" ~ 0,
    is.na(BICE) ~ 0)) %>% 
  mutate(BICE = as.double(BICE)) %>% 
  mutate(BICE = BICE/100)

fb <- fb %>% 
  mutate(BIFR = case_when(
    BIFR == "<1" ~ 0.5,
    BIFR == ">99" ~ 99.5,
    BIFR == "1" ~ 5,
    BIFR == "10" ~ 15,
    BIFR == "20" ~ 25,
    BIFR == "30" ~ 35,
    BIFR == "40" ~ 45,
    BIFR == "50" ~ 55,
    BIFR == "60" ~ 65,
    BIFR == "70" ~ 75,
    BIFR == "80" ~ 85,
    BIFR == "90" ~ 95,
    BIFR == "0" ~ 0,
    BIFR == "" ~ 0,
    is.na(BIFR) ~ 0)) %>% 
  mutate(BIFR = as.double(BIFR)) %>% 
  mutate(BIFR = BIFR/100)

fb <- fb %>% 
  mutate(EUOC = case_when(
    EUOC == "<1" ~ 0.5,
    EUOC == ">99" ~ 99.5,
    EUOC == "1" ~ 5,
    EUOC == "10" ~ 15,
    EUOC == "20" ~ 25,
    EUOC == "30" ~ 35,
    EUOC == "40" ~ 45,
    EUOC == "50" ~ 55,
    EUOC == "60" ~ 65,
    EUOC == "70" ~ 75,
    EUOC == "80" ~ 85,
    EUOC == "90" ~ 95,
    EUOC == "0" ~ 0,
    EUOC == "" ~ 0,
    is.na(EUOC) ~ 0)) %>% 
  mutate(EUOC = as.double(EUOC)) %>% 
  mutate(EUOC = EUOC/100)

fb <- fb %>% 
  mutate(MUAS = case_when(
    MUAS == "<1" ~ 0.5,
    MUAS == ">99" ~ 99.5,
    MUAS == "1" ~ 5,
    MUAS == "10" ~ 15,
    MUAS == "20" ~ 25,
    MUAS == "30" ~ 35,
    MUAS == "40" ~ 45,
    MUAS == "50" ~ 55,
    MUAS == "60" ~ 65,
    MUAS == "70" ~ 75,
    MUAS == "80" ~ 85,
    MUAS == "90" ~ 95,
    MUAS == "0" ~ 0,
    MUAS == "" ~ 0,
    is.na(MUAS) ~ 0)) %>% 
  mutate(MUAS = as.double(MUAS)) %>% 
  mutate(MUAS = MUAS/100)

fb <- fb %>% 
  mutate(SCAM = case_when(
    SCAM == "<1" ~ 0.5,
    SCAM == ">99" ~ 99.5,
    SCAM == "1" ~ 5,
    SCAM == "10" ~ 15,
    SCAM == "20" ~ 25,
    SCAM == "30" ~ 35,
    SCAM == "40" ~ 45,
    SCAM == "50" ~ 55,
    SCAM == "60" ~ 65,
    SCAM == "70" ~ 75,
    SCAM == "80" ~ 85,
    SCAM == "90" ~ 95,
    SCAM == "0" ~ 0,
    SCAM == "" ~ 0,
    is.na(SCAM) ~ 0)) %>% 
  mutate(SCAM = as.double(SCAM)) %>% 
  mutate(SCAM = SCAM/100)

fb <- fb %>% 
  mutate(RUMA = case_when(
    RUMA == "<1" ~ 0.5,
    RUMA == ">99" ~ 99.5,
    RUMA == "1" ~ 5,
    RUMA == "10" ~ 15,
    RUMA == "20" ~ 25,
    RUMA == "30" ~ 35,
    RUMA == "40" ~ 45,
    RUMA == "50" ~ 55,
    RUMA == "60" ~ 65,
    RUMA == "70" ~ 75,
    RUMA == "80" ~ 85,
    RUMA == "90" ~ 95,
    RUMA == "0" ~ 0,
    RUMA == "" ~ 0,
    is.na(RUMA) ~ 0)) %>% 
  mutate(RUMA = as.double(RUMA)) %>% 
  mutate(RUMA = RUMA/100)

fb <- fb %>% 
  mutate(RUST = case_when(
    RUST == "<1" ~ 0.5,
    RUST == ">99" ~ 99.5,
    RUST == "1" ~ 5,
    RUST == "10" ~ 15,
    RUST == "20" ~ 25,
    RUST == "30" ~ 35,
    RUST == "40" ~ 45,
    RUST == "50" ~ 55,
    RUST == "60" ~ 65,
    RUST == "70" ~ 75,
    RUST == "80" ~ 85,
    RUST == "90" ~ 95,
    RUST == "0" ~ 0,
    RUST == "" ~ 0,
    is.na(RUST) ~ 0)) %>% 
  mutate(RUST = as.double(RUST)) %>% 
  mutate(RUST = RUST/100)

fb <- fb %>% 
  mutate(Unk_Forb = case_when(
    Unk_Forb == "<1" ~ 0.5,
    Unk_Forb == ">99" ~ 99.5,
    Unk_Forb == "1" ~ 5,
    Unk_Forb == "10" ~ 15,
    Unk_Forb == "20" ~ 25,
    Unk_Forb == "30" ~ 35,
    Unk_Forb == "40" ~ 45,
    Unk_Forb == "50" ~ 55,
    Unk_Forb == "60" ~ 65,
    Unk_Forb == "70" ~ 75,
    Unk_Forb == "80" ~ 85,
    Unk_Forb == "90" ~ 95,
    Unk_Forb == "0" ~ 0,
    Unk_Forb == "" ~ 0,
    is.na(Unk_Forb) ~ 0)) %>% 
  mutate(Unk_Forb = as.double(Unk_Forb)) %>% 
  mutate(Unk_Forb = Unk_Forb/100)

fb <- fb %>% 
  mutate(Unk_Grass = case_when(
    Unk_Grass == "<1" ~ 0.5,
    Unk_Grass == ">99" ~ 99.5,
    Unk_Grass == "1" ~ 5,
    Unk_Grass == "10" ~ 15,
    Unk_Grass == "20" ~ 25,
    Unk_Grass == "30" ~ 35,
    Unk_Grass == "40" ~ 45,
    Unk_Grass == "50" ~ 55,
    Unk_Grass == "60" ~ 65,
    Unk_Grass == "70" ~ 75,
    Unk_Grass == "80" ~ 85,
    Unk_Grass == "90" ~ 95,
    Unk_Grass == "0" ~ 0,
    Unk_Grass == "" ~ 0,
    is.na(Unk_Grass) ~ 0)) %>% 
  mutate(Unk_Grass = as.double(Unk_Grass)) %>% 
  mutate(Unk_Grass = Unk_Grass/100)

fb <- fb %>% 
  mutate(Unk_Rush = case_when(
    Unk_Rush == "<1" ~ 0.5,
    Unk_Rush == ">99" ~ 99.5,
    Unk_Rush == "1" ~ 5,
    Unk_Rush == "10" ~ 15,
    Unk_Rush == "20" ~ 25,
    Unk_Rush == "30" ~ 35,
    Unk_Rush == "40" ~ 45,
    Unk_Rush == "50" ~ 55,
    Unk_Rush == "60" ~ 65,
    Unk_Rush == "70" ~ 75,
    Unk_Rush == "80" ~ 85,
    Unk_Rush == "90" ~ 95,
    Unk_Rush == "0" ~ 0,
    Unk_Rush == "" ~ 0,
    is.na(Unk_Rush) ~ 0)) %>% 
  mutate(Unk_Rush = as.double(Unk_Rush)) %>% 
  mutate(Unk_Rush = Unk_Rush/100) %>% 
  rename(Unk_Bulrush = Unk_Rush) #and change name

fb <- fb %>% 
  mutate(SARU = case_when(
    SARU == "<1" ~ 0.5,
    SARU == ">99" ~ 99.5,
    SARU == "1" ~ 5,
    SARU == "10" ~ 15,
    SARU == "20" ~ 25,
    SARU == "30" ~ 35,
    SARU == "40" ~ 45,
    SARU == "50" ~ 55,
    SARU == "60" ~ 65,
    SARU == "70" ~ 75,
    SARU == "80" ~ 85,
    SARU == "90" ~ 95,
    SARU == "0" ~ 0,
    SARU == "" ~ 0,
    is.na(SARU) ~ 0)) %>% 
  mutate(SARU = as.double(SARU)) %>% 
  mutate(SARU = SARU/100)

fb <- fb %>% 
  mutate(Tamarisk = case_when(
    Tamarisk == "<1" ~ 0.5,
    Tamarisk == ">99" ~ 99.5,
    Tamarisk == "1" ~ 5,
    Tamarisk == "10" ~ 15,
    Tamarisk == "20" ~ 25,
    Tamarisk == "30" ~ 35,
    Tamarisk == "40" ~ 45,
    Tamarisk == "50" ~ 55,
    Tamarisk == "60" ~ 65,
    Tamarisk == "70" ~ 75,
    Tamarisk == "80" ~ 85,
    Tamarisk == "90" ~ 95,
    Tamarisk == "0" ~ 0,
    Tamarisk == "" ~ 0,
    is.na(Tamarisk) ~ 0)) %>% 
  mutate(Tamarisk = as.double(Tamarisk)) %>% 
  mutate(Tamarisk = Tamarisk/100)

glimpse(fb)

#check measurements to make sure they make sense
min(fb$Measurement.1)
max(fb$Measurement.1)

min(fb$Measurement.2)
max(fb$Measurement.2)

min(fb$Measurement.3)
max(fb$Measurement.3)

#Add a new section for invasives and natives
fb <- fb %>% 
  rowwise() %>% 
  mutate(Invasive.Cover = sum(PHAU, Typha, RUST, Tamarisk, na.rm = T),
         Native.Cover = sum(Cheno, BOMA, DISP, EUMA, SYCI, LEFA, SCAC,
                            BICE, BIFR, EUOC, MUAS, SCAM, RUMA, Unk_Bulrush, 
                            SARU, na.rm = T))

#check to make sure the new sections make sense
max(fb$Invasive.Cover, na.rm = T)
max(fb$Native.Cover, na.rm = T)

#change NAs and 0s to a trace value so it can be used with the beta
#trace value should be at least half the smallest recorded value
min(fb$Invasive.Cover)
fb$Invasive.Cover[is.na(fb$Invasive.Cover)] <- 0.0025
fb$Invasive.Cover[fb$Invasive.Cover==0] <- 0.0025

min(fb$Native.Cover, na.rm = TRUE)
fb$Native.Cover[is.na(fb$Native.Cover)] <- 0.0025
fb$Native.Cover[fb$Native.Cover==0] <- 0.0025

#Clean Utah Lake 2022 Data####
glimpse(ul)

#make block and plot factors, change group C to 10 to make it easier to follow
ul$Block <- as.factor(ul$Block)
ul$Group[ul$Group == "C"] <- 10
ul$Group <- as.factor(ul$Group)

#check density and group and make factor 
unique(ul$Group)
ul$Group <- as.factor(ul$Group)

unique(ul$Density)
ul$Density <- as.factor(ul$Density)

#fix the date
ul$Date <- lubridate::mdy(ul$Date)

#change cover values to the midpoints, make numeric, and make a decimal
ul <- ul %>% 
  mutate(Total.Cover = case_when(
    Total.Cover == "<1" ~ 0.5,
    Total.Cover == ">1" ~ 0.5,
    Total.Cover == ">99" ~ 99.5,
    Total.Cover == "1" ~ 5,
    Total.Cover == "10" ~ 15,
    Total.Cover == "20" ~ 25,
    Total.Cover == "30" ~ 35,
    Total.Cover == "40" ~ 45,
    Total.Cover == "50" ~ 55,
    Total.Cover == "60" ~ 65,
    Total.Cover == "70" ~ 75,
    Total.Cover == "80" ~ 85,
    Total.Cover == "90" ~ 95,
    Total.Cover == "0" ~ 0,
    Total.Cover == "" ~ 0,
    is.na(Total.Cover) ~ 0)) %>%
  mutate(Total.Cover = as.double(Total.Cover)) %>% 
  mutate(Total.Cover = Total.Cover/100)

names(ul)[7] <- "Unk_Forb" #fix the name
ul <- ul %>% 
  mutate(Unk_Forb = case_when(
    Unk_Forb == "<1" ~ 0.5,
    Unk_Forb == ">99" ~ 99.5,
    Unk_Forb == "1" ~ 5,
    Unk_Forb == "10" ~ 15,
    Unk_Forb == "20" ~ 25,
    Unk_Forb == "30" ~ 35,
    Unk_Forb == "40" ~ 45,
    Unk_Forb == "50" ~ 55,
    Unk_Forb == "60" ~ 65,
    Unk_Forb == "70" ~ 75,
    Unk_Forb == "80" ~ 85,
    Unk_Forb == "90" ~ 95,
    Unk_Forb == "0" ~ 0,
    Unk_Forb == "" ~ 0,
    is.na(Unk_Forb) ~ 0)) %>% 
  mutate(Unk_Forb = as.double(Unk_Forb)) %>% 
  mutate(Unk_Forb = Unk_Forb/100)

ul <- ul %>% 
  mutate(PHAU = case_when(
    PHAU == "<1" ~ 0.5,
    PHAU == ">99" ~ 99.5,
    PHAU == "1" ~ 5,
    PHAU == "10" ~ 15,
    PHAU == "20" ~ 25,
    PHAU == "30" ~ 35,
    PHAU == "40" ~ 45,
    PHAU == "50" ~ 55,
    PHAU == "60" ~ 65,
    PHAU == "70" ~ 75,
    PHAU == "80" ~ 85,
    PHAU == "90" ~ 95,
    PHAU == "0" ~ 0,
    PHAU == "" ~ 0,
    is.na(PHAU) ~ 0)) %>% 
  mutate(PHAU = as.double(PHAU)) %>% 
  mutate(PHAU = PHAU/100)

ul <- ul %>% 
  mutate(Unk_Sedge = case_when(
    Unk_Sedge == "<1" ~ 0.5,
    Unk_Sedge == ">99" ~ 99.5,
    Unk_Sedge == "1" ~ 5,
    Unk_Sedge == "10" ~ 15,
    Unk_Sedge == "20" ~ 25,
    Unk_Sedge == "30" ~ 35,
    Unk_Sedge == "40" ~ 45,
    Unk_Sedge == "50" ~ 55,
    Unk_Sedge == "60" ~ 65,
    Unk_Sedge == "70" ~ 75,
    Unk_Sedge == "80" ~ 85,
    Unk_Sedge == "90" ~ 95,
    Unk_Sedge == "0" ~ 0,
    Unk_Sedge == "" ~ 0,
    is.na(Unk_Sedge) ~ 0)) %>% 
  mutate(Unk_Sedge = as.double(Unk_Sedge)) %>% 
  mutate(Unk_Sedge = Unk_Sedge/100)

ul <- ul %>% 
  mutate(BOMA = case_when(
    BOMA == "<1" ~ 0.5,
    BOMA == ">99" ~ 99.5,
    BOMA == "1" ~ 5,
    BOMA == "10" ~ 15,
    BOMA == "20" ~ 25,
    BOMA == "30" ~ 35,
    BOMA == "40" ~ 45,
    BOMA == "50" ~ 55,
    BOMA == "60" ~ 65,
    BOMA == "70" ~ 75,
    BOMA == "80" ~ 85,
    BOMA == "90" ~ 95,
    BOMA == "0" ~ 0,
    BOMA == "" ~ 0,
    is.na(BOMA) ~ 0)) %>% 
  mutate(BOMA = as.double(BOMA)) %>% 
  mutate(BOMA = BOMA/100)

ul <- ul %>% 
  mutate(BICE = case_when(
    BICE == "<1" ~ 0.5,
    BICE == ">99" ~ 99.5,
    BICE == "1" ~ 5,
    BICE == "10" ~ 15,
    BICE == "20" ~ 25,
    BICE == "30" ~ 35,
    BICE == "40" ~ 45,
    BICE == "50" ~ 55,
    BICE == "60" ~ 65,
    BICE == "70" ~ 75,
    BICE == "80" ~ 85,
    BICE == "90" ~ 95,
    BICE == "0" ~ 0,
    BICE == "" ~ 0,
    is.na(BICE) ~ 0)) %>% 
  mutate(BICE = as.double(BICE)) %>% 
  mutate(BICE = BICE/100)

ul <- ul %>% 
  mutate(CYER = case_when(
    CYER == "<1" ~ 0.5,
    CYER == ">99" ~ 99.5,
    CYER == "1" ~ 5,
    CYER == "10" ~ 15,
    CYER == "20" ~ 25,
    CYER == "30" ~ 35,
    CYER == "40" ~ 45,
    CYER == "50" ~ 55,
    CYER == "60" ~ 65,
    CYER == "70" ~ 75,
    CYER == "80" ~ 85,
    CYER == "90" ~ 95,
    CYER == "0" ~ 0,
    CYER == "" ~ 0,
    is.na(CYER) ~ 0)) %>% 
  mutate(CYER = as.double(CYER)) %>% 
  mutate(CYER = CYER/100)

ul <- ul %>% 
  mutate(RUMA = case_when(
    RUMA == "<1" ~ 0.5,
    RUMA == ">99" ~ 99.5,
    RUMA == "1" ~ 5,
    RUMA == "10" ~ 15,
    RUMA == "20" ~ 25,
    RUMA == "30" ~ 35,
    RUMA == "40" ~ 45,
    RUMA == "50" ~ 55,
    RUMA == "60" ~ 65,
    RUMA == "70" ~ 75,
    RUMA == "80" ~ 85,
    RUMA == "90" ~ 95,
    RUMA == "0" ~ 0,
    RUMA == "" ~ 0,
    is.na(RUMA) ~ 0)) %>% 
  mutate(RUMA = as.double(RUMA)) %>% 
  mutate(RUMA = RUMA/100)

ul <- ul %>% 
  mutate(BASC = case_when(
    BASC == "<1" ~ 0.5,
    BASC == ">99" ~ 99.5,
    BASC == "1" ~ 5,
    BASC == "10" ~ 15,
    BASC == "20" ~ 25,
    BASC == "30" ~ 35,
    BASC == "40" ~ 45,
    BASC == "50" ~ 55,
    BASC == "60" ~ 65,
    BASC == "70" ~ 75,
    BASC == "80" ~ 85,
    BASC == "90" ~ 95,
    BASC == "0" ~ 0,
    BASC == "" ~ 0,
    is.na(BASC) ~ 0)) %>% 
  mutate(BASC = as.double(BASC)) %>% 
  mutate(BASC = BASC/100)

ul <- ul %>% 
  mutate(LASE = case_when(
    LASE == "<1" ~ 0.5,
    LASE == ">99" ~ 99.5,
    LASE == "1" ~ 5,
    LASE == "10" ~ 15,
    LASE == "20" ~ 25,
    LASE == "30" ~ 35,
    LASE == "40" ~ 45,
    LASE == "50" ~ 55,
    LASE == "60" ~ 65,
    LASE == "70" ~ 75,
    LASE == "80" ~ 85,
    LASE == "90" ~ 95,
    LASE == "0" ~ 0,
    LASE == "" ~ 0,
    is.na(LASE) ~ 0)) %>% 
  mutate(LASE = as.double(LASE)) %>% 
  mutate(LASE = LASE/100)

ul <- ul %>% 
  mutate(Cheno = case_when(
    Cheno == "<1" ~ 0.5,
    Cheno == ">99" ~ 99.5,
    Cheno == "1" ~ 5,
    Cheno == "10" ~ 15,
    Cheno == "20" ~ 25,
    Cheno == "30" ~ 35,
    Cheno == "40" ~ 45,
    Cheno == "50" ~ 55,
    Cheno == "60" ~ 65,
    Cheno == "70" ~ 75,
    Cheno == "80" ~ 85,
    Cheno == "90" ~ 95,
    Cheno == "0" ~ 0,
    Cheno == "" ~ 0,
    is.na(Cheno) ~ 0)) %>% 
  mutate(Cheno = as.double(Cheno)) %>% 
  mutate(Cheno = Cheno/100)

ul <- ul %>% 
  mutate(SCAC = case_when(
    SCAC == "<1" ~ 0.5,
    SCAC == ">99" ~ 99.5,
    SCAC == "1" ~ 5,
    SCAC == "10" ~ 15,
    SCAC == "20" ~ 25,
    SCAC == "30" ~ 35,
    SCAC == "40" ~ 45,
    SCAC == "50" ~ 55,
    SCAC == "60" ~ 65,
    SCAC == "70" ~ 75,
    SCAC == "80" ~ 85,
    SCAC == "90" ~ 95,
    SCAC == "0" ~ 0,
    SCAC == "" ~ 0,
    is.na(SCAC) ~ 0)) %>% 
  mutate(SCAC = as.double(SCAC)) %>% 
  mutate(SCAC = SCAC/100)

ul <- ul %>% 
  mutate(SCPU = case_when(
    SCPU == "<1" ~ 0.5,
    SCPU == ">99" ~ 99.5,
    SCPU == "1" ~ 5,
    SCPU == "10" ~ 15,
    SCPU == "20" ~ 25,
    SCPU == "30" ~ 35,
    SCPU == "40" ~ 45,
    SCPU == "50" ~ 55,
    SCPU == "60" ~ 65,
    SCPU == "70" ~ 75,
    SCPU == "80" ~ 85,
    SCPU == "90" ~ 95,
    SCPU == "0" ~ 0,
    SCPU == "" ~ 0,
    is.na(SCPU) ~ 0)) %>% 
  mutate(SCPU = as.double(SCPU)) %>% 
  mutate(SCPU = SCPU/100)

ul <- ul %>% 
  mutate(SCAM = case_when(
    SCAM == "<1" ~ 0.5,
    SCAM == ">99" ~ 99.5,
    SCAM == "1" ~ 5,
    SCAM == "10" ~ 15,
    SCAM == "20" ~ 25,
    SCAM == "30" ~ 35,
    SCAM == "40" ~ 45,
    SCAM == "50" ~ 55,
    SCAM == "60" ~ 65,
    SCAM == "70" ~ 75,
    SCAM == "80" ~ 85,
    SCAM == "90" ~ 95,
    SCAM == "0" ~ 0,
    SCAM == "" ~ 0,
    is.na(SCAM) ~ 0)) %>% 
  mutate(SCAM = as.double(SCAM)) %>% 
  mutate(SCAM = SCAM/100)

ul <- ul %>% 
  mutate(DISP = case_when(
    DISP == "<1" ~ 0.5,
    DISP == ">99" ~ 99.5,
    DISP == "1" ~ 5,
    DISP == "10" ~ 15,
    DISP == "20" ~ 25,
    DISP == "30" ~ 35,
    DISP == "40" ~ 45,
    DISP == "50" ~ 55,
    DISP == "60" ~ 65,
    DISP == "70" ~ 75,
    DISP == "80" ~ 85,
    DISP == "90" ~ 95,
    DISP == "0" ~ 0,
    DISP == "" ~ 0,
    DISP == "  " ~ 0,
    is.na(DISP) ~ 0)) %>% 
  mutate(DISP = as.double(DISP)) %>% 
  mutate(DISP = DISP/100)

ul <- ul %>% 
  mutate(RACY = case_when(
    RACY == "<1" ~ 0.5,
    RACY == ">99" ~ 99.5,
    RACY == "1" ~ 5,
    RACY == "10" ~ 15,
    RACY == "20" ~ 25,
    RACY == "30" ~ 35,
    RACY == "40" ~ 45,
    RACY == "50" ~ 55,
    RACY == "60" ~ 65,
    RACY == "70" ~ 75,
    RACY == "80" ~ 85,
    RACY == "90" ~ 95,
    RACY == "0" ~ 0,
    RACY == "" ~ 0,
    is.na(RACY) ~ 0)) %>% 
  mutate(RACY = as.double(RACY)) %>% 
  mutate(RACY = RACY/100)

ul <- ul %>% 
  mutate(ASIN = case_when(
    ASIN == "<1" ~ 0.5,
    ASIN == ">99" ~ 99.5,
    ASIN == "1" ~ 5,
    ASIN == "10" ~ 15,
    ASIN == "20" ~ 25,
    ASIN == "30" ~ 35,
    ASIN == "40" ~ 45,
    ASIN == "50" ~ 55,
    ASIN == "60" ~ 65,
    ASIN == "70" ~ 75,
    ASIN == "80" ~ 85,
    ASIN == "90" ~ 95,
    ASIN == "0" ~ 0,
    ASIN == "" ~ 0,
    is.na(ASIN) ~ 0)) %>% 
  mutate(ASIN = as.double(ASIN)) %>% 
  mutate(ASIN = ASIN/100)

ul <- ul %>% 
  mutate(Unk_Grass = case_when(
    Unk_Grass == "<1" ~ 0.5,
    Unk_Grass == ">99" ~ 99.5,
    Unk_Grass == "1" ~ 5,
    Unk_Grass == "10" ~ 15,
    Unk_Grass == "20" ~ 25,
    Unk_Grass == "30" ~ 35,
    Unk_Grass == "40" ~ 45,
    Unk_Grass == "50" ~ 55,
    Unk_Grass == "60" ~ 65,
    Unk_Grass == "70" ~ 75,
    Unk_Grass == "80" ~ 85,
    Unk_Grass == "90" ~ 95,
    Unk_Grass == "0" ~ 0,
    Unk_Grass == "" ~ 0,
    is.na(Unk_Grass) ~ 0)) %>% 
  mutate(Unk_Grass = as.double(Unk_Grass)) %>% 
  mutate(Unk_Grass = Unk_Grass/100)

ul <- ul %>% 
  mutate(ALPR = case_when(
    ALPR == "<1" ~ 0.5,
    ALPR == ">99" ~ 99.5,
    ALPR == "1" ~ 5,
    ALPR == "10" ~ 15,
    ALPR == "20" ~ 25,
    ALPR == "30" ~ 35,
    ALPR == "40" ~ 45,
    ALPR == "50" ~ 55,
    ALPR == "60" ~ 65,
    ALPR == "70" ~ 75,
    ALPR == "80" ~ 85,
    ALPR == "90" ~ 95,
    ALPR == "0" ~ 0,
    ALPR == "" ~ 0,
    is.na(ALPR) ~ 0)) %>% 
  mutate(ALPR = as.double(ALPR)) %>% 
  mutate(ALPR = ALPR/100)

ul <- ul %>% 
  mutate(CYDA = case_when(
    CYDA == "<1" ~ 0.5,
    CYDA == ">99" ~ 99.5,
    CYDA == "1" ~ 5,
    CYDA == "10" ~ 15,
    CYDA == "20" ~ 25,
    CYDA == "30" ~ 35,
    CYDA == "40" ~ 45,
    CYDA == "50" ~ 55,
    CYDA == "60" ~ 65,
    CYDA == "70" ~ 75,
    CYDA == "80" ~ 85,
    CYDA == "90" ~ 95,
    CYDA == "0" ~ 0,
    CYDA == "" ~ 0,
    is.na(CYDA) ~ 0)) %>% 
  mutate(CYDA = as.double(CYDA)) %>% 
  mutate(CYDA = CYDA/100)

ul <- ul %>% 
  mutate(POFR = case_when(
    POFR == "<1" ~ 0.5,
    POFR == ">99" ~ 99.5,
    POFR == "1" ~ 5,
    POFR == "10" ~ 15,
    POFR == "20" ~ 25,
    POFR == "30" ~ 35,
    POFR == "40" ~ 45,
    POFR == "50" ~ 55,
    POFR == "60" ~ 65,
    POFR == "70" ~ 75,
    POFR == "80" ~ 85,
    POFR == "90" ~ 95,
    POFR == "0" ~ 0,
    POFR == "" ~ 0,
    is.na(POFR) ~ 0)) %>% 
  mutate(POFR = as.double(POFR)) %>% 
  mutate(POFR = POFR/100)

ul <- ul %>% 
  mutate(SAAM = case_when(
    SAAM == "<1" ~ 0.5,
    SAAM == ">99" ~ 99.5,
    SAAM == "1" ~ 5,
    SAAM == "10" ~ 15,
    SAAM == "20" ~ 25,
    SAAM == "30" ~ 35,
    SAAM == "40" ~ 45,
    SAAM == "50" ~ 55,
    SAAM == "60" ~ 65,
    SAAM == "70" ~ 75,
    SAAM == "80" ~ 85,
    SAAM == "90" ~ 95,
    SAAM == "0" ~ 0,
    SAAM == "" ~ 0,
    is.na(SAAM) ~ 0)) %>% 
  mutate(SAAM = as.double(SAAM)) %>% 
  mutate(SAAM = SAAM/100)

ul <- ul %>% 
  mutate(Unk_Bulrush = case_when(
    Unk_Bulrush == "<1" ~ 0.5,
    Unk_Bulrush == ">99" ~ 99.5,
    Unk_Bulrush == "1" ~ 5,
    Unk_Bulrush == "10" ~ 15,
    Unk_Bulrush == "20" ~ 25,
    Unk_Bulrush == "30" ~ 35,
    Unk_Bulrush == "40" ~ 45,
    Unk_Bulrush == "50" ~ 55,
    Unk_Bulrush == "60" ~ 65,
    Unk_Bulrush == "70" ~ 75,
    Unk_Bulrush == "80" ~ 85,
    Unk_Bulrush == "90" ~ 95,
    Unk_Bulrush == "0" ~ 0,
    Unk_Bulrush == "" ~ 0,
    is.na(Unk_Bulrush) ~ 0)) %>% 
  mutate(Unk_Bulrush = as.double(Unk_Bulrush)) %>% 
  mutate(Unk_Bulrush = Unk_Bulrush/100)

ul <- ul %>% 
  mutate(BY = case_when(
    BY == "<1" ~ 0.5,
    BY == ">99" ~ 99.5,
    BY == "1" ~ 5,
    BY == "10" ~ 15,
    BY == "20" ~ 25,
    BY == "30" ~ 35,
    BY == "40" ~ 45,
    BY == "50" ~ 55,
    BY == "60" ~ 65,
    BY == "70" ~ 75,
    BY == "80" ~ 85,
    BY == "90" ~ 95,
    BY == "0" ~ 0,
    BY == "" ~ 0,
    is.na(BY) ~ 0)) %>% 
  mutate(BY = as.double(BY)) %>% 
  mutate(BY = BY/100)

ul <- ul %>% 
  mutate(SYCI = case_when(
    SYCI == "<1" ~ 0.5,
    SYCI == ">99" ~ 99.5,
    SYCI == "1" ~ 5,
    SYCI == "10" ~ 15,
    SYCI == "20" ~ 25,
    SYCI == "30" ~ 35,
    SYCI == "40" ~ 45,
    SYCI == "50" ~ 55,
    SYCI == "60" ~ 65,
    SYCI == "70" ~ 75,
    SYCI == "80" ~ 85,
    SYCI == "90" ~ 95,
    SYCI == "0" ~ 0,
    SYCI == "" ~ 0,
    is.na(SYCI) ~ 0)) %>% 
  mutate(SYCI = as.double(SYCI)) %>% 
  mutate(SYCI = SYCI/100)

ul <- ul %>% 
  mutate(EUOC = case_when(
    EUOC == "<1" ~ 0.5,
    EUOC == ">99" ~ 99.5,
    EUOC == "1" ~ 5,
    EUOC == "10" ~ 15,
    EUOC == "20" ~ 25,
    EUOC == "30" ~ 35,
    EUOC == "40" ~ 45,
    EUOC == "50" ~ 55,
    EUOC == "60" ~ 65,
    EUOC == "70" ~ 75,
    EUOC == "80" ~ 85,
    EUOC == "90" ~ 95,
    EUOC == "0" ~ 0,
    EUOC == "" ~ 0,
    is.na(EUOC) ~ 0)) %>% 
  mutate(EUOC = as.double(EUOC)) %>% 
  mutate(EUOC = EUOC/100)

ul <- ul %>% 
  mutate(TYPHA = case_when(
    TYPHA == "<1" ~ 0.5,
    TYPHA == ">99" ~ 99.5,
    TYPHA == "1" ~ 5,
    TYPHA == "10" ~ 15,
    TYPHA == "20" ~ 25,
    TYPHA == "30" ~ 35,
    TYPHA == "40" ~ 45,
    TYPHA == "50" ~ 55,
    TYPHA == "60" ~ 65,
    TYPHA == "70" ~ 75,
    TYPHA == "80" ~ 85,
    TYPHA == "90" ~ 95,
    TYPHA == "0" ~ 0,
    TYPHA == "" ~ 0,
    is.na(TYPHA) ~ 0)) %>% 
  mutate(TYPHA = as.double(TYPHA)) %>% 
  mutate(TYPHA = TYPHA/100)

ul <- ul %>% 
  mutate(Tamarisk = case_when(
    Tamarisk == "<1" ~ 0.5,
    Tamarisk == ">99" ~ 99.5,
    Tamarisk == "1" ~ 5,
    Tamarisk == "10" ~ 15,
    Tamarisk == "20" ~ 25,
    Tamarisk == "30" ~ 35,
    Tamarisk == "40" ~ 45,
    Tamarisk == "50" ~ 55,
    Tamarisk == "60" ~ 65,
    Tamarisk == "70" ~ 75,
    Tamarisk == "80" ~ 85,
    Tamarisk == "90" ~ 95,
    Tamarisk == "0" ~ 0,
    Tamarisk == "" ~ 0,
    is.na(Tamarisk) ~ 0)) %>% 
  mutate(Tamarisk = as.double(Tamarisk)) %>% 
  mutate(Tamarisk = Tamarisk/100)

ul <- ul %>% 
  mutate(POPE = case_when(
    POPE == "<1" ~ 0.5,
    POPE == ">99" ~ 99.5,
    POPE == "1" ~ 5,
    POPE == "10" ~ 15,
    POPE == "20" ~ 25,
    POPE == "30" ~ 35,
    POPE == "40" ~ 45,
    POPE == "50" ~ 55,
    POPE == "60" ~ 65,
    POPE == "70" ~ 75,
    POPE == "80" ~ 85,
    POPE == "90" ~ 95,
    POPE == "0" ~ 0,
    POPE == "" ~ 0,
    is.na(POPE) ~ 0)) %>% 
  mutate(POPE = as.double(POPE)) %>% 
  mutate(POPE = POPE/100)

glimpse(ul)

#check the measurements
min(ul$Measurement.1)
max(ul$Measurement.1)

min(ul$Measurement.2)
max(ul$Measurement.2)

min(ul$Measurement.3, na.rm = TRUE)
max(ul$Measurement.3, na.rm = TRUE)


#Add a new section for invasives and natives
ul <- ul %>% 
  rowwise() %>% 
  mutate(Invasive.Cover = sum(PHAU, TYPHA, Tamarisk, ALPR, CYDA, BY, 
                              BASC, LASE, na.rm = T),
         Native.Cover = sum(Unk_Bulrush, BOMA, BICE, CYER, RUMA, Cheno, 
                            SCAC, SCAM,
                            SCPU, DISP, RACY, ASIN, SYCI, EUOC, POPE, 
                            POFR, SAAM, na.rm = T))

max(ul$Invasive.Cover, na.rm = TRUE)
max(ul$Native.Cover, na.rm = TRUE)

#change NAs and 0s to a trace value so it can be used with the beta
#trace value should be at least half the smallest recorded value
min(ul$Invasive.Cover, na.rm = TRUE)
ul$Invasive.Cover[is.na(ul$Invasive.Cover)] <- 0.0025
ul$Invasive.Cover[ul$Invasive.Cover==0] <- 0.0025

min(ul$Native.Cover, na.rm = TRUE)
ul$Native.Cover[is.na(ul$Native.Cover)] <- 0.0025
ul$Native.Cover[ul$Native.Cover==0] <- 0.0025

#Clean Farmington Bay 2023 Data####
#View(fb23)
glimpse(fb23)

#rename block and make a factor
names(fb23)[1] <- "Block"
fb23$Block <- as.factor(fb23$Block)

#make everything a factor, and rename C group to 10 to make easier to follow
fb23$Plot <- as.factor(fb23$Plot)
fb23$Group[fb23$Group == "C"] <- 10
fb23$Group <- as.factor(fb23$Group)
fb23$Density <- as.factor(fb23$Density)

#fix the date
fb23$Date <- lubridate::mdy(fb23$Date)

#change cover values to the midpoints, make numeric, and make a decimal
fb23 <- fb23 %>% 
  mutate(Total.Cover = case_when(
    Total.Cover == "<1" ~ 0.5,
    Total.Cover == ">99" ~ 99.5,
    Total.Cover == "1" ~ 5,
    Total.Cover == "10" ~ 15,
    Total.Cover == "20" ~ 25,
    Total.Cover == "30" ~ 35,
    Total.Cover == "40" ~ 45,
    Total.Cover == "50" ~ 55,
    Total.Cover == "60" ~ 65,
    Total.Cover == "70" ~ 75,
    Total.Cover == "80" ~ 85,
    Total.Cover == "90" ~ 95,
    Total.Cover == "0" ~ 0.25, #add in a trace amount
    Total.Cover == "" ~ 0.25,
    is.na(Total.Cover) ~ 0.25)) %>% 
  mutate(Total.Cover = as.double(Total.Cover)) %>% 
  mutate(Total.Cover = Total.Cover/100)

#now do it for all the other columns
fb23 <- fb23 %>% 
  mutate(PHAU = case_when(
    PHAU == "<1" ~ 0.5,
    PHAU == ">99" ~ 99.5,
    PHAU == "1" ~ 5,
    PHAU == "10" ~ 15,
    PHAU == "20" ~ 25,
    PHAU == "30" ~ 35,
    PHAU == "4" ~ 45, #fix a mistake in the data
    PHAU == "40" ~ 45,
    PHAU == "50" ~ 55,
    PHAU == "60" ~ 65,
    PHAU == "70" ~ 75,
    PHAU == "80" ~ 85,
    PHAU == "90" ~ 95,
    PHAU == "0" ~ 0.25, #add in a trace amount
    PHAU == "" ~ 0.25,
    is.na(PHAU) ~ 0.25)) %>% 
  mutate(PHAU = as.double(PHAU)) %>% 
  mutate(PHAU = PHAU/100)

fb23 <- fb23 %>% 
  mutate(Typha = case_when(
    Typha == "<1" ~ 0.5,
    Typha == ">99" ~ 99.5,
    Typha == "1" ~ 5,
    Typha == "10" ~ 15,
    Typha == "20" ~ 25,
    Typha == "30" ~ 35,
    Typha == "40" ~ 45,
    Typha == "50" ~ 55,
    Typha == "60" ~ 65,
    Typha == "70" ~ 75,
    Typha == "80" ~ 85,
    Typha == "90" ~ 95,
    Typha == "0" ~ 0.25, #add in a trace amount
    Typha == "" ~ 0.25,
    is.na(Typha) ~ 0.25)) %>% 
  mutate(Typha = as.double(Typha)) %>% 
  mutate(Typha = Typha/100)

fb23 <- fb23 %>% 
  mutate(BOMA = case_when(
    BOMA == "<1" ~ 0.5,
    BOMA == ">99" ~ 99.5,
    BOMA == "1" ~ 5,
    BOMA == "10" ~ 15,
    BOMA == "20" ~ 25,
    BOMA == "30" ~ 35,
    BOMA == "40" ~ 45,
    BOMA == "50" ~ 55,
    BOMA == "60" ~ 65,
    BOMA == "70" ~ 75,
    BOMA == "80" ~ 85,
    BOMA == "90" ~ 95,
    BOMA == "0" ~ 0.25, #add in a trace amount
    BOMA == "" ~ 0.25,
    is.na(BOMA) ~ 0.25)) %>% 
  mutate(BOMA = as.double(BOMA)) %>% 
  mutate(BOMA = BOMA/100)

fb23 <- fb23 %>% 
  mutate(DISP = case_when(
    DISP == "<1" ~ 0.5,
    DISP == ">99" ~ 99.5,
    DISP == "1" ~ 5,
    DISP == "10" ~ 15,
    DISP == "20" ~ 25,
    DISP == "30" ~ 35,
    DISP == "40" ~ 45,
    DISP == "50" ~ 55,
    DISP == "60" ~ 65,
    DISP == "70" ~ 75,
    DISP == "80" ~ 85,
    DISP == "90" ~ 95,
    DISP == "0" ~ 0.25, #add in a trace amount
    DISP == "" ~ 0.25,
    is.na(DISP) ~ 0.25)) %>% 
  mutate(DISP = as.double(DISP)) %>% 
  mutate(DISP = DISP/100)

fb23 <- fb23 %>% 
  mutate(SCAC = case_when(
    SCAC == "<1" ~ 0.5,
    SCAC == ">99" ~ 99.5,
    SCAC == "1" ~ 5,
    SCAC == "10" ~ 15,
    SCAC == "20" ~ 25,
    SCAC == "30" ~ 35,
    SCAC == "40" ~ 45,
    SCAC == "50" ~ 55,
    SCAC == "60" ~ 65,
    SCAC == "70" ~ 75,
    SCAC == "80" ~ 85,
    SCAC == "90" ~ 95,
    SCAC == "0" ~ 0.25, #add in a trace amount
    SCAC == "" ~ 0.25,
    is.na(SCAC) ~ 0.25)) %>% 
  mutate(SCAC = as.double(SCAC)) %>% 
  mutate(SCAC = SCAC/100)

fb23 <- fb23 %>% 
  mutate(SCAM = case_when(
    SCAM == "<1" ~ 0.5,
    SCAM == ">99" ~ 99.5,
    SCAM == "1" ~ 5,
    SCAM == "10" ~ 15,
    SCAM == "20" ~ 25,
    SCAM == "30" ~ 35,
    SCAM == "40" ~ 45,
    SCAM == "50" ~ 55,
    SCAM == "60" ~ 65,
    SCAM == "70" ~ 75,
    SCAM == "80" ~ 85,
    SCAM == "90" ~ 95,
    SCAM == "0" ~ 0.25, #add in a trace amount
    SCAM == "" ~ 0.25,
    is.na(SCAM) ~ 0.25)) %>% 
  mutate(SCAM = as.double(SCAM)) %>% 
  mutate(SCAM = SCAM/100)

fb23 <- fb23 %>% 
  mutate(RUMA = case_when(
    RUMA == "<1" ~ 0.5,
    RUMA == ">99" ~ 99.5,
    RUMA == "1" ~ 5,
    RUMA == "10" ~ 15,
    RUMA == "20" ~ 25,
    RUMA == "30" ~ 35,
    RUMA == "40" ~ 45,
    RUMA == "50" ~ 55,
    RUMA == "60" ~ 65,
    RUMA == "70" ~ 75,
    RUMA == "80" ~ 85,
    RUMA == "90" ~ 95,
    RUMA == "0" ~ 0.25, #add in a trace amount
    RUMA == "" ~ 0.25,
    is.na(RUMA) ~ 0.25)) %>% 
  mutate(RUMA = as.double(RUMA)) %>% 
  mutate(RUMA = RUMA/100)

fb23 <- fb23 %>% 
  mutate(RUST = case_when(
    RUST == "<1" ~ 0.5,
    RUST == ">99" ~ 99.5,
    RUST == "1" ~ 5,
    RUST == "10" ~ 15,
    RUST == "20" ~ 25,
    RUST == "30" ~ 35,
    RUST == "40" ~ 45,
    RUST == "50" ~ 55,
    RUST == "60" ~ 65,
    RUST == "70" ~ 75,
    RUST == "80" ~ 85,
    RUST == "90" ~ 95,
    RUST == "0" ~ 0.25, #add in a trace amount
    RUST == "" ~ 0.25,
    is.na(RUST) ~ 0.25)) %>% 
  mutate(RUST = as.double(RUST)) %>% 
  mutate(RUST = RUST/100)

glimpse(fb23)

#check measurements to make sure they make sense
min(fb23$Measurement.1)
max(fb23$Measurement.1)

min(fb23$Measurement.2)
max(fb23$Measurement.2)

min(fb23$Measurement.3)
max(fb23$Measurement.3)

#Add a new section for invasives and natives
fb23 <- fb23 %>% 
  rowwise() %>% 
  mutate(Invasive.Cover = sum(PHAU, Typha, RUST, na.rm = T),
         Native.Cover = sum(BOMA, SCAC, SCAM, RUMA, DISP, na.rm = T))

max(fb23$Invasive.Cover, na.rm = T)
max(fb23$Native.Cover, na.rm = T)

save(ul, fb, fb23, file = "clean_dfs.RData")


#Clean the well data####
wells_ul <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/UL_wells.csv")
wells_fb <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/FB_wells.csv")
wells_2023 <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/2023_wells.csv")

##Farmington Bay####
#glimpse(wells_fb)

#Fix the names of important columns
names(wells_fb)[11] <- "depth_cm"

#make date a date
wells_fb$Date <- lubridate::mdy(wells_fb$Date)

#make block a factor
wells_fb$Block <- as.factor(wells_fb$Block)

##Ul####
#glimpse(wells_ul)

#Fix the names of important columns
names(wells_ul)[11] <- "depth_cm"

#make date a date
wells_ul$Date <- lubridate::mdy(wells_ul$Date)

#make block a factor
wells_ul$Block <- as.factor(wells_ul$Block)

##2023####
#glimpse(wells_2023)

#Fix the names of important columns
names(wells_2023)[8] <- "depth_cm"

#make date a date
wells_2023$Date <- lubridate::mdy(wells_2023$Date)

#make block a factor
wells_2023$Block <- as.factor(wells_2023$Block)

save(wells_ul, wells_fb, wells_2023, file = "wells.RData")

