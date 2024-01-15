#import data
fb <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/clean_fb.csv")
ul <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/clean_ul.csv")


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
unique(fb$Total.Cover)
fb$Total.Cover[fb$Total.Cover == "<1"] <- "0.5"
fb$Total.Cover[fb$Total.Cover == 1.00] <- 5.00
fb$Total.Cover[fb$Total.Cover == 10.00] <- 15.00
fb$Total.Cover[fb$Total.Cover == 20.00] <- 25.00
fb$Total.Cover[fb$Total.Cover == 30.00] <- 35.00
fb$Total.Cover[fb$Total.Cover == 40.00] <- 45.00
fb$Total.Cover[fb$Total.Cover == 50.00] <- 55.00
fb$Total.Cover[fb$Total.Cover == 60.00] <- 65.00
fb$Total.Cover[fb$Total.Cover == 70.00] <- 75.00
fb$Total.Cover <- as.double(fb$Total.Cover)
fb$Total.Cover <- fb$Total.Cover/100

#all NAs should actually be 0s
fb$Total.Cover[is.na(fb$Total.Cover) ]<- 0

#now do it for all the other columns
unique(fb$PHAU)
fb$PHAU[fb$PHAU == "<1"] <- "0.5"
fb$PHAU[fb$PHAU == 1.00] <- 5.00
fb$PHAU[fb$PHAU == 10.00] <- 15.00
fb$PHAU <- as.double(fb$PHAU)
fb$PHAU <- fb$PHAU/100
fb$PHAU[is.na(fb$PHAU)] <- 0

unique(fb$Cheno)
fb$Cheno[fb$Cheno == "<1"] <- "0.5"
fb$Cheno[fb$Cheno == 1.00] <- 5.00
fb$Cheno[fb$Cheno == 10.00] <- 15.00
fb$Cheno[fb$Cheno == 20.00] <- 25.00
fb$Cheno[fb$Cheno == 30.00] <- 35.00
fb$Cheno <- as.double(fb$Cheno)
fb$Cheno <- fb$Cheno/100
fb$Cheno[is.na(fb$Cheno)] <- 0

unique(fb$Typha)
fb$Typha[fb$Typha == "<1"] <- "0.5"
fb$Typha[fb$Typha == 1.00] <- 5.00
fb$Typha[fb$Typha == 10.00] <- 15.00
fb$Typha <- as.double(fb$Typha)
fb$Typha <- fb$Typha/100
fb$Typha[is.na(fb$Typha)] <- 0

unique(fb$BOMA)
fb$BOMA[fb$BOMA == "<1"] <- "0.5"
fb$BOMA[fb$BOMA == 1.00] <- 5.00
fb$BOMA[fb$BOMA == 10.00] <- 15.00
fb$BOMA[fb$BOMA == 20.00] <- 25.00
fb$BOMA <- as.double(fb$BOMA)
fb$BOMA <- fb$BOMA/100
fb$BOMA[is.na(fb$BOMA)] <- 0

unique(fb$DISP)
fb$DISP[fb$DISP == "<1"] <- "0.5"
fb$DISP[fb$DISP == 1.00] <- 5.00
fb$DISP[fb$DISP == 10.00] <- 15.00
fb$DISP[fb$DISP == 20.00] <- 25.00
fb$DISP[fb$DISP == 30.00] <- 35.00
fb$DISP[fb$DISP == 40.00] <- 45.00
fb$DISP[fb$DISP == 50.00] <- 55.00
fb$DISP[fb$DISP == 60.00] <- 65.00
fb$DISP <- as.double(fb$DISP)
fb$DISP <- fb$DISP/100
fb$DISP[is.na(fb$DISP)] <- 0

unique(fb$EUMA)
fb$EUMA[fb$EUMA == "<1"] <- "0.5"
fb$EUMA[fb$EUMA == 1.00] <- 5.00
fb$EUMA <- as.double(fb$EUMA)
fb$EUMA <- fb$EUMA/100
fb$EUMA[is.na(fb$EUMA)] <- 0

unique(fb$SYCI)
fb$SYCI[fb$SYCI == "<1"] <- "0.5"
fb$SYCI[fb$SYCI == 1.00] <- 5.00
fb$SYCI[fb$SYCI == 10.00] <- 15.00
fb$SYCI <- as.double(fb$SYCI)
fb$SYCI <- fb$SYCI/100
fb$SYCI[is.na(fb$SYCI)] <- 0

unique(fb$LEFA)
fb$LEFA[fb$LEFA == "<1"] <- "0.5"
fb$LEFA[fb$LEFA == 1.00] <- 5.00
fb$LEFA[fb$LEFA == 10.00] <- 15.00
fb$LEFA[fb$LEFA == 20.00] <- 25.00
fb$LEFA <- as.double(fb$LEFA)
fb$LEFA <- fb$LEFA/100
fb$LEFA[is.na(fb$LEFA)] <- 0

unique(fb$SCAC)
fb$SCAC[fb$SCAC == "<1"] <- "0.5"
fb$SCAC[fb$SCAC == 1.00] <- 5.00
fb$SCAC <- as.double(fb$SCAC)
fb$SCAC <- fb$SCAC/100
fb$SCAC[is.na(fb$SCAC)] <- 0

unique(fb$BICE)
fb$BICE[fb$BICE == "<1"] <- "0.5"
fb$BICE[fb$BICE == 1.00] <- 5.00
fb$BICE[fb$BICE == 10.00] <- 15.00
fb$BICE <- as.double(fb$BICE)
fb$BICE <- fb$BICE/100
fb$BICE[is.na(fb$BICE)] <- 0

unique(fb$BIFR)
fb$BIFR[fb$BIFR == "<1"] <- "0.5"
fb$BIFR[fb$BIFR == 1.00] <- 5.00
fb$BIFR <- as.double(fb$BIFR)
fb$BIFR <- fb$BIFR/100
fb$BIFR[is.na(fb$BIFR)] <- 0

unique(fb$EUOC)
fb$EUOC[fb$EUOC == 1.00] <- 5.00
fb$EUOC <- as.double(fb$EUOC)
fb$EUOC <- fb$EUOC/100
fb$EUOC[is.na(fb$EUOC)] <- 0

unique(fb$MUAS)
fb$MUAS[fb$MUAS == 1.00] <- 5.00
fb$MUAS[fb$MUAS == 10.00] <- 15.00
fb$MUAS <- as.double(fb$MUAS)
fb$MUAS <- fb$MUAS/100
fb$MUAS[is.na(fb$MUAS)] <- 0

unique(fb$SCAM)
fb$SCAM[fb$SCAM == 1.00] <- 5.00
fb$SCAM <- as.double(fb$SCAM)
fb$SCAM <- fb$SCAM/100
fb$SCAM[is.na(fb$SCAM)] <- 0

unique(fb$RUMA)
fb$RUMA[fb$RUMA == "<1"] <- "0.5"
fb$RUMA[fb$RUMA == 1.00] <- 5.00
fb$RUMA <- as.double(fb$RUMA)
fb$RUMA <- fb$RUMA/100
fb$RUMA[is.na(fb$RUMA)] <- 0

unique(fb$RUST)
fb$RUST[fb$RUST == "<1"] <- "0.5"
fb$RUST[fb$RUST == 1.00] <- 5.00
fb$RUST[fb$RUST == 10.00] <- 15.00
fb$RUST <- as.double(fb$RUST)
fb$RUST <- fb$RUST/100
fb$RUST[is.na(fb$RUST)] <- 0

unique(fb$Unk_Forb)
fb$Unk_Forb[fb$Unk_Forb == "<1"] <- "0.5"
fb$Unk_Forb[fb$Unk_Forb == 1.00] <- 5.00
fb$Unk_Forb <- as.double(fb$Unk_Forb)
fb$Unk_Forb <- fb$Unk_Forb/100
fb$Unk_Forb[is.na(fb$Unk_Forb)] <- 0

unique(fb$Unk_Grass)
fb$Unk_Grass[fb$Unk_Grass == "<1"] <- "0.5"
fb$Unk_Grass[fb$Unk_Grass == 1.00] <- 5.00
fb$Unk_Grass <- as.double(fb$Unk_Grass)
fb$Unk_Grass <- fb$Unk_Grass/100
fb$Unk_Grass[is.na(fb$Unk_Grass)] <- 0

unique(fb$Unk_Rush)
fb$Unk_Rush[fb$Unk_Rush == "<1"] <- "0.5"
fb$Unk_Rush[fb$Unk_Rush == 1.00] <- 5.00
fb$Unk_Rush <- as.double(fb$Unk_Rush)
names(fb)[25] <- "Unk_Bulrush"
fb$Unk_Bulrush <- fb$Unk_Bulrush/100
fb$Unk_Bulrush[is.na(fb$Unk_Bulrush)] <- 0

unique(fb$SARU)
fb$SARU[fb$SARU == "<1"] <- "0.5"
fb$SARU[fb$SARU == 1.00] <- 5.00
fb$SARU <- as.double(fb$SARU)
fb$SARU <- fb$SARU/100
fb$SARU[is.na(fb$SARU)] <- 0

unique(fb$Tamarisk)
fb$Tamarisk[fb$Tamarisk == "<1"] <- "0.5"
fb$Tamarisk[fb$Tamarisk == 1.00] <- 5.00
fb$Tamarisk <- as.double(fb$Tamarisk)
fb$Tamarisk <- fb$Tamarisk/100
fb$Tamarisk[is.na(fb$Tamarisk)] <- 0

glimpse(fb)

#check measurements to make sure they make sense
min(fb$Measurement.1)
max(fb$Measurement.1)

min(fb$Measurement.2)
max(fb$Measurement.2)

min(fb$Measurement.3)
max(fb$Measurement.3)

#Add a new section for invasives and natives
df <- fb %>%
  dplyr::select("PHAU", "Typha", "RUST", "Tamarisk") %>%
  mutate(Invasive = rowSums(.,na.rm = T))
df1 <- fb %>%
  dplyr::select("Cheno", "BOMA", "DISP", "EUMA", "SYCI", "LEFA", "SCAC",
         "BICE", "BIFR", "EUOC", "MUAS", "SCAM", "RUMA", "Unk_Bulrush", 
         "SARU") %>%
  mutate(Native = rowSums(.,na.rm = T))

fb$Invasive.Cover <- df$Invasive
fb$Native.Cover <- df1$Native

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
unique(ul$Total.Cover)
ul$Total.Cover[ul$Total.Cover == "<1"] <- "0.5"
ul$Total.Cover[ul$Total.Cover == ">1"] <- "0.5"
ul$Total.Cover[ul$Total.Cover == 1.00] <- 5.00
ul$Total.Cover[ul$Total.Cover == 10.00] <- 15.00
ul$Total.Cover[ul$Total.Cover == 20.00] <- 25.00
ul$Total.Cover[ul$Total.Cover == 30.00] <- 35.00
ul$Total.Cover[ul$Total.Cover == 40.00] <- 45.00
ul$Total.Cover[ul$Total.Cover == 50.00] <- 55.00
ul$Total.Cover[ul$Total.Cover == 60.00] <- 65.00
ul$Total.Cover[ul$Total.Cover == 70.00] <- 75.00
ul$Total.Cover[ul$Total.Cover == 80.00] <- 85.00
ul$Total.Cover[ul$Total.Cover == 90.00] <- 95.00
ul$Total.Cover <- as.double(ul$Total.Cover)
ul$Total.Cover <- ul$Total.Cover/100
ul$Total.Cover[is.na(ul$Total.Cover)] <- 0

names(ul)[7] <- "Unk_Forb" #fix the name
unique(ul$Unk_Forb)
ul$Unk_Forb[ul$Unk_Forb == "<1"] <- "0.5"
ul$Unk_Forb[ul$Unk_Forb == 1.00] <- 5.00
ul$Unk_Forb <- as.double(ul$Unk_Forb)
ul$Unk_Forb <- ul$Unk_Forb/100
ul$Unk_Forb[is.na(ul$Unk_Forb)] <- 0

unique(ul$PHAU)
ul$PHAU[ul$PHAU == "<1"] <- "0.5"
ul$PHAU[ul$PHAU == 1.00] <- 5.00
ul$PHAU[ul$PHAU == 10.00] <- 15.00
ul$PHAU <- as.double(ul$PHAU)
ul$PHAU <- ul$PHAU/100
ul$PHAU[is.na(ul$PHAU)] <- 0

unique(ul$Unk_Sedge)
ul$Unk_Sedge[ul$Unk_Sedge == "<1"] <- "0.5"
ul$Unk_Sedge[ul$Unk_Sedge == 1.00] <- 5.00
ul$Unk_Sedge[ul$Unk_Sedge == 10.00] <- 15.00
ul$Unk_Sedge[ul$Unk_Sedge == 20.00] <- 25.00
ul$Unk_Sedge[ul$Unk_Sedge == 30.00] <- 35.00
ul$Unk_Sedge[ul$Unk_Sedge == 40.00] <- 45.00
ul$Unk_Sedge <- as.double(ul$Unk_Sedge)
ul$Unk_Sedge <- ul$Unk_Sedge/100
ul$Unk_Sedge[is.na(ul$Unk_Sedge)] <- 0

unique(ul$BOMA)
ul$BOMA[ul$BOMA == "<1"] <- "0.5"
ul$BOMA[ul$BOMA == 1.00] <- 5.00
ul$BOMA[ul$BOMA == 10.00] <- 15.00
ul$BOMA[ul$BOMA == 20.00] <- 25.00
ul$BOMA[ul$BOMA == 30.00] <- 35.00
ul$BOMA[ul$BOMA == 40.00] <- 45.00
ul$BOMA[ul$BOMA == 50.00] <- 55.00
ul$BOMA <- as.double(ul$BOMA)
ul$BOMA <- ul$BOMA/100
ul$BOMA[is.na(ul$BOMA)] <- 0

unique(ul$BICE)
ul$BICE[ul$BICE == 1.00] <- 5.00
ul$BICE[ul$BICE == 10.00] <- 15.00
ul$BICE[ul$BICE == 20.00] <- 25.00
ul$BICE[ul$BICE == 30.00] <- 35.00
ul$BICE <- as.double(ul$BICE)
ul$BICE <- ul$BICE/100
ul$BICE[is.na(ul$BICE)] <- 0

unique(ul$CYER)
ul$CYER[ul$CYER == 1.00] <- 5.00
ul$CYER[ul$CYER == 10.00] <- 15.00
ul$CYER[ul$CYER == 20.00] <- 25.00
ul$CYER[ul$CYER == 30.00] <- 35.00
ul$CYER[ul$CYER == 40.00] <- 45.00
ul$CYER[ul$CYER == 60.00] <- 65.00
ul$CYER[ul$CYER == 70.00] <- 75.00
ul$CYER <- as.double(ul$CYER)
ul$CYER <- ul$CYER/100
ul$CYER[is.na(ul$CYER)] <- 0

unique(ul$RUMA)
ul$RUMA[ul$RUMA == "<1"] <- "0.5"
ul$RUMA[ul$RUMA == 1.00] <- 5.00
ul$RUMA[ul$RUMA == 10.00] <- 15.00
ul$RUMA[ul$RUMA == 20.00] <- 25.00
ul$RUMA[ul$RUMA == 30.00] <- 35.00
ul$RUMA[ul$RUMA == 40.00] <- 45.00
ul$RUMA[ul$RUMA == 50.00] <- 55.00
ul$RUMA[ul$RUMA == 60.00] <- 65.00
ul$RUMA[ul$RUMA == 70.00] <- 75.00
ul$RUMA[ul$RUMA == 80.00] <- 85.00
ul$RUMA <- as.double(ul$RUMA)
ul$RUMA <- ul$RUMA/100
ul$RUMA[is.na(ul$RUMA)] <- 0

unique(ul$BASC)
ul$BASC[ul$BASC == 1.00] <- 5.00
ul$BASC[ul$BASC == 10.00] <- 15.00
ul$BASC <- as.double(ul$BASC)
ul$BASC <- ul$BASC/100
ul$BASC[is.na(ul$BASC)] <- 0

unique(ul$LASE)
ul$LASE[ul$LASE == 1.00] <- 5.00
ul$LASE[ul$LASE == 10.00] <- 15.00
ul$LASE <- as.double(ul$LASE)
ul$LASE <- ul$LASE/100
ul$LASE[is.na(ul$LASE)] <- 0

unique(ul$Cheno)
ul$Cheno[ul$Cheno == "<1"] <- "0.5"
ul$Cheno[ul$Cheno == 1.00] <- 5.00
ul$Cheno[ul$Cheno == 10.00] <- 15.00
ul$Cheno[ul$Cheno == 20.00] <- 25.00
ul$Cheno[ul$Cheno == 30.00] <- 35.00
ul$Cheno[ul$Cheno == 40.00] <- 45.00
ul$Cheno[ul$Cheno == 50.00] <- 55.00
ul$Cheno <- as.double(ul$Cheno)
ul$Cheno <- ul$Cheno/100
ul$Cheno[is.na(ul$Cheno)] <- 0

unique(ul$SCAC)
ul$SCAC[ul$SCAC == "<1"] <- "0.5"
ul$SCAC[ul$SCAC == 1.00] <- 5.00
ul$SCAC <- as.double(ul$SCAC)
ul$SCAC <- ul$SCAC/100
ul$SCAC[is.na(ul$SCAC)] <- 0

unique(ul$SCPU)
ul$SCPU[ul$SCPU == "<1"] <- "0.5"
ul$SCPU[ul$SCPU == 1.00] <- 5.00
ul$SCPU <- as.double(ul$SCPU)
ul$SCPU <- ul$SCPU/100
ul$SCPU[is.na(ul$SCPU)] <- 0

unique(ul$SCAM)
ul$SCAM[ul$SCAM == "<1"] <- "0.5"
ul$SCAM[ul$SCAM == 1.00] <- 5.00
ul$SCAM[ul$SCAM == 10.00] <- 15.00
ul$SCAM <- as.double(ul$SCAM)
ul$SCAM <- ul$SCAM/100
ul$SCAM[is.na(ul$SCAM)] <- 0

unique(ul$DISP)
ul$DISP[ul$DISP == "<1"] <- "0.5"
ul$DISP[ul$DISP == 1.00] <- 5.00
ul$DISP[ul$DISP == 10.00] <- 15.00
ul$DISP <- as.double(ul$DISP)
ul$DISP <- ul$DISP/100
ul$DISP[is.na(ul$DISP)] <- 0

unique(ul$RACY)
ul$RACY[ul$RACY == "<1"] <- "0.5"
ul$RACY[ul$RACY == 1.00] <- 5.00
ul$RACY <- as.double(ul$RACY)
ul$RACY <- ul$RACY/100
ul$RACY[is.na(ul$RACY)] <- 0

unique(ul$ASIN)
ul$ASIN[ul$ASIN == "<1"] <- "0.5"
ul$ASIN[ul$ASIN == 1.00] <- 5.00
ul$ASIN <- as.double(ul$ASIN)
ul$ASIN <- ul$ASIN/100
ul$ASIN[is.na(ul$ASIN)] <- 0

unique(ul$Unk_Grass)
ul$Unk_Grass[ul$Unk_Grass == "<1"] <- "0.5"
ul$Unk_Grass[ul$Unk_Grass == 1.00] <- 5.00
ul$Unk_Grass[ul$Unk_Grass == 10.00] <- 15.00
ul$Unk_Grass[ul$Unk_Grass == 20.00] <- 25.00
ul$Unk_Grass <- as.double(ul$Unk_Grass)
ul$Unk_Grass <- ul$Unk_Grass/100
ul$Unk_Grass[is.na(ul$Unk_Grass)] <- 0

unique(ul$ALPR)
ul$ALPR[ul$ALPR == "<1"] <- "0.5"
ul$ALPR[ul$ALPR == 1.00] <- 5.00
ul$ALPR[ul$ALPR == 10.00] <- 15.00
ul$ALPR[ul$ALPR == 20.00] <- 25.00
ul$ALPR <- as.double(ul$ALPR)
ul$ALPR <- ul$ALPR/100
ul$ALPR[is.na(ul$ALPR)] <- 0

unique(ul$CYDA)
ul$CYDA[ul$CYDA == 1.00] <- 5.00
ul$CYDA[ul$CYDA == 10.00] <- 15.00
ul$CYDA <- as.double(ul$CYDA)
ul$CYDA <- ul$CYDA/100
ul$CYDA[is.na(ul$CYDA)] <- 0

unique(ul$POFR)
ul$POFR[ul$POFR == "<1"] <- "0.5"
ul$POFR[ul$POFR == 1.00] <- 5.00
ul$POFR <- as.double(ul$POFR)
ul$POFR <- ul$POFR/100
ul$POFR[is.na(ul$POFR)] <- 0

unique(ul$SAAM)
ul$SAAM[ul$SAAM == "<1"] <- "0.5"
ul$SAAM[ul$SAAM == 1.00] <- 5.00
ul$SAAM <- as.double(ul$SAAM)
ul$SAAM <- ul$SAAM/100
ul$SAAM[is.na(ul$SAAM)] <- 0

unique(ul$Unk_Bulrush)
ul$Unk_Bulrush[ul$Unk_Bulrush == "<1"] <- "0.5"
ul$Unk_Bulrush[ul$Unk_Bulrush == 1.00] <- 5.00
ul$Unk_Bulrush <- as.double(ul$Unk_Bulrush)
ul$Unk_Bulrush <- ul$Unk_Bulrush/100
ul$Unk_Bulrush[is.na(ul$Unk_Bulrush)] <- 0

unique(ul$BY)
ul$BY[ul$BY == 1.00] <- 5.00
ul$BY <- as.double(ul$BY)
ul$BY <- ul$BY/100
ul$BY[is.na(ul$BY)] <- 0

unique(ul$SYCI)
ul$SYCI[ul$SYCI == 1.00] <- 5.00
ul$SYCI[ul$SYCI == 10.00] <- 15.00
ul$SYCI <- as.double(ul$SYCI)
ul$SYCI <- ul$SYCI/100
ul$SYCI[is.na(ul$SYCI)] <- 0

unique(ul$EUOC)
ul$EUOC[ul$EUOC == "<1"] <- "0.5"
ul$EUOC[ul$EUOC == 1.00] <- 5.00
ul$EUOC[ul$EUOC == 10.00] <- 15.00
ul$EUOC <- as.double(ul$EUOC)
ul$EUOC <- ul$EUOC/100
ul$EUOC[is.na(ul$EUOC)] <- 0

unique(ul$TYPHA)
ul$TYPHA[ul$TYPHA == "<1"] <- "0.5"
ul$TYPHA[ul$TYPHA == 1.00] <- 5.00
ul$TYPHA <- as.double(ul$TYPHA)
ul$TYPHA <- ul$TYPHA/100
ul$TYPHA[is.na(ul$TYPHA)] <- 0

unique(ul$Tamarisk)
ul$Tamarisk[ul$Tamarisk == "<1"] <- "0.5"
ul$Tamarisk[ul$Tamarisk == 1.00] <- 5.00
ul$Tamarisk[ul$Tamarisk == 10.00] <- 15.00
ul$Tamarisk <- as.double(ul$Tamarisk)
ul$Tamarisk <- ul$Tamarisk/100
ul$Tamarisk[is.na(ul$Tamarisk)] <- 0

unique(ul$POPE)
ul$POPE[ul$POPE == "<1"] <- "0.5"
ul$POPE[ul$POPE == 1.00] <- 5.00
ul$POPE <- as.double(ul$POPE)
ul$POPE <- ul$POPE/100
ul$POPE[is.na(ul$POPE)] <- 0

glimpse(ul)

#check the measurements
min(ul$Measurement.1)
max(ul$Measurement.1)

min(ul$Measurement.2)
max(ul$Measurement.2)

min(ul$Measurement.3, na.rm = TRUE)
max(ul$Measurement.3, na.rm = TRUE)


#Add a new section for invasives and natives
df <- ul %>%
  dplyr::select("PHAU", "TYPHA", "Tamarisk", "ALPR", "CYDA", "BY", 
         "BASC", "LASE") %>%
  mutate(Invasive = rowSums(.,na.rm = T))
df1 <- ul %>%
  dplyr::select("Unk_Bulrush", "BOMA", "BICE", 'CYER', 'RUMA', 'Cheno', 'SCAC', 'SCAM',
         'SCPU', 'DISP', 'RACY', 'ASIN', 'SYCI', 'EUOC', 'POPE', 'POFR', 'SAAM') %>%
  mutate(Native = rowSums(.,na.rm = T))

ul$Invasive.Cover <- df$Invasive
ul$Native.Cover <- df1$Native

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
fb23 <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/FB2023-CLEAN.csv")

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
unique(fb23$Total.Cover)
fb23$Total.Cover[fb23$Total.Cover == "<1"] <- 0.5
fb23$Total.Cover[fb23$Total.Cover == 0] <- 0.25
fb23$Total.Cover[fb23$Total.Cover == 1.00] <- 5.00
fb23$Total.Cover[fb23$Total.Cover == 10.00] <- 15.00
fb23$Total.Cover[fb23$Total.Cover == 20.00] <- 25.00
fb23$Total.Cover[fb23$Total.Cover == 30.00] <- 35.00
fb23$Total.Cover[fb23$Total.Cover == 40.00] <- 45.00
fb23$Total.Cover[fb23$Total.Cover == 50.00] <- 55.00
fb23$Total.Cover[fb23$Total.Cover == 60.00] <- 65.00
fb23$Total.Cover[fb23$Total.Cover == 70.00] <- 75.00
fb23$Total.Cover[fb23$Total.Cover == 80.00] <- 85.00
fb23$Total.Cover[fb23$Total.Cover == 90.00] <- 95.00
fb23$Total.Cover <- as.double(fb23$Total.Cover)
fb23$Total.Cover <- fb23$Total.Cover/100

#now do it for all the other columns
unique(fb23$PHAU)
fb23$PHAU[fb23$PHAU == ""] <- 0
fb23$PHAU[fb23$PHAU == "<1"] <- 0.5
fb23$PHAU[fb23$PHAU == 0] <- 0.25
fb23$PHAU[fb23$PHAU == 1.00] <- 5.00
fb23$PHAU[fb23$PHAU == 10.00] <- 15.00
fb23$PHAU[fb23$PHAU == 20.00] <- 25.00
fb23$PHAU[fb23$PHAU == 30.00] <- 35.00
fb23$PHAU[fb23$PHAU == 40.00] <- 45.00
fb23$PHAU[fb23$PHAU == 50.00] <- 55.00
fb23$PHAU[fb23$PHAU == 60.00] <- 65.00
fb23$PHAU[fb23$PHAU == 70.00] <- 75.00
fb23$PHAU[fb23$PHAU == 80.00] <- 85.00
fb23$PHAU[fb23$PHAU == 90.00] <- 95.00
fb23$PHAU <- as.double(fb23$PHAU)
fb23$PHAU <- fb23$PHAU/100

unique(fb23$Typha)
fb23$Typha[fb23$Typha == ""] <- 0
fb23$Typha[fb23$Typha == "<1"] <- 0.5
fb23$Typha[fb23$Typha == 0] <- 0.25
fb23$Typha[fb23$Typha == 1.00] <- 5.00
fb23$Typha[fb23$Typha == 10.00] <- 15.00
fb23$Typha[fb23$Typha == 20.00] <- 25.00
fb23$Typha[fb23$Typha == 30.00] <- 35.00
fb23$Typha[fb23$Typha == 40.00] <- 45.00
fb23$Typha <- as.double(fb23$Typha)
fb23$Typha <- fb23$Typha/100

unique(fb23$BOMA)
fb23$BOMA[fb23$BOMA == ""] <- 0
fb23$BOMA[fb23$BOMA == "<1"] <- 0.5
fb23$BOMA[fb23$BOMA == 0] <- 0.25
fb23$BOMA[fb23$BOMA == 1.00] <- 5.00
fb23$BOMA <- as.double(fb23$BOMA)
fb23$BOMA <- fb23$BOMA/100

unique(fb23$DISP)
fb23$DISP[fb23$DISP == ""] <- 0
fb23$DISP[fb23$DISP == "<1"] <- 0.5
fb23$DISP[fb23$DISP == 0] <- 0.25
fb23$DISP[fb23$DISP == 1.00] <- 5.00
fb23$DISP[fb23$DISP == 10.00] <- 15.00
fb23$DISP <- as.double(fb23$DISP)
fb23$DISP <- fb23$DISP/100

unique(fb23$SCAC)
fb23$SCAC[is.na(fb23$SCAC)] <- 0
fb23$SCAC[fb23$SCAC == 1.00] <- 5.00
fb23$SCAC[fb23$SCAC == 0] <- 0.25
fb23$SCAC[fb23$SCAC == 10.00] <- 15.00
fb23$SCAC[fb23$SCAC == 20.00] <- 25.00
fb23$SCAC <- as.double(fb23$SCAC)
fb23$SCAC <- fb23$SCAC/100

unique(fb23$SCAM)
fb23$SCAM[fb23$SCAM == ""] <- 0
fb23$SCAM[fb23$SCAM == "<1"] <- 0.5
fb23$SCAM[fb23$SCAM == 0] <- 0.25
fb23$SCAM[fb23$SCAM == 1.00] <- 5.00
fb23$SCAM[fb23$SCAM == 10.00] <- 15.00
fb23$SCAM[fb23$SCAM == 30.00] <- 35.00
fb23$SCAM <- as.double(fb23$SCAM)
fb23$SCAM <- fb23$SCAM/100

unique(fb23$RUMA)
fb23$RUMA[is.na(fb23$RUMA)] <- 0
fb23$RUMA[fb23$RUMA == 1.00] <- 5.00
fb23$RUMA[fb23$RUMA == 0] <- 0.25
fb23$RUMA[fb23$RUMA == 20.00] <- 25.00
fb23$RUMA[fb23$RUMA == 10.00] <- 15.00
fb23$RUMA[fb23$RUMA == 30.00] <- 35.00
fb23$RUMA[fb23$RUMA == 40.00] <- 45.00
fb23$RUMA[fb23$RUMA == 50.00] <- 55.00
fb23$RUMA <- as.double(fb23$RUMA)
fb23$RUMA <- fb23$RUMA/100

unique(fb23$RUST)
fb23$RUST[is.na(fb23$RUST)] <- 0
fb23$RUST[fb23$RUST == 1.00] <- 5.00
fb23$RUST[fb23$RUST == 0] <- 0.25
fb23$RUST <- as.double(fb23$RUST)
fb23$RUST <- fb23$RUST/100

glimpse(fb23)

#check measurements to make sure they make sense
min(fb23$Measurement.1)
max(fb23$Measurement.1)

min(fb23$Measurement.2)
max(fb23$Measurement.2)

min(fb23$Measurement.3)
max(fb23$Measurement.3)

#Add a new section for invasives and natives
df <- fb23 %>%
  dplyr::select("PHAU", "Typha", "RUST") %>%
  mutate(Invasive = rowSums(.,na.rm = T))
df1 <- fb23 %>%
  dplyr::select("BOMA", "SCAC",
         "SCAM",  "RUMA", 
         "DISP") %>%
  mutate(Native = rowSums(.,na.rm = T))

fb23$Invasive.Cover <- df$Invasive
fb23$Native.Cover <- df1$Native

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

