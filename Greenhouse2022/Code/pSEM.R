####Lefchek example from presentation####

#####-----Advances in piecewise estimation of path models-----------------------

# EFI/ESA Seminar
# Date: 05 December 2022
# Author: Jon Lefcheck
# Contact: LefcheckJ@si.edu

# Load required libraries
# devtools::install_github("jslefche/piecewiseSEM@devel") # version 2.3.0
library(piecewiseSEM)
library(lavaan)

# Load Keeley data set
data(keeley)

# Examine Keeley data
head(keeley)

#####-----Fit structural equation model to Keeley data--------------------------

# Fit simple/multiple regressions using `lm`

# Break down component regressions
abiotic_model <- lm(abiotic ~ distance, data = keeley)
hetero_model <- lm(hetero ~ distance, data = keeley)
richness_model <- lm(rich ~ abiotic + hetero, data = keeley)

# Use the `psem` function to create the SEM
model <- psem(abiotic_model, hetero_model, richness_model)

# Look at object
model

# Step 1: conduct tests of directed separation
# Establish the basis set & evaluate independence claims
# Missing path #1:
dsep1 <- lm(abiotic ~ hetero + distance, data = keeley)
# Missing path #2:
dsep2 <- lm(rich ~ distance + abiotic + hetero, data = keeley)

# Get P-values
P1 <- summary(dsep1)$coefficients[2, "Pr(>|t|)"]
P2 <- summary(dsep2)$coefficients[2, "Pr(>|t|)"]

# Construct C-statistic
C <- -2 * (log(P1) + log(P2))

C

# Compare to chi-squared distribution with 2*2 degrees of freedom
1 - pchisq(C, 4) # P < 0.05 == poor fit!

# Can use `dsep` function to perform the tests automagically
dSep(model)

# Can use `fisherC` function to evaluate claims
fisherC(model)

# The relationship between rich and distance is significant
# Re-introduce to the model
model2 <- update(model, rich ~ abiotic + hetero + distance)

model2

dSep(model2) # only 1 claim now
fisherC(model2) # P > 0.05 == model fits well!

# Get coefficients from good-fitting SEM
coefs(model2)

# Standardized estimates are in units of standard deviations of the mean
# Can be directly compared even though initial units are very different

# Plot SEM with standardized coefficients
plot(model2)

# Use `summary` function to get all information at once
summary(model2)

##More examples on the github page but this is where the presentation ended

####My data####

#I only want the values for the end
library(dplyr)

final.traits <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16" & Phrag_Presence == "W") %>%
  select(Species, Block, Density, Cover.Native, Height.Native, Cover.Phrag)

final.biomass <- biomass %>%
  filter(Phrag_Presence == "W") %>%
  select(Species, Block, Density, Native.Biomass, Phrag.Biomass)


final.all <- left_join(final.traits, final.biomass, by = c("Species", "Density", "Block"))
final.all$Cover.Native[is.na(final.all$Cover.Native)] <- 0
final.all$Height.Native[is.na(final.all$Height.Native)] <- 0
final.all$Native.Biomass[is.na(final.all$Native.Biomass)] <- 0
final.all$Cover.Phrag[is.na(final.all$Cover.Phrag)] <- 0

#now make the models for each piece
biomass_model <- lm(Native.Biomass ~ Density, data = final.all)
height_model <- lm(Height.Native ~ Density + Native.Biomass, data = final.all)
cover_model <- lm(Cover.Native ~ Density + Native.Biomass + Height.Native, data = final.all)
phrag_cover <- lm(Cover.Phrag ~ Cover.Native + Height.Native + Native.Biomass + Density, data = final.all)


model <- psem(biomass_model, height_model, cover_model, phrag_cover)
summary(model)
coefs(model)
