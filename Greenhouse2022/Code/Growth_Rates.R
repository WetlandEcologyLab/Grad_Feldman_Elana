#Load everything ####

#load data
load("Data/main_dfs.RData")

#load packages 
#All package version saved in renv.lock 
#renv::init, renv::restore
library(ggplot2)
library(magrittr)
library(dplyr)
library(gtools)
library(bbmle)
library(quantreg)

# Calculate growth rates for each species individually ####
## Model 1: all treatments have an effect####

### Make the matrix to work with ####
native.dat <- greenhouse %>%
  filter(Species == "EUMA") %>% #change this for each individual species
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#make an ID column out of the species, density, phrag presence, and block
native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec <- rep(1:4, each = 3)

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

#combine it all back into one matrix
native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, r3, r4,  obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, lr3, lr4, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  r3<-exp(lr3)
  r4<-exp(lr4)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99

  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}


###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lr3 = log(.1),
                 lr4 = log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_mp<-exp(coef(fit_mp)) 
cfs_mp

### Predict historical dynamics from MLE parameter estimates####

pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
                      r4 = cfs_mp[4], n0 = cfs_mp[5], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_mp)))

## Model 2: only density has an effect####

### Make the matrix to work with ####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "EPCI") %>% #change this for each individual species
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#make an ID column from the species, density, phrag presence, and block 
native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = H, 2 = L
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)

  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  

  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####
# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

### Predict historical dynamics from MLE parameter estimates####
pred_np<-multi.func.p(r1 = cfs_np[1], r2 = cfs_np[2],
                      n0 = cfs_np[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_np, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_np)))

## Model 3: only phrag presence has an effect####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAC") %>% #change this for each individual species
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Phrag_Presence, Density) #put likes together

#make an ID column
native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = W, 2 = WO
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

### Predict historical dynamics from MLE parameter estimates####
pred_nd<-multi.func.p(r1 = cfs_nd[1], r2 = cfs_nd[2],
                      n0 = cfs_nd[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_nd, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_nd)))

## Model 4: nothing  has an effect####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "EUMA") %>% #change this for each individual species
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Phrag_Presence, Density) #put likes together

#make an ID column
native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

### Predict historical dynamics from MLE parameter estimates####

pred_n<-multi.func.p(r = cfs_n[1],
                     n0 = cfs_n[2], obs = native.mat)
plot(native.mat, pred_n, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_n)))

## AIC calculations ####
# Calculate AIC for each model to determine which model is the best
AICmp <- AIC(fit_mp)
AICnd <- AIC(fit_nd)
AICnp <- AIC(fit_np)
AICn <- AIC(fit_n)

# Calculate delta AIC for each model
dAICmp <- AICmp-min(AICmp, AICnd, AICnp, AICn)
dAICnd <- AICnd-min(AICmp, AICnd, AICnp, AICn)
dAICnp <- AICnp-min(AICmp, AICnd, AICnp, AICn)
dAICn <- AICn-min(AICmp, AICnd, AICnp, AICn)

# Calculate relative likelihood for each model
rAICmp <- exp(-.5*dAICmp)
rAICnd <- exp(-.5*dAICnd)
rAICnp <- exp(-.5*dAICnp)
rAICn <- exp(-.5*dAICn)

# Calculate AIC weights for each model
AICwmp <- rAICmp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwn <- rAICn/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnp <- rAICnp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnd <- rAICnd/sum(rAICmp,rAICnd, rAICnp, rAICn)

AICwmp
AICwn
AICwnp
AICwnd

#plot the AIC weight
par(mar = c(4, 4, 2, 2))
allAIC<-c(AICwmp, AICwnd, AICwnp, AICwn)
allAICw<-allAIC[order(allAIC)]
plot(allAICw,xaxt="n",ylab="AIC weight",
     xlab="Model",pch=16,las=1)
axis(side=1,at=seq(1, 4),labels=c("All", "Phrag Only", "Density Only", "None")[order(allAIC)])


# Calculate growth rates for BICE ####
#Must use these functions for the BICE predictions because there was an extra replicate

## Model 1: all treatments have an effect####
### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

#make an ID column
native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec1 <- rep(1, each = 4)
species.vec2 <- rep(2, each = 2)
species.vec3 <- rep(3:4, each = 3)
species.vec <- c(species.vec1, species.vec2, species.vec3)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, r3, r4,  obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}


###NLL Function####

nll.multi.func.p<-function(lr1, lr2, lr3, lr4, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  r3<-exp(lr3)
  r4<-exp(lr4)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}


###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lr3 = log(.1),
                 lr4 = log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_mp<-exp(coef(fit_mp)) 
cfs_mp

### Predict historical dynamics from MLE parameter estimates####

pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
                      r4 = cfs_mp[4], n0 = cfs_mp[5], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_mp)))

## Model 2: only density has an effect####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = H, 2 = L
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}


###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

### Predict historical dynamics from MLE parameter estimates####
pred_np<-multi.func.p(r1 = cfs_np[1], r2 = cfs_np[2],
                      n0 = cfs_np[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_np, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_np)))

## Model 3: only phrag presence has an effect####

### Make the matrix to work with ####
native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = W, 2 = WO
species.vec1 <- rep(1, 7)
species.vec2 <- rep(2, 5)
species.vec <- c(species.vec1, species.vec2)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

### Predict historical dynamics from MLE parameter estimates####

pred_nd<-multi.func.p(r1 = cfs_nd[1], r2 = cfs_nd[2],
                      n0 = cfs_nd[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_nd, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_nd)))

## Model 4: nothing  has an effect####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

### Predict historical dynamics from MLE parameter estimates####

pred_n<-multi.func.p(r = cfs_n[1],
                      n0 = cfs_n[2], obs = native.mat)
plot(native.mat, pred_n, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_n)))

## AIC calculations ####
# Calculate AIC for each model to determine which model is the best
AICmp <- AIC(fit_mp)
AICnd <- AIC(fit_nd)
AICnp <- AIC(fit_np)
AICn <- AIC(fit_n)

# Calculate delta AIC for each model
dAICmp <- AICmp-min(AICmp, AICnd, AICnp, AICn)
dAICnd <- AICnd-min(AICmp, AICnd, AICnp, AICn)
dAICnp <- AICnp-min(AICmp, AICnd, AICnp, AICn)
dAICn <- AICn-min(AICmp, AICnd, AICnp, AICn)

# Calculate relative likelihood for each model
rAICmp <- exp(-.5*dAICmp)
rAICnd <- exp(-.5*dAICnd)
rAICnp <- exp(-.5*dAICnp)
rAICn <- exp(-.5*dAICn)

# Calculate AIC weights for each model
AICwmp <- rAICmp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwn <- rAICn/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnp <- rAICnp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnd <- rAICnd/sum(rAICmp,rAICnd, rAICnp, rAICn)

AICwmp
AICwn
AICwnp
AICwnd

# Calculate growth rates for SCAM ####
#must use these functions for SCAM because it only had 2 replicates

## Model 1: all treatments have an effect####
### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec1 <- rep(1, each = 2)
species.vec2 <- rep(2, each = 4)
species.vec3 <- rep(3:4, each = 3)
species.vec <- c(species.vec1, species.vec2, species.vec3)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, r3, r4,  obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, lr3, lr4, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  r3<-exp(lr3)
  r4<-exp(lr4)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####
# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lr3 = log(.1),
                 lr4 = log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# store MLE parameter estimates
cfs_mp<-exp(coef(fit_mp)) 
cfs_mp

### Predict historical dynamics from MLE parameter estimates####

pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
                      r4 = cfs_mp[4], n0 = cfs_mp[5], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_mp)))

## Model 2: only density has an effect####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = H, 2 = L
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)

  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

### Predict historical dynamics from MLE parameter estimates####

pred_np<-multi.func.p(r1 = cfs_np[1], r2 = cfs_np[2],
                      n0 = cfs_np[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_np, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_np)))

## Model 3: only phrag presence has an effect####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = W, 2 = WO
species.vec1 <- rep(1, each = 5)
species.vec2 <- rep(2, each = 7)
species.vec <- c(species.vec1, species.vec2)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####
# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

### Predict historical dynamics from MLE parameter estimates####

pred_nd<-multi.func.p(r1 = cfs_nd[1], r2 = cfs_nd[2],
                      n0 = cfs_nd[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_nd, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_nd)))

## Model 4: nothing  has an effect####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)

  predN[predN==0]<-.01
  predN[predN==1]<-.99

  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

### Predict historical dynamics from MLE parameter estimates####

pred_n<-multi.func.p(r = cfs_n[1],
                     n0 = cfs_n[2], obs = native.mat)
plot(native.mat, pred_n, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_n)))

## AIC calculations ####
# Calculate AIC for each model to determine which model is the best
AICmp <- AIC(fit_mp)
AICnd <- AIC(fit_nd)
AICnp <- AIC(fit_np)
AICn <- AIC(fit_n)

# Calculate delta AIC for each model
dAICmp <- AICmp-min(AICmp, AICnd, AICnp, AICn)
dAICnd <- AICnd-min(AICmp, AICnd, AICnp, AICn)
dAICnp <- AICnp-min(AICmp, AICnd, AICnp, AICn)
dAICn <- AICn-min(AICmp, AICnd, AICnp, AICn)

# Calculate relative likelihood for each model
rAICmp <- exp(-.5*dAICmp)
rAICnd <- exp(-.5*dAICnd)
rAICnp <- exp(-.5*dAICnp)
rAICn <- exp(-.5*dAICn)

# Calculate AIC weights for each model
AICwmp <- rAICmp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwn <- rAICn/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnp <- rAICnp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnd <- rAICnd/sum(rAICmp,rAICnd, rAICnp, rAICn)

AICwmp
AICwn
AICwnp
AICwnd

# Calculate confidence intervals ####
#Once growth rates have been calculated for each of the species individually,
#you can use the following script to calculate confidence intervals 

#Only use whichever of the following models was best for the individual species 
#Does not include model with everything because no species was affected by both factors

## Model with only density ####
### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "EPCI") %>% #change this for each individual species
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = H, 2 = L
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

### Confidence intervals ####

#### r1 - High Density ####

r1vec<-seq(0.2,0.35,length.out=100) # Vector of r values to explore
nlls.r1<-rep(NA,length(r1vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r1vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(lr2=log(cfs_np[2]),ln0=log(cfs_np[3]), lsd = log(cfs_np[4])),
               fixed=list(lr1=log(r1vec[i])),
               data=list(obs=native.mat, species.vec = species.vec, method = "Nelder-Mead"))
  nlls.r1[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r1vec,nlls.r1--1*logLik(fit_np))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r1)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[1:which.min(abs(r1vec-cfs_np[1]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[1:which.min(abs(r1vec-cfs_np[1]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[which.min(abs(r1vec-cfs_np[1])):length(r1vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[which.min(abs(r1vec-cfs_np[1])):length(nlls.r1)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y

lwr.r1
upr.r1

#### r2  - Low Density####

r2vec<-seq(.1,.26,length.out=100) # Vector of r values to explore
nlls.r2<-rep(NA,length(r2vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r2vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(lr1=log(cfs_np[1]),ln0=log(cfs_np[3]), lsd =log(cfs_np[4])),
               fixed=list(lr2=log(r2vec[i])),
               data=list(obs=native.mat, species.vec = species.vec, method = "Nelder-Mead"))
  nlls.r2[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r2vec,nlls.r2--1*logLik(fit_np))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r2)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r2.test<-r2vec[1:which.min(abs(r2vec-cfs_np[2]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r2[1:which.min(abs(r2vec-cfs_np[2]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r2<-approx(y=r2.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r2.test<-r2vec[which.min(abs(r2vec-cfs_np[2])):length(r2vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r2[which.min(abs(r2vec-cfs_np[2])):length(nlls.r2)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r2<-approx(y=r2.test,x=nll.test,xout=target.nll)$y

lwr.r2
upr.r2

## Model with only phrag ####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAC") %>% #change this for each individual species
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Phrag_Presence, Density) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = W, 2 = WO
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}


###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)

  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        
      }
    }
  }
  
  
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}


###Find MLE parameters####

library(bbmle)

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

### Confidence intervals ####

#### r1 - Phrag Present ####

r1vec<-seq(0,.04,length.out=100) # Vector of r values to explore
nlls.r1<-rep(NA,length(r1vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r1vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(lr2=log(cfs_nd[2]),ln0=log(cfs_nd[3]), lsd = log(cfs_nd[4])),
               fixed=list(lr1=log(r1vec[i])),
               data=list(obs=native.mat, species.vec = species.vec, method = "SANN"))
  nlls.r1[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r1vec,nlls.r1--1*logLik(fit_nd))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r1)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[1:which.min(abs(r1vec-cfs_nd[1]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[1:which.min(abs(r1vec-cfs_nd[1]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[which.min(abs(r1vec-cfs_nd[1])):length(r1vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[which.min(abs(r1vec-cfs_nd[1])):length(nlls.r1)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y

lwr.r1
upr.r1

#### r2  - Phrag Absent####

r2vec<-seq(.01,.06,length.out=100) # Vector of r values to explore
nlls.r2<-rep(NA,length(r2vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r2vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(lr1=log(cfs_nd[1]),ln0=log(cfs_nd[3]), lsd = log(cfs_nd[4])),
               fixed=list(lr2=log(r2vec[i])),
               data=list(obs=native.mat, species.vec = species.vec, method = "SANN"))
  nlls.r2[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r2vec,nlls.r2--1*logLik(fit_nd))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r2)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r2.test<-r2vec[1:which.min(abs(r2vec-cfs_nd[2]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r2[1:which.min(abs(r2vec-cfs_nd[2]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r2<-approx(y=r2.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r2.test<-r2vec[which.min(abs(r2vec-cfs_nd[2])):length(r2vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r2[which.min(abs(r2vec-cfs_nd[2])):length(nlls.r2)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r2<-approx(y=r2.test,x=nll.test,xout=target.nll)$y

lwr.r2
upr.r2

## Model with nothing ####

### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCPU") %>% #change this for each individual species
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Phrag_Presence, Density) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}


###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))

# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

### Confidence intervals ####

#### r - only one growth rate####

r1vec<-seq(0,0.07,length.out=100) # Vector of r values to explore
nlls.r1<-rep(NA,length(r1vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r1vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(ln0=log(cfs_n[2]), lsd = log(cfs_n[3])),
               fixed=list(lr=log(r1vec[i])),
               data=list(obs=native.mat, method = "SANN"))
  nlls.r1[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r1vec,nlls.r1--1*logLik(fit_n))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r1)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[1:which.min(abs(r1vec-cfs_n[1]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[1:which.min(abs(r1vec-cfs_n[1]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[which.min(abs(r1vec-cfs_n[1])):length(r1vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[which.min(abs(r1vec-cfs_n[1])):length(nlls.r1)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y

lwr.r1
upr.r1

# Calculate BICE Confidence Intervals####
#Use this model for BICE because an extra replicate
#only the only density model because that was best for BICE 

## Model with only density ####
### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = H, 2 = L
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

###NLL Function####

nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)

  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  
 
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}


###Find MLE parameters####

# Create list of starting guesses 
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

### Confidence intervals ####

#### r1 - High Density####

r1vec<-seq(.25,.45,length.out=100) # Vector of r values to explore
nlls.r1<-rep(NA,length(r1vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r1vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(lr2=log(cfs_np[2]),ln0=log(cfs_np[3]), lsd = log(cfs_np[4])),
               fixed=list(lr1=log(r1vec[i])),
               data=list(obs=native.mat, species.vec = species.vec, method = "Nelder-Mead"))
  nlls.r1[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r1vec,nlls.r1--1*logLik(fit_np))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r1)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[1:which.min(abs(r1vec-cfs_np[1]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[1:which.min(abs(r1vec-cfs_np[1]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[which.min(abs(r1vec-cfs_np[1])):length(r1vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[which.min(abs(r1vec-cfs_np[1])):length(nlls.r1)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y

lwr.r1
upr.r1

#### r2 - Low Density####

r2vec<-seq(.2,.35,length.out=100) # Vector of r values to explore
nlls.r2<-rep(NA,length(r2vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r2vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(lr1=log(cfs_np[1]),ln0=log(cfs_np[3]), lsd = log(cfs_np[4])),
               fixed=list(lr2=log(r2vec[i])),
               data=list(obs=native.mat, species.vec = species.vec, method = "Nelder-Mead"))
  nlls.r2[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r2vec,nlls.r2--1*logLik(fit_np))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r2)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r2.test<-r2vec[1:which.min(abs(r2vec-cfs_np[2]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r2[1:which.min(abs(r2vec-cfs_np[2]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r2<-approx(y=r2.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r2.test<-r2vec[which.min(abs(r2vec-cfs_np[2])):length(r2vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r2[which.min(abs(r2vec-cfs_np[2])):length(nlls.r2)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r2<-approx(y=r2.test,x=nll.test,xout=target.nll)$y

lwr.r2
upr.r2

#Calculate SCAM Confidence Intervals####
#Use this model for SCAM because missing a replicate
#Only model with nothing because that was best for SCAM

## Model with nothing ####
### Make the matrix to work with ####
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###State Prediction Funcions: ####

multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}


###NLL Function####

nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
      }
    }
  }
  
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

###Find MLE parameters####

# Create list of starting guesses
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

### Confidence intervals ####

#### r  - only one growth rate####

r1vec<-seq(0,0.08,length.out=100) # Vector of r values to explore
nlls.r1<-rep(NA,length(r1vec)) # Storage vector for nll of the model at each fixed value of r

# Loop through all values in rvec, fit the model with r fixed, store NLL
for(i in 1:length(r1vec)){
  fittmp<-mle2(minuslogl=nll.multi.func.p,start=list(ln0=log(cfs_n[2]), lsd = log(cfs_n[3])),
               fixed=list(lr=log(r1vec[i])),
               data=list(obs=native.mat, method = "SANN"))
  nlls.r1[i]<--1*logLik(fittmp) # extract NLL from model fit
}

# Plot the difference of nll between models with fixed r values and the NLL of the full model.
plot(r1vec,nlls.r1--1*logLik(fit_n))
abline(h=1.92) # add line at critical value

target.nll<-min(nlls.r1)+1.92 # What is the target value of NLL for CI limits?
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[1:which.min(abs(r1vec-cfs_n[1]))]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[1:which.min(abs(r1vec-cfs_n[1]))]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
lwr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y
# Specify the range of r values in which the lower CI limit exists
r1.test<-r1vec[which.min(abs(r1vec-cfs_n[1])):length(r1vec)]
# specify vector of nll values associated with r values being examined
nll.test<-nlls.r1[which.min(abs(r1vec-cfs_n[1])):length(nlls.r1)]
# Estimate the r value at which the relationship between r and nll crosses
# the target.nll
upr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y

lwr.r1
upr.r1

# Quantile regression ####
#Now graph the quantile regression of relationship between r and PHAU cover

#load data
values <- read.csv("Data/r_values.csv") #this csv was made by manually entering all the results from above into a spreadsheet

#Clean up the sheet
dat <- values %>% 
  select(1:4, 7, 10, 13, 16, 17) %>% 
  pivot_longer(4:7,
               names_to = "Tub",
               values_to = "r_value") %>% 
  separate(col = "Tub",
           into = c("Density", "Phrag_Presence"),
           sep = 1) 

#First, we need to make a table that includes both of these values
dat2 <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16",
         Phrag_Presence == "W") %>%
  select(Species, Density, Phrag_Presence, Block, Cover.Phrag) %>%
  left_join(dat, by = c("Species", "Density", "Phrag_Presence"))

#Now make the calculations to see the relationship between the two
#Choose the quantiles by changing the tau

rqfit <- rq(Cover.Phrag ~ r_value, tau = c(.05, .25, .5, .75, .95),data = dat2)
summary(rqfit) 

#Now graph them
color <- c("#1e90ff", "#192bc2")
dat2 %>% 
  ggplot(aes(x = r_value, y = Cover.Phrag))+
  geom_point() +
  geom_quantile(quantiles = c(0.5, 0.95),
                aes(color = factor(..quantile..)),
                size = 1) +
  xlab("Intrinsic Rate of Growth (*r*)") +
  ylab("Proportional *P.australis* Cover") +
  labs(color = "Quantiles") +
  theme(axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown()) +
  scale_color_manual(values = color) +
  ylim(0, 0.5)
