# Load everything ####
load("main_dfs.RData")

library(ggplot2)
library(magrittr)
library(dplyr)
library(gtools)
library(bbmle)

# Model with only density ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "EPCI") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
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


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
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

##NLL Function####


#Multi process error 
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

##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

# Calculate confidence intervals ####

## r1 ####

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

## r2 ####

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


# Model with only phrag ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAC") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Phrag_Presence, Density) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
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


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
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


##NLL Function####
#Multi process error
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
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


##Find MLE parameters####

library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

# Calculate confidence intervals ####

## r1 ####

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

## r2 ####

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

# Model with nothing ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCPU") %>% 
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


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
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

##NLL Function####
#Multi process error 
nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  
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
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}


##Find MLE parameters####

# Create list of starting guesses for the multi model
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# # Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

# Calculate confidence intervals ####

## r ####

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

# BICE ####
# Model with only density ####
## Make a new dataframe that is in a better format####

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
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
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


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
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

#test
# multi.func.p(r1 = .7, r2 = .9, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
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
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}


##Find MLE parameters####

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

# Calculate confidence intervals ####

## r1 ####

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

## r2 ####

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

#SCAM ####
# Model with nothing ####
## Make a new dataframe that is in a better format####
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



##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
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


##NLL Function####


#Multi process error 
nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)

  
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
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
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

# Calculate confidence intervals ####

## r ####

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
