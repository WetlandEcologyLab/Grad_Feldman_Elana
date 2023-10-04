n0<-.01
r<-.3

K<-.999
ts<-100
N<-rep(n0,ts)

ts<-100

for(i in 2:ts){
  ent<-N[i-1]*(1+r*(1-N[i-1]/K))
  N[i]<-plogis(rnorm(1,qlogis(ent)),.02)
}

plot(N,ylim=c(0,1))



#hist(plogis(rnorm(10000,.5,.2)),xlim=c(0,1),breaks=seq(0,1,by=.02))
#hist(plogis(rnorm(10000,.5,.4)),xlim=c(0,1),breaks=seq(0,1,by=.02),col="black",add=T)


log.p<-function(r,K,N0,obs){
  ts<-length(obs)
  Nout<-rep(N0,ts)
  
  Nout[1]<-N0*(1+r*(1-N0/K))
  for(i in 2:ts){
    if(!is.na(obs[i-1])){
      Nout[i]<-obs[i-1]*(1+r*(1-obs[i-1]/K))
    }
    if(is.na(obs[i-1])){
      Nout[i]<-Nout[i-1]*(1+r*(1-Nout[i-1]/K))
    }
    
    
  }
  return(Nout)
}

nll<-function(lr,lK,lN0,obs,lsd){
  r<-exp(lr)
  K<-exp(lK)
  N0<-exp(lN0)
  s<-exp(lsd)
  
  predN<-log.p(r=r,K=K,N0=N0,obs=obs)
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  print(obs)
  print(predN)
  lastobs<-0
  liks<-0
  #for(j in 1:nrow(obs)){
    for(i in 1:length(obs)){
      if(!is.na(obs[i])){
        tbtwn<-i-lastobs
        print(tbtwn)
        liks<-c(liks,dnorm(x=qlogis(obs[i]),mean=qlogis(predN[i]),sd=sqrt(tbtwn*s^2)))  
        lastobs<-i
        print(liks)
      }
    }
    
 # }

  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

N2<-N
N2[sample(seq(1,100),75,replace=F)]<-NA

nll(lr=log(.4),lK=log(.999),lN0=log(.01),obs=N2,lsd=log(.2))




