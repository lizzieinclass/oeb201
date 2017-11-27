#Nick Herrmannn
#OEB201 Final Project
#Due on 11/29/2017
###Lizard Perch Heights on Experimental Islands###


#set working directory and clean environment
setwd("")
rm(list=ls())
options(stringsAsFactors = FALSE)


#load packages
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(truncnorm)

##Outline of contents###
# 1) SIMULATE FAKE DATA - LINE 28
# 2) FIT MODEL (Stan) - LINE 72
# 3) POSTERIOR PREDICTIVE CHECK - LINE103
# 4) FUNK UP THE FAKE DATA AND SEE IF THE MODEL BREAKS - LINE 165
# 5) RUN THE MODEL USING REAL BASELINE DATA - LINE 174


###########################
###1) SIMULATE FAKE DATA###
###########################

rm(list=ls())
set.seed(8)

nreps = 26  #replicate observations per island
J = 6     #total number of islands
years= 2 #number of years
N = nreps*J*years

islandnum <- rep(c(1:J), each=nreps*years)   #list of island numbers for each observation (length N)
t <- rep(c(0,1),each=N/2) #assign half islands to control (0) and half to treatment (1)
s <- rep(rep(c(0,1),each=nreps/2),J*years) #assign half lizards within each island as female (0) and half as male (1)
r <- rep(rep(c(0,1),each=nreps),J) #observations on an island evenly split between years

#predictors (t=treatment,s=sex,r=year)
b_t=0
b_tr=-30
b_r=0
b_s=80
b_tsr=-40

#mu_a and sig_a are the mean and variance of the intercept across sites (islands)
mu_a <- 180
sig_a <- 40

#for each island, draw the intercept from the appropriate site mean and sd
a_island <- rep(0,J)
#assume same within-site variance for all sites
for (j in 1:J){
  a_island[j] <- rnorm(1,mu_a, sig_a);
}

#simulate data
sig_y <- 100 #residual (within island) variance
y <- rep(0,N)
L <-0 # lower bound on response variable
for (n in 1:N){
  y[n] <- rtruncnorm(1,a=L,b=Inf,a_island[islandnum[n]] + b_t*t[n] + b_tr*t[n]*r[n] +
                       b_r*r[n] + b_s*s[n] + b_tsr*t[n]*s[n]*r[n], sig_y)
}

##################
###2) FITMODEL ###
##################

dat <- list(N=N,J=J,islandnum=islandnum,t=t,s=s,r=r,L=L,y=y)

#WITH LARGE N THIS WILL TAKE A WHILE
fitme <- stan(file = 'perchheight_truncnormal_v2.stan', 
              data=c("N","J","islandnum","t","s","r","L","y"), iter=1000, 
              control=list(adapt_delta = 0.9, stepsize = 0.5))

nreps
J
years
N
b_t
b_tr
b_r
b_s
b_tsr
a_island
mu_a
sig_a
sig_y

print(fitme)
plot(fitme, pars=c("b_t","b_tr","b_r","b_s","b_tsr","mu_a","sig_a","sig_y"))
#dev.copy2pdf(file="fitme4")
plot(fitme, pars=c("a_island[1]","a_island[2]","a_island[3]","a_island[4]","a_island[5]","a_island[6]"))
#dev.copy2pdf(file="fitme4_islandints")

###################################
###3) POSTERIOR PREDICTIVE CHECK###
###################################

set.seed(Sys.time()) #undo original seed set to reintroduce randonmness

sims=as.data.frame(as.matrix(fitme)) #posterior distribution of paramter estimates
#write.csv(sims, "posterior4.csv")
#sims=read.csv(file="posterior1.csv")
n_sims=nrow(sims) #number of sets parameter estimates
y_rep=array(NA,c(n_sims,n)) #will hold fake data simulated from posterior parameter estimates

#Simulate data
#WITH LARGE N and n_sims THIS WILL TAKE A WHILE
for (k in 1:n_sims){
  a_island_temp <- rnorm(J,sims$mu_a[k], sims$sig_a[k]); #generates island intercepts
  for(p in 1:N){
    y_rep[k,p]=rtruncnorm(1,a=L,b=Inf, a_island_temp[islandnum[p]] +
                        sims$b_t[k]*t[p] +
                        sims$b_tr[k]*t[p]*r[p] +
                        sims$b_r[k]*r[p] +
                        sims$b_s[k]*s[p] +
                        sims$b_tsr[k]*t[p]*s[p]*r[p],sims$sig_y[k])
  }
}

#This data frame PPCdat contains all predictors (islandnum,t=treatment,s=sex,r-year),...
#perch height from original simulated fake data (y), and simulated data columns using
#parameter values from the posterior distribution of the model fitme (there are n_sims such columns)
PPCdat=as.data.frame(cbind(islandnum,t,s,r,y,t(y_rep)))
write.csv(PPCdat, "PPCdat4.csv")
#PPCdat=read.csv(file="PPCdat2.csv")

#Now run PPC by comparing between year perch height difference on treatment islands to
#between year perch height difference on control islands
#If lizards on treatment islands did perch lower, PHdiff will be negative
treatY0=subset(PPCdat,t==1 & r==0)
treatY1=subset(PPCdat,t==1 & r==1)
consY0=subset(PPCdat,t==0 & r==0)
consY1=subset(PPCdat,t==0 & r==1)

PHdiff=rep(0,n_sims)
for (r in 1:n_sims){ #this loop calculates perch height difference for simulated data
  PHdiff[r]=(mean(treatY1[,(r+5)])-mean(treatY0[,(r+5)]))-
    (mean(consY1[,(r+5)])-mean(consY0[,(r+5)]))

}
realPHdiff=(mean(treatY1$y)-mean(treatY0$y))-
  (mean(consY1$y)-mean(consY0$y)) #the "real" perch height difference (from original fake data)
                                      
hist(PHdiff,breaks=100) #plot values and compare to the "real value"
abline(v=realPHdiff,col="red",lty=2)
abline(v=-50,col="blue")
#dev.copy2pdf(file="PPC4")

mean(PHdiff>=0) #calculate % of PPC values that are on the "wrong" side of zero


#STEPS 1-3 OUTLINED ABOVE WERE USED TO EXECUTE SIMULATIONS 1 AND 2
#PRESENTED IN THE WRITTEN REPORT


##############################
###4) FUNK UP THE FAKE DATA###
##############################

#STEPS 1-3 OUTLINED ABOVE WERE USED TO EXECUTE SIMULATIONS 3 AND 4
#PRESENTED IN THE WRITTEN REPORT
#SIMULATION 3: large treatment effect 
#SIMULATION 4: substantially increases both within island and between island variance

##################################################
### 5) RUN THE MODEL USING REAL BASELINE DATA  ###
##################################################

rm(list=ls())
set.seed(8)

#import data
greens<-read.csv(file="greens.csv",header=TRUE)
#data have been sorted into alphabetical order by site

nreps = 24  #set replicate observations in second year
J = as.numeric(length(unique(greens$Site))) #total number of islands
years= 2 #number of years
Year1N=as.numeric(length(greens$Site)) #number of observations in year 1
Year2N=nreps*J #number of observations in year 2
N=Year1N+Year2N

#vectors of predictors variables are concatentations of actual data and projected future catches
islandnum <- c(as.numeric(as.factor(greens$Site)),rep(c(1:J), each=nreps))   #list of island numbers for each observation (length N)
t <- c(greens$Treatment,rep(c(1,0,0,1,1,0),each=nreps)) #assign appropriate islands as control (0) or treatment (1)
s <- c(greens$Sex,rep(rep(c(0,1),each=nreps/2),J)) #assign sexes as female (0) or male (1)
r <- c(greens$Year, rep(1,nreps*J)) #observations on an island evenly split between years

#predictors (t=treatment,s=sex,r=year)
b_t=0
b_tr=-30
b_r=0
b_s=80
b_tsr=-40


#island-specific means and SDs are pulled from actual baseline data
sites=unique(greens$Site)
real=data.frame(Site=sites,
                Avg=rep(NA,length(sites)),
                Sigma=rep(NA,length(sites)))


for(i in 1:length(real$Site)){
  rv=greens$PerchHeight[greens$Site==as.character(real$Site[i])]
  real$Avg[i]=mean(rv,na.rm = TRUE)
  real$Sigma[i]=sqrt(var(rv,na.rm = TRUE))
}


#define island intercepts
a_island=real$Avg
mu_a=mean(a_island) #just for reference
sig_a=sd(a_island) #just for reference

#simulate data
sig_y <- mean(real$Sigma) #calculate residual (within island) variance from mean within-island variance of realbaseline data
####   OR   ####
#sig_y=40 #assume less residual variation than there probably actually is
y <- c(greens$PerchHeight,rep(0,Year2N))
L <-0 # lower bound on response variable
for (n in (1+Year1N):(N)){
  y[n] <- rtruncnorm(1,a=L,b=Inf,a_island[islandnum[n]] + b_t*t[n] + b_tr*t[n]*r[n] +
                       b_r*r[n] + b_s*s[n] + b_tsr*t[n]*s[n]*r[n], sig_y)
}


##################
###   FITMODEL ###
##################

dat <- list(N=N,J=J,islandnum=islandnum,t=t,s=s,r=r,L=L,y=y)

#WITH LARGE N THIS WILL TAKE A WHILE
fitme <- stan(file = 'perchheight_truncnormal_v2.stan', 
              data=c("N","J","islandnum","t","s","r","L","y"), iter=1000, 
              control=list(adapt_delta = 0.9, stepsize = 0.5))

N
b_t
b_tr
b_r
b_s
b_tsr
a_island
mu_a
sig_a
sig_y

print(fitme)
plot(fitme, pars=c("b_t","b_tr","b_r","b_s","b_tsr","mu_a","sig_a","sig_y"))
#dev.copy2pdf(file="fitme7")
plot(fitme, pars=c("a_island[1]","a_island[2]","a_island[3]","a_island[4]","a_island[5]","a_island[6]"))
#dev.copy2pdf(file="fitme7_islandints")


###################################
### POSTERIOR PREDICTIVE CHECK  ###
###################################

set.seed(Sys.time()) #undo original seed set to reintroduce randonmness

sims=as.data.frame(as.matrix(fitme)) #posterior distribution of paramter estimates
#write.csv(sims, "posterior7.csv")
n_sims=nrow(sims) #number of sets parameter estimates
y_rep=array(NA,c(n_sims,n)) #will hold fake data simulated from posterior parameter estimates

#Simulate data
#WITH LARGE N and n_sims THIS WILL TAKE A WHILE
for (k in 1:n_sims){
  a_island_temp <- rnorm(J,sims$mu_a[k], sims$sig_a[k]); #generates island intercepts
  for(p in 1:N){
    y_rep[k,p]=rtruncnorm(1,a=L,b=Inf, a_island_temp[islandnum[p]] +
                            sims$b_t[k]*t[p] +
                            sims$b_tr[k]*t[p]*r[p] +
                            sims$b_r[k]*r[p] +
                            sims$b_s[k]*s[p] +
                            sims$b_tsr[k]*t[p]*s[p]*r[p],sims$sig_y[k])
  }
}

#This data frame PPCdat contains all predictors (islandnum,t=treatment,s=sex,r-year),...
#perch height from original simulated fake data (y), and simulated data columns using
#parameter values from the posterior distribution of the model fitme (there are n_sims such columns)
PPCdat=as.data.frame(cbind(islandnum,t,s,r,y,t(y_rep)))
#write.csv(PPCdat, "PPCdat7.csv")
#PPCdat=read.csv(file="PPCdat2.csv")

#Now run PPC by comparing between year perch height difference on treatment islands to
#between year perch height difference on control islands
#If lizards on treatment islands did perch lower, PHdiff will be negative
treatY0=subset(PPCdat,t==1 & r==0)
treatY1=subset(PPCdat,t==1 & r==1)
consY0=subset(PPCdat,t==0 & r==0)
consY1=subset(PPCdat,t==0 & r==1)

PHdiff=rep(0,n_sims)
for (r in 1:n_sims){ #this loop calculates perch height difference for simulated data
  PHdiff[r]=(mean(treatY1[,(r+5)])-mean(treatY0[,(r+5)]))-
    (mean(consY1[,(r+5)])-mean(consY0[,(r+5)]))
  
}
realPHdiff=(mean(treatY1$y)-mean(treatY0$y))-
  (mean(consY1$y)-mean(consY0$y)) #the "real" perch height difference (from original fake data)

par(mfcol=c(1,1))
hist(PHdiff,breaks=100) #plot values and compare to the "real value"
abline(v=realPHdiff,col="red",lty=2)
abline(v=-50,col="blue")
#dev.copy2pdf(file="PPC7")

mean(PHdiff>0) #calculate % of PPC values that fall on the "wrong" side of zero

par(mfcol=c(2,2)) #plot "real data" from years 1 and 2
hist(consY0$y,breaks=100,xlim=c(0,600),ylim=c(0,5))
hist(consY1$y,breaks=100,xlim=c(0,600),ylim=c(0,5))
hist(treatY0$y,breaks=100,xlim=c(0,600),ylim=c(0,5))
hist(treatY1$y,breaks=100,xlim=c(0,600),ylim=c(0,5))
#dev.copy2pdf(file="Hist of Real Data 7")

#THE CODE HERE IN SECTION 5 WAS USED TO EXECUTE...
#SIMULATIONS 5 AND 6 PRESENTED IN THE WRITTEN REPORT...
#MODEL FITTING AND PPCs WERE IDENTICAL TO METHODS PERFORMED ON...
#SIMULATED FAKE DATA (SIMULATIONS 1-4).
