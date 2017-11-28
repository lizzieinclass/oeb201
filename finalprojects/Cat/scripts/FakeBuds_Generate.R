## Building fake data for experiment data and duration of vegetative risk
# Based on script by Dan Flynn and Lizzie from Buds repo 
## Updated: 6 November 2017 - Cat

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)

setwd("~/Documents/git/freezingexperiment/analyses/scripts")
bb <- read.csv("..//output/birches_buddata.csv", header=TRUE) ## to check data

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: 2 species, two levels for treatment, 7 individuals per species per treatment, 22 buds per individual
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsp = 2
nind = 7 # random effect

ntx = 2 

rep = 22 # within each combination of treatments 

(ntot = nsp*ntx*rep) # 88 rows

# Build up the data frame
sp = gl(nsp, rep, length=ntot)

tx = gl(ntx, rep*nsp, length = ntot)

(d <- data.frame(sp, tx)) 

###### Set up differences for each level
spdiff = 0.2
txdiff = 2.5

######## SD for each treatment
spdiff.sd = 0.1
txdiff.sd = 0.1

mm <- model.matrix(~(sp+tx), data.frame(sp, tx)) ### ORDER HERE REALLY MATTERS!!! MAKE SURE IT LINES UP WITH "COEFF"
## Coding check below - keep for future tweaks to code/data
#coeff <- c(1, spdiff, txdiff)
#risk <- rnorm(n = length(tx), mean = mm%*%coeff, sd = 1) # should be able to do sd = mm %*% sd.coeff as well, with a different sd for each parameter.
#(fake <- data_frame(risk, tx, sp))

##### Again, now with individuals.

baseinter = 11 # baseline intercept across all individuals for DVR
spint <- baseinter + c(1:nind)-mean(1:nind) # different intercepts by individual

fake <- vector()

for(i in 1:nind){ # loop over individual (random effect of model)
  
  # Give individuals different difference values, drawn from normal
  
  coeff <- c(spint[i], 
             rnorm(1, spdiff, spdiff.sd),
             rnorm(1, txdiff, txdiff.sd)
  )
  
  dvr <- rnorm(n = length(tx), mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(dvr, ind=i, sp, tx)
  
  fake<-rbind(fake, fakex)
}    

summary(lm(dvr ~ tx+sp, data = fake)) # sanity check 

# now fix the levels to 0/1 (not 1/2) as R does
fake$tx <- as.numeric(fake$tx)
fake$tx[fake$tx==1] <- 0
fake$tx[fake$tx==2] <- 1

summary(lm(dvr ~ tx+sp, data = fake)) # double check 

#save(list=c("fake"), file = "Fake Buds.RData")
#write.csv(fake, file="~/Documents/git/freezingexperiment/analyses/output/fakedata_exp.csv", row.names = FALSE)

#mean(fake$risk)
#sd(fake$risk)
#hist(fake$risk)
#length(fake$sp[fake$sp==1])

### run rstanarm models to check the fake data - prepare for running stan code
mod1<-stan_glmer(dvr~tx+sp+(1|ind), data=fake)
plot(mod1, pars="beta")
pp_check(mod1)

### Use this script to learn apply version to build fake data
