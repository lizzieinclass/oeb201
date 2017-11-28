## Building fake data for experiment data and duration of vegetative risk
# Based on script by Dan Flynn and Lizzie from Buds repo 
## Updated: 6 November 2017 - Cat

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)
library(rstanarm)

setwd("~/Documents/git/freezingexperiment/analyses/scripts")
bb <- read.csv("..//output/percentBB_betula.csv", header=TRUE) ## to check data
bb$tx<-ifelse(bb$tx=="A", 0, 1)
bb$perc<-bb$perc.bb/100
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: 2 species, two levels for treatment, 7 individuals per species per treatment, 22 buds per individual
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsp = 2
ntx = 2 

rep = 7 # within each combination of treatments 

(ntot = nsp*ntx*rep) # 88 rows

# Build up the data frame
#sp = gl(nsp, rep, length=ntot)

tx = gl(ntx, rep, length = ntot)
sp = gl(nsp, rep*ntx, length = ntot)

(d <- data.frame(tx, sp)) 

###### Set up differences for each level
txdiff = -.04
spdiff = .01

######## SD for each treatment
txdiff.sd = 0.005
spdiff.sd = 0.001

mm <- model.matrix(~(tx+sp), data.frame(tx,sp)) ### ORDER HERE REALLY MATTERS!!! MAKE SURE IT LINES UP WITH "COEFF"


##### Again, now with individuals.

baseinter = 0.70 # baseline intercept across all individuals for DVR


coeff <- c(baseinter, 
             rnorm(1, txdiff, txdiff.sd),
             rnorm(1, spdiff, spdiff.sd)
  )
  
perc <- rnorm(n = length(tx), mean = mm %*% coeff, sd = 0.01)
  
fake <- data.frame(perc, sp, tx)
  
summary(lm(perc ~ tx + sp, data = fake)) # sanity check 

# now fix the levels to 0/1 (not 1/2) as R does
fake$tx <- as.numeric(fake$tx)
fake$tx[fake$tx==1] <- 0
fake$tx[fake$tx==2] <- 1

summary(lm(perc ~ tx + sp, data = fake)) # double check 

#save(list=c("fake"), file = "Fake Buds.RData")
#write.csv(fake, file="~/Documents/git/freezingexperiment/analyses/output/fakebeta.csv", row.names = FALSE)

#mean(fake$risk)
#sd(fake$risk)
#hist(fake$risk)
#length(fake$sp[fake$sp==1])

### run rstanarm models to check the fake data - prepare for running stan code
mod1<-stan_betareg(perc~tx+sp, data=fake, link="logit", link.phi = "log")
plot(mod1)
pp_check(mod1)

### Use this script to learn apply version to build fake data
