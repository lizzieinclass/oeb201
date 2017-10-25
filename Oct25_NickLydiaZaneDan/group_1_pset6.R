#Problem Set 6, 10/25/2017
#Group	
#Dan dbuonaiuto@g.harvard.edu
#Nick	nherrmann@g.harvard.edu
#Zane	rzwolf@g.harvard.edu
#Lydia	lakrasilnikova@g.harvard.edu


rm(list=ls())

library (foreign)
library(arm)
library(rstanarm)
library(dplyr)
# loading data (download: http://www.stat.columbia.edu/~gelman/arm/examples/child.iq/child.iq.dta)
setwd("~/Desktop/child.iq")
child <- read.dta ("child.iq.dta")
str(child)
summary(child)

#Find a model you have fit previously for homework using Gelman & Hill data and
#perform posterior predictive checks (PPC) on it (see section 15.5).

#univariate model predicting child iq from mom's age

fit = stan_glm(ppvt ~ momage,data=child)
print(fit)

#simulate fake data using the 4000 parameter estimates extracted from the model (posterior)

sims=as.matrix(fit)
n_sims=nrow(sims)

n=length(child$ppvt)
y_rep = array (NA,c(n_sims,n))
for (i in 1:n_sims){
  y_rep[i,]= sims[i,1]+sims[i,2]*child$momage+rnorm(n,0,sims[i,3])
}

#now y_rep contains 4000 simulated data sets for childiq, each with 400 entries
#(same size as original data set)

#quick graph showing original data and least squares line (top left)
#with the first 11 simulated data sets and best fit lines

par(mfrow=c(4,3))
plot(ppvt ~ momage,data=child)
mod1=lm(ppvt ~ momage,data=child)
abline(mod1)
for(j in 1:11){
  plot(y_rep[j,]~child$momage)
  mod=lm(y_rep[j,] ~ momage,data=child)
  abline(mod)
}

#How well do the coefficient's for mom's age from these simulated data match the original model?

#This loop extracts the coefficient from all 4000 simulated data sets
betas=as.numeric()
for(j in 1:nrow(y_rep)){
  mod=lm(y_rep[j,] ~ momage,data=child)
  betas=c(betas,coef(mod)[2])
}

#Plot a histogram of coefficient with a red line for the coefficient from the original data/model
par(mfrow=c(1,1))
hist(betas, breaks=50)
abline(v=coef(mod1)[2],col="red")

#looks good!!

#We can also use the pp_check function from rstanarm to visual assess the distributional fits.
pp_check(fit)
#also looks pretty good
