####This is the script for Dan's OEB201 project. Fall 2017.
###GOAL: model actual bud volume for the dataset Jehane collected as if all buds were measured on the same day.

###Load packages and data
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()
setwd("~/Documents/git/twoohonebuds")
#load (file = ".RData")

library(ggplot2)
library(dplyr)
library("tidyr")
library(tidyverse)
library(plyr)
library(lme4)
library(rstanarm)
library(rstan)
library("arm")
library("rmutil")
library("MCMCglmm")

dater<-read.csv("input/BUDSET_Dissection_Data_April30.csv") ###read in bud data

###PART IA: Data cleaning######
colnames(dater)

unique(dater$species)
unique(dater$Genus)
##### spaces in genus;fix
dater$Genus <- sub("^ | $", "", dater$Genus) 

####make $name column-- Genus_species
dater$name<-paste(dater$Genus,dater$species,sep="_")
unique(dater$name)
###returns Populus grandifolia (not a species) Fix
dater$species[dater$Genus=="Populus" & dater$species=="grandifolia"] <- "grandidentata"
dater$name<-paste(dater$Genus,dater$species,sep="_")
unique(dater$name)

#clean date
dater$date_measure1 <- as.Date(dater$date_measure, format="%Y-%m-%d")
###make day of year
dater$doy <- as.numeric(format(dater$date_measure1, "%j"))

#clean site
unique(dater$Site)
dater$Site[dater$Site=="St. Hippolyte"] <- "Saint Hippolyte"
unique(dater$Site)

###clean location on twig: 
dater$bud_location <- sub("^ | $", "", dater$bud_location) 
unique(dater$bud_location)
dater$bud_location[dater$bud_location=="Terminal_twig"] <- "Terminal"
dater$bud_location[dater$bud_location=="Pseudoterminal_twig"] <- "Pseudoterminal" 
unique(dater$bud_location)

####calculate bud volume: Formula for cone volume: V=pi*r^2*(h/3)
dater$bud_volume <- pi*(dater$bud_width/2)^2*(dater$bud_length/3)

####Part IB: Data exploration#####

##give things nicknames so will me more visable on figures
dater$nickname[dater$name=="Acer_pensylvanicum"] <- "A.pe"
dater$nickname[dater$name=="Acer_rubrum"] <- "A.ru"
dater$nickname[dater$name=="Acer_saccharum"] <- "A.sa"
dater$nickname[dater$name=="Alnus_incana"] <- "A.in"
dater$nickname[dater$name=="Betula_alleghaniensis" ] <- "B.al"
dater$nickname[dater$name=="Betula_papyrifera" ] <- "B.pa"
dater$nickname[dater$name=="Corylus_cornuta" ] <- "C.co"
dater$nickname[dater$name=="Fagus_grandifolia" ] <- "F.gr"
dater$nickname[dater$name=="Fraxinus_nigra" ] <- "F.ni"
dater$nickname[dater$name=="Ilex_mucronata" ] <- "I.mu"
dater$nickname[dater$name=="Lonicera_canadensis" ] <- "L.ca"
dater$nickname[dater$name=="Populus_grandidentata" ] <- "P.gr"
dater$nickname[dater$name=="Prunus_pensylvanica" ] <- "P.pe"
dater$nickname[dater$name=="Quercus_rubra" ] <- "Q.ru"
dater$nickname[dater$name=="Spiraea_alba" ] <- "S.al"
dater$nickname[dater$name=="Vaccinium_myrtilloides" ] <- "V.my"
dater$nickname[dater$name=="Viburnum_cassinoides" ] <- "V.ca"
dater$nickname[dater$name=="Viburnum_lantanoides" ] <- "V.la"

###data summary
budvolume_summary <- ddply(dater, c("Site", "name"), summarise,
                           N = length(bud_volume),
                           mean = mean(bud_volume),
                           sd   = sd(bud_volume),
                           se   = sd / sqrt(N))
budvolume_summary

##some exploratory plots
###plot: bud volume by species and site
ggplot(dater,aes(nickname,bud_volume,col=Site))+stat_summary() ###no consistant trend in sites differences. could be an artifact of when they were measured.
###plot: measurement date
ggplot(dater, aes(doy))+geom_histogram(binwidth=1,aes(color=Site)) ###significant temporal bias
##plot: location on twig
ggplot(dater,aes(nickname,bud_volume,col=bud_location))+stat_summary() ###terminal buds generally largest.


###############################Part II: Modeling#######################################################
###Part II:A: Fake data
set.seed(73)

#Varying slope, varying intercept
nsp = 20 # number of species
ntot = 100 # numbers of obs per species.
baseinter <- 8 # baseline intercept (budvol) across all species
spint <- baseinter +rnorm(20,0,1) # different intercepts by species
baseeff<-.1 ##baseline effect size
speff<- baseeff+rnorm(20,0,.02)  ##diferent effect by species
# now start building ...
testdat2 <- vector()
for(i in 1:nsp){ # loop over species. i = 1
  # continuous predictors, generate level for each observation
  doy<- rtnorm(ntot,45,3,lower=30,upper=60) 
  ## set up effect size
  doycoef<-speff
  doycoef.sd<-.1
  # build model matrix 
  mm <- model.matrix(~doy, data.frame(doy))
  # coefficients need to match the order of the colums in the model matrix (mm)
  coeff <- c(spint[i], 
             rnorm(1, doycoef[i], doycoef.sd))
  
  bvol <- rnorm(n = ntot, mean = mm %*% coeff, sd = 4) ###sd here is sigma
  
  testdatx <- data.frame(bvol, sp = i, 
                         doy)
  testdat2 <- rbind(testdat2, testdatx)  
}

##Visualize new data
ggplot(testdat2,aes(bvol))+geom_density()
ggplot(testdat2,aes(bvol))+geom_density()+facet_wrap(~sp)
##some have negative volumes, which seems bioligcally unrealistic, but overall pretty good

###model for fake data
truvol.slope<-stan_lmer(bvol~doy+(doy|sp), testdat2)


truvol.slope
fixef(truvol.slope)
posterior_interval(truvol.slope)
ranef(truvol.slope)

##posterior predictive check
pp_check(truvol.slope)

##plot origanl vs. predicted
yrep<-as.data.frame(posterior_predict(truvol.slope,draws=1))
ncol(yrep)
yrep<-gather(yrep,ob,pred,1:2000)
yrep$orig<-testdat2$bvol

ggplot(yrep,aes(orig,pred))+geom_point()+geom_abline(mapping = NULL, data = NULL, slope=1, intercept=0,color="red",
                                                     na.rm = FALSE, show.legend = NA)+geom_smooth(method = "lm", se=TRUE, color="blue", formula = y ~ x)
###okayish

### Below adjusts to "true" bud vol at day if everything was measured at day 40
slopes<-coef(truvol.slope)
B<-slopes$sp$doy ## coefficients for each species
#c<-40 ### imaginary day of estimation
#vol<-testdat2$bvol###

#bias correction for each observation
for(i in 1:nsp){
  testdat2$tru<-testdat2$bvol-(testdat2$doy-40)*B[i]
}

###Part II:B, real data
##To gauge bud vol change, subset the data excluding data measured in March. 
# Logic: exclude the bulk of the SH measurements, now focusing on initial and repeated HF measures, and SH measures made at the repeated measure time (Late April)
daternoMarch <- subset(dater, doy<60 | doy>90)

###look at distribution
ggplot(daternoMarch,aes(bud_volume))+geom_density()
ggplot(daternoMarch,aes(bud_volume))+geom_density()+facet_wrap(~nickname) ##not so normal

###log transform response:
daternoMarch$log_bvol<-log(daternoMarch$bud_volume)
ggplot(daternoMarch,aes(log_bvol))+geom_density() ##now its normal

### partial pooling model
truvol<-stan_lmer(log_bvol~doy+(doy|name), daternoMarch,cores=4)
##check it out
truvol
coef(truvol)
posterior_interval(truvol)
ranef(truvol)
pp_check(truvol)
#launch_shinystan(truvol)

### correct to day 40
slopes<-coef(truvol)
B<-slopes$name$doy
c<-40

#loop to apply beta for a true estimate
N<-as.data.frame(specieslist)
N<-nrow(N)
for(i in 1:N){
  dater$tru<-dater$bud_volume-((dater$doy-c)*B[i])
}

#Plot adjusted (true) vs original bud volume
Z<-dplyr::select(dater,nickname,bud_volume,tru)
Q<-gather(Z,change,volume,2:3)
ggplot(Q,aes(nickname,volume))+geom_jitter(height=0,width=2,aes(color=change))


####Part III: Phenology
d<-read.csv("input/Budburst By Day.csv") ##read in Dan Flynn's data
dd<-dplyr::select(d,ind,sp,rep,lday,bday) ### choose relevant columns
meanbb<- dplyr::group_by(dd,ind)
meanbb<-dplyr::summarise(meanbb,meanbday=mean(bday,na.rm=TRUE)) ### take the mean for budburstday

###average budvol from Jehane's data (corrected for bias)
colnames(dater)[which(names(dater) == "individual_ID")] <- "ind"
gooby<- dplyr::group_by(dater,ind)
gooby1<-dplyr::summarise(gooby,meanbvol=mean(tru)) ##take the mean
gooby2<-dplyr::summarise(gooby,meanstemdiam=mean(stem_diameter,na.rm=TRUE)) ## take the mean of stem diameter too
###make new dataset 
newdat<-right_join(meanbb,gooby1,by="ind")
newdat<-right_join(newdat,gooby2,by="ind")
###make a column with just Genus_species 
newdat<-transform(newdat, SP = substr(ind, 1, 6), ID = substr(ind, 7, 11))

###View data
ggplot(newdat,aes(meanbvol))+geom_density() ## yuck
ggplot(newdat,aes(meanbday))+geom_density() ### ew
ggplot(newdat,aes(meanstemdiam))+geom_density() ###none are normal

###o well, try a model
moodle<-stan_glmer(meanbday ~ meanstemdiam + meanbvol + (meanstemdiam+meanbvol|SP),
         data = newdat, family = gaussian, 
         prior = normal(0,2.5), prior_intercept = normal(0,5),
          cores = 4)
moodle
pp_check(moodle)
coef(moodle)
posterior_interval(moodle)
launch_shinystan(moodle)
citation("rstanarm")


