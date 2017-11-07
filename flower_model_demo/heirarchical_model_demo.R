##Flickr data heirarchical modeling demo.
##Author: Ian Breckheimer
##Created: 1 November 2017

##Sets up workspace and loads packages
##To get this to work, you need to first install STAN.
library(ggplot2)
library(dplyr)
library(rstanarm)

##Set working directory (replace with the correct path for your machine)
setwd("~/code/PhenologyPostdoc")

##Brings in data.
fdat <- read.csv("./data/Flickr_photo_demodata.csv")
fdat$gap_lc_code <- as.factor(fdat$gap_lc_code)

##Data summary.
fdat_sum <- summary(fdat)
fdat_sum

####Question 1: Do all sites contain enough photos of flowers to be useful? Stated more precisely, are
####there any sites where the proportion of flower photos is less than 10%? If so, we might want to
####drop those sites from the study.
summary(fdat$site)

##Fits a model with an estimate for each site (no pooling).
fdat$flwr_binary <- as.numeric(fdat$has_flower=="Y")
flwr_prob_mod <- stan_glm(flwr_binary~site,family=binomial(link="logit"),data=fdat,cores=4)
summary(flwr_prob_mod)
plot(flwr_prob_mod)

##Gets posterior estimates on the probability scale.
sites <- data.frame(site=unique(fdat$site))

lp <- posterior_linpred(flwr_prob_mod,newdata=sites,transform=TRUE)
quantfun <- function(x){quantile(x,probs = c(0.025,0.25,0.5,0.75,0.975))}
lp_quants <- t(apply(lp,FUN=quantfun,MARGIN=2))
sites$lwr_025 <- lp_quants[,1]
sites$lwr_25 <- lp_quants[,2]
sites$median <- lp_quants[,3]
sites$upr_75 <- lp_quants[,4]
sites$upr_975 <- lp_quants[,5]

##Plots the results.
fig1 <- ggplot(sites)+
          geom_linerange(aes(x=site,ymin=lwr_025,ymax=upr_975))+
          geom_linerange(aes(x=site,ymin=lwr_25,ymax=upr_75),lwd=1.5)+
          geom_point(aes(x=site,y=median),shape=21,fill="white")+
          geom_abline(aes(intercept=0.1,slope=0),linetype="dotted")+
          coord_flip()+
          scale_y_continuous("Proportion with Flowers",limits=c(0,1))+
          theme_bw()
fig1

##Check your understanding:

#1. Are there any sites where we are extremely confident (p>0.975) that 
#   the proportion of flowers is less than 10%? 

#2. Even though all the sample sizes are identical, there is some variation
#   in how wide the uncertainty intervals are. Additionally, some of these intervals
#   are asymmetric. Why is this the case?

#Independent Problem 1: This is a small sample of a very large
#dataset consisting of 476334 photos. Calculate a best estimate and 95% uncertainty intervals
#for the total number of flower photos in the dataset based on the sample of 3800 photos
#we used here.


####Question 2: Are there ecological patterns in the data? Is there significant variation 
#### in how sensitive flower timing is to
#### climate across sites? If so, how much?

fdat_f <- subset(fdat,flwr_binary==1)
summary(fdat_f)

##Centers predictor
fdat_f$spring_t_ct<- fdat_f$daymet_spring_t - mean(fdat_f$daymet_spring_t)

##Re-orders levels by sample size.
site_n <- summary(fdat_f$site)
sites <- levels(fdat_f$site)
sites <- sites[order(site_n)]

fdat_f$site <- factor(fdat_f$site,levels = sites,
                      ordered=TRUE)

##Plots independent fits with no pooling.
fig2 <- ggplot(fdat_f)+
          geom_point(aes(x=spring_t_ct,y=datetaken_doy,group=site))+
          geom_smooth(aes(x=spring_t_ct,y=datetaken_doy,group=site),method="lm",se=FALSE)+
          facet_wrap(facets=~site)+
          theme_bw()
fig2

##Estimates these fits independently.
sites <- levels(fdat_f$site)
nsites <- length(sites)
nopool_est <- data.frame(site=factor(levels(fdat_f$site),levels=levels(fdat_f$site)),
                         sl_med = rep(NA,nsites),
                         sl_upr_90 = rep(NA,nsites),
                         sl_lwr_10 = rep(NA,nsites),
                         int_med = rep(NA,nsites),
                         int_upr_90 = rep(NA,nsites),
                         int_lwr_10 = rep(NA,nsites))

for(i in 1:nsites){
  site_mod <- stan_glm(datetaken_doy~spring_t_ct,
                      family=gaussian,prior=normal(0,100),
                      data=subset(fdat_f,site==sites[i]))
  site_int <- posterior_interval(site_mod,prob=0.80)
  nopool_est$int_med[i] <- site_mod$coefficients[1]
  nopool_est$int_lwr_10[i] <- site_int[1,1]
  nopool_est$int_upr_90[i] <- site_int[1,2]
  nopool_est$sl_med[i] <- site_mod$coefficients[2]
  nopool_est$sl_lwr_10[i] <- site_int[2,1]
  nopool_est$sl_upr_90[i] <- site_int[2,2]
}

nopool_est$method <- "No Pooling"

##Plots the results.
fig3 <- ggplot(nopool_est)+
          geom_linerange(aes(x=site,ymin=sl_lwr_10,ymax=sl_upr_90))+
          geom_point(aes(x=site,y=sl_med),shape=21,fill="white")+
          geom_abline(aes(intercept=0,slope=0),linetype="dotted")+
          coord_flip()+
          scale_y_continuous("Temp. Sensitivity (days / C)",
                             limits=c(-40,30))+
          theme_bw()
fig3

###Sensitivities with partial pooling.
ppool_est <- data.frame(site=factor(levels(fdat_f$site),levels=levels(fdat_f$site)),
                         sl_med = rep(NA,nsites),
                         sl_upr_90 = rep(NA,nsites),
                         sl_lwr_10 = rep(NA,nsites),
                         int_med = rep(NA,nsites),
                         int_upr_90 = rep(NA,nsites),
                         int_lwr_10 = rep(NA,nsites))
doy_mod_partialp1 <- stan_glmer(datetaken_doy~spring_t_ct + (1 | site),family=gaussian,
                              prior=normal(),data=fdat_f,
                              cores=4)
doy_mod_partialp2 <- stan_glmer(datetaken_doy~spring_t_ct + (spring_t_ct | site),family=gaussian,
                                prior=normal(),data=fdat_f,
                                cores=4)
p2_samples <- tbl_df(as.data.frame(doy_mod_partialp2))
p2_sl <- p2_samples$spring_t_ct
p2_int <- p2_samples$'(Intercept)'

quantfun2 <- function(x){quantile(x,probs = c(0.1,0.25,0.5,0.75,0.9))}

for(i in 1:length(sites)){
  p2_params_site <- select(p2_samples,contains(as.character(sites[i])))
  sl_samps <- rowSums(cbind(p2_sl,p2_params_site[,2]))
  int_samps <- rowSums(cbind(p2_int,p2_params_site[,1]))
  site_samps <- cbind(int_samps,sl_samps)
  site_quants <- t(apply(site_samps,FUN=quantfun2,MARGIN=2))
  ppool_est$int_med[i] <- site_quants[1,3]
  ppool_est$int_upr_90[i] <- site_quants[1,5]
  ppool_est$int_lwr_10[i] <- site_quants[1,1]
  ppool_est$sl_med[i] <- site_quants[2,3]
  ppool_est$sl_upr_90[i] <-  site_quants[2,5]
  ppool_est$sl_lwr_10[i] <- site_quants[2,1]
}
ppool_est$method <- "Partial Pooling"
all_est <- rbind(nopool_est,ppool_est)

##Sensitivity with complete pooling.
doy_mod_completep <- stan_glm(datetaken_doy~spring_t_ct,family=gaussian,
                              prior=normal(0,100),data=fdat_f,
                              cores=4)
completep_int80 <- posterior_interval(doy_mod_completep,prob=0.80)

partialp_int80 <- posterior_interval(doy_mod_partialp2,prob=0.80,
                                     pars=c('(Intercept)','spring_t_ct'))

overall_est_comp <- data.frame(site="Overall",
                          sl_med=doy_mod_completep$coefficients[2],
                          sl_upr_90=completep_int80[2,2],
                          sl_lwr_10=completep_int80[2,1],
                          int_med=doy_mod_completep$coefficients[1],
                          int_upr_90=completep_int80[1,2],
                          int_lwr_10=completep_int80[1,1],
                          method="No Pooling")
overall_est_part <- data.frame(site="Overall",
                               sl_med=doy_mod_partialp2$coefficients[2],
                               sl_upr_90=partialp_int80[2,2],
                               sl_lwr_10=partialp_int80[2,1],
                               int_med=doy_mod_partialp2$coefficients[1],
                               int_upr_90=partialp_int80[1,2],
                               int_lwr_10=partialp_int80[1,1],
                               method="Partial Pooling")
all_est_overall <- rbind(all_est,overall_est_comp,overall_est_part)

##Plots the results.
fig4 <- ggplot(all_est_overall)+
            geom_linerange(aes(x=site,ymin=sl_lwr_10,ymax=sl_upr_90))+
            geom_point(aes(x=site,y=sl_med),shape=21,fill="white")+
            geom_abline(aes(intercept=0,slope=0),linetype="dotted")+
            facet_wrap(facets=~method)+
            scale_y_continuous("Temp. Sensitivity (days / C)")+
            coord_flip()+
            theme_bw()
fig4

##Plots just the overall effects.
overall_est <- subset(all_est_overall,site=="Overall")
overall_est$method <- c("Complete Pooling","Partial Pooling")

fig5 <- ggplot(overall_est)+
  geom_linerange(aes(x=method,ymin=sl_lwr_10,ymax=sl_upr_90))+
  geom_point(aes(x=method,y=sl_med),shape=21,fill="white")+
  geom_abline(aes(intercept=0,slope=0),linetype="dotted")+
  #facet_wrap(facets=~method)+
  scale_y_continuous("Temp. Sensitivity (days / C)")+
  coord_flip()+
  theme_bw()
fig5

##Check your understanding:

#3. Why is there so much less variation in the climate sensitivity of different sites in the
#partial pooling example compared to the no pooling example? How might this change if we had
#data for all 470000 photos? What if the linear relationships for each group were very strong?

#4. Why does there seem to be more uncertainty in the partial pooling estimate of 
# the overall relationship between flowering time and spring temperature compared 
# to the complete pooling estimate?

##Independent Problem 2:
#Write down a mathematical description of the partial pooling model that is 
#fit on line 143 "doy_mod_partialp2". Start with a statement about the probability 
#distribution that the data is drawn from.



