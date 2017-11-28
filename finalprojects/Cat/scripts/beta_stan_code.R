## Started 27 September 2017 ##
## By Cat ##

## Try to run REAL data ##
## With Stan! ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/freezingexperiment/analyses/")
source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
bb<-read.csv("output/percentBB_betula.csv", header=TRUE)
bb<-read.csv("output/fakebeta.csv", header=TRUE)

## make a bunch of things numeric 
bb$tx<-ifelse(bb$tx=="A", 0, 1)
bb$sp <- as.numeric(as.factor(bb$sp))
bb$perc <- as.numeric(bb$perc)
bb$perc <- bb$perc/100


## subsetting data, preparing genus variable, removing NAs
pp.prepdata <- subset(bb, select=c("perc", "tx", "sp")) # removed "sp" when doing just one species
pp.stan <- pp.prepdata[complete.cases(pp.prepdata),]


perc = pp.stan$perc
tx = pp.stan$tx
sp = pp.stan$sp
N = length(perc)


# making a list out of the processed data. It will be input for the model
datalist.td <- list(perc=perc,tx=tx,sp=sp,N=N) # removed sp=sp and n_sp=s_sp for one species



##############################
###### real data rstanarm first

fit1<-stan_betareg(perc~tx+sp, data=pp.stan)
fit1
plot(fit1, pars=c("tx","sp"))
pp_check(fit1)
prior_summary(fit1)

### Another posterior predictive check
yrep <- posterior_predict(fit1)
all.equal(ncol(yrep), nobs(fit1)) # TRUE
nd <- data.frame(perc = mean(pp.stan$perc), tx, sp)
ytilde <- posterior_predict(fit1, newdata = nd)
all.equal(ncol(ytilde), nrow(nd)) # TRUE

#### Now using rstan model
# Had divergent transitions and the number would vary each time, I increased the warmup and now there are 4
# divergent transitions
pp.td4 = stan('scripts/perc_sp_pred_beta.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.99)) 
betas <- as.matrix(pp.td4, pars = c("mu_tx", "mu_sp"))
mcmc_intervals(betas)


posterior<-extract(pp.td4)
y_pred <- as.matrix(unlist(posterior, use.names=FALSE))
color_scheme_set("brightblue")
#pp<-mcmc_trace(posterior, pars=c("mu_b_tx", "mu_b_sp"), n_warmup=6000, facet_args = list(nrow = 2,
                                                                                #labeller=label_parsed))
#pp+facet_text(size = 15)
mcmc_areas(posterior,
           pars = c("mu_tx", "mu_sp"),
           prob = 0.8) 

ppc_intervals(
  y = pp.stan$perc,
  yrep = posterior_predict(fit1),
  x = pp.stan$tx,
  prob = 0.5
) +
  panel_bg(fill="gray95", color=NA) +
  grid_lines(color="white") +
  labs(x = "Treatment", y = "Percent Budburst")

ppc_intervals(
  y = pp.stan$perc,
  yrep = posterior_predict(fit1),
  x = pp.stan$sp,
  prob = 0.5
) +
  panel_bg(fill="gray95", color=NA) +
  grid_lines(color="white") +
  labs(x = "Species", y = "Percent Budburst")

launch_shinystan(pp.td4) # use for posterior predictive checks

td4 <- summary(pp.td4)$summary # yhats around 1! double yay!
preds.4<-td4[grep("yhat", rownames(td4)),]

#save(td4, file="output/Buds_individLevel.Rda")
#save(dvr.td4, file="~/Documents/git/freezingexperiment/analyses/output/buds_2level_real.Rda")


