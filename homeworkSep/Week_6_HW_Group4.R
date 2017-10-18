### Week 5 - 11 October 2017
# Meghan Blumstein, Cat Chamberlain, and Dave Matthews 

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load Libraries
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
library(rstanarm)

# Set working directory and load data
setwd("~/Documents/git/statsclass/ARM_Data")


###########
# 13.6 #3 #
###########

### A ###

n80Perc <- function(p){
  #Find 80% power sample size
  #n=P(1-P)(2.8/(P-P0))^2
  nExact <- p*(1-p)*(2.8/(p-0))^2
  
  #Use conservative estimate, SE=.5/sqrt(n)
  #So n = (.5/SE)^2
  n <- c(nExact)
  return(n)
}

# 1 unit dose, .01% risk. True proportion is .0001
n1 <- n80Perc(.0001)
print(n1)

#We need a sample size of at least 78392 to get a positive result 80% of the time.

# 100 unit dose, 1% risk. True proportion .01
n2 <- n80Perc(.01)
print(n2)

#We need a sample size of at least 776 to get a positive result 80% of the time.

# 1 unit dose, 100% risk. True proportion is 1
n3 <- n80Perc(1)
print(n3)

#We need a sample size of at least 1 (? clearly not) to get a positive result 80% of the time.


### B ###
#Now log scaling of drug effect
# 1 unit dose, .01% risk. True proportion is .0001
n4 <- n80Perc(.0001)
print(n4)

#We need a sample size of at least 78392 to get a positive result 80% of the time.

# 100 unit dose, logarithmic scaling of effect. .03% risk. True proportion .0003
n5 <- n80Perc(.0003)
print(n5)

#We need a sample size of at least 26125 to get a positive result 80% of the time.

# 1 unit dose, logarithmic scaling of effect. .05% risk. True proportion is .0005
n6 <- n80Perc(.0005)
print(n6)

#We need a sample size of at least 15672 to get a positive result 80% of the time.


# When the drug has a linearly increasing effect, each increase in dosing brought a reduction in
#the necessary sample size of several orders of magnitude. By giving more of the toxin (easy change)
#we were able to greatly reduce the sampling effort.
#When the toxin effects were logarithmic, a huge increase in drug dosage lowered the necessary
#sample size, but didn't change the order of magnitude of the sampling effort.

####### Chapter 14 Question 2
### Linear model fits with more explanation -- see below another version that uses stan_glm models

## The older the mom, the more likely they were to respond to the survey
d<-read.dta("child.iq/kidiq.dta")
full_data<-d

full_data$Response_Probability <- (full_data$mom_age/max(full_data$mom_age)) * 0.65 ## Pick probabilties that increase with mom's age and scale so half data is missing
full_data$Response <- sapply(full_data$Response_Probability,function(x) rbinom(1, 1, prob = x) ) ## Random generate which respond and which don't
available_data <- full_data[full_data$Response == 1,] ## keep only responders

## (b) Perform the regression of x on y (that is, with y as predictor and x as outcome), using complete-case analysis
##    (that is, using only the data fro which both variables are observed) and show that it is consistent with the 
##    regression on the full data. 

m1 <- lm(mom_age ~ kid_score, data = available_data)
display(m1)
m2 <- lm(mom_age ~ kid_score, data = full_data)
display(m2)

## Full data and subsetted data produce roughly the same results


## (c) Perform the complete-case regression of y on x and show that is is not consistent with the corresponding regression
##     on the full data 

m1 <- lm(kid_score ~ mom_age, data = available_data)
display(m1)
m2 <- lm(kid_score ~ mom_age, data = full_data)
display(m2)

## In this case, the slope of mom_age is almost halved, indicating that for every year older a mom is, a child performs ~0.48 
## points better on their IQ test on average than in the complete model, whereas the slope is 0.7. The intercepts differ
## by ~5Pts as well. 

## (d) Using just the available data, fit a model in R for x given y, and use this model to randomly impute the missing x data, 
##     Perform the regression of y on x, using this imputed dataset and compare your results to (c). 

predict_model <- lm(mom_age~kid_score, data = available_data)

full_data$Predicted_Mom_Age[full_data$Response == 0] <- predict(predict_model, data.frame(kid_score = full_data$kid_score[full_data$Response == 0]))
full_data$Predicted_Mom_Age[full_data$Response == 0] <-  sapply(full_data$Predicted_Mom_Age[full_data$Response == 0], function(x) x + rnorm(1, mean = 0, sd = sd(available_data$mom_age)) ) ## Add normal skew for random imputation
full_data$Predicted_Mom_Age[full_data$Response == 1] <- full_data$mom_age[full_data$Response == 1]

print("Available Data")
m1 <- lm(kid_score ~ mom_age, data = available_data)
display(m1)

print("Full Data")
m2 <- lm(kid_score ~ mom_age, data = full_data)
display(m2)

print("Predicted Data")
m3 <- lm(kid_score ~ Predicted_Mom_Age, data = full_data)
display(m3)

par(mfrow = c(1,2))
plot(kid_score ~ mom_age, data = full_data, pch = 16)
abline(m1, col = "cornflowerblue")


plot(kid_score ~ Predicted_Mom_Age, data = full_data, pch = 16)
abline(m3, col = "cornflowerblue")

## The original model fit is terrible (R2 = 0), as can be seen in the left-hand figure. Thus when we predict mom's age from 
## child's test scores, all the ages fall between 22 1/2 and 23 1/2. However, when we add random noise that matches the mom_age
## distribution, we have a fairly similar output to the full dataset. 
## When we impute the missing data, we come much closer to the predictions from the full dataset.  

###################################################################################################
###################################################################################################
###################################################################################################
#### Extra version using stan_glm function but same dataset and show similar findings
## Chapter 14 Question 2
## Part A
d<-read.dta("child.iq/kidiq.dta")
dx<-subset(d, mom_iq>=92)
p<-nrow(dx)/nrow(d)
dx<-dplyr::select(dx, mom_iq)
missing<-as.data.frame(lapply(dx, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.26, 0.74), size = length(cc), replace = TRUE) ]))
d.add<-subset(d, mom_iq<92)
d.add<-dplyr::select(d.add, mom_iq)
missing<-rbind(missing, d.add)
missing<-dplyr::rename(missing, miss=mom_iq)
df<-cbind(d, missing)
## Part B
d.miss<-df[!is.na(df$miss),]
mod<-stan_glm(miss~kid_score +mom_age+mom_hs, data=d.miss)
mod1<-stan_glm(mom_iq~kid_score +mom_age+mom_hs, data=df)
print(mod);print(mod1)
#stan_glm(formula = miss ~ kid_score + mom_age + mom_hs, data = d.miss)

#stan_glm
#family:  gaussian [identity]
#formula: miss ~ kid_score + mom_age + mom_hs
#------
  
#  Estimates:
#  Median MAD_SD
#(Intercept) 63.9    7.8  
#kid_score    0.2    0.0  
#mom_age      0.6    0.3  
#mom_hs       1.0    2.2  
#sigma       13.4    0.6  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 93.4    1.2  

#------
#  For info on the priors used see help('prior_summary.stanreg').stan_glm
#family:  gaussian [identity]
#formula: mom_iq ~ kid_score + mom_age + mom_hs
#------
  
#  Estimates:
#  Median MAD_SD
#(Intercept) 66.9    5.8  
#kid_score    0.3    0.0  
#mom_age      0.1    0.2  
#mom_hs       6.7    1.6  
#sigma       13.2    0.5  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 100.0    0.9 

#------
#  For info on the priors used see help('prior_summary.stanreg').
### They are consistent, however the mom_hs has a bigger effect in the dataframe with no missing data
## Part C
mod2<-stan_glm(kid_score~miss +mom_age+mom_hs, data=d.miss)
mod3<-stan_glm(kid_score~mom_iq +mom_age+mom_hs, data=df)
print(mod2);print(mod3)
#stan_glm(formula = kid_score ~ miss + mom_age + mom_hs, data = d.miss)

#Estimates:
#  Median MAD_SD
#(Intercept) 37.6   15.2  
#miss         0.4    0.1  
#mom_age      0.1    0.5  
#mom_hs       9.4    3.1  
#sigma       19.7    0.9  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 80.9    1.9  
#stan_glm(formula = kid_score ~ mom_iq + mom_age + mom_hs, data = df)

#Estimates:
#  Median MAD_SD
#(Intercept) 21.2    9.3  
#mom_iq       0.6    0.1  
#mom_age      0.2    0.3  
#mom_hs       5.7    2.2  
#sigma       18.1    0.6  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 86.8    1.2  
## They are slightly less consistent but similar to the above.
## Part D
d_pred<-dplyr::select(d.miss,-kid_score)
pred_1<- colMeans(posterior_linpred(mod2, newdata=d_pred))
impute<-function(a, a_impute){
  ifelse(is.na(a), a_impute, a)
}
kid_score_imp<-impute(df$kid_score, pred_1)
final.fit<-stan_glm(kid_score_imp~df$mom_iq+df$mom_age+df$mom_hs)
print(final.fit);print(mod3)
#stan_glm
#family:  gaussian [identity]
#formula: kid_score_imp ~ df$mom_iq + df$mom_age + df$mom_hs
#------
  
#  Estimates:
#  Median MAD_SD
#(Intercept) 21.0    9.0  
#df$mom_iq    0.6    0.1  
#df$mom_age   0.2    0.3  
#df$mom_hs    5.7    2.3  
#sigma       18.2    0.6  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 86.8    1.2  

#------
#  For info on the priors used see help('prior_summary.stanreg').stan_glm
#family:  gaussian [identity]
#formula: kid_score ~ mom_iq + mom_age + mom_hs
#------
  
#  Estimates:
#  Median MAD_SD
#(Intercept) 20.8    9.4  
#mom_iq       0.6    0.1  
#mom_age      0.2    0.3  
#mom_hs       5.6    2.2  
#sigma       18.2    0.6  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 86.9    1.2  

#------
#  For info on the priors used see help('prior_summary.stanreg').

