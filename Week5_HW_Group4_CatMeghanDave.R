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

# Set working directory and load data
setwd("~/Documents/git/statsclass/ARM_Data")
d<- read.dta("nes/nes5200_processed_voters_realideo.dta")

## Chapter 10 Problem 1
# clean the data and make new dataframe
df<-d%>%filter(year==1992)%>%dplyr::select(rep_presvote, income, educ1, white, female, ideo7, year,partyid7)
df$vote<-df$rep_presvote
df$income<-as.numeric(substr(df$income, 0, 1))
df$educ<-as.numeric(substr(df$educ1, 0, 1))
df$ideo<-as.numeric(substr(df$ideo7, 0, 1))
yes_ideo<-c(1:7)
df$ideo<-ifelse(df$ideo%in%yes_ideo, df$ideo, NA)
df$party<-as.numeric(substr(df$partyid7, 0, 1))
yes_party<-c(1:7)
df$party<-ifelse(df$party%in%yes_party, df$party, NA)

# Part A and B: making models and interpreting
fit_1<-glm(vote~female+white+educ+party+ideo, family=binomial(link="logit"), data=df)
display(fit_1)
#glm(formula = vote ~ female + white + educ + party + ideo, family = binomial(link = "logit"), 
#    data = df)
#coef.est coef.se
#(Intercept) -7.23     0.58  
#female       0.23     0.19  
#white        0.49     0.25  
#educ         0.15     0.11  
#party        0.95     0.06  
#ideo         0.51     0.07  
#---
#  n = 1207, k = 6
#residual deviance = 765.1, null deviance = 1638.3 (difference = 873.2)

## fit_1 does not include any interactions, which is probably not thorough enough -- party affiliation and 
# political ideology are closely related and may represent collinearity issues. We should input these
# predictors as an interaction. At first glance, gender does not have a big effect, nor does eduction. 

fit_2<-glm(vote~female+white+educ+party:ideo, family=binomial(link="logit"), data=df)
display(fit_2)
#glm(formula = vote ~ female + white + educ + party:ideo, family = binomial(link = "logit"), 
#    data = df)
#coef.est coef.se
#(Intercept) -4.45     0.41  
#female       0.19     0.19  
#white        0.57     0.24  
#educ         0.14     0.10  
#party:ideo   0.18     0.01  
#---
#  n = 1207, k = 5
#residual deviance = 806.8, null deviance = 1638.3 (difference = 831.5)

## fit_2 does include the interaction. Gender still does not have a big effect size, however, race now 
# has a larger effect and eduction did not really change. Gender and education have large standard errors.
#After race, the interaction of party affiliation and political ideology has the greatest effect size, 
#which seems accurate. If we apply the "divide by 4" rule, then the interaction term suggests that
#for each jump towards conservatism, given more republican ideologies, voters were 4.5% more likely to vote for Bush.
#Similarly, the coefficient for race tells us that whites are about 14% more likely to vote for bush than are other
#races. However, the error of this coefficient is very high, so we have less confidence in this estimate.

fit_3<-glm(vote~female+white+educ+party*ideo, family=binomial(link="logit"), data=df)
display(fit_3)
#glm(formula = vote ~ female + white + educ + party * ideo, family = binomial(link = "logit"), 
#    data = df)
#coef.est coef.se
#(Intercept) -8.26     1.02  
#female       0.23     0.19  
#white        0.51     0.25  
#educ         0.17     0.11  
#party        1.20     0.21  
#ideo         0.72     0.18  
#party:ideo  -0.05     0.04  
#---
#  n = 1207, k = 7
#residual deviance = 763.4, null deviance = 1638.3 (difference = 874.8)


## fit_3 suggests an interesting interaction effect. I believe this exemplifies the 
# colinearity issues and nonidentifiability issues. 

# Part C: choosing the right model
# fit_2 seems to be the best model
# gender is not a big predictor of vote, race is however. White people were ~14% more likely to vote 
# for Bush. Eduction was not a strong predictor. The interaction suggests more conservative republicans
# were 4.5% more likely to vote for Bush.






