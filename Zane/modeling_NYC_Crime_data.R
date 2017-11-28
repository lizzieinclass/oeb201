########
# Author: Zane Wolf
# Date Created: 10/20/2017
# Purpose: To model NYC Crime Data


# Date Updated: 11/28/2017

########

####################################################################################################################################################

#                                                                 HOUSEKEEPING

####################################################################################################################################################
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("C:/Users/zane/Documents/School/Classes/Harvard/OEB 201 Experimental Design and Stats/NYC_Crime")

# libraries
library(tidyverse)
  #includes ggplot2, tibble, tidyr, readr, purrr, dplyr
library(plyr)
library(rstan)
library(rstanarm)
library(rstantools)
library(arm)
library(shinystan)
####################################################################################################################################################

#                                                                   IMPORT

####################################################################################################################################################
nyc <- read_csv("NYPD_Crime_Data_CLEAN_2010.csv")

View(nyc)

#trying to figure out which predictors are most likely important, looking at the ratios between things

as.data.frame(table(nyc$OFNS_DESC, nyc$Year))
as.data.frame(table(nyc$OFNS_DESC, nyc$BORO_NM))
as.data.frame(table(nyc$OFNS_DESC, nyc$Hour, nyc_M_small$PARKS_NM))
as.data.frame(table(nyc$OFNS_DESC ,nyc$LOC_OF_OCCUR_DESC))
as.data.frame(table(nyc$OFNS_DESC ,nyc$Holiday))
as.data.frame(table(nyc$OFNS_DESC ,nyc$Weekend))
as.data.frame(table(nyc$OFNS_DESC ,nyc$HADEVELOPT))
as.data.frame(table(nyc$OFNS_DESC ,nyc$JURIS_DESC))
as.data.frame(table(nyc$OFNS_DESC ,nyc$Hour))

#make some plots to visualize these relationships and stuff

ggplot(data=nyc, mapping=aes(x=BORO_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=Holiday, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=PARKS_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=Weekend, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=HADEVELOPT, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
ggplot(data=nyc, mapping=aes(x=BORO_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")+facet_wrap(~LOC_OF_OCCUR_DESC)
ggplot(data=nyc, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")+facet_grid(BORO_NM~LOC_OF_OCCUR_DESC)

#need to run a quick model
#take an 1/8 of the dataset at random and use in models
nyc_2010_sample=nyc[sample(nrow(nyc),10000),]

#compare a bunch of different model
# m2_no_int <- stan_glmer(as.factor(OFNS_DESC) ~ -1 +Hour+LOC_OF_OCCUR_DESC+(1|BORO_NM), data=nyc_2010_sample, family=binomial(link="logit"))
# 
# m4_comp_pool <- stan_glm(as.factor(OFNS_DESC) ~ Hour+LOC_OF_OCCUR_DESC+Hour:LOC_OF_OCCUR_DESC+BORO_NM, data=nyc_2010_sample, family=binomial(link="logit"))


m5_part_pool <- stan_glmer(as.factor(OFNS_DESC) ~  Hour+LOC_OF_OCCUR_DESC+Hour:LOC_OF_OCCUR_DESC+(1|BORO_NM), data=nyc_2010_sample, family=binomial(link="logit"))


#loo analysis shows that m4 is like +2 better than m2, and that m5 is 0.2 better than m4

####################################################################################################################################################
#                                                               FAKE DATA

####################################################################################################################################################


#############################################

#              Attempt #3

#############################################
#Previous attempts included at the end of the script 


# going to try to make a completely numerical model by giving each variable value it's own binary column
#so many dummy variables. sooo many. 

#make an empty matrix
colnum <- 32
nsims <- 5000

fake_data3 <- matrix(0, nsims, colnum)

#change the column names to be more helpful
my_names <- c('Crimes', 'Location', 'Queens', 'Manhattan', 'StatenIsland', 'Bronx', 'Brooklyn', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'H7', 'H8', 'H9', 'H10', 'H11', 'H12', 'H13', 'H14', 'H15', 'H16', 'H17', 'H18', 'H19', 'H20', 'H21', 'H22', 'H23','BoroName', 'Hour')

colnames(fake_data3) <- my_names

#create lists of these column names, separated by type (location, neighborhood, hour)
loc_values <- c(0, 1)
boro_names <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "StatenIsland")
hour_names <- c('H0','H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'H7', 'H8', 'H9', 'H10', 'H11', 'H12', 'H13', 'H14', 'H15', 'H16', 'H17', 'H18', 'H19', 'H20', 'H21', 'H22', 'H23')

#for each row in the matrix, choose one column from each list and set the value in that row:column to 1. This ensures that only column of each type will have a value. 
for (i in 1:nsims){
  random_boro <- sample(x=boro_names, size=1, replace=TRUE, prob=hood_prob)
  random_loc <- sample(x=loc_values, size=1, replace=TRUE, prob=rep(1/2,2))
  random_hour <- sample(x=hour_names, size=1, replace=TRUE, prob=hour_prob)
  
  fake_data3[i, random_boro] <- 1
  fake_data3[i, "Location"] <- random_loc
  if (random_hour!="H0"){
    fake_data3[i, random_hour] <- 1
  }
}

#give BoroName some values so I have a grouping variable later
#probs could've done apply again, but whoops; when I'm in a hurry, I try to brute force things, rather than use finesse. 
for (i in 1:nsims){
  if (fake_data3[[i,"Queens"]]==1){
    fake_data3[[i,"BoroName"]] <- "Queens"
  }
  else if (fake_data3[[i,"Bronx"]]==1){
    fake_data3[[i,"BoroName"]] <- "Bronx"
  }
  else if (fake_data3[[i,"Brooklyn"]]==1){
    fake_data3[[i,"BoroName"]] <- "Brooklyn"
  }
  else if (fake_data3[[i,"Manhattan"]]==1){
    fake_data3[[i,"BoroName"]] <- "Manhattan"
  }
  else if (fake_data3[[i,"StatenIsland"]]==1){
    fake_data3[[i,"BoroName"]] <- "StatenIsland"
  }
  
  if(fake_data3[[i,"H1"]]==1){
    fake_data3[[i, "Hour"]] <- "01"
  }
  else if(fake_data3[[i,"H2"]]==1){
    fake_data3[[i, "Hour"]] <- "02"
  }
  else if(fake_data3[[i,"H3"]]==1){
    fake_data3[[i, "Hour"]] <- "03"
  }
  else if(fake_data3[[i,"H4"]]==1){
    fake_data3[[i, "Hour"]] <- "04"
  }
  else if(fake_data3[[i,"H5"]]==1){
    fake_data3[[i, "Hour"]] <- "05"
  }
  else if(fake_data3[[i,"H6"]]==1){
    fake_data3[[i, "Hour"]] <- "06"
  }
  else if(fake_data3[[i,"H7"]]==1){
    fake_data3[[i, "Hour"]] <- "07"
  }
  else if(fake_data3[[i,"H8"]]==1){
    fake_data3[[i, "Hour"]] <- "08"
  }
  else if(fake_data3[[i,"H9"]]==1){
    fake_data3[[i, "Hour"]] <- "09"
  }
  else if(fake_data3[[i,"H10"]]==1){
    fake_data3[[i, "Hour"]] <- "10"
  }
  else if(fake_data3[[i,"H11"]]==1){
    fake_data3[[i, "Hour"]] <- "11"
  }
  else if(fake_data3[[i,"H12"]]==1){
    fake_data3[[i, "Hour"]] <- "12"
  }
  else if(fake_data3[[i,"H13"]]==1){
    fake_data3[[i, "Hour"]] <- "13"
  }
  else if(fake_data3[[i,"H14"]]==1){
    fake_data3[[i, "Hour"]] <- "14"
  }
  else if(fake_data3[[i,"H15"]]==1){
    fake_data3[[i, "Hour"]] <- "15"
  }
  else if(fake_data3[[i,"H16"]]==1){
    fake_data3[[i, "Hour"]] <- "16"
  }
  else if(fake_data3[[i,"H17"]]==1){
    fake_data3[[i, "Hour"]] <- "17"
  }
  else if(fake_data3[[i,"H18"]]==1){
    fake_data3[[i, "Hour"]] <- "18"
  }
  else if(fake_data3[[i,"H19"]]==1){
    fake_data3[[i, "Hour"]] <- "19"
  }
  else if(fake_data3[[i,"H20"]]==1){
    fake_data3[[i, "Hour"]] <- "20"
  }
  else if(fake_data3[[i,"H21"]]==1){
    fake_data3[[i, "Hour"]] <- "21"
  }
 else if(fake_data3[[i,"H22"]]==1){
    fake_data3[[i, "Hour"]] <- "22"
  }
  else if(fake_data3[[i,"H23"]]==1){
    fake_data3[[i, "Hour"]] <- "23"
  }
  else if(any(as.numeric(fake_data3[i,8:30])==1)){
  fake_data3[[i, "Hour"]] <- "00"
  }
}

#looking good
head(fake_data3)

#coefficients for predictors, taken from the coefficients of m5_part_pool
mu_in <- 1.2
mu_1 <- 0.2
mu_2 <- 0.1
mu_3 <- 0.1
mu_4 <- 0.1
mu_5  <- 0.0
mu_6  <- -0.4
mu_7 <- -0.1
mu_8 <- -0.2
mu_9 <- 0.7
mu_10 <- 0.7
mu_11 <- 0.9
mu_12 <- 0.8
mu_13 <- 0.8
mu_14  <- 0.9
mu_15 <- 0.9
mu_16 <- 0.1
mu_17 <- 0.0
mu_18 <- 0.0
mu_19 <- 0.1
mu_20 <- 0.0
mu_21 <- 0.1
mu_22  <- -0.2
mu_23 <- -0.1
#coefficients for interaction terms
mu_loc_1 <- -0.4
mu_loc_2 <- 0.0
mu_loc_3 <- -0.5
mu_loc_4 <- -0.4
mu_loc_5 <- -0.1
mu_loc_6 <- 0.7
mu_loc_7 <- 0.3
mu_loc_8 <- 0.7
mu_loc_9 <- 0.4
mu_loc_10 <- -0.1
mu_loc_11 <- -0.2
mu_loc_12 <- 0.4
mu_loc_13 <- 0.2
mu_loc_14 <- -0.1
mu_loc_15 <- 0.0
mu_loc_16 <- 0.3
mu_loc_17 <- 0.4
mu_loc_18 <- 0.8
mu_loc_19 <- 0.1
mu_loc_20 <- 0.4
mu_loc_21 <- 0.4
mu_loc_22 <- 0.3
mu_loc_23 <- 0.4


#store the data in a dummy matrix so that the original is in good shape if I need it
fake_data3a <- fake_data3

#time to fake some crime

for (i in 1:nsims){
  y <- mu_in*as.numeric(fake_data3a[[i, 'Location']])+mu_1*as.numeric(fake_data3a[[i, 'H1']])+mu_2*as.numeric(fake_data3a[[i, 'H2']])+mu_3*as.numeric(fake_data3a[[i, 'H3']])+mu_4*as.numeric(fake_data3a[[i, 'H4']])+mu_5*as.numeric(fake_data3a[[i, 'H5']])+mu_6*as.numeric(fake_data3a[[i, 'H6']])+mu_7*as.numeric(fake_data3a[[i, 'H7']])+mu_8*as.numeric(fake_data3a[[i, 'H8']])+mu_9*as.numeric(fake_data3a[[i, 'H9']])+mu_10*as.numeric(fake_data3a[[i, 'H10']])+mu_11*as.numeric(fake_data3a[[i, 'H11']])+mu_12*as.numeric(fake_data3a[[i, 'H12']])+mu_13*as.numeric(fake_data3a[[i, 'H13']])+mu_14*as.numeric(fake_data3a[[i, 'H14']])+mu_15*as.numeric(fake_data3a[[i, 'H15']])+mu_16*as.numeric(fake_data3a[[i, 'H16']])+mu_17*as.numeric(fake_data3a[[i, 'H17']])+mu_18*as.numeric(fake_data3a[[i, 'H18']])+mu_19*as.numeric(fake_data3a[[i, 'H19']])+mu_20*as.numeric(fake_data3a[[i, 'H20']])+mu_21*as.numeric(fake_data3a[[i, 'H21']])+mu_22*as.numeric(fake_data3a[[i, 'H22']])+mu_23*as.numeric(fake_data3a[[i, 'H23']]) +mu_loc_1*as.numeric(fake_data3a[[i, 'H1']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_2*as.numeric(fake_data3a[[i, 'H2']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_3*as.numeric(fake_data3a[[i, 'H3']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_4*as.numeric(fake_data3a[[i, 'H4']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_5*as.numeric(fake_data3a[[i, 'H5']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_6*as.numeric(fake_data3a[[i, 'H6']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_7*as.numeric(fake_data3a[[i, 'H7']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_8*as.numeric(fake_data3a[[i, 'H8']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_9*as.numeric(fake_data3a[[i, 'H9']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_10*as.numeric(fake_data3a[[i, 'H10']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_11*as.numeric(fake_data3a[[i, 'H11']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_12*as.numeric(fake_data3a[[i, 'H12']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_13*as.numeric(fake_data3a[[i, 'H13']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_14*as.numeric(fake_data3a[[i, 'H14']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_15*as.numeric(fake_data3a[[i, 'H15']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_16*as.numeric(fake_data3a[[i, 'H16']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_17*as.numeric(fake_data3a[[i, 'H17']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_18*as.numeric(fake_data3a[[i, 'H18']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_19*as.numeric(fake_data3a[[i, 'H19']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_20*as.numeric(fake_data3a[[i, 'H20']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_21*as.numeric(fake_data3a[[i, 'H21']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_22*as.numeric(fake_data3a[[i, 'H22']])*as.numeric(fake_data3a[[i, 'Location']])+mu_loc_23*as.numeric(fake_data3a[[i, 'H23']])*as.numeric(fake_data3a[[i, 'Location']]) +mu_Queen*as.numeric(fake_data3a[[i, 'Queens']])+mu_Man*as.numeric(fake_data3a[[i, 'Manhattan']])+mu_Bronx*as.numeric(fake_data3a[[i, 'Bronx']])+mu_Brook*as.numeric(fake_data3a[[i, 'Brooklyn']])+mu_SI*as.numeric(fake_data3a[[i, 'StatenIsland']])
  
  if (y<1.0){
    my_crime='Criminal Mischief'
  }
  else if (y>=1.0){
    my_crime='Misc. Offenses'
  }
  fake_data1a[[i, 'Crimes']] <- my_crime
}

#set as a data frame for use in ggplot
fake_data_df_2 <- as.data.frame(fake_data1a)

#make some graphs to see how it compares to the real data graphs above
ggplot(data=fake_data_df_2, mapping=aes(x=BoroName, group=Crimes, fill=Crimes))+geom_bar(position="dodge")
ggplot(data=fake_data_df_2, mapping=aes(x=Hour, group=Crimes, fill=Crimes))+geom_bar(position="dodge")
ggplot(data=fake_data_df_2, mapping=aes(x=Hour, group=Crimes, fill=Crimes))+geom_bar(position="dodge")+facet_wrap(~Location)

#make the model
#literally the same equation as m5 tho
m_fake_loc_3 <- stan_glmer(as.factor(Crimes) ~ Location+Hour+Hour:Location+(1|BoroName), data=fake_data_df_2, family=binomial(link="logit"))

#shiny stan time
launch_shinystan(m5_part_pool)
launch_shinystan(m_fake_loc_3)


####################################################################################################################################################

#                                                               EXTRA CODE

####################################################################################################################################################
# #create predictor vectors
# 
# #I tried three different ways to fake data
# 
# #create the vectors of values and store the real probability ratios associated with each value
# location=c("Inside", "Outside")
# prop.table(table(nyc$LOC_OF_OCCUR_DESC))
# #these values were 0.495 and 0.505...decided it would be okay to stick to half and half and not worry about a 'real prob' list
# 
# hours=c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23') #i realize now I probably could have just done c(unique(nyc$Hour))...whoops. 
# hour_prob <- prop.table(table(nyc$Hour))
# police=c(1,0)
# popo_prob <- prop.table(table(nyc$JURIS_DESC))
# hood=c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
# hood_prob <- prop.table(table(nyc$BORO_NM))
# crimes=c("Mischief", "Miscellaneous")
# crime_prob <- prop.table(table(nyc$OFNS_DESC))
# 
# #############################################
# 
# #              Attempt #1
# 
# #############################################
# 
# #Make the first set of fake data where the values are sampled at their real ratios
# 
# LOC_OF_OCCUR_DESC <- sample(x = location, size=5000, replace=TRUE, prob=rep(1/2,2))
# Hour <- sample(x = hours, size=5000, replace=TRUE, prob=hour_prob)
# JURIS_DESC <- sample(x=police, size=5000, replace=TRUE, prob=c(0.124, 0.876))
# BORO_NM <- sample(x=hood, size=5000, replace=TRUE, prob=c(0.208, 0.065, 0.198, 0.233, 0.296))
# OFNS_DESC <- sample(x=crimes, size=5000, replace=TRUE, prob=rep(1/2,2))
# 
# test_df1 <- cbind.data.frame(OFNS_DESC, JURIS_DESC, BORO_NM, Hour, LOC_OF_OCCUR_DESC, deparse.level=1, stringsAsFactors=TRUE)
# #############################################
# 
# #              Attempt #2
# 
# #############################################
# 
# #make the second set of fake data where the values are sampled at unbiased probabilities
# LOC_OF_OCCUR_DESC <- sample(x = location, size=5000, replace=TRUE, prob=rep(1/2,2))
# Hour <- sample(x = hours, size=5000, replace=TRUE, prob=rep(1/24,24))
# JURIS_DESC <- sample(x=police, size=5000, replace=TRUE, prob=rep(1/2, 2))
# BORO_NM <- sample(x=hood, size=5000, replace=TRUE, prob=rep(1/5,5))
# OFNS_DESC <- sample(x=crimes, size=5000, replace=TRUE, prob=rep(1/2,2))
# 
# test_df2 <- cbind.data.frame(OFNS_DESC, JURIS_DESC, BORO_NM, Hour, LOC_OF_OCCUR_DESC, deparse.level=1, stringsAsFactors=TRUE)
# 
# #visualize these fake data
# ggplot(data=test_df1, mapping=aes(x=BORO_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
# ggplot(data=test_df1, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
# ggplot(data=test_df1, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")+facet_grid(BORO_NM~LOC_OF_OCCUR_DESC)
# 
# 
# ggplot(data=test_df2, mapping=aes(x=BORO_NM, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
# ggplot(data=test_df2, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")
# ggplot(data=test_df2, mapping=aes(x=Hour, group=OFNS_DESC, fill=OFNS_DESC))+geom_bar(position="dodge")+facet_grid(BORO_NM~LOC_OF_OCCUR_DESC)
