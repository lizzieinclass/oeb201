## Started 12 September 2017 ##
## By Lizzie ##
## Updated 19 September 2017 ##
## Build data for OEB 201 class ##

## housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# libraries
library(ggplot2)
library(plyr)
library(dplyr)

setwd("~/Documents/git/teaching/oeb201_bayesstats/gitclass")

## Build the data
x <- c(1:21)
yrearly <- 23+0.5*x + rnorm(21, mean=1, sd=2)
yrlater <- 29+0.75*x + rnorm(21, mean=1, sd=2)


d <- data.frame(stage=c(1:21), temp=c(yrearly,yrlater), year=c(rep(1904, 21), rep(2017, 21)))

d[21, 1] <- "last"

write.csv(d, "testday/tour.csv", row.names=FALSE)

## Do the test
# (1) Read in the data, clean as needed
# (2) Plot temperatures by stage for each year
# (3) Calculate the mean and standard error of temperature for each year
# (4) Count the number of days above 30 in each year (in R)

## Some notes from students' tests
# Save your files as .R
# Use a good text editor (make sure it is has syntax highlighting)
# Always clean your data in a scripted language (required for term project)
# Avoid attaching and detaching data
# Avoid factors when possible
# Don't use T or F to replace TRUE or FALSE, note:
T <- c(1:4)
TRUE <- c(1:4)
# Comment your code
# See resources on my webpage


## Lizzie does the test

# (1) Read in the data, clean as needed
d <- read.csv("testday/tour.csv", header=TRUE)

# d[21, 1] <- 21 # alternative to below, but below is safer
d$stage[d$stage=="last"] <- 21
d$stage.corr <- as.numeric(d$stage)

# (2) Plot temperatures by stage for each year
ggplot(d, aes(stage.corr, temp, color=year))+
   geom_point()

# (3) Calculate the mean and standard error of temperature for each year 
meanse <-
      ddply(d, c("year"), summarise, # note that you can add more factors after year
      # also not this example uses only temp, but if you had more y variables you could vary them
      mean.temp = mean(temp), 
      sd = sd(temp),
      sem = sd(temp)/sqrt(length(temp)))

# (4) Count the number of days above 30 in each year (in R)
over30 <- subset(d, temp>30)
ddply(over30, c("year"), summarise, 
      yrz.over30 = length(temp))
