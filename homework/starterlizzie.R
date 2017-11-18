## housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
 
# libraries
library(ggplot2)
library(plyr)
library(dplyr)
library("ggplot2")
library("bayesplot")
library("arm")
library("rstanarm")
library("foreign")

# and away we go!
setwd("~/Documents/git/teaching/oeb201_bayesstats/gitclass")
kidiq <- read.dta(file="ROS/KidIQ/kidiq.dta")
kidiq <- read.dta(file="homework/input/child.iq.dta")
