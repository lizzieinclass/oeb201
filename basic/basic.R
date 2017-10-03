## Date ##
## Who worked on this ##

## File contents overview/aim etc. ##
## And away we go! ##

## Notes, todo items etc. ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory (example with multiple uses): 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/teaching/oeb201_bayesstats/gitclassroom") 
    } else if
   (length(grep("billybob", getwd()))>0) { setwd("~/GitHub") 
    } else if
   (length(grep("sally", getwd()))>0) {setwd("/Users/sally")
   } else 
   setwd("~/Documents/gitrocks")

# Load libraries
library(rstanarm)
library(shinystan)

# Get the data
d <- read.csv("basic/input/basic.csv", header=TRUE)
