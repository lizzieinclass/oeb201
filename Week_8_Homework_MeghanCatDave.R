### Week 8 - 25 October 2017
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
library(haven)

# Set working directory and load data
setwd("~/Documents/git/statsclass/ARM_Data")
kid_score<-read.dta("child.iq/kidiq.dta")

## Color Pallette
alpha <- 150
castle <- c(rgb(113, 42, 59, max = 255, alpha = alpha), rgb(229, 118, 97, max = 255, alpha = alpha), 
            rgb(248, 197, 140, max = 255, alpha = alpha), rgb(248, 231, 162, max = 255, alpha = alpha), 
            rgb(134, 221, 178, max = 255, alpha = alpha))

## Set up Models - Regressing if a child's iq is related to his mother's iq
m1 <- stan_glm(kid_score ~ mom_iq, data = kidiq, family = gaussian(link = "identity"))


## Plot Data with all simulated lines from model
draws <- as.data.frame(m1)
plot(kid_score ~ mom_iq, data = kidiq, pch = 16, xlab = "Mom IQ", ylab = "Kid Score")
for(i in 1:nrow(draws)){abline(draws$`(Intercept)`[i], draws$mom_iq[i], lwd = 1, col = castle[3])}

## Check data parameters predictions versus our own data - we can see the paramters predict a distribution that is centered
## around the mean, while our data has a longer tail off to the left. Therefore, we will compare minimum observed values
## between the actual data and the simulated data as a way to numerically compare them.
par(mfrow = c(3,2), mar = c(0, 0, 0 ,0), oma = c(5, 5, 0, 0))
hist(kidiq$kid_score, col = "darkblue", border = "white", breaks = 30, xlim = c(20, 140),ylim = c(0,80), axes = F, main = "")
axis(2, at = seq(0, 80, by = 20))
for(i in 1:5){
  b <- hist(rnorm(434, mean = mean(m1$y), sd = sd(m1$residuals)), col = "lightblue", border = "white", 
                   breaks = 30, xlim = c(20, 140), ylim = c(0,80), axes = F, main = "")
  if(i == 4 | i == 5){axis(1, at = seq(20, 140, by = 20))}
  if(i == 2 | i == 4){axis(2, at = seq(0, 80, by = 20))}
}
legend("topright", col = c("darkblue", "lightblue"), legend = c("y", "y{rep}"), lwd = 3)

## Check the estimated mins versus the actual
pp_check(m1, plotfun = "stat", stat = "min")

#Although the actual min value is below a majority of mins in the simulations, it is near the average min value.
#There is nothing in this plot to suggest that the model is failing to capture the variability in the data.