# Script for final project, November 20 2017
# Inbar Maayan

rm(list = ls())

# Load libraries (they are called again where needed, so that it's clear which library was used for which code)
library(truncnorm)
library(lme4)
library(rstanarm)
library(shinystan)
library(dplyr)
library(ggplot2)
library(rvertnet)
library(maps)
library(mapdata)

####################
# Simulate fake data

set.seed(95)
I <- 100 # number of islands in simulated dataset
spp <- round(rtruncnorm(n = I, a = 0, b = 15, mean = 6, sd = 3)) # how many species are on each island. 
N <- sum(spp) #total number of species in the simulated dataset

hist(spp) #looks good 

ID <- seq(1:N) # naming each species, here by number (species 1 through species N) 

island <- as.numeric() # which island number (1-I) the species is on, for each species ID
for(i in 1:I){
  island = c(island, rep(i, spp[i]))
  }

sp_num <- as.numeric() #number of species that occur on the island that a given species is on 
  for(i in 1:I){
    sp_num = c(sp_num, rep(spp[i], spp[i]))
  }

###############
# Delete some species from the data. must re-run script from top each time you delete in order to make full dataframe to delete rows from
N #find out how many I have to begin with
fake_anolis <- data.frame(ID, island, sp_num)
delrows <- round(runif(200, 1, N))
obs_fake_anolis <- fake_anolis[-delrows,]
N <- length(obs_fake_anolis$ID) ###### a new N! 
island <- obs_fake_anolis$island ##### a new island!
sp_num <- obs_fake_anolis$sp_num ##### a new sp_num!
###############

# Hyperparameters for alpha (intercept by island)
mu_a <- 1.3
sigma_a <- 0.04

b <- -0.004 # the relationship between number of congeners and sexual dimorphism (SD)

sigma_y <- 0.14 # the error not explained by the predictors in the model

# simulate intercepts (int) for each island
int_island <- rep(0,I)
for(i in 1:I){
  int_island[i] <- rnorm(1, mu_a, sigma_a)
}

# Visualize
hist(int_island)
hist(rnorm(1000, 0, 0.15 ))

# My MODEL for making sexual dimorphism for each species, which is sd ~ a(island) + b*number of species on each island + error
sd <- rep(0, N)
for(n in 1:N){
  sd[n] <- rnorm (1, int_island[island[n]] + b*sp_num[n], sigma_y)
}

# Center SD data
sd_c <- scale(sd, center= TRUE, scale = FALSE)

# Visualize
plot(sd_c~sd)
hist(sd_c)

library(lme4)
fit <- lmer(sd_c ~ sp_num + (1|island))
fit

#A note on the rstanarm default prior: "The default priors used in the various rstanarm modeling functions are intended to be 
# weakly informative in that they provide moderate regularlization and help stabilize computation." 

# Model in rstanarm
library(rstanarm)
library(shinystan)
fit <- stan_lmer(sd_c ~ sp_num + (1|island))
summary(fit, digits = 3)

launch_shinystan(fit)

################
## The real data
setwd("C:/Users/Inbar/Desktop/HARVARD/G2/Fall2017/OEB201_Modeling/Project")
library(dplyr)
library(ggplot2)

# Some housekeeping
dat <- read.csv ("BaseData.csv", header = TRUE)
options(stringsAsFactors = FALSE)
summary(dat)
names(dat)
dat$Digitizer <- as.factor(dat$Digitizer)
dat$Sex <- as.factor(dat$Sex)
dat$Species <- as.factor(dat$Species)
dat$Island <- as.factor(dat$Island)
dat$Ecomorph <- as.factor(dat$Ecomorph)

dat <- tbl_df(dat)
dat <- rename(dat, CommSize = friends)

# I have A. wattsi from two islands, but one of them is the subspecies anolis wattsi pogus. I remove it for simplicity.
dat <- dat[- grep("wattsi pogus", dat$ID),]

# Too little data for Anolis equestris, Anolis alutaceus, Anolis brunneus.  Inelegantly remove species from data
dat2 <- tbl_df(filter(dat, !Species == "equestris"))
droplevels(dat2$Species)
dat2$Species <- factor(dat2$Species)
levels(dat2$Species)

dat3 <- tbl_df(filter(dat2, !Species == "alutaceus"))
droplevels(dat3$Species)
dat3$Species <- factor(dat3$Species)
levels(dat3$Species)

dat4 <- tbl_df(filter(dat3, !Species == "brunneus"))
droplevels(dat4$Species)
dat4$Species <- factor(dat4$Species)
levels(dat4$Species)

dat <- dat4

# Dataframe indicating which island each species comes from
d <- select(dat, Species, Island)
dd <- unique(d)
ddd <- arrange(dd, Species)
length(ddd$Species) #check that I have the correct number of species

# Calculate SD, number of congeners -- SD based on male & female mean values
females <- subset(dat, dat$Sex == "F")
fem <- aggregate(females$SVLruler, list(females$Species), mean, na.rm=TRUE)
fem <- rename(fem, species = Group.1, f_svl = x)
males <- subset(dat, dat$Sex == "M")
mal <- aggregate(males$SVLruler, list(males$Species), mean, na.rm=TRUE)
mal <- rename(mal, species = Group.1, m_svl = x)

spp_friends <- aggregate(dat$CommSize, list(dat$Species), mean) 
# friends are the total number of species in the community in question
# these numbers are estimated from the "Mapping distributions" part (see below) and from some prior knowledge of Anolis distributions

anolis <- data.frame(fem$species, fem$f_svl, mal$m_svl, spp_friends$x, ddd$Island)
anolis <- rename(anolis, species = fem.species, f_svl = fem.f_svl, m_svl = mal.m_svl, friends = spp_friends.x, island = ddd.Island)

# Computing standard deviation, the response variable. 
anolis$SD <- anolis$m_svl / anolis$f_svl

p <- ggplot() + geom_jitter(data = anolis, aes(x=friends, y = SD), size = 2)
p + theme_minimal()

library(lme4)
fit <- lmer(SD ~ friends + (1|island), data=anolis)  
fit

fit <- stan_lmer(SD ~ friends + (1|island), data=anolis)
summary(fit, digits=3)
launch_shinystan(fit)

# Calculate SD, number of congeners -- SD based on male & female max values
females <- subset(dat, dat$Sex == "F")
fem <- aggregate(females$SVLruler, list(females$Species), max, na.rm=TRUE)
fem <- rename(fem, species = Group.1, f_svl = x)
males <- subset(dat, dat$Sex == "M")
mal <- aggregate(males$SVLruler, list(males$Species), max, na.rm=TRUE)
mal <- rename(mal, species = Group.1, m_svl = x)

spp_friends <- aggregate(dat$CommSize, list(dat$Species), mean)

anolis <- data.frame(fem$species, fem$f_svl, mal$m_svl, spp_friends$x, ddd$Island)
anolis <- rename(anolis, species = fem.species, f_svl = fem.f_svl, m_svl = mal.m_svl, friends = spp_friends.x, island = ddd.Island)

anolis$SD <- anolis$m_svl / anolis$f_svl

p <- ggplot() + geom_jitter(data = anolis, aes(x=friends, y = SD), size = 2)
p + theme_minimal()


library(lme4)
fit <- lmer(SD ~ friends + (1|island), data=anolis)
fit

fit <- stan_lmer(SD ~ friends + (1|island), data=anolis)
summary(fit, digits=3)
launch_shinystan(fit)

###############################
## Mapping lizard distributions, in order to get an idea of how many species are in each community

# Scrape museum data from VertNet repository (http://www.vertnet.org)
install.packages("rvertnet")
library(rvertnet)

# vector of species names to search Vertnet
spp <- c(names(liz[,1:60]))
name <- spp[60] 

# Search vertnet
bigsearch(specificepithet = name, genus = "Anolis", mappable = TRUE, 
          rfile = "anoles", email = "your@email") # to use, input your email

# After combining and cleaning the VertNet files in Excel (apologies for this), mapping species distributions
library(maps)
library(mapdata)
library(dplyr)
library(ggplot2)


# Read in lizard locality data
dist <- read.csv("C:/Users/Inbar/Desktop/anoles/csv/AllPoints.csv")
names(dist)
dist <- tbl_df(dist)

# Map the Caribbean
carib <- map("worldHires", col="gray95", fill=TRUE, ylim=c(9.7,29), xlim=c(-86,-60))

# Subsets of the Caribbean, to look at species distributions
dist$country <- as.factor(dist$country)
dist$col <- as.character(dist$col)

levels(dist$country)

# Northern Lesser Antilles
lessAnt <- map("worldHires", col="gray95", fill=TRUE, ylim=c(16.5,18.5), xlim=c(-65.5,-60))
title("Northern Lesser Antilles")
Nlessers <- c("USVI", "Leeward")
LessAnt <- filter(dist, country %in% Nlessers)
points(LessAnt$decimallongitude, LessAnt$decimallatitude, pch=8, col=LessAnt$col, cex=1)
legnames <- unique(LessAnt$specificepithet)
legcol <- unique(LessAnt$col)
legend("bottomright", legend=legnames, col=legcol, pch = 8, bg = "gray95")

# Southern Lesser Antilles
lessAnt <- map("worldHires", col="gray95", fill=TRUE, ylim=c(11.4,16), xlim=c(-64,-58))
title("Southern Lesser Antilles")
Slessers <- c("Dominica", "Martinique", "St. Lucia", "Grenada", "Saint Vincent and the Grenadines")
LessAnt <- filter(dist, country %in% Slessers)
points(LessAnt$decimallongitude, LessAnt$decimallatitude, pch=8, col=LessAnt$col, cex=1)
legnames <- unique(LessAnt$specificepithet)
legcol <- unique(LessAnt$col)
legend("topright", legend=legnames, col=legcol, pch = 8, bg = "gray95")

# Navassa: can't draw Navassa because it's too small and uninhabited to be on the map, but it only has A. longiceps on it. 

# Cayman Islands
cayman <- map("worldHires", col="gray95", fill=TRUE, ylim=c(19.2,19.8), xlim=c(-81.5,-79.8))
title("Cayman Islands")
Caymans <- filter(dist, dist$country == "Cayman Islands")
points(Caymans$decimallongitude, Caymans$decimallatitude, pch=8, col=Caymans$col, cex=1)
legnames <- unique(Caymans$specificepithet)
legcol <- unique(Caymans$col)
legend("bottomright", legend=legnames, col=legcol, pch = 8, bg = "gray95")

# The Bahamas
bahamas <- map("worldHires", col="gray95", fill=TRUE, ylim=c(20.95,27.5), xlim=c(-78.9,-71.5))
title("The Bahamas")
Bahamas <- filter(dist, dist$country == "Bahamas")
points(Bahamas$decimallongitude, Bahamas$decimallatitude, pch=8, col=Bahamas$col, cex=1)
legnames <- unique(Bahamas$specificepithet)
legcol <- unique(Bahamas$col)
legend("topright", legend=legnames, col=legcol, pch = 8, bg = "gray95")

# Cuba
cuba <- map("worldHires", col="gray95", fill=TRUE, ylim=c(19,23.3), xlim=c(-85,-74.2))
title("Cuba")
Cuba <- filter(dist, dist$country == "Cuba")
points(Cuba$decimallongitude, Cuba$decimallatitude, pch=8, col=Cuba$col, cex=1)
legnames <- unique(Cuba$specificepithet)
legcol <- unique(Cuba$col)
legend("bottomleft", legend=legnames, col=legcol, pch = 8, bg = "gray95")

# Hispaniola
hispanola <- map("worldHires", col="gray95", fill=TRUE, ylim=c(17.1,20.3), xlim=c(-74.5,-67.25))
title("Hispaniola \n (Haiti & The Dominican Republic)")
Hispanola <- filter(dist, dist$country == "Hispanola")
points(Hispanola$decimallongitude, Hispanola$decimallatitude, pch=8, col=Hispanola$col, cex=1)
legnames <- unique(Hispanola$specificepithet)
legcol <- unique(Hispanola$col)
legend("topright", legend=legnames, col=legcol, pch = 8, bg = "gray95")

# Puerto Rico
puerto <- map("worldHires", col="gray95", fill=TRUE, ylim=c(17.9,18.7), xlim=c(-68,-65.2))
title("Puerto Rico")
Puerto <- filter(dist, dist$country == "Puerto Rico")
points(Puerto$decimallongitude, Puerto$decimallatitude, pch=8, col=Puerto$col, cex=1)
legnames <- unique(Puerto$specificepithet)
legcol <- unique(Puerto$col)
legend("topleft", legend=legnames, col=legcol, pch = 8, bg = "gray95")

# Jamaica (probably has too sparse sampling in my records to say anything about distributions)
jamaica <- map("worldHires", col="gray95", fill=TRUE, ylim=c(17.5,18.6), xlim=c(-79,-76))
title("Jamaica")
Jamaica <- filter(dist, dist$country == "Jamaica")
points(Jamaica$decimallongitude, Jamaica$decimallatitude, pch=8, col=Jamaica$col, cex=1)
legnames <- unique(Jamaica$specificepithet)
legcol <- unique(Jamaica$col)
legend("topleft", legend=legnames, col=legcol, pch = 8, bg = "gray95")

