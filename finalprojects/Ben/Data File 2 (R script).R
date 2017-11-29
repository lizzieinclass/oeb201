#Setting working directory and reading in data file
getwd()
setwd("/Users/benjaminrice/Desktop/Rproject")
getwd()
site_data_all = read.csv("site_data_all.csv")
site_data_all

#Performing a dummy regression to make sure everything is gravy
site_model_test <- lm(site_data_all$s_prevalence ~ site_data_all$s_doc_walk_hours, 
                      data=site_data_all)
summary(site_model_test)

#Fitting a model to all sites data (R5 included)
site_model_all <- glm(site_data_all$s_prevalence ~ site_data_all$s_doc_walk_hours 
                   + as.factor(site_data_all$s_road_access)
                   + as.factor(site_data_all$s_coastal)
                   + site_data_all$s_distance_hospital
                   , data=site_data_all)
summary(site_model_all)

#Fitting a model, but now excluding R5
site_data_noR5 = read.csv("site_data_noR5.csv")
site_model_noR5 <- glm(site_data_noR5$s_prevalence ~ site_data_noR5$s_doc_walk_hours 
                      + as.factor(site_data_noR5$s_road_access)
                      + as.factor(site_data_noR5$s_coastal)
                      + site_data_noR5$s_distance_hospital
                      , data=site_data_noR5)

summary(site_model_noR5)

#For simulating data, first fitting a model of travel time to a doctor and distance to
#   a hospital only to obtain empirical estimates
lm.sims <- lm(formula = site_data_all$s_prevalence ~ site_data_all$s_doc_walk_hours 
           + site_data_all$s_distance_hospital)
summary(lm.sims)

#For simulating data, let us define a model y = a1 + B1*X1 + C1*X2 + sigma1
#First, we can explore the fit by inputting the empirical estimates into a simulation where
#   fake data is generated using the intercept, coefficients, and sigma

a <- 14
B <- -0.7
C <- 0.13
sigma <- 15
X1 <- runif(24, 0.1, 10)
X2 <- sample(1:57, 24)
n <- length(X)

y <- a + B*X1 + C*X2 + rnorm (n, mean = 0, sd = sigma)
lm.1 <- lm (formula = y ~ X1 + X2)
summary(lm.1)

#Need to repeat simulation process many times
#First, extracting the coefficient estimates
#Second, checking whether the simulation outputs fall within 95% CI of the inputted 
#    parameter value:
#Third, using a for loop to repeat 1000 times

n.fake <-1000
cover.95.B <- rep(NA, n.fake)
cover.95.C <- rep(NA, n.fake)
B.hat_holder <- rep(NA, n.fake)
C.hat_holder <- rep(NA, n.fake)
a.hat_holder <- rep(NA, n.fake)
sigma.hat_holder <- rep(NA, n.fake)

for (s in 1:n.fake){
  y <- a + B*X1 + C*X2 + rnorm (n, mean = 0, sd = sigma)
  lm.1 <- lm (formula = y ~ X1 + X2)
  B.hat <- coef(lm.1)[2]
  B.se <- coef(summary(lm.1))[2, 2]
  C.hat <- coef(lm.1)[3]
  C.se <- coef(summary(lm.1))[3, 2]
  a.hat <- coef(lm.1)[1]
  
  cover.95.B[s] <- abs(B-B.hat) < 2*B.se
  cover.95.C[s] <- abs(C-C.hat) < 2*C.se
  B.hat_holder[s] <- B.hat
  C.hat_holder[s] <- C.hat
  a.hat_holder <- a.hat
  sigma.hat_holder[s] <- summary(lm.1)$sigma
}

#Returning the proportion of repetitions in which the simulation returned parameter estimates
#   that covered the inputted parameter values and mean parameter estimates
cat (paste ("X1: 95% coverage: ", mean(cover.95.B), "\n"))
cat (paste ("X2: 95% coverage: ", mean(cover.95.C), "\n"))
mean(B.hat_holder)
mean(C.hat_holder)
mean(sigma.hat_holder)
mean(a.hat_holder)




#We can input values of B and C that are 2X greater and a value for sigma 2X smaller
#     We will leave the value of the intercept a unchanged
a1 <- 14
B1 <- -1.5
C1 <- 0.26
sigma1 <- 7.3
X1 <- runif(24, 0.1, 10)
X2 <- sample(1:57, 24)
n <- length(X1)

y <- a1 + B1*X1 + C1*X2 + rnorm (n, mean = 0, sd = sigma1)
lm.1 <- lm (formula = y ~ X1 + X2)
summary(lm.1)

#Same as above, we can repeat the simulation 1000 times

n.fake <-1000
cover.95.B1 <- rep(NA, n.fake)
cover.95.C1 <- rep(NA, n.fake)
B1.hat_holder <- rep(NA, n.fake)
C1.hat_holder <- rep(NA, n.fake)
a1.hat_holder <- rep(NA, n.fake)
sigma.hat_holder <- rep(NA, n.fake)

for (s in 1:n.fake){
  y <- a1 + B1*X1 + C1*X2 + rnorm (n, mean = 0, sd = sigma1)
  lm.1 <- lm (formula = y ~ X1 + X2)
  B1.hat <- coef(lm.1)[2]
  B1.se <- coef(summary(lm.1))[2, 2]
  C1.hat <- coef(lm.1)[3]
  C1.se <- coef(summary(lm.1))[3, 2]
  a1.hat <- coef(lm.1)[1]
  
  cover.95.B1[s] <- abs(B1-B1.hat) < 2*B1.se
  cover.95.C1[s] <- abs(C1-C1.hat) < 2*C1.se
  B1.hat_holder[s] <- B1.hat
  C1.hat_holder[s] <- C1.hat
  a1.hat_holder <- a1.hat
  sigma.hat_holder[s] <- summary(lm.1)$sigma
}

#Returning the proportion of repetitions in which the simulation returned parameter estimates
#   that covered the inputted parameter values and mean parameter estimates
cat (paste ("X1: 95% coverage: ", mean(cover.95.B1), "\n"))
cat (paste ("X2: 95% coverage: ", mean(cover.95.C1), "\n"))
mean(B1.hat_holder)
mean(C1.hat_holder)
mean(sigma.hat_holder)
mean(a1.hat_holder)

#Transitioning to household size...

#reading in data files
hh_size_data_all <- read.csv("hh_size_data_all.csv")
hh_size_data_noR5 <- read.csv("hh_size_data_noR5.csv")
hh_size_data_all
hh_size_data_noR5


hh_size_model_all <- lm (formula = hh_size_data_all$s_prevalence ~ hh_size_data_all$hh_size)
summary(hh_size_model_all)

hh_size_model_noR5 <- lm (formula = hh_size_data_noR5$s_prevalence ~ hh_size_data_noR5$hh_size)
summary(hh_size_model_noR5)

#Transitioning to age...

library("ggplot2")
age_data <- read.csv("age_data.csv")
head(age_data)

ggplot(age_data, aes(age, fill = status)) + geom_density(alpha = 0.2)

