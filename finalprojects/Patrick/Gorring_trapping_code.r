# Patrick Gorring Beetle trapping fake data simulation, model runs and posterior predictive checks

library("rstan")
library("rstanarm")
## build a custom neg binomial stan model
  model <- "
// negative binomial parameterized as eta (log(mu)) and dispersion (phi)
// see p286 in stan-reference-2.4.0.pdf
// a basic GLM example
data {
  int<lower=1> N;    // rows of data
  vector[N] x1;       // predictor
  vector[N] x2;       // predictor
  int<lower=0> y[N]; // response
}
parameters {
  real<lower=0> phi; // neg. binomial dispersion parameter
  real b0;  // intercept
  real b1;  // slope
  real b2;  // slope
}
model {
  // priors:
  phi ~ cauchy(0, 3);
  b0 ~ normal(0, 5);
  b1 ~ normal(0, 5);
  b2 ~ normal(0, 5);
  // data model:
  y ~ neg_binomial_2_log(b0 + b1 * x1 + b2 * x2, phi);
}
"
write(model, file = "negbin-glm.stan")

sm <- stan_model("negbin-glm.stan")

## simulate fake data using the negative binomial, phi=theta
set.seed(123)
N <- 1000
phi <- 1.2
b0 <- 1
b1 <- 3
b2 <-1.5
b3 <- 1
#x1 <- rep(0:1, length.out = N)
#x2 <- rep(0:1, length.out = N)
x1 <- rbinom(N, 1, 0.5)
x2 <- rbinom(N, 1, 0.5)
#y <- rnbinom(N, size = phi, mu = exp(b0 + (x1 - mean(x1)) * b1 + (x2 - mean(x2)) * b2 + b3*(x1 - mean(x1))*(x2 - mean(x2)))
y_2var <- rnbinom(N, size = phi, mu = exp(b0 + (x1 - mean(x1)) * b1 + (x2 - mean(x2)) * b2))
plot(x1, y_2var)
plot(x2, y_2var)

m <- sampling(sm, data = list(N = N, y = y_2var, x1 = x1 - mean(x1), x2 = x2 - mean(x2)),
  pars = c("b0", "b1", "b2", "phi"),
  iter = 500, chains = 4)

##model output
m

## comparing modeled to real (simulated) data values
e <- extract(m, pars = c("b0", "b1", "b2", "phi"))
true_pars <- c(b0 = b0, b1 = b1, b2 = b2, phi = phi)
x_cent1 <- x1 - mean(x1)
x_cent2 <- x2 - mean(x2)
m_mass <- MASS::glm.nb(y_2var ~ x_cent1 + x_cent2)
coefs_mass <- c(coef(m_mass), summary(m_mass)$theta)
par(mfrow = c(1, 4))
for(i in 1:4) {
  if(i %in% 1:4) {
    plot(density(e[[i]]), main = names(true_pars)[i])
  } else {
    plot(density(e[[i]]), main = names(true_pars)[i], log = "x1")
  }
  abline(v = true_pars[i], lwd = 3, col = "grey", lty = 2)
  abline(v = coefs_mass[i], lwd = 4, col = "red")
}

##modeling for actual beetle trapping data
dat <- read.table("trap_data_table_R.txt", header = TRUE, sep = "")
stan_model1_nb <- stan_glm.nb(Monochamus_tot ~ lures + height, link = "log", data = dat)
stan_model1b_nb <- stan_glm.nb(Monochamus_tot ~ lures + height + date, link = "log", data = dat)
stan_model1c_nb <- stan_glm.nb(Monochamus_tot ~ lures + height + date + block, link = "log", data = dat)
stan_model2_nb <- stan_glm.nb(Monochamus_tot ~ lures + height + lures:height, link = "log", data = dat)
stan_model1_poiss <- stan_glm(Monochamus_tot ~ lures + height, family=poisson, data = dat)
stan_model1c_poiss <- stan_glm(Monochamus_tot ~ lures + height + date + block, family=poisson, data = dat)
stan_model2_poiss <- stan_glm(Monochamus_tot ~ lures + height + lures:height, family=poisson, data = dat)
##hierarchical random effects model add block as (1|dat$block) or date (1|dat$date), probably want to keep date fixed but this allows me to make the random effects more 'invisible' for output
#maybe try to use species as random effects? how to do this since you cannot assign a sp to a count while using total count, model each species

##compare models, using leave one out comparison
library("loo")
stan_poiss1_loo <- loo(stan_model1_poiss)
stan_poiss1c_loo <- loo(stan_model1c_poiss)
stan_nb1_loo <- loo(stan_model1_nb)
stan_nb1b_loo <- loo(stan_model1b_nb)
stan_nb1c_loo <- loo(stan_model1c_nb)
stan_nb2_loo <- loo(stan_model2_nb)
compare_models(stan_poiss1_loo, stan_nb1_loo)
#elpd_diff 106.4  se 30.8, favors NB
compare_models(stan_nb1_loo, stan_nb2_loo)
#elpd_diff -.4  se 1.0, no model preference and diff is within se
compare_models(stan_nb1_loo, stan_nb1b_loo)
#elpd_diff 24.6  se 6.0, in favor of adding date
compare_models(stan_nb1b_loo, stan_nb1c_loo)
#elpd_diff 12.4  se 4.9, in favor of adding block
compare_models(stan_poiss1c_loo, stan_nb1c_loo)
#elpd_diff .2  se 1.5, no difference between NB/poiss when fully parameterized

##do posterior predicts with fully parameterized model next


##posterior predictive checks, try with 4 variable models
#graph proportion of zeros comparing data (y) to 4000 simulated datasets-looks bad, try negbinom!
yrep_2 <- posterior_predict(stan_model1_poiss) 
prop_zero_poiss <- function(y) mean(y == 0)
 (prop_zero_test_poiss <- pp_check(stan_model1_poiss, plotfun = "stat", stat = "prop_zero_poiss"))

#graph proportion of zeros comparing data (y) to 4000 simulated datasets-looks good for negbin
prop_zero <- function(y) mean(y == 0)
(prop_zero_test1 <- pp_check(stan_model1_nb, plotfun = "stat", stat = "prop_zero"))
if (require(gridExtra)) {
grid.arrange(prop_zero_test_poiss + ggtitle("Poisson"), 
               prop_zero_test1 + ggtitle("Negative Binomial"), 
               ncol = 2)
}

#develop for max value prediction
yrep_3 <- posterior_predict(stan_model1_poiss) 
prop_max_poiss <- function(y) mean(y >= 20)
 (prop_max_test_poiss <- pp_check(stan_model1_poiss, plotfun = "stat", stat = "prop_max_poiss"))
prop_max <- function(y) mean(y >= 20)
(prop_max_test1 <- pp_check(stan_model1_nb, plotfun = "stat", stat = "prop_max"))
if (require(gridExtra)) {
grid.arrange(prop_max_test_poiss + ggtitle("Poisson"), 
               prop_max_test1 + ggtitle("Negative Binomial"), 
               ncol = 2)
}

#develop for max value prediction, four variable model
yrep_6 <- posterior_predict(stan_model1c_poiss) 
prop_max_poiss_4var <- function(y) mean(y >= 20)
 (prop_max_4var_poiss <- pp_check(stan_model1c_poiss, plotfun = "stat", stat = "prop_max_poiss_4var"))
prop_max_4var <- function(y) mean(y >= 20)
(prop_4var_max <- pp_check(stan_model1c_nb, plotfun = "stat", stat = "prop_max_4var"))
if (require(gridExtra)) {
grid.arrange(prop_max_4var_poiss + ggtitle("Poisson"), 
               prop_4var_max + ggtitle("Negative Binomial"), 
               ncol = 2)
}
#do for number of max values instead of mean
#yrep_4 <- posterior_predict(stan_model1_poiss) 
#prop_max_num_poiss <- function(y) length(y >= 20)
# (prop_max__numtest_poiss <- pp_check(stan_model1_poiss, plotfun = "stat", stat = "prop_max_num_poiss"))

#four predictor PP
yrep_5 <- posterior_predict(stan_model1c_poiss) 
prop_zero_poiss_4var <- function(y) mean(y == 0)
 (prop_zero_4var_poiss <- pp_check(stan_model1c_poiss, plotfun = "stat", stat = "prop_zero_poiss_4var"))
prop_zero_4var <- function(y) mean(y == 0)
(prop_zero_4var <- pp_check(stan_model1c_nb, plotfun = "stat", stat = "prop_zero_4var"))
if (require(gridExtra)) {
grid.arrange(prop_zero_4var_poiss + ggtitle("Poisson"), 
               prop_zero_4var + ggtitle("Negative Binomial"), 
               ncol = 2)
}

##my trapping data using a log(x+1) transformation and ANOVA like the forestry community
library(rcompanion)
log_trans_data<-log(dat$Monochamus_tot + 1)
dat$log_trans<-log(dat$Monochamus_tot + 1)
plotNormalHistogram(dat$Monochamus_tot)
plotNormalHistogram(log_trans_data)
#log transformation makes data look much more normal

Anova_model = lm(dat$log_trans ~ dat$lures + dat$height, data=dat)
anova(Anova_model)
summary(Anova_model)
#both are significant variables
Anova_model_all = lm(dat$log_trans ~ dat$lures + dat$height + dat$date + dat$block, data=dat)
anova(Anova_model_all)
summary(Anova_model_all)
anova(Anova_model,Anova_model_all)
#all four variables are significant 
## LRT supports transformed model 2 (Anova_model_all), interactions not significant in either model background, coefficients change

#future run predictive checks for each to compare
#from other trapping data-pick out the species that have many zeroes or low mean differences because these are where the log transformation would be more likely to fail to give the correct answers
