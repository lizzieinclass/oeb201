// Stan Model for Buds analysis - first stab, use buds as reps and have one level of hierarchy
// 1st is individual
// Based off Danf's and Lizzie's stan models for chilling experiment
// For Beta Distribution model in Percent Budburst

data {
  int<lower=0> N;
  
  // Dependent variable
  real<lower=0, upper=1> perc[N];
  
  vector[N] tx;
  vector[N] sp;
  
}

parameters {
  real<lower=0.001> b_tx;
  real<lower=0.001> b_sp;
  
  real<lower=0.001> sigma_tx;
  real<lower=0.001> sigma_sp;
  
  real mu_tx;
  real mu_sp;
 
}


model {
  b_tx ~ normal(0, 10);
  b_sp ~ normal(0, 10);
  
  sigma_tx ~ normal(0, 5);
  sigma_sp ~ normal(0, 5);
  
  mu_tx ~ normal(b_tx, sigma_tx);
  mu_sp ~ normal(b_sp, sigma_tx);
  
  perc ~ beta(mu_tx, mu_sp);
	  
}



