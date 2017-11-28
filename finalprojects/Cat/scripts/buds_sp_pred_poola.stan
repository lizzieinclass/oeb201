// Stan Model for Buds analysis - first stab, use buds as reps and have one level of hierarchy
// 1st is individual
// Based off Danf's and Lizzie's stan models for chilling experiment

data {
  int<lower=0> N;
  int<lower=0> n_ind;
  int<lower=0> n_sp;
  int<lower=1, upper=n_ind> ind[N];
  vector[N] dvr;
  vector[N] tx;
  vector[N] sp;
  
}

parameters {
  vector[n_ind] a_ind;
  vector[n_ind] b_tx;
  vector[n_ind] b_sp;

  real mu_a; 
  real mu_b_tx;
  real mu_b_sp;

  real<lower=0> sigma_b_tx;
  real<lower=0> sigma_b_sp;

  real<lower=0> sigma_a;
    
  real<lower=0> sigma_y; 
  
}


transformed parameters { 
		vector[N] y_hat;
		
	for(i in 1:N){
		y_hat[i] = a_ind[ind[i]] + 
		b_sp[ind[i]] * sp[i] + 
		b_tx[ind[i]] * tx[i] 
		;
				
		}
	
}

model {
	// Priors. Make them flat
	mu_b_tx ~ normal(0, 15); 
	mu_b_sp ~ normal(0, 15);
	
	sigma_b_tx ~ normal(0, 10);
	sigma_b_sp ~ normal(0, 10);

	a_ind ~ normal(mu_a, sigma_a);  
	
	b_tx ~ normal(mu_b_tx, sigma_b_tx);
	b_sp ~ normal(mu_b_sp, sigma_b_sp);
	
	dvr ~ normal(y_hat, sigma_y);

}

