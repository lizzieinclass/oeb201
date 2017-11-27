// By Nick started on November 19 2017
// Building a model to predict perch height shifts on IRL islands
// Data structure: 
	// (1) lizards observed within islands
        // (2) male and female lizards expected to perch differently
        // (3) islands are split between control and treatment (removal of a congeneric competitor)
    // Predictor data observed only at observation level (individual lizards)
    // ... (that is, no additional predictors added at island level)
// Model structure does not assume same number of lizards across sites
// Code below includes varying intercepts across islands...
// assumes that all islands have the same within-island variance.  
// http://stackoverflow.com/questions/29379001/nested-model-in-stan

data {
  int N;    //total number of observations  (N = number lizards per island x number of islands)
  int J;    //total number of islands
  int islandnum[N];  // column of island number identifiers for each lizard observation
  vector[N] t;        // vector of predictor treatment for each lizard
  vector[N] s;        // vector of predictor sex for each lizard
  vector[N] r;        // vector of predictor year for each lizard
  real L;	//lower bound on response variable y
  real<lower=L> y[N]; // perch height, y, must be greater than or equal to L
}

parameters {
  real b_t;   // treatment effect
  real b_tr;   // treatment*year effect
  real b_r;   // year effect
  real b_s;   // sex effect
  real b_tsr;   // treatment*sex*year effect

  vector[J] a_island;    //estimated intercept for each island
  
  real mu_a;                    //mean intercept of all islands; 
      // the island intercept for island is drawn from distribution with mean mu_a...
  real<lower=0> sig_a;          //...and standard deviation sig_a

  real<lower=0> sig_y;          //variance in y within island
}

model {
  for (i in 1:N) {
    y[i] ~normal(a_island[islandnum[i]] + b_t*t[i] + b_tr*t[i]*r[i] + b_r*r[i] + b_s*s[i] + b_tsr*t[i]*s[i]*r[i],sig_y) T[L,];
  } //response variable y is distributed as a truncated normal with lower bound L

  //Defining island intercepts
  for (j in 1:J){
    a_island[j] ~ normal(mu_a,sig_a);
  }

}
