data {
  int<lower=0> N; // number of observations
  int<lower=1> G; // number of unique players
  int<lower=0> y[N]; // response variable, scores
  int<lower=1> P; // number of predictors
  matrix[N, P] x; // predictor matrix
  int<lower=1,upper=G> g[N]; // player assignment

  vector<lower=0>[G] mu0; // prior mean for each player's intercept
  vector<lower=0>[G] sigma0; // prior standard deviation for each player's intercept
}

parameters {
  vector[P] beta; // fixed-effect coefficients
  vector<lower=0>[G] mu_g; // player specific intercepts
}

model {
  
  // Coefficient priors
  beta ~ normal(-1, 1);
  
  // Player specific priors
  for (j in 1:G){
    mu_g[j] ~ normal(mu0[j],sigma0[j]);
  }

  // Likelihood
  y ~ poisson_log(mu_g[g] + x * beta);
}
