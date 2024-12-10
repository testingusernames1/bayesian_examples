data {
  int<lower=0> N; // number of observations
  int<lower=1> G; // number of unique players
  int<lower=0> y[N]; // response variable, scores
  int<lower=1,upper=G> g[N]; // player assignment

  vector<lower=0>[G] mu0; // prior mean for each player's intercept
}

parameters {
  vector<lower=0>[G] lambda; // player specific means
}

model {

  // Player specific priors
  for (j in 1:G){
    lambda[j] ~ gamma(mu0[j]*200,200);
  }

  // Likelihood
  y ~ poisson(lambda[g]);
}
