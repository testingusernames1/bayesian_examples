```{stan output.var = "compiled_model"}
data {
  int<lower=0> N;           // number of data points
  int<lower=1> G;           // number of players
  int<lower=1> C;           // number of courses
  int<lower=0> y[N];        // scores (count data)
  int<lower=1> P;           // number of predictors
  matrix[N, P] x;           // predictor matrix
  int<lower=1,upper=G> g[N]; // player assignment for each observation
  int<lower=1,upper=C> c[N]; // course assignment for each observation

  vector[G] mu0;            // prior means for player intercepts
  vector[G] sigma0;         // prior std devs for player intercepts
}

parameters {
  // Fixed effects
  vector[P] beta;           // fixed-effect coefficients

  // Player-level effects
  vector[G] mu_g;           // player-specific intercepts

  // Course-level effects
  vector[C] mu_c;           // course-specific intercepts
  vector[P] beta_c[C];      // course-specific slopes
}

model {
  // Priors on fixed effects
  beta ~ normal(0, 2);

  // Player-level priors
  for (l in 1:G) {
    mu_g[l] ~ normal(mu0[l], sigma0[l]);
  }

  // Course-level priors
  mu_c ~ normal(0, 2);      // course intercept priors

  for (k in 1:C) {
    beta_c[k] ~ normal(0, 4);
  }

  // Likelihood
  for (n in 1:N) {
    // linear predictor:
    // include player intercept, course intercept, fixed effects,
    // fixed coefficients, and course-specific coefficients
    real lambda = mu_g[g[n]]
               + mu_c[c[n]]
               + dot_product(beta, x[n])
               + dot_product(beta_c[c[n]], x[n]);

    y[n] ~ poisson_log(lambda);
  }
}