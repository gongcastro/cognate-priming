// generated with brms 2.19.0
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  // data for group-level effects of ID 1
  int<lower=1> N_1; // number of grouping levels
  int<lower=1> M_1; // number of coefficients per level
  array[N] int<lower=1> J_1; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2; // number of grouping levels
  int<lower=1> M_2; // number of coefficients per level
  array[N] int<lower=1> J_2; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_phi_1;
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  
}
parameters {
  real Intercept; // temporary intercept for centered predictors
  real Intercept_phi; // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  array[M_1] vector[N_1] z_1; // standardized group-level effects
  vector<lower=0>[M_2] sd_2; // group-level standard deviations
  array[M_2] vector[N_2] z_2; // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1; // actual group-level effects
  vector[N_2] r_2_phi_1; // actual group-level effects
  real lprior = 0; // prior contributions to the log posterior
  r_1_1 = sd_1[1] * z_1[1];
  r_2_phi_1 = sd_2[1] * z_2[1];
  lprior += normal_lpdf(Intercept | 0, 2);
  lprior += normal_lpdf(Intercept_phi | 4, 2);
  lprior += exponential_lpdf(sd_1 | 4);
  lprior += exponential_lpdf(sd_2 | 4);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] phi = rep_vector(0.0, N);
    mu += Intercept;
    phi += Intercept_phi;
    for (n in 1 : N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      phi[n] += r_2_phi_1[J_2[n]] * Z_2_phi_1[n];
    }
    mu = inv_logit(mu);
    phi = exp(phi);
    for (n in 1 : N) {
      target += beta_lpdf(Y[n] | mu[n] * phi[n], (1 - mu[n]) * phi[n]);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  // actual population-level intercept
  real b_phi_Intercept = Intercept_phi;
  // additionally sample draws from priors
  real prior_Intercept = normal_rng(0, 2);
  real prior_Intercept_phi = normal_rng(4, 2);
  real prior_sd_1 = exponential_rng(4);
  real prior_sd_2 = exponential_rng(4);
  // use rejection sampling for truncated priors
  while (prior_sd_1 < 0) {
    prior_sd_1 = exponential_rng(4);
  }
  while (prior_sd_2 < 0) {
    prior_sd_2 = exponential_rng(4);
  }
}

