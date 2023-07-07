// generated with brms 2.19.0
functions {
  /* compute correlated group-level effects
   * Args:
   *   z: matrix of unscaled group-level effects
   *   SD: vector of standard deviation parameters
   *   L: cholesky factor correlation matrix
   * Returns:
   *   matrix of scaled group-level effects
   */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  int<lower=1> K; // number of population-level effects
  matrix[N, K] X; // population-level design matrix
  int<lower=1> K_phi; // number of population-level effects
  matrix[N, K_phi] X_phi; // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1; // number of grouping levels
  int<lower=1> M_1; // number of coefficients per level
  array[N] int<lower=1> J_1; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  vector[N] Z_1_3;
  vector[N] Z_1_4;
  vector[N] Z_1_5;
  vector[N] Z_1_6;
  int<lower=1> NC_1; // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2; // number of grouping levels
  int<lower=1> M_2; // number of coefficients per level
  array[N] int<lower=1> J_2; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_phi_1;
  vector[N] Z_2_phi_2;
  vector[N] Z_2_phi_3;
  int<lower=1> NC_2; // number of group-level correlations
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc; // centered version of X without an intercept
  vector[Kc] means_X; // column means of X before centering
  int Kc_phi = K_phi - 1;
  matrix[N, Kc_phi] Xc_phi; // centered version of X_phi without an intercept
  vector[Kc_phi] means_X_phi; // column means of X_phi before centering
  for (i in 2 : K) {
    means_X[i - 1] = mean(X[ : , i]);
    Xc[ : , i - 1] = X[ : , i] - means_X[i - 1];
  }
  for (i in 2 : K_phi) {
    means_X_phi[i - 1] = mean(X_phi[ : , i]);
    Xc_phi[ : , i - 1] = X_phi[ : , i] - means_X_phi[i - 1];
  }
}
parameters {
  vector[Kc] b; // population-level effects
  real Intercept; // temporary intercept for centered predictors
  vector[Kc_phi] b_phi; // population-level effects
  real Intercept_phi; // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  matrix[M_1, N_1] z_1; // standardized group-level effects
  cholesky_factor_corr[M_1] L_1; // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2; // group-level standard deviations
  matrix[M_2, N_2] z_2; // standardized group-level effects
  cholesky_factor_corr[M_2] L_2; // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  vector[N_1] r_1_3;
  vector[N_1] r_1_4;
  vector[N_1] r_1_5;
  vector[N_1] r_1_6;
  matrix[N_2, M_2] r_2; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_phi_1;
  vector[N_2] r_2_phi_2;
  vector[N_2] r_2_phi_3;
  real lprior = 0; // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_1 = r_1[ : , 1];
  r_1_2 = r_1[ : , 2];
  r_1_3 = r_1[ : , 3];
  r_1_4 = r_1[ : , 4];
  r_1_5 = r_1[ : , 5];
  r_1_6 = r_1[ : , 6];
  // compute actual group-level effects
  r_2 = scale_r_cor(z_2, sd_2, L_2);
  r_2_phi_1 = r_2[ : , 1];
  r_2_phi_2 = r_2[ : , 2];
  r_2_phi_3 = r_2[ : , 3];
  lprior += normal_lpdf(b | 0, 2);
  lprior += normal_lpdf(Intercept | 0, 2);
  lprior += normal_lpdf(b_phi | 0, 2);
  lprior += normal_lpdf(Intercept_phi | 4, 2);
  lprior += exponential_lpdf(sd_1 | 4);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 4);
  lprior += exponential_lpdf(sd_2 | 4);
  lprior += lkj_corr_cholesky_lpdf(L_2 | 4);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] phi = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    phi += Intercept_phi + Xc_phi * b_phi;
    for (n in 1 : N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n]
               + r_1_3[J_1[n]] * Z_1_3[n] + r_1_4[J_1[n]] * Z_1_4[n]
               + r_1_5[J_1[n]] * Z_1_5[n] + r_1_6[J_1[n]] * Z_1_6[n];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      phi[n] += r_2_phi_1[J_2[n]] * Z_2_phi_1[n]
                + r_2_phi_2[J_2[n]] * Z_2_phi_2[n]
                + r_2_phi_3[J_2[n]] * Z_2_phi_3[n];
    }
    mu = inv_logit(mu);
    phi = exp(phi);
    for (n in 1 : N) {
      target += beta_lpdf(Y[n] | mu[n] * phi[n], (1 - mu[n]) * phi[n]);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
  target += std_normal_lpdf(to_vector(z_2));
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_phi_Intercept = Intercept_phi - dot_product(means_X_phi, b_phi);
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1, upper=1>[NC_1] cor_1;
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1, upper=1>[NC_2] cor_2;
  // additionally sample draws from priors
  real prior_b = normal_rng(0, 2);
  real prior_Intercept = normal_rng(0, 2);
  real prior_b_phi = normal_rng(0, 2);
  real prior_Intercept_phi = normal_rng(4, 2);
  real prior_sd_1 = exponential_rng(4);
  real prior_cor_1 = lkj_corr_rng(M_1, 4)[1, 2];
  real prior_sd_2 = exponential_rng(4);
  real prior_cor_2 = lkj_corr_rng(M_2, 4)[1, 2];
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_1) {
    for (j in 1 : (k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_2) {
    for (j in 1 : (k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
  // use rejection sampling for truncated priors
  while (prior_sd_1 < 0) {
    prior_sd_1 = exponential_rng(4);
  }
  while (prior_sd_2 < 0) {
    prior_sd_2 = exponential_rng(4);
  }
}

