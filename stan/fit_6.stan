// generated with brms 2.16.1
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
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  vector[N] Z_1_3;
  vector[N] Z_1_4;
  vector[N] Z_1_5;
  vector[N] Z_1_6;
  vector[N] Z_1_7;
  vector[N] Z_1_8;
  vector[N] Z_1_9;
  vector[N] Z_1_10;
  vector[N] Z_1_11;
  vector[N] Z_1_12;
  vector[N] Z_1_13;
  vector[N] Z_1_14;
  vector[N] Z_1_15;
  vector[N] Z_1_16;
  vector[N] Z_1_17;
  vector[N] Z_1_18;
  vector[N] Z_1_19;
  vector[N] Z_1_20;
  vector[N] Z_1_21;
  vector[N] Z_1_22;
  vector[N] Z_1_23;
  vector[N] Z_1_24;
  vector[N] Z_1_25;
  vector[N] Z_1_26;
  vector[N] Z_1_27;
  vector[N] Z_1_28;
  vector[N] Z_1_29;
  vector[N] Z_1_30;
  vector[N] Z_1_31;
  vector[N] Z_1_32;
  vector[N] Z_1_33;
  vector[N] Z_1_34;
  vector[N] Z_1_35;
  vector[N] Z_1_36;
  int<lower=1> NC_1;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  vector[N_1] r_1_3;
  vector[N_1] r_1_4;
  vector[N_1] r_1_5;
  vector[N_1] r_1_6;
  vector[N_1] r_1_7;
  vector[N_1] r_1_8;
  vector[N_1] r_1_9;
  vector[N_1] r_1_10;
  vector[N_1] r_1_11;
  vector[N_1] r_1_12;
  vector[N_1] r_1_13;
  vector[N_1] r_1_14;
  vector[N_1] r_1_15;
  vector[N_1] r_1_16;
  vector[N_1] r_1_17;
  vector[N_1] r_1_18;
  vector[N_1] r_1_19;
  vector[N_1] r_1_20;
  vector[N_1] r_1_21;
  vector[N_1] r_1_22;
  vector[N_1] r_1_23;
  vector[N_1] r_1_24;
  vector[N_1] r_1_25;
  vector[N_1] r_1_26;
  vector[N_1] r_1_27;
  vector[N_1] r_1_28;
  vector[N_1] r_1_29;
  vector[N_1] r_1_30;
  vector[N_1] r_1_31;
  vector[N_1] r_1_32;
  vector[N_1] r_1_33;
  vector[N_1] r_1_34;
  vector[N_1] r_1_35;
  vector[N_1] r_1_36;
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_1 = r_1[, 1];
  r_1_2 = r_1[, 2];
  r_1_3 = r_1[, 3];
  r_1_4 = r_1[, 4];
  r_1_5 = r_1[, 5];
  r_1_6 = r_1[, 6];
  r_1_7 = r_1[, 7];
  r_1_8 = r_1[, 8];
  r_1_9 = r_1[, 9];
  r_1_10 = r_1[, 10];
  r_1_11 = r_1[, 11];
  r_1_12 = r_1[, 12];
  r_1_13 = r_1[, 13];
  r_1_14 = r_1[, 14];
  r_1_15 = r_1[, 15];
  r_1_16 = r_1[, 16];
  r_1_17 = r_1[, 17];
  r_1_18 = r_1[, 18];
  r_1_19 = r_1[, 19];
  r_1_20 = r_1[, 20];
  r_1_21 = r_1[, 21];
  r_1_22 = r_1[, 22];
  r_1_23 = r_1[, 23];
  r_1_24 = r_1[, 24];
  r_1_25 = r_1[, 25];
  r_1_26 = r_1[, 26];
  r_1_27 = r_1[, 27];
  r_1_28 = r_1[, 28];
  r_1_29 = r_1[, 29];
  r_1_30 = r_1[, 30];
  r_1_31 = r_1[, 31];
  r_1_32 = r_1[, 32];
  r_1_33 = r_1[, 33];
  r_1_34 = r_1[, 34];
  r_1_35 = r_1[, 35];
  r_1_36 = r_1[, 36];
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + rep_vector(0.0, N);
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n] + r_1_3[J_1[n]] * Z_1_3[n] + r_1_4[J_1[n]] * Z_1_4[n] + r_1_5[J_1[n]] * Z_1_5[n] + r_1_6[J_1[n]] * Z_1_6[n] + r_1_7[J_1[n]] * Z_1_7[n] + r_1_8[J_1[n]] * Z_1_8[n] + r_1_9[J_1[n]] * Z_1_9[n] + r_1_10[J_1[n]] * Z_1_10[n] + r_1_11[J_1[n]] * Z_1_11[n] + r_1_12[J_1[n]] * Z_1_12[n] + r_1_13[J_1[n]] * Z_1_13[n] + r_1_14[J_1[n]] * Z_1_14[n] + r_1_15[J_1[n]] * Z_1_15[n] + r_1_16[J_1[n]] * Z_1_16[n] + r_1_17[J_1[n]] * Z_1_17[n] + r_1_18[J_1[n]] * Z_1_18[n] + r_1_19[J_1[n]] * Z_1_19[n] + r_1_20[J_1[n]] * Z_1_20[n] + r_1_21[J_1[n]] * Z_1_21[n] + r_1_22[J_1[n]] * Z_1_22[n] + r_1_23[J_1[n]] * Z_1_23[n] + r_1_24[J_1[n]] * Z_1_24[n] + r_1_25[J_1[n]] * Z_1_25[n] + r_1_26[J_1[n]] * Z_1_26[n] + r_1_27[J_1[n]] * Z_1_27[n] + r_1_28[J_1[n]] * Z_1_28[n] + r_1_29[J_1[n]] * Z_1_29[n] + r_1_30[J_1[n]] * Z_1_30[n] + r_1_31[J_1[n]] * Z_1_31[n] + r_1_32[J_1[n]] * Z_1_32[n] + r_1_33[J_1[n]] * Z_1_33[n] + r_1_34[J_1[n]] * Z_1_34[n] + r_1_35[J_1[n]] * Z_1_35[n] + r_1_36[J_1[n]] * Z_1_36[n];
    }
    target += normal_id_glm_lpdf(Y | Xc, mu, b, sigma);
  }
  // priors including constants
  target += normal_lpdf(b | 0, 0.1);
  target += normal_lpdf(Intercept | 0, 0.1);
  target += exponential_lpdf(sigma | 4);
  target += exponential_lpdf(sd_1 | 4);
  target += std_normal_lpdf(to_vector(z_1));
  target += lkj_corr_cholesky_lpdf(L_1 | 5);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // additionally sample draws from priors
  real prior_b = normal_rng(0,0.1);
  real prior_Intercept = normal_rng(0,0.1);
  real prior_sigma = exponential_rng(4);
  real prior_sd_1 = exponential_rng(4);
  real prior_cor_1 = lkj_corr_rng(M_1,5)[1, 2];
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
  // use rejection sampling for truncated priors
  while (prior_sigma < 0) {
    prior_sigma = exponential_rng(4);
  }
  while (prior_sd_1 < 0) {
    prior_sd_1 = exponential_rng(4);
  }
}
