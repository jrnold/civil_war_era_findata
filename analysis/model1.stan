data {
  int N; // number of times
  int M; // number of variables

  int y_obs; // number of observations
  vector[y_obs] y;
  int<lower = 1, upper = M> y_variable[y_obs];
  int<lower = 1, upper = N> y_times[y_obs];

  // loadings
  int<lower = 1, upper = M> lambda_par_n;
  int lambda_par_idx[lambda_par_n];
  int<lower = 1, upper = M> lambda_obs_n;
  int lambda_obs_idx[lambda_obs_n];
  vector[lambda_obs_n] lambda_obs_val;

  // intercept
  int<lower = 1, upper = M> mu_obs_n;
  int mu_obs_idx[mu_obs_n];
  vector[mu_obs_n] mu_obs_val;
  int<lower = 1, upper = M> mu_par_n;
  int mu_par_idx[mu_par_n];

  // misc
  real<lower = 0.0> nu;
  // initialization
  real theta_init_mean;
  real theta_init_sd;
}
parameters {
  vector<lower = 0.0>[M] sigma;
  real<lower = 0.0> xi;
  vector[lambda_par_n] lambda_par_val;
  vector[mu_par_n] mu_par_val;
  vector[N] omega;
}
transformed parameters{
  vector[M] lambda;
  vector[M] mu;
  vector[N] theta;
  vector[y_obs] y_loc;
  vector[y_obs] y_scale;

  // system equation
  theta[1] <- theta_init_mean + theta_init_sd * omega[1];
  for (i in 2:N) {
    theta[i] <- theta[i - 1] + xi * omega[i];
  }
  // observation equation
  for (i in 1:lambda_par_n) {
    lambda[lambda_par_idx[i]] <- lambda_par_val[i];
  }
  for (i in 1:lambda_obs_n) {
    lambda[lambda_obs_idx[i]] <- lambda_obs_val[i];
  }
  for (i in 1:mu_par_n) {
    mu[mu_par_idx[i]] <- mu_par_val[i];
  }
  for (i in 1:mu_obs_n) {
    mu[mu_obs_idx[i]] <- mu_obs_val[i];
  }
  for (i in 1:y_obs) {
    y_loc[i] <- mu[y_variable[i]] + lambda[y_variable[i]] * theta[y_times[i]];
    y_scale[i] <- sigma[y_variable[i]];
  }

}
model {
  // system errors
  omega ~ normal(0.0, 1.0);
  // observation equation
  y ~ student_t(nu, y_loc, y_scale);
}
generated quantities {
  vector[y_obs] log_lik;
  for (i in 1:y_obs) {
    log_lik[i] <- student_t_log(y[i], nu, y_loc[i], y_scale[i]);
  }
}
