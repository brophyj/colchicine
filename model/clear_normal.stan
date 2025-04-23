data {
  real y;
  real<lower=0> sigma;
  real mu_prior;
  real<lower=0> mu_prior_sd;
}
parameters {
  real mu;
}
model {
  mu ~ normal(mu_prior, mu_prior_sd);
  y ~ normal(mu, sigma);
}
