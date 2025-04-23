data {
  real y;
  real mu_prior;
  real<lower=0> sigma_prior;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(mu_prior, sigma_prior);
  sigma ~ cauchy(0, 2);
  y ~ normal(mu, sigma);
}
