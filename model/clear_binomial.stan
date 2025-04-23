data {
  int<lower=0> n1;
  int<lower=0> e1;
  int<lower=0> n2;
  int<lower=0> e2;
  real mu_prior;
  real<lower=0> sigma_prior;
}
parameters {
  real<lower=0,upper=1> p_raw;
  real delta;
}
transformed parameters {
  real p = fmin(p_raw, 0.999);
  real p1 = p * exp(delta);
}
model {
  delta ~ normal(mu_prior, sigma_prior);
  e1 ~ binomial(n1, fmin(p1, 0.999));
  e2 ~ binomial(n2, p);
}
