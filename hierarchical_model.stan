data {
  int<lower=0> N; // number of individuals
  int<lower=0> J; // number of counties
  int<lower=1,upper=J> county[N]; // county indicator for each individual
  int<lower=0,upper=1> y[N]; // cognitive decline indicator
  int<lower=1,upper=6> AGE_GROUP[N]; // Age group (6 categories)
  int<lower=1,upper=6> RACE_GROUP[N]; // Race group (6 categories)
  int<lower=1,upper=6> EDUCA[N]; // Education level (6 categories)
  int<lower=1,upper=2> SEX_GROUP[N]; // Sex group (2 categories)
  real LLCPWT[N]; // weights
}

parameters {
  real beta_0;
  vector[5] beta_age; // 6 age groups minus one reference group
  vector[5] beta_race; // 6 race groups minus one reference group
  vector[5] beta_educa; // 6 education levels minus one reference group
  vector[1] beta_sex; // 2 sex groups minus one reference group
  vector[J] u; // county-level random effect
  real<lower=0> sigma_u; // standard deviation of random effect
}

model {
  // Priors
  beta_0 ~ normal(0, 10);
  beta_age ~ normal(0, 10);
  beta_race ~ normal(0, 10);
  beta_educa ~ normal(0, 10);
  beta_sex ~ normal(0, 10);
  u ~ normal(0, sigma_u);
  sigma_u ~ cauchy(0, 2.5);
  
  // Likelihood
  for (i in 1:N) {
    target += LLCPWT[i] * bernoulli_logit_lpmf(y[i] | beta_0 +
                           beta_age[AGE_GROUP[i]] +
                           beta_race[RACE_GROUP[i]] +
                           beta_educa[EDUCA[i]] +
                           beta_sex[SEX_GROUP[i]] +
                           u[county[i]]);
  }
}

