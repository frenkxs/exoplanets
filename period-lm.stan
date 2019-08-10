data {
  // Define all the data here
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of explanatory variables
  matrix[N, K] x; // explanatory variables
  vector[N] y; // response variable
}
parameters {
  // Define parameters here
  real alpha; // intercept
  vector[K] beta; // slope
  real<lower=0> sigma; // residual sd
}
transformed parameters {
  vector[N] fits;
  fits = alpha + x * beta;
}
model {
  // In this version X is a matrix
  y ~ normal(fits, sigma);
}
generated quantities {
  vector[N] res;
  res = y - fits;
}
