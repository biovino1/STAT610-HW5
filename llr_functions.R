llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

make_weight_matrix = function(z, x, omega) {
  #' Defines a diagonal matrix where every i-th element is the result of:
  #'    W(abs(x[i] - z)/omega)
  #' where W(r) is the function below:
  #'    (1-|z|^3)^3  if  |z| <1
  #'    0   o.w.
  
  values <- (abs(x-z)/omega)
  transformed_vals <- ifelse((abs(values)<1), 1-(abs(values)^3)^3, 0)
  Wz = diag(transformed_vals)
  return(Wz)
}

make_predictor_matrix = function(x) {
  #' Defines a predictor matrix whose first column contains all 1's and whose
  #' second column contains the values of the predictor
  
  X <- matrix(1, nrow=length(x), ncol=2)
  X[, 2] <- x
  return(X)
}

compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

## a very simple regression model
#n = 15
#x = rnorm(n)
#y = rnorm(x + rnorm(n))
#z = seq(-1, 1, length.out = 100)
#llr(x, y, z, 1)

