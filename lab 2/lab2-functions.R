u <- function(beta, x, m, y) {
  eta <- beta[1] + beta[2]*x
  mu <- exp(eta)/ (1+exp(eta))
  u0 <- sum(m*(y-mu))
  u1 <- sum(m*x*(y-mu))
  c(u0,u1)
}

FIM <- function(beta, x, m) {
  eta <- beta[1] + beta[2] * x
  mu <- exp(eta) / (1 + exp(eta))
  W <- m * mu * (1 - mu)
  M <- matrix(0, ncol = 2, nrow = 2)
  M[1,1] <- sum(W)
  M[1,2] <- sum(W*x)
  M[2,1] <- sum(W*x)
  M[2,2] <- sum(W*x^2)
  return(M)
}
