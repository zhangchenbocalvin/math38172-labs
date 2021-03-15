u <- function(beta, x, y) {
  eta <- beta[1] + beta[2]*x
  mu <- exp(eta)
  u0 <- sum(y-mu)
  u1 <- sum(x*(y-mu))
  c(u0,u1)
}

FIM <- function(beta, x, m) {
  eta <- beta[1] + beta[2] * x
  mu <- exp(eta)
  M <- matrix(0, ncol = 2, nrow = 2)
  M[1,1] <- sum(mu)
  M[1,2] <- sum(mu*x)
  M[2,1] <- sum(mu*x)
  M[2,2] <- sum(mu*x^2)
  return(M)
}

# inspect the data frame
str(nminer2)

# fit a Poisson regression model
fit = glm(Minerab~Eucs, family=poisson, data=nminer2)
print(summary(fit))


# using three different methods, find a 95% confidence interval for the expected
# abundance in an area with 6 Eucalypt trees

# method 1 -> apply the delta method to h(b) = exp(b_0 + 6*b_1)
betah = coef(fit)
print(betah)
phih = exp(betah[1] + 6 * betah[2])
print(phih)
gradh = c(phih, 6*phih)
print(gradh)
se = sqrt(t(gradh)%*%vcov(fit)%*%gradh)
print(se)
print(phih + c(-1,1)*qnorm(0.975)*c(se))

# method 2 -> apply the delta method to h(b) = b_0 + 6*b_1 and then transform the endpoints
etah = betah[1] + 6 * betah[2]
print(etah)
gradh = c(1, 6)
print(gradh)
se = sqrt(t(gradh)%*%vcov(fit)%*%gradh)
print(se)
print(exp(etah + c(-1,1)*qnorm(0.975)*c(se)))

# method 3 -> find a profile likelihood CI with modified explanatory variables
fit2 = glm(Minerab~I(Eucs-6), family=poisson, data=nminer2)
print(summary(fit2))
CI = exp(confint(fit2))
print(CI)


# using three different methods, find a 95% confidence interval for the percentage
# increase in expected abundance associated with one additional Eucalypt tree

# method 1 -> apply the delta method to the percentage increase h(b) = 100*(exp(b_1)-1)
lambdah = 100 * (exp(betah[2])-1)
print(lambdah)
gradh = c(0, 100*exp(betah[2]))
print(gradh)
se = sqrt(t(gradh)%*%vcov(fit)%*%gradh)
print(se)
print(lambdah + c(-1,1)*qnorm(0.975)*c(se))

# method 2 -> calculate the 95% Wald interval for b_1 and transform the endpoints
print(100* (exp(0.11398 + c(-1,1)*qnorm(0.975)*0.01243) - 1))

# method 3 -> calculate the 95% likelihood interval for b_1 and transform the endpoints
print(100 * (exp(confint(fit)) - 1)[2,])


# calculate the estimated asymptotic variance matrix of betah
x=nminer2$Eucs
betah = coef(fit)
print(solve(FIM(beta=betah, x=x)))
print(vcov(fit))

# without using the glm() function, carry out 15 iterations of Fisher scoring
y = nminer2$Minerab
betah = c(0, 0)
for (i in 1:10) {
  betah = betah + solve(FIM(beta=betah, x=x, m=m)) %*% u(betah, x, y)
  cat(betah, "\n")
}
print(betah)
print(coef(fit))