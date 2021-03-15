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

# inspect the data frame
str(orings)

# fit a logistic regression model
fit = glm(damage/6~temp, weights=rep(6,nrow(orings)), family=binomial, data=orings)
print(summary(fit))

# compute the standard error of the two estimated parameters and compare with the output of summary()
cov = vcov(fit)
print(cov)
se = c(sqrt(cov[1, 1]), sqrt(cov[2, 2]))
print(se) # we can see that the values agree with those obtained with summary()

# calculate the 95% Wald interval for the intercept parameter
print(11.66299 + c(-1,1)*qnorm(0.975)*3.29626) # width=12.921102 -> narrower and symmetric
# calculate the 95% profile likelihood interval for the intercept parameter
print(confint(fit, level=0.95)[1,]) # width=13.1624 -> wider and asymmetric

# conduct Wald and profile likelihood tests of the hypothesis
# H_0 : Temp = ???0.33 vs H_1 : Temp != ???0.33 at a 5% significance level
z = (-0.21623 - (-0.33))/se[2]
print(z) # 2.139458 > 1.96 => reject H_0, alternatively notice that -.33 is outside the 95% Wald CI
# for the profile likelihood test, we cannot reject -0.33 at 5% significance level as it is contained in the CI

# calculate the estimated asymptotic variance matrix of betah
x=orings$temp
m=rep(6,23)
betah = coef(fit)
print(solve(FIM(beta=betah, x=x, m=m)))
print(cov)

# without using the glm() function, carry out 10 iterations of Fisher scoring starting from betah = (0, 0)
y = orings$damage/6
betah = c(0, 0)
for (i in 1:10) {
  betah = betah + solve(FIM(beta=betah, x=x, m=m)) %*% u(betah, x, m, y)
  cat(betah, "\n")
}
print(betah)
print(coef(fit))