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