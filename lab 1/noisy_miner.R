# inspect the data frame
str(nminer)

# we will fit a simple Poisson regression to predict the number of noisy miners
# with number of Eucalypt trees as explanatory variables
fit = glm(Miners~Eucs, family=poisson, data=nminer)

# the coefficients are the following
print(coef(fit))

# more detailed information is given by the following function
print(summary(fit))