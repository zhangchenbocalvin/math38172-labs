# inspect the data frame
str(orings)

# we will fit a simple logistic regression, with the temperature as the explanatory variable

# first let us compute the response variable y = proportion of orings damaged (out of 6)
y = orings$damage / 6
print(y)

# now we fit the model using glm in two ways
fit1 = glm(y~temp, family=binomial, weights=rep(6, dim(orings)[1]), data=orings)
fit2 = glm(cbind(damage, 6-damage)~temp, family=binomial, data=orings)

# the coefficients are the following
print(coef(fit1))
print(coef(fit2))

# more detailed information is given by the following function
print(summary(fit1))
print(summary(fit2))

# we can see that the coefficients are the same