# inspect the data frame
str(beetle)

# we will fit a simple logistic regression, with log_10(x) as the explanatory variable

# first let us compute the response variable y = proportion of beetles affected by dose x
y = beetle$affected / beetle$exposed
print(y)

# now fit the model using the glm function
# y~logdose tells R to fit a model with response y using logdose as explanatory variable
# family=binomial tells R to use a binomial response distribution - R will then automatically fit a logistic regression model
# weights=exposed tells R that the numbers of binomial trials for each observation are given in the vector exposed
# data=beetle tells R to look for the vector logdose in the data frame beetle
fit1 = glm(y~logdose, family=binomial, weights=exposed, data=beetle)

# the coefficients are the following
print(coef(fit1))

# more detailed information is given by the following function
print(summary(fit1))


# another way of specifying the response is as a matrix where the first column gives the number of binomial 
# successes and the second column gives the number of binomial failures
fit2 = glm(cbind(affected, exposed-affected)~logdose, family=binomial, data=beetle)

# the coefficients are the following
print(coef(fit2))

# more detailed information is given by the following function
print(summary(fit2))

# we can clearly see from the print statements above that the two different ways of specifying the repsponse
# variable give the same results