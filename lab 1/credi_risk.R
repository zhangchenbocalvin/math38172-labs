# inspect the data frame
str(SBA)

# we will fit a simple logistic regression to predict the probability of loan default, with three explanatory variables
fit = glm(Default~Portion+Recession+RealEstate, family=binomial, data=SBA)

# the coefficients are the following
print(coef(fit))

# more detailed information is given by the following function
print(summary(fit))

# predict the probability for a loan with no SBA guarantee, and no real estate backing
# we could have simply subbed in the fitted parameters to find eta and then use the logistic function to find mu
print(predict.glm(fit, data.frame(Portion=0, Recession=0, RealEstate=0), type='response'))