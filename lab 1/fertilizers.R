# inspect the data sets
str(fertilizers1)  # here the fertilizer is represented by a letter (char)
str(fertilizers2)  # here the fertilizer is represented by an integer (int)

# fit a linear model
fit1 = lm(Heights~Fertilizer, data=fertilizers1)  # here Fertilizer is treated as a categorical variable, hence the creation of all the dummy variables
fit2 = lm(Heights~Fertilizer, data=fertilizers2)  # here Fertilizer is treated as a numerical variable
fit3 = lm(Heights~factor(Fertilizer), data=fertilizers2)  # this fitted model behaves like fit1

# the coefficients are the following
print(coef(fit1))
print(coef(fit2))
print(coef(fit3))