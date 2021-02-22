# inspect the data frame
str(chemistry)

# create variable for quadratic term
z=chemistry$x^2

# fit the linear model
fit = lm(y~x+z, data=chemistry)
fit1 = lm(y~x+x^2, data=chemistry)  # even though there is x^2, it fits a linear model in x
fit2 = lm(y~x+I(x^2),data=chemistry)  # this fits the quadratic model in x

# the coefficients are the following
print(coef(fit))
print(coef(fit1))
print(coef(fit2))