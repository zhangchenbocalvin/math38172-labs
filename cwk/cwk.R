data = read.csv("cwkdata.csv")
print(str(data))
print(summary(data))

# fit GLM using only "Anorexia" as explanatory variable
fitA = glm(Severe~Anorexia, family=binomial, data=data)
print(coef(fitA))
print(summary(fitA))

# fit GLM using only "Temperature" as explanatory variable
fitT = glm(Severe~Temperature, family=binomial, data=data)
print(coef(fitT))
print(summary(fitT))

# fit GLM using only "Lymphocyte" as explanatory variable
fitL = glm(Severe~Lymphocyte, family=binomial, data=data)
print(coef(fitL))
print(summary(fitL))

# fit GLM using all three explanatory variables 
fit = glm(Severe~Anorexia+Temperature+Lymphocyte, family=binomial, data=data)
print(coef(fit))
print(summary(fit))

# analysis of deviance
print(anova(fitA, fit, test="Chisq"))
print(anova(fitT, fit, test="Chisq"))
print(anova(fitL, fit, test="Chisq"))

# estimate the probability of severe disease for a non-anorexic patient with a 
# temperature of 38°C and lymphocyte level of 1.3 billion cells per litre of blood
print(predict.glm(fit, data.frame(Anorexia="No", Temperature=38, Lymphocyte=1.3), type="response"))
print(predict.glm(fitL, data.frame(Anorexia="No", Temperature=38, Lymphocyte=1.3), type="response")) # as we cannot reject filL at any significance level