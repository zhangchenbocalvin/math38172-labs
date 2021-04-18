SBA = read.csv("data/SBA.csv")

# fit two GLMs (A nested in B)
fitA = glm(Default~Portion, family=binomial, data=SBA)
fitB = update(fitA, ~.+Recession+RealEstate)

# analysis of deviance
print(anova(fitA, fitB, test="Chisq"))

print(deviance(fitA))
print(df.residual(fitA))

# include interaction Portion:RealEstate in model B
fitC = update(fitB, ~.+Portion:RealEstate)
# include interaction Portion:Recession in model B
fitD = update(fitB, ~.+Portion:Recession)

# analysis of deviance
print(anova(fitB, fitC, test="Chisq"))  # -> reject model B in favor of model C at any reasonable significance level
print(anova(fitB, fitD, test="Chisq"))  # -> reject model B in favor of model D at any reasonable significance level

# fit GLM
fitE = glm(Default~Portion+Recession+RealEstate+Portion:RealEstate:Recession, family=binomial, data=SBA)

print(fitE)
print(table(SBA$Recession, SBA$RealEstate))  # no Portion:RealEstate:Recession interaction