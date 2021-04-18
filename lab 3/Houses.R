Houses = read.csv("data/Houses.csv")

# fit two GLMs (A nested in B)
fitA = glm(price~size, family=Gamma(link="identity"), data=Houses)
fitB = glm(price~size+new+new:size, family=Gamma(link="identity"), data=Houses)

# analysis of deviance
print(anova(fitA, fitB, test="F"))