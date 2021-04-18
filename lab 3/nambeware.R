nambe = read.csv("data/nambeware.csv")

# plot price vs diameter
plot(nambe$Price, nambe$Diam)  # positive response

# fit GLM
fitA = glm(Price~Diam+Time, family=Gamma(link="log"), data=nambe)
fitB = update(fitA, ~.+Type)

# analysis of deviance
print(anova(fitA, fitB, test="F"))