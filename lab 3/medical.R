# n=300, m=100
# Treatment A 87%, Treatment B 83%, Treatment C 78%

df <- data.frame (treatment  = c("A", "B", "C"),
                  recovery = c(87, 83, 78)
)

# fit two GLMs
fitA = glm(cbind(recovery, 100-recovery)~1, family=binomial, data=df)
fitB = update(fitA, ~.+treatment)

# analysis of variance
print(anova(fitA, fitB, test="Chisq"))