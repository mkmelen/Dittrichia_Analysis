## Mixed effects models summary ##

library(lme4)
library(car)
library(ggplot2)

radon <- read.csv("Radon_Data_RB.csv", header=T)
str(radon)
summary(radon)

boxplot(radon ~ floor, data=radon, xlab="floor", ylab="radon concentration")

## ==== Models ====

## random intercept model
radon.lmer1 <- lmer(radon ~ floor + (1|county), data=radon)
summary(radon.lmer1)

## random intercept and slope model
radon.lmer2 <- lmer(radon ~ floor + (1 + floor|county), data=radon)
summary(radon.lmer2)


## ==== Model checking ====

# 1. residuals (overall residuals = epsilon) vs. fitted: we want a starry sky. Similar amount of points on both sides of zero, no trend in the residuals. See if the relationship is linear (if resid in the middle are as well predicted as those on the edges). Make sure there's no wedge shape (heteroscadascity).
resids <- resid(radon.lmer1, type='pearson')
plot(resids~fitted(radon.lmer1))
lines(lowess(resids~fitted(radon.lmer1)), col='red')

# 2. compare residuals across the grouping variables (heteroscedacity). Boxplots should be similar
boxplot(resids~radon$floor)

# 3. sqrt of the absolute residuals vs. fitted (lower left of lm model checking panels; same as the first plot, but with a transformation). Should be no trend here: it's easier to look for the possible wedge shape now. Focus on homoscedasticity. 
plot(sqrt(abs(resids))~fitted(radon.lmer1))
lines(lowess(sqrt(abs(resids))~fitted(radon.lmer1)), col='red')

# 4. are the residuals normally distributed? --> normal Q-Q plot. If they deviate at the edges, you are underestimating the variance (making anti-conservative inferences). S-shape: you're overestimating the variance (making conservative inferences)
# what to do if you have horrible Q-Q plots? You may be leaving out important variables / your data are biomial/poisson/gamma distributed (variance increasing with the mean) / you need to transform your data.
# if all that doesn't help: use the models to help you understand your dataset; look for outliers etc. If you want to go with it anyway, use some sort of non-parametric bootstrap procedure. If you want more complicated error distributions: go Bayesian...
qqnorm(resids)
qqline(resids, col='red')

# 5. are the random effects normally distributed? Both intercepts and slopes!
# these plots often look odd, because you often only have few blocks or something like that (here we have 68 counties). You're not looking for perfection.
qqnorm(ranef(radon.lmer1)$county$'(Intercept)')
qqline(ranef(radon.lmer1)$county$'(Intercept)', col='red')

par(mfrow=c(1,2))
qqnorm(ranef(radon.lmer2)$county$'(Intercept)')
qqline(ranef(radon.lmer2)$county$'(Intercept)', col='red')

qqnorm(ranef(radon.lmer2)$county$floor)
qqline(ranef(radon.lmer2)$county$floor, col='red')


## ==== How do you get P-values? ====

# the lmer guys don't like it, but practically we may often want to get them! If you want a nice rant about p-values: 
?pvalues

radon.lmer0 <- update(radon.lmer1, ~.-floor)


# A) Kenward - Rogers approximation (nice)
library(car)
Anova(radon.lmer1, test="F")  # note: Anova with capital A, comes from the car package
KRmodcomp(radon.lmer0, radon.lmer1)  # model comparison  (this is another way of getting the same result)

# B) Satterthwaite approximation to the den dfs  (pretty accurate, but not very handy because you need to refit the model using lmerTest)
library(lmerTest)
radon.lmer1b <- lmerTest::lmer(radon ~ floor + (1|county), data=radon)
anova(radon.lmer1b) # gives a p-value
summary(radon.lmer1b) # nice because it also includes df for t 
detach("package:lmerTest")  # detach lmerTest because it interferes with lmer

# C) Likelihood ratio test (use if other methods fail - not ideal)
anova(radon.lmer1) # no P values...
anova(radon.lmer0, radon.lmer1) # compare to chisqr distribution: wrong for mixed models




