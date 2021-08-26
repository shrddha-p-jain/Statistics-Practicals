studytime=c(40,43,18,10,25,33,27,17,30,47) #x
science=c(58,73,56,47,58,54,45,32,68,69) #y
simpreg=data.frame(studytime,science)
simpreg
cor(studytime,science) # simple correlation
simpreg.lm=lm(science~studytime,data=simpreg)
summary(simpreg.lm) 
anova(simpreg.lm)

names(simpreg.lm)
simpreg.lm$coefficients
coef(simpreg.lm)
confint(simpreg.lm)
confint(simpreg.lm,level = 0.9)

science
fitted(simpreg.lm)
residuals(simpreg.lm)

plot(studytime,science)
abline(lm(science~studytime,data=simpreg))
#abline(coef(simpreg.lm))

opt=par(mfrow=c(2,2))
plot(simpreg.lm)
#1. Residuals vs Fitted : 
#This plot shows if residuals have non-linear patterns.
#Linear pattern: equally spread residuals around a horizontal line 
#2. Normal Q - Q:
#Residual are normally distributed.
#3. Scale(Spread) - Location
#Residuals are spread equally along the ranges of predictors.
#Assumptions of equal variance (Homoscadasticity)
#4. Residuals vs Leverage
#understanding outliers
forcast.value=data.frame(studytime=c(20,40,45))
predict(simpreg.lm,forcast.value)
