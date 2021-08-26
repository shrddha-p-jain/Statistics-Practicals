square_feet=c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)
house_price=c(245,312,279,308,199,219,405,324,319,255)
model=data.frame(square_feet,house_price)
cor(model)
plot(model)
model.lm=lm(house_price~square_feet,data=model)
summary(model.lm) 
anova(model.lm)
abline(lm(house_price~square_feet,data=model))
coef(model.lm)
qt(0.025,df=8)
qf(0.05,1,8,lower.tail = F)


names(model.lm)
model.lm$coefficients
coef(model.lm)
confint(model.lm)
confint(model.lm,level = 0.9)

house_price
fitted(model.lm)
residuals(model.lm)

opt=par(mfrow=c(2,2))
plot(model.lm)
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
forcast.value=data.frame(square_feet=c(1500,2000,2500))
predict(model.lm,forcast.value)

