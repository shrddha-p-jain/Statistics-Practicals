studytime=c(40,43,18,10,25,33,27,17,30,47)
science=c(58, 73, 56, 47, 58, 54, 45, 32, 68, 69)
intelligence=c(118, 128, 110, 114, 138, 120, 106, 124, 132, 130)
maths=c(44, 61, 58, 55, 54, 50, 56, 39, 71, 65)
attend=c(51, 57, 55, 66, 67, 72, 66, 63, 70, 71)
literacy=c(30, 21, 42, 27, 49, 23, 50, 21, 38, 17)
sc_score=data.frame(science,studytime,intelligence,maths,attend,literacy)
summary(sc_score)

#Method - 1: start with no predictor variable and forward selection
sc_score.lm=lm(science~1,data=sc_score)
summary(sc_score.lm)

add1(sc_score.lm,scope=sc_score,test = 'F')#start adding one by one variable
sc_score.lm=lm(science~maths,data=sc_score)

add1(sc_score.lm,scope=sc_score,test='F')#check for significance of each variable using test command
sc_score.lm=lm(science~maths+studytime,data=sc_score)

add1(sc_score.lm,scope=sc_score,test='F')

#all variables are not significant difference
sc_score.lm=lm(science~maths+studytime,data=sc_score)#final answer
summary(sc_score.lm)
anova(sc_score.lm)#check anova result also

#Method - 2: start with all predictor variables and backward deletion
sc_score.lm=lm(science~.,data=sc_score)
formula(sc_score.lm)

drop1(sc_score.lm,test = 'F')#remove that variable which has lowest AIC
sc_score.lm=lm(science~studytime + intelligence + maths + attend,data=sc_score)

drop1(sc_score.lm,test = 'F')
sc_score.lm=lm(science~studytime + maths + attend,data=sc_score)

drop1(sc_score.lm,test = 'F')
sc_score.lm=lm(science~studytime + maths,data=sc_score)
drop1(sc_score.lm,test = 'F')
summary(sc_score.lm)
anova(sc_score.lm)

#comparing different linear models
sc_score.lm1=lm(science~studytime+maths,data=sc_score)
sc_score.lm2=lm(science~.,data=sc_score)
anova(sc_score.lm1,sc_score.lm2)
#there is no statistically significant difference between them i.e. not worth adding anything in original model

#Example - 2
head(mtcars)
head(mtcars)
summary(fitstart)
fitall=lm(mpg~.,data=mtcars)
fitstart=lm(mpg~1,data=mtcars)

#Method-3:Selecting a subset of predictor variables from a larger set 
#(e.g. stepwise selection). stepAIC performs stepwise model selection by exact AIC

#shortcut
back=step(lm(science~.,data=sc_score),direction="backward")
forw=step(lm(science~1,data=sc_score),direction='forward',scope=~studytime + intelligence + maths + attend + literacy)
step=step(lm(science~.,data=sc_score),direction='both')	
step(fitstart,direction="forward",scope=formula(fitall))

library(MASS)
fit=step(lm(science~.,data=sc_score))
fit
step=stepAIC(fit,direction = "both")
step$anova

#Method - 1: start with highest correlation
cor(sc_score)
pairs(sc_score)

#highest correlation between dependent variable science and maths
sc_score.lm=lm(science~maths,data=sc_score)
summary(sc_score.lm)
