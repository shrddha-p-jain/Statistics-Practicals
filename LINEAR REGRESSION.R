#LINEAR REGRESSION 
#find linear regression between 2 varibles.
square_feet=c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)#X
house_price=c(245,312,279,308,199,219,405,324,319,255)#Y
data=data.frame(square_feet,house_price)
data
reg=lm(house_price~square_feet,data=data)
reg
summary(reg)
plot(house_price,square_feet)
cor.test(house_price,square_feet)

#MULTIPLE REGRESSION
studytime=c(40,43,18,10,25,33,27,17,30,47)
science=c(58,73,56,47,58,54,45,32,68,69)
intelligence=c(118,128,110,114,138,120,106,124,132,130)
maths=c(44,61,58,55,54,50,56,39,71,65)
attend=c(51,57,55,66,67,72,66,63,70,71)
literacy=c(30,21,42,27,49,23,50,21,38,17)
sc_score=data.frame(science,studytime,intelligence,maths,attend,literacy)
sc_score
summary(sc_score)

#FORWARD SELECTION(ADD)
#BACKWARD SELECTION(DROP)
#STEPWISE(BOTH)

#METHOD -1:START WITH NO PREDICTOR VARIABLE AND FORWARD SELECTION
sc_score.lm=lm(science~1,data=sc_score)#one intercept
summary(sc_score.lm)
add1(sc_score.lm,scope=sc_score,test='F')#start adding one by one variable
#check pr(>f) * 2 studytime and maths then AIC value minimum of maths.
sc_score.lm=lm(science~maths,data=sc_score)#insead of 1 put maths
sc_score.lm
add1(sc_score.lm,scope = sc_score,test = 'F')
sc_score.lm=lm(science~maths+studytime,data=sc_score)
sc_score.lm
add1(sc_score.lm,scope = sc_score,test='F')#ADD STUDYTIME 
#REMOVE EFFECT OF 2 VARIABLES

#METHOD -2:BACKWARD SELECTION
sc_score.lm=lm(science~.,data=sc_score)
sc_score.lm
formula(sc_score.lm)
summary(sc_score.lm)
drop1(sc_score.lm,test='F')#remove that variable which has lowest AIC
sc_score.lm=lm(science~studytime+intelligence+maths+attend,data=sc_score)
sc_score.lm
drop1(sc_score.lm,test = 'F')
sc_score.lm=lm(science~studytime+maths+attend,data=sc_score)
drop1(sc_score.lm,test = 'F')
sc_score.lm=lm(science~studytime+maths,data=sc_score)
drop1(sc_score.lm,test='F')
#REMOVE EFFECT OF 3 VARIABLES 
#Y=A+B(MATHS)+B(STUDYTIME)+T
summary(sc_score.lm)
#CHECKING OF BOTH THE MODELS
sc_score.lm1=lm(science~studytime+maths,data = sc_score)
sc_score.lm2=lm(science~.,data = sc_score)
anova(sc_score.lm1,sc_score.lm2)

