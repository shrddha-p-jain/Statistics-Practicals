#Q1
#H0:NORMAL
#H1:NOT NORMAL
users=c(1:10)
bl=c(2780,2800,2300,3100,2750,2600,2895,3175,2500,2400)
blub=data.frame(users,bl)
attach(blub)
summary(blub)
#NORMALITY
hist(bl,col='red')
qqnorm(bl)
qqline(bl,col=2)
#bulb_z=(bl-mean)
#hist(bulb_z,col="red")
#ks.test(bl,"pnorm")
shapiro.test(bl)

#RANDOMNESS CHECK
library(randtests)
runs.test(bl)

#T TEST
t.test(bl,mu=3000)
#h0 accepted

#Q2 the heigth of 10 males from this data test te hypothesis that the avg heigth
#is 66 
#H0:Mu=66
#H1:Mu !=66
male=c(1:10)
fr=c(63,66,63,67,68,69,70,70,71,72)
smean=sum(fr)/10
smean
sd(fr)
tc=((66-smean)*sqrt(9))/sd(fr)
tc
t.test(fr,mu=66)
z=data.frame(male,fr)
attach(z)
summary(z)
hist(fr,col="blue")
qqnorm(fr)
qqline(fr,col=2)
#bulb_z=(bl-mean)
#hist(bulb_z,col="red")
#ks.test(bl,"pnorm")
shapiro.test(fr)
#RANDOMNESS CHECK
library(randtests)
runs.test(fr)
#T TEST
t.test(fr,mu=66)
#h0 accepted

#Q3 2 INDP GRP OF 10 CHILDREN.EACH WERE TESTED TO FIND OUT HOW MANY DIGIGTS THEY
# COULD REPEAT FROM MEMORY ATER HEARING THEM.
#IS THE DIFF B/T THE MEAN SCORE OF THE 2 GRP SIGNIFICANT?
#h0:there is not sig diff b/w 2 grp
#h1: there is sig diff b/w 2 grp
a1=c(1:10)
a=c(9,6,5,7,6,8,7,4,5,6)
b1=c(1:10)
b=c(10,6,7,8,6,9,7,6,7,7)
shapiro.test(a)
shapiro.test(b)
#HOMOGENITY TEST
var.test(a,b)
#H0:EQUAL VAR
#H1:NOT EQUAL VAR
#T-TEST
t.test(a,b)
#h0 accepted

#Q4 salary of male female faculty in private inst is equal or not? 
#H0: SALARY IS EQUAL
#H1: SALARY IS  NOT EQUAL
x=c(1:20)
male=c(45,46,54,57,60,64,58,55,49,54,55,58,47,53,47,36,39,52,34,35)
female=c(38,29,34,42,52,36,39,42,41,37,29,46,47,38,34,51,35,54,36,28)
gender=factor(c(rep(c('male','female'),each=20)))
b=c(male,female)
z=data.frame(gender,b)
z
shapiro.test(male)
shapiro.test(female)


mean(z$male[gender=="male"])
shapiro.test(z$male[gender=="male"])
mean(z$female[gender=="female"])
shapiro.test(z$female[gender=="female"])

#h0 rejected

#PAIRED T TEST dep
# a certain medicine was given to each of 5 pat. test wether there is any 
#change in which after the medicine at 5% level of sig. results 
x=c(1:5)
before=c(42,39,48,60,41)
after=c(38,42,48,67,40)
#h0:no sig effect on median
#h1:sig effect on med
z=data.frame(before,after)
z
shapiro.test(before)
shapiro.test(after)
v1=t.test(before,after,paired =T)
v1
v2=var.test(before,after,paired =T)
v2
#h0 accepted
names(v1)
v1$p.value
v1$conf.int


#t test 30 if only2 sample. else anova
