users=c(1,2,3,4,5,6,7,8,9,10)
BL=c(2780,2800,2300,3100,2750,2600,2895,3175,2500,2400)
C=data.frame(users,BL)
C
smean=sum(BL)/10
sd(BL)
tc=((3000-smean)*sqrt(9))/sd(BL)
tc




#Q1
#H0:normal
#H1:not normal
users=c(1:10)
bl=c(2780,2800,2300,3100,2750,2600,2895,3175,2500,2400)
bulb=data.frame(users,bl)
attach(bulb)
summary(bl)
#normality
hist(bl,col='red')
qqnorm(bl)
qqline(bl,col=2)
#bulb_z=(bl-mean)
#hist(bulb_z,col='red')
#ks.test(bl,"pnorm")
shapiro.test(bl)

#randomness check
library(randtests)
runs.test(bl)

#t test
t.test(bl,mu=3000)
#H0 acepted



#Q2. the height of 10 males
# 63,66,63,67,68,69,70,70,71,72
# from this data test the hypothesis that the avg height
# is 66

#H0:mu=66
#H1:mu != 66
height=c(63,66,63,67,68,69,70,70,71,72)

smean=sum(height)/10
sd(height)
tc=((66-smean)*sqrt(9))/sd(height)
tc
t.test(height,mu=66)

summary(height)
#normality
hist(height,col='red')
qqnorm(height)
qqline(height,col=2)
#h=(height-mean)
#hist(h,col='red')
#ks.test(h,"pnorm")
shapiro.test(height)

#randomness check
library(randtests)
runs.test(height)

#t test
t.test(height,mu=66)
#H0 accepted

#Q3. 2 independent grps of 10 children each were tested to find out
# how many digits they coud repeat from memory after hearing them
# the results are:
# is the diff. btween the mean score o the 2 grps significant
# grp A: 9,6,5,7,6,8,7,4,5,6
# grp B: 10,6,7,8,6,9,7,6,7,7

#H0: there is no signifint differene between 2 grps
#H1: there is signifint differene between 2 grps
#H0: mu1=mu2
#H1: mu1!=mu2
n=c(1:10)
a=c(9,6,5,7,6,8,7,4,5,6)
b=c(10,6,7,8,6,9,7,6,7,7)
nab=data.frame(a,b)
nab
shapiro.test(a)
shapiro.test(b)
#homogenity test
#H0:equal variance
#H1: variance not equal
var.test(a,b)

t.test(a,b)
#H0 acepted



#Q4. the eg. to know whether the salary of male faculty members in
# private institutions is equal to salary of female faculty members
# researcher colected data from 20 males & 20 female faculty members
# to know the diference

#H0:mu1=mu2
#H1:mu1>mu2
m=c(45,46,54,57,0,64,58,55,49,54,55,58,47,53,47,36,39,52,34,35,38,29,34,42,52,36,39,42,41,37,29,46,47,38,34,51,35,54,36,28)
gender=c(rep(c(,'male','female'),each=20))
m
gender
z=data.frame(gender,m)
z
mean(z$m[gender=='male'])
mean(z$m[gender=='female'])
shapiro.test(z$m[z$gender=='male'])
shapiro.test(z$m[z$geder=='female'])
t.test()


#paired t test
#Q5.a certain medicine was given to each of 5 patients, test whether there
# is any chnge in which after the med at 5% level of significance
# result=> b4=42,39,48,60,41
#       after=38,42,48,67,40

#H0:no efect of med
#H1:there is significant effect of med

before=c(42,39,48,60,41)
after=c(38,42,48,67,40)
z=data.frame(before,after)
z
shapiro.test(after)
shapiro.test(before)
v=t.test(before,after,paired=T)
v
names(v)
v$p.value
v$conf.int


