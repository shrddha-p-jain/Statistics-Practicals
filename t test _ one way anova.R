#One sample t-test
users=c(1:10)
bulblife=c(2789,2800,2300,3100,2750,2600,2895,3175,2500,2400)
bulb=data.frame(users,bulblife)
bulb
attach(bulb)
summary(bulblife)
#Normality 
hist(bulblife,col = "Red")
qqnorm(bulblife)
qqline(bulblife,col=2)
#bulb_z=(bulblife-mean(bulblife))/sd(bulblife)
#hist(bulb_z, col="Blue")
#ks.test(bulb_z,"pnorm")
shapiro.test(bulblife)
 #randomness check
library(randtests)
runs.test(bulblife)
#t test
t.test(bulblife,mu=3000)

#Two independent samples t-test- salary data
#Example-1:
night_errors=c(9,10,8,9,9,7)
day_errors=c(8,7,5,6,5,4,5,6)
mean(night_errors)
mean(day_errors)

#1. Normality test 
shapiro.test(night_errors)
shapiro.test(day_errors)

#2.two populations have same 
#variance i. e. Homogeneity test
var.test(night_errors,day_errors)

#t-test
errors.t=t.test(night_errors,day_errors,var.equal = TRUE)
errors.t
names(errors.t)
errors.t$estimate
errors.t$conf.int

#2. Independency
#two samples independents
#since the samples from 
#different drivers,so data
#are nt related

#Normality test for two samples
#ks.test(night_errors,day_errors)
#qqplot(night_errors,day_errors)

#Example-4:
gender=factor(rep(c("male","female"),each=20))
salary=scan()
45 46 54 57 60 64 58 55 49 54 55 58 47 53 47 36 39 52 34 35 38 29 
34 42 52 36 39 42 41 37 29 46 47 38 34 51 35 54 36 28
salary_data=data.frame(gender,salary)
mean(salary_data$salary[salary_data$gender=="male"])
mean(salary_data$salary[salary_data$gender=="female"])
sd(salary_data$salary[salary_data$gender=="male"])
sd(salary_data$salary[salary_data$gender=="female"])
summary(salary_data)

attach(salary_data)
cor(salary[gender=="male"],salary[gender=="female"])
ks.test(salary[gender=="male"],salary[gender=="female"])
qqplot(salary[gender=="male"],salary[gender=="female"])

var.test(salary[gender=="male"],salary[gender=="female"])

salary.t=t.test(salary~gender,data = salary_data,paired = FALSE,var.equal = TRUE)
salary.t
names(salary.t)
salary.t$conf.int

#Paired or dependent t-test:
#Example-1:
am=c(6,4,3,5,7,6,5,6)
pm=c(5,2,4,4,3,4,5,3)
test_score=data.frame(am,pm)

#correaltion
cor(test_score)
test_pair=t.test(am,pm,paired = TRUE)
test_pair
names(test_pair)

#Example-2:
#ABC university arrange special FDP for one segment of its
#teachers. The university wants to measure the change in the
#attitude of its teachers after the FDP. For this purpose
#it has used a well-designed questionnaire, which consists of
#10 questions on a 1 to 5 rating scale (1 = SD, 5 = SA).
#The university selected a random sample of 20 teachers.
before_fdp=scan()
22 26 28 24 26 27 29 21 26 28 25 26 28 22 20 30 22 20 21 24

after_fdp=scan()
35 36 36 37 38 34 39 40 38 36 32 30 32 34 32 28 25 30 25 28

fdp_score=data.frame(before_fdp,after_fdp)
cor(fdp_score)

fdp_pair=t.test(before_fdp,after_fdp,paired = TRUE)
fdp_pair

#Example-3:
dietx=c(25,32,30,32,24,14,32)
diety=c(24,34,22,30,42,31,40,30,32,35)

#one way anova
first_letter = scan()
15 20 14 13 18 16 13 12 18 11

last_letter = scan()
21 25 29 18 26 22 26 24 28 21

no_letter = scan()
28 30 32 28 26 30 25 36 20 25

ex1 = data.frame (first_letter, last_letter, no_letter)

ex1 #show structure

final.ex1 = stack(ex1)#arrange data in one column

names(final.ex1) = c("minutes", "letter.types")#give column title

final.ex1

#checking homogeneity of variance
bartlett.test(minutes ~ letter.types, data = final.ex1)

#Welch test for equal variance not assume
#oneway.test (minutes ~ letter.types, data = final.ex1)

ex1.aov = aov(minutes ~ letter.types, data = final.ex1) 
ex1.aov

summary (ex1.aov)

#show pairwise comparison
TukeyHSD(ex1.aov)
plot(TukeyHSD(ex1.aov))
model.tables (ex1.aov, type = "means")
boxplot(minutes ~ letter.types, data = final.ex1)
title(xlab = "letter type", ylab = "minutes")

