
#ANOVA

#1 sample independent ANOVA

#Q1. a research was interested int the effects of hints on a person's ability
# to solve anagrams. the time took a participant to solve 5 eight letter 
# anagrams was measures. the same 5 anagrams were used in 3 conditions:
# 1 letter(where the 1st letter of the word is given)
# last letter( where the last letter was given)
# no letter(where no help was given)
# 30 participants were chosen nd 10 were randomly allocated to each condition
# the no. of min it took to solve the 5 anagrams was recorded


fl=c(15,20,14,13,18,16,13,12,18,11)
ll=c(21,25,29,18,26,22,26,24,28,21)
nl=c(28,30,32,28,26,30,25,36,20,25)
#H0: mu1=mu2=mu3
#H1: atleast 2 methods r different
letter=data.frame(fl,ll,nl)
final=stack(letter)
final
names(final)=c('minutes','letter')
final

#checking homogeneity of variance
#h0: =var
#h1: !=var
bartlett.test(minutes~letter,data=final)
#welch test for equa variane not assume
oneway.test(minutes~letter,data=final)
#in case of unequal variance we use welch test
#since hypothesis is accepted v use aov
final.aov=aov(minutes~letter,data=final)
final.aov
summary(final.aov)
#H0 is rejected

#show pairwise comparison
#m1,m2 or m2,m3 or m3,m1 h0: no diff /h1: diff
TukeyHSD(final.aov)
plot(TukeyHSD(final.aov))
#only nl,ll h0 accepted bcuz it touches 0
model.tables(final.aov,type='means')

boxplot(minutes~letter,data=final)



#Q2. 4 salesmen were posted in diff areas by a company. the no. of commodity sold by
# them is given in the table.on the basis of this info, can it b concluded that
# there is a significant diff in the performance of the 4 sales persons?
p1=c(70,73,78,79)
p2=c(75,82,80,71)
p3=c(73,78,85,68)
p4=c(65,71,69,75)

#h0: there is no significant diff
#h1: there is significant diff
person=data.frame(p1,p2,p3,p4)
ps=stack(person)
ps
names(ps)=c('csold','person')
ps
bartlett.test(csold~person,data=ps)
#equal variance
# so v directy use aov instead of welch test
ps.aov=aov(csold~person,data=ps)
ps.aov
summary(ps.aov)

TukeyHSD(ps.aov)
plot(TukeyHSD(ps.aov))
#only nl,ll h0 accepted bcuz it touches 0
model.tables(ps.aov,type='means')

boxplot(csold~person,data=ps)
#h0 accepted

#Q3. in response to customer request, an electronics firm is developing a new dvd 
# player using a prototype,the marketing team has collected focus grp data. ANOVA 
# is being used to discover if consumers of various ages rated design diferently
dvdplayer=read.csv(file.choose())
dvdplayer
n1=c('under_25','25-34','35-44','45-54','55-64','65_and_over')
dvdplayer$agegroup=factor(dvdplayer$agegroup)
levels(dvdplayer$agegroup)=c(n1)
dvdplayer
attach(dvdplayer)
bartlett.test(dvdscore~agegroup,data=dvdplayer)
View(dvdplayer)
ex2.aov=aov(dvdscore~agegroup,data=dvdplayer)
summary(ex2.aov)
TukeyHSD(ex2.aov)
plot(TukeyHSD(ex2.aov))
plot(TukeyHSD(ex2.aov,ordered=T),cex.axis=0.5,las=1)
contrast1=c(0,0,-1,1,0,0)
contrast2=c(0.5,0.5,0,0,-0.5,-0.5)
cont.comb=cbind(contrast1,contrast2)
contrasts(dvdplayer$agegroup)=cont.comb
ex2.aov=aov(dvdscore~agegroup,data=dvdplayer)
summary(ex2.aov)
summary.aov(ex2.aov,split=list(agegroup=list('3 & 4'=1, '1 & 2 vs 5 & 6'=2)))

#Q4 folowing are the speed of cars on 4 different types off road conditions. 
# Using analysis of variance table, test if the road condition differ with
# accept to average speed
# type1 vs type 4
# combine type 2 and type 3 and compare with type 1
d=c(77,70,63,84,96,81,88,75,92,82,94,90,73,71,91,93,86,85,79,95,46,54,60,70,74,49,58,80,69,76,81,83,89,65,72)
type=c(rep(c('type1'),each=11),rep(c('type2'),each=9),rep(c('type3'),each=8),rep(c('type4'),each=7))
exercise=data.frame(type,d)
exercise
attach(exercise)
bartlett.test(d~type,data=exercise)
ex4.aov=aov(d~type,data=exercise)
summary(ex4.aov)
TukeyHSD(ex4.aov)
plot(TukeyHSD(ex4.aov))
plot(TukeyHSD(ex4.aov, oredred=T),cex.axis=0.5,las=1)
contrast1=c(1,0,0,-1)
contrast2=c(-1,0.5,0.5,0)
cont.comb=cbind(contrast1,contrast2)
contrasts(d~type,data=exercise)=cont.comb
summary.aov(ex4.aov,split=list(type=list('')))