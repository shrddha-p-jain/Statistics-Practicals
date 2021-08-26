#DEPENDENT ANOVA

#A RESEARCH PROGRAM WAS SET UP TO DEVELOP USER_FRIENDLY COMPUTER EQUIPMWNT 
#FOR PEOPLE WITH PHYSICAL DISABILITIES. 3 NEW THINGS OF COMPUTER KEYBOARD 
#FOR PEOPLE WITH DIFFICULTUES IN HAND MOMENTS WERE DEVELOP AND PROTOTYPES
#CREATED. THE RESEARCH TASK WAS TO DECIDE WHICH OF THESE PROTOTYPE WAS THE MOST
#SUCCESSFUL.4 POTENTIAL USERS OF THE NEW EQUIPMENT AGREED TO TAKE PART IN A TEST OF
#THE NEW KEYBOAORD.EACH PARTICIPANT WAS ASKED TO USE THE KEYBOARD TO INPUT A PIECE OF
#TEXT AND THE NUMBER OF ERROR WAS RECORDED.
key.data=c(5,6,10,1,2,3,0,4,5,2,4,6)
key.sub=c(rep(c('s1','s2','s3','s4'),each=3))
key.diff=c(rep(c('k1','k2','k3'),time=4))
keyboard=data.frame(key.data,key.sub,key.diff)
keyboard
summary(keyboard)
with(keyboard,table(key.diff,key.sub))#
with(keyboard,tapply(key.data,key.diff,sum))#total of keyboards(k1,k2,k3)
with(keyboard,tapply(key.data,key.diff,mean))#
#a=aov(key.data~key.sub+key.diff,data=keyboard) independent variable
#a=aov(key.data~key.sub*key.diff,data=keyboard) independent variable
key.aov=aov(key.data~key.diff+Error(key.sub/key.diff),data=keyboard)
summary(key.aov)
#h0:rejected
pairwise.t.test(x=key.data,g=key.diff,p.adjust.method = 'bonf',paired=T)#difference between keyboards
interaction.plot(key.sub,key.diff,key.data,pch=1:3,col=c('Red','Blue','Green'),lty=3:1)
interaction.plot(key.sub,key.diff,key.data)#normal one


#assume that a statistics professor is interested in the effects of taking a
#statistics course on performance on an algebra test. she administers a 20-items
#college algebra test to ten randomly selected statistics students at the begining of the term,at the
#end of the term,and six months after the course is finished.
begin=c(13,8,12,12,19,10,10,8,14,11)
end=c(15,8,15,17,20,15,13,12,15,16)
six_months=c(17,7,14,16,20,14,15,11,13,9)
times=factor(c(rep(c('begin','end','six_months'),each=10)))
times#factor
students=factor(c(rep(c('s1','s2','s3','s4','s5','s6','s7','s8','s9','s10'),time=3)))
students#factor
score=c(begin,end,six_months)
data=data.frame(students,score,times)
data
str(data)
with(data,table(times,students))
with(data,tapply(score,times,sum))
with(data,tapply(score,times,mean))
#ANOVA
data.aov=aov(score~times+Error(students/times),data=data)#dep var anova test
summary(data.aov)
#error(both var1/var2),data ,no other var 
#h0:all score same
#h1:not same has time factor
#h0:rejected. time factor affect the set 

#MULTIPLE COMPARISION
pairwise.t.test(x=score,g=times,p.adjust.method = 'bonf',paired = T)

#TWO FACTOR REPEATED
#IN A FACTORY MACHINE PRODUCES 2 KIND OF PRODUCT,ONE THAT REQUIRES THE OPEATOR TO FOLLOW
#A COMPLEX SET OF INSTRUCTIONS AND ONE THAT IS VERY SIMPLE TO MAKE.
#THERE ARE 2 SHIFTS IN THE FACTORY,A DAY AND A NIGHT SHIFT.
#THE FACTORY MANAGER WANTS THE FACTORY TO MAKE THE PRODUCTS WITH THE MINIMUM OF ERRORS.
# A RESEARCHERS DECIDES TO STUDY THE EFFECT OF SHIFT(DAY VERSUS NIGHT)AND PRODUCT
#(COMPLEX VERSUS SIMPLE TO MAKE)ON THE ERRORS MADE BY THE OPERATORS.
#ALL OPERATORS WORK BITH SIDES IN A ROTATION SYSTEM. 6 OPERATORS ARE RANDOMLY SELECTED AND 
#THEIR ERROR PERFORMANCE IS MEASURED DURING S DAY AND NIGHT SHIFT.
day_complex=c(5,5,7,6,4,6)
pm_complex=c(9,8,7,10,8,9)
day_simple=c(3,2,4,5,3,5)
pm_simple=c(2,4,5,4,3,6)
type=factor(c(rep(c('complex','simple'),each=12)))
type
shift=factor(c(rep(c('day','night'),each=6,time=2)))
shift
score=c(day_complex,pm_complex,day_simple,pm_simple)
score
operator=factor(c(rep(c('o1','o2','o3','o4','o5','o6'),time=2)))
operator
factory=data.frame(operator,shift,type,score)
factory
#h0:no diff btw shift#rejected
#h1:no diff btw type#rejected
#h2:no  diff(interactions)of type and shift#rejected
#can't apply t test
with(factory,table(shift,type))
with(factory,tapply(score,list(shift,type),mean))
#ANOVA
factory.aov=aov(score~type*shift+Error(operator/(type*shift)),data=factory)
factory.aov
summary(factory.aov)
