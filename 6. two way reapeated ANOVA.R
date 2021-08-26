errors=scan()
5 5 7 6 4 6 9 8 7 10 8 9 3 2 4 5 3 5 2 4 5 4 3 6
optr=factor(c(rep(c('op1','op2','op3','op4','o5','o6'),4)))
shift=factor(rep(c(rep(c('day','night'),each=6)),times=2))
instn=factor(rep(rep(c('comp','simple'),each=12)))
factory=data.frame(optr,shift,instn,errors)
factory
summary(factory)

#check for balance design
with(factory,table(shift,instn))
#each subject appears once in each of the treatment cells

with(factory, tapply(errors, list(shift,instn), mean)) 

#ANOVA
factory.aov=aov(errors~instn*shift+Error(optr/(instn*shift)),data=factory)
summary(factory.aov)
interaction.plot(instn,shift,errors,pch=2:1,col=c('Red','Blue'))
