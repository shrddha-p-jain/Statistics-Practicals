test=c(13,15,17,8,8,7,12,15,14,12,17,16,19,20,20,10,15,14,10,13,15,8,12,11,14,15,13,11,16,9)
stud=factor(rep(c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10"),each=3))
time=c(rep(c('Begin','End','Six months'),10))
algebra=data.frame(stud,test,time)
algebra
summary(algebra)
str(algebra)

#check balance design
with(algebra,table(stud,time))                  

with(algebra, tapply(test,time, sum))
with(algebra, tapply(test,time, mean))

#ANOVA
algebra.aov=aov(test~time+Error(stud/time),data=algebra)
summary(algebra.aov)

model.tables(algebra.aov,type = "means")

#Multiple Comparison
pairwise.t.test(x=test, g=time,p.adjust.method='bonf',paired=T)
interaction.plot(stud,time,test,pch=1:3,col=c('Red','Blue','Green'),lty=3:1)
