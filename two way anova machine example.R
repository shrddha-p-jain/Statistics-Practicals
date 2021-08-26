persons=factor(c(rep(c("New","Exp"),each=12)))
machine=factor(c(rep(c("Old","New"),each=6,times=2)))
errors=scan()
4 5 7 6 8 5 5 6 5 6 5 6 1 2 2 3 2 3 8 9 8 8 7 9 
machine.ex=data.frame(persons,machine,errors)
summary(machine.ex)
machine.aov=aov(errors~persons+machine,machine.ex)
machine.aov
machine.aov=aov(errors~persons*machine,machine.ex)
machine.aov=aov(errors~persons+machine+persons:machine,machine.ex)
machine.aov=aov(errors~persons+machine,machine.ex)
machine.aov
summary(machine.aov)

model.tables(machine.aov,type = "means")
plot.design(machine.ex)
interaction.plot(persons,machine,errors)

TukeyHSD(machine.aov)
TukeyHSD(machine.aov,ordered =T )

plot(TukeyHSD(machine.aov))

op =par(mar=c(5,8,4,2))
#(set margin bottom, left, top and right)
plot(TukeyHSD(machine.aov, ordered=T),cex.axis=1,las=1)
#(las=0,1,2,3 = labels parallel to the axis, horizontal,perpendicular,vertical) 

abline(v=0, lty=2,col='Red')
par(op)

boxplot(errors~persons*machine,machine.ex)
title(xlab='Interaction',ylab='Errors')


