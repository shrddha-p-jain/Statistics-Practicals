height=c(9,11,6,14,17,19,28,31,32,7,6,5,14,17,15,44,38,37)
plant=factor(c(rep("vulgaris",9),rep("sativa",9)))
water=factor(c(rep(c('low','med','high'),each = 3,times=2)))
pw=data.frame(height,plant,water)
pw
summary(pw)
pw.aov=aov(height~plant+water, data=pw)
summary(pw.aov)
pw.aov=aov(height~plant*water,data=pw)
summary(pw.aov)
pw.aov=aov(height~plant+water+plant:water,data=pw)
summary(pw.aov)

model.tables(pw.aov,type = "means")
plot.design(pw)
interaction.plot(water,plant,height)
interaction.plot(plant,water,height, type='b',pch=3:1,lty=3:1,col=c('Red','Green','Blue'),xlab='Plant treatement', ylab='Mean plant height cm',main = 'Interaction plot')

TukeyHSD(pw.aov)
TukeyHSD(pw.aov, which='water')
TukeyHSD(pw.aov, which=c('plant','water'))
TukeyHSD(pw.aov, which='plant:water')
TukeyHSD(pw.aov, which='plant:water',ordered=TRUE)
TukeyHSD(pw.aov, which='plant:water',conf.level=0.99)

plot.design(pw)
plot(TukeyHSD(pw.aov))
op =par(mar=c(5,8,4,2))
#(set margin bottom, left, top and right)
plot(TukeyHSD(pw.aov, ordered=T),cex.axis=0.7,las=1)
#(las=0,1,2,3 = labels parallel to the axis, horizontal,perpendicular,vertical) 
abline(v=0, lty=2,col='Red')
par(op)

boxplot(height~plant*water, data=pw, cex.axis=0.9)
title(xlab='Interaction',ylab='Height')

pw.mt=model.tables(pw.aov,type='means',se=T)
model.tables(pw.aov,type='means',cterms=c('plant:water'))
pw.mt
names(pw.mt)
names(pw.mt$tables)
pw.mt$tables$'plant:water'
attach(pw)
tapply(height,list(plant,water),FUN=mean)
detach(pw)

