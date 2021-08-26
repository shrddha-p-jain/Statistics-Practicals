blocks=factor(rep(c('b1','b2','b3','b4'),times=6))
tr=factor(rep(c('t1','t2','t3','t4','t5','t6'),each=4))
yld=scan()
24.7 27.3 38.5 28.5 20.6 28.8 39.5 31 27.7 22.9 36.8 34.9 16.2 15 19.6 14.1 16.2 17 15.4 17.7 24.9 22.5 26.3 22.6
rbd=data.frame(yld,blocks,tr)
rbd
summary(rbd)
rbd.aov=aov(yld~blocks+tr,data=rbd)
summary(rbd.aov)
model.tables(rbd.aov,type='means',se=T)

TukeyHSD(rbd.aov,which='blocks',ordered = T)
plot(TukeyHSD(rbd.aov,which='blocks',ordered = T))

TukeyHSD(rbd.aov,which='tr',ordered = T)
plot(TukeyHSD(rbd.aov,which='tr',ordered = T))

op =par(mar=c(5,8,4,2))
#(set margin bottom, left, top and right)
plot(TukeyHSD(rbd.aov, ordered=T),cex.axis=0.7,las=1)
#(las=0,1,2,3 = labels parallel to the axis, horizontal,perpendicular,vertical) 
par(op)

interaction.plot(blocks,tr,yld)
interaction.plot(tr,blocks,yld)
interaction.plot(tr,blocks,yld, type='b',pch=4:1,lty=4:1,col=c('Red','Green','Blue','Yellow'),xlab='treatement', ylab='Yield',main = 'Interaction plot')
