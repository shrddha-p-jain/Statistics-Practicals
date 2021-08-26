wt=scan()
55 49 42 21 52 61 112 30 89 63 42 97 81 95 92 169 137 169 85 154
feed=factor(rep(c('A','B','C','D'),each=5))
crd=data.frame(wt,feed)
crd
summary(crd)

#checking balance design
with(crd,table(feed))

matrix(crd$feed,nrow=4)

crd.aov=aov(wt~feed,data=crd)
summary(crd.aov)

model.tables(crd.aov,type='means',se=T)
TukeyHSD(crd.aov,ordered = T)
op =par(mar=c(5,8,4,2))
plot(TukeyHSD(crd.aov, ordered=T),cex.axis=0.7,las=1)
par(op)

