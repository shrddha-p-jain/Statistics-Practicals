claydata=c(29.1,16.4,5.4,24.9,18.9,10.2,38.8,41.7,29.4,21.2,24,9.5,5.7,19.1,37,28.9)
clayrow=factor(rep(c('1','2','3','4'),times=4))
claycol=factor(rep(c('1','2','3','4'),each=4))
treat=factor(c('D','C','A','B','B','A','D','C','C','D','B','A','A','B','C','D'))
clay=data.frame(claydata,clayrow,claycol,treat)
clay
summary(clay)

#checking balance design
with(clay,table(clayrow,claycol))
#here is LSD

matrix(clay$treat,nrow=4)
model.tables(clay.aov,type='means',se=T)

clay.aov=aov(claydata~clayrow+claycol+treat,data=clay)
summary(clay.aov)

TukeyHSD(clay.aov,ordered = T)
op =par(mar=c(5,8,4,2))
plot(TukeyHSD(clay.aov, ordered=T),cex.axis=0.7,las=1)
par(op)

