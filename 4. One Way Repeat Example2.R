key.data = c(5, 6, 10, 1, 2, 3, 0, 4, 5, 2, 4, 6)
key.diff = factor(rep(c("k1","k2","k3"),times=4))
key.sub = factor(rep(c("s1","s2","s3","s4"),each=3))
keyboard = data.frame(key.data, key.sub, key.diff)
keyboard
summary(keyboard)

#check balance design
with(keyboard,table(key.diff,key.sub))                  
with(keyboard, tapply(key.data,key.diff, sum))
with(keyboard, tapply(key.data,key.diff, mean))
with(keyboard, tapply(key.data,key.diff, sd))

#ANOVA
key.aov=aov(key.data ~ key.diff+Error(key.sub/key.diff), data=keyboard)
summary(key.aov)

model.tables(key.aov,type = "means")

pairwise.t.test(x=key.data, g=key.diff, p.adjust.method='bonf',paired=T)
interaction.plot(key.sub,key.diff,key.data,pch=1:3,col=c('Red','Blue','Green'),lty=3:1)

#Two way ANOVA
key.two = aov(key.data ~ key.diff+key.sub, data = keyboard)
summary(key.two)
TukeyHSD(key.two,which = 'key.diff',ordered = T)

#Multiple comparision by Choice
cons = cbind(c(-1,0,1), c(1,1,-2))
cons
contrasts(keyboard$key.diff) = cons
aov1.out = aov(key.data ~ key.diff+Error(key.sub/key.diff), data=keyboard)
summary(aov1.out)
summary(aov1.out, split=list(key.diff=list("Key1 vs key3"=1,"key1&2 vs key3"=2)))

