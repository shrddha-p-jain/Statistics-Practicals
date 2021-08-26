#CRD [COMPLETELY RANDOMIZED DESIGN]
#a set of data involving 4 tropical feed stuff A,B,C,D tried on 20 chicks.
#all the 20 chicks are treated alike in all respects ecxept the feeding treatments 
#and each feeding treatment is given 5 chicks. analyse the data.
#h0:there is no significant diff
#h1:atleast 2 treatment are different.
feed=factor(c(rep(c('A','B','C','D'),each=5)))
c1=c(55,49,42,21,52,61,112,30,89,63,42,97,81,95,92,169,137,169,85,154)
data3=data.frame(feed,c1)
data3
summary(data3)
data3.aov=aov(c1~feed,data=data3)
data3.aov
summary(data3.aov)
#rejected h0. atleast 2 treatment are different.
#MULTIPLE COMPARISION 
#TUKEY
tukey=TukeyHSD(data3.aov,ordered=T)
tukey
plot(tukey)
#D is comomn that means d is different.
model.tables(data3.aov,type='means')
#conclusion1= treatment D significantly different from A,B,C.
#conclusion2=all the remaining difference are not significant.

#RBD [RANDOMIZED BLOCKED DESIGN]
#consider the result given in the following table for an experiment involving 6 treatments.
#in 4 randomized blockes. the treatments are indicated by numbers within parenthesis.
#test whether the treatments differ significantly also.
#1.determine the critical difference between means of any 2 treatments if possible.
#h0:
#h1:
blocks=factor(c(rep(c('1','2','3','4'),each=6)))
yield=factor(c(rep(c('1','2','3','4','5','6'),times=4)))
obs=c(24.7,20.6,27.7,16.2,16.2,24.9,27.3,28.8,22.7,15.0,17.0,22.5,38.5,39.5,36.8,19.6,15.4,26.3,28.5,31.0,34.9,14.1,17.7,22.6)
rbd=data.frame(blocks,yield,obs)
rbd
rbd.aov=aov(obs~blocks+yield,data = rbd)
summary(rbd.aov)
#reject both
tukey=TukeyHSD(rbd.aov,ordered=F)
tukey
plot(tukey,cex.axis=0.5,las=1)
#pairs 4th and 5th treatment with others
model.tables(rbd.aov,type='means')
tukey.blocks=TukeyHSD(rbd.aov,which='blocks')
plot(tukey.blocks,cex.axis=0.7,las=1)
#3rd block

#LSD [LATIN SQUARE DESIGN]
#an experiment was carried out to determine the effect of claying the ground on 
#the field of barley graines. ammount of clay used were as follows.
#A=no clay. B=clay at 100 per acre. C=clay at 200 per acre. D=clay at 300 per acre.
#the yields were in plots of 8X8 meters and aee given in the following table.
#analyse the data.
amt=factor


