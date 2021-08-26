#2 WAY ANOVA
#AN EXPANDING COMPANY WANTED TO KNOW HOW TO INTRODUCE A NEW TYPE OF MACHINE INTO THE FACTORY.
#SHOULD IT TRANSFER STAFF WORKING ON THE OLD MACHINE TO OPEARTE IF OR EMPLOY NEW STAFF WHO HAD NOT WORKED ON ANY MACHINE BEFORE?
 # A RESEARCHER SELECTED 12 STAFF WHO HAD EXPERIENCE OF THE OLD MACHINE AND 12 STAFF WHO HAD NO SUCH EXPERIENCE. HALF 
#THE PARTICIOANTS FROM EACH GROUP WERE ALLOCATED TO THE NEW Mchine and half to ther old machine.
#the number of errors made by the praticipamnts over a set period was measured.

#H01:PERSOM:THERE IS NO SIGNIFICSNCE DIFFERENCE BETWEEN NEW AND EXPERIENCED PERSON.
#H02:MACHINE:THERE IS NO SIGNIFICSNCE DIFFERENCE BETWEEN OLD AND NEW MACHINE.
#H03:INTERACTIOPN:THERE IS NO SIGNIFICSNCE DIFFERENCE BETWEEN PERSON AND MACHINE.

person=rep(rep(c('newly','experience'),each=6),time=2)
person
machine=rep(c('old','new'),each=12)
errors=c(4,5,7,6,8,5,1,2,2,3,2,3,5,6,5,6,5,6,8,9,8,8,7,9)
data1=data.frame(person,machine,errors)
data1
str(data1)
data1.aov=aov(errors~person+machine,data=data1)                                                                                                      
summary(data1.aov)
#h01:accepted (no diff)
#h02:rejected (diff)
data2.aov=aov(errors~person*machine,data=data1)#3rd                                                                                                       
summary(data2.aov)
#h03:rejected (diff)
model.tables(data2.aov,type="means")
plot.design(data1)#single
#interaction
interaction.plot(person,machine,errors)
TukeyHSD(data2.aov)
TukeyHSD(data2.aov,ordered=T)
plot(TukeyHSD(data2.aov))
op=par(mar=c(5,8,4,2))
op
plot(TukeyHSD(data2.aov,ordered=T),cex.axis=1,las=0)
plot(TukeyHSD(data2.aov,ordered=T),cex.axis=1,las=1)
plot(TukeyHSD(data2.aov,ordered=T),cex.axis=1,las=2)
plot(TukeyHSD(data2.aov,ordered=T),cex.axis=1,las=3)


#O2:ASSUME THAT YOU ARE STUDING THE EFFECTS OF OBSERVING VIOLENT ACYS ON SUBSEQUENT AGGRESSSIVE BEHAVIOUR.
#YPU ARE INTERESTED IN THE KIND OF VIOLENCE OBSERVED 1.VIOLENCE CARTOON V/S VIDEO OF REAL ACTION.
#SECOND FACTOR IS THE AMT OF TIME ONE IS EXPOSED TO VIOLENCE 10 OR 30 MINS.
#U RANDOMLY ASSIGN 8 CHILDREN TO EACH GRP.AFTER THE CHILD WATCHES THE VIOLENT CARTOON OR ACTION VIDEO,THE CHILD PLAYS A TETRIS LIKE COMPUTER VIDEO FOR 30 MINS.
#THE GAME PROVIDES OPTIONS FOR POINTS WITHOUT INTERFEARING WITH THE OTHER PLAYER.THE PROGRSM PROVIDES 100 OPPORTUNITIES FOR THE PLAYER TO MAKE AN AGGRESSIVE CHOICE
#AND RECIRDS THE NO. OF TIME THE CHILD CHOOSE AN AGGRESSIVE WHEN THE GAME PROVIDES THE CHOICE.

#H01:PERSOM:THERE IS NO SIGNIFICSNCE DIFFERENCE BETWEEN NEW AND EXPERIENCED PERSON.
#H02:MACHINE:THERE IS NO SIGNIFICSNCE DIFFERENCE BETWEEN OLD AND NEW MACHINE.
#H03:INTERACTIOPN:THERE IS NO SIGNIFICSNCE DIFFERENCE BETWEEN PERSON AND MACHINE.

time=rep(rep(c('10 mins','30 mins'),each=8),times=2)
e=c(47,56,48,51,46,44,50,51,67,69,65,62,67,69,59,72,52,62,57,49,64,39,50,48,
    81,92,82,92,82,94,86,83)
kind=rep(c('cartoon','real action'),each=16)
kov=data.frame(time,kind,e)
kov#kind of varience
str(kov)
kov.aov=aov(e~kind+time,data=kov)
summary(kov.aov)
kov.aov1=aov(e~kind*time,data=kov)
summary(kov.aov1)
model.tables(kov.aov1,type="means")
plot.design(kov)#single
#interaction
interaction.plot(time,kind,e)
TukeyHSD(kov.aov1)
TukeyHSD(kov.aov1,ordered=T)
plot(TukeyHSD(kov.aov1))
op=par(mar=c(5,8,4,2))
op
plot(TukeyHSD(kov.aov1,ordered=T),cex.axis=1,las=0)
plot(TukeyHSD(kov.aov1,ordered=T),cex.axis=1,las=1)
plot(TukeyHSD(kov.aov1,ordered=T),cex.axis=1,las=2)
plot(TukeyHSD(kov.aov1,ordered=T),cex.axis=1,las=3)








