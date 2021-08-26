#ANOVA (MORE THAN 2 VARIENCE) #ONE FACTOR INDEPENDENT ANOVA
# Q1 #ANAGRAM TO BE SOLVED (JUMBLED WORD)TIME TO SOLVE 5-8WORD ANAGRAM IS CALCULATED.
#SAME 5 ANAGRAM IS USED IN 3 conditioNS.
#1st.letter 1st letter of word was given.
#last letter(last letter given).
#no letter(no help).
#30 participants were chosen 10 allocate to each condition. no. of min took to solve 
#5ANAGRAM was recorded. 

#h0:mu1=mu2=mu3
#h1:atleast two methods are different
fl=c(15,20,14,13,18,16,13,12,18,11)
ll=c(21,25,29,18,26,22,26,24,28,21)
nl=c(28,30,32,28,26,30,25,36,20,25)
#STACK#before applying anova merge all 3 in single column.
letter=data.frame(fl,ll,nl)
combine=stack(letter)
combine
names(combine)=c("minute","letters")
combine
#labels for combine
#CHECKING HOMOGENITY OF VARIENCE
#h0: =var
#h1: !=var
bartlett.test(minute~letters,data=combine)

#WELCH TEST FOR EQUAL VARIENE NOT ASSUME
#oneway.test(minute~letters,data=combine)#when h0 is ture(accepted)

#h0 is not true(rejected)
#in case of unequal varience we use welch test
combine.aov=aov(minute~letters,data=combine)
combine.aov
summary(combine.aov)#ANOVA TABLE
#H0 REJECTED ATLEAST TWO METHODS ARE DIFFERENT.

#SHOW PAIRWISE COMPARISON
#m1,m2 or m2,m3 or m1,m3 h0:no diff ,h1:diff
TukeyHSD(combine.aov)
plot(TukeyHSD(combine.aov))
#ll-fl,h0:reject
#fl-nl,h0:reject
#nl-ll,h0:accepted
model.tables(combine.aov,type="means")
boxplot(minute~letters,data=combine)
title(xlab="letters",ylab="minute")

#Q2 # 4 salesman were posted in diff areas by company.no. of 
#comodity sold by them is given in table.
#on basis of this info can it b concluded that there is a 
#significance diff in the performance of 4 salesman.
p1=c(70,73,78,79)
p2=c(75,82,80,71)
p3=c(73,78,85,68)
p4=c(65,71,69,75)
#h0:no sig diff
#h1:atleast 2 has them
person=data.frame(p1,p2,p3,p4)
person
ps=stack(person)
ps
names(ps)=c("sold","person")
ps
bartlett.test(sold~person,data=ps)
#accepted varience is equal
ps.aov=aov(sold~person,data=ps)
ps.aov
summary(ps.aov)
TukeyHSD(ps.aov)
plot(TukeyHSD(ps.aov))
model.tables(ps.aov,type="means")
boxplot(sold~person,data=ps)
#h0:accepted there is no sig diff no change ifwe change the working area.

