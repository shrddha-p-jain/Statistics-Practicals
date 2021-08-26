# Simple logistic regression for each variable
bin.log=read.csv(file.choose(),header = T)
bin.bubinhead(bin.log)
str(bin.log)
bin.log$Loyalty=as.factor(bin.log$Loyalty)
str(bin.log)
levels(bin.log$Loyalty)=c("Disloyal","Loyal")
#Logistic Regression Model
bin.model=glm(Loyalty~Brand+Income+Shopping,data = bin.log,family = binomial)
summary(bin.model)

#Prediction
p1=predict(bin.model,bin.log,type = 'response')
head(p1)
head(bin.log)
bin.log


#y=-19.66944+(0.05964*Brand)+(0.22941*Income)+(0.05472*Shopping)
#y=-19.66944+(0.05964*55)+(0.22941*75)+(0.05472*58)
#find probability for first row
#exp(y)/(1+exp(y)) for loyality

#Misclassification error - train data
p1
pred1=ifelse(p1>0.5,"Loyal","Disloyal")
View(pred1)
pred1
tab1=table(Predicted=pred1,Actual=bin.log$Loyalty)
tab1

1-sum((diag(tab1))/sum(tab1))
#Misclassification is 15% 

check=data.frame(Pridicted=pred1,Actual=bin.log$Loyalty)
check

#Goodness of fit test
names(bin.model)
null.deviance=bin.model$null.deviance
deviance=bin.model$deviance
df.null=bin.model$df.null
df.residual=bin.model$df.residual
with(bin.log,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))

#Model Correction
bin.model=glm(Loyalty~Income,data = bin.log,family = binomial)
summary(bin.model)

#Prediction
p1=predict(bin.model,bin.log,type = 'response')
head(p1)
head(bin.log)
bin.log
p1

#y=-10.45788+(0.1906*Income)
#y=-10.45788+(0.1906*75)
#find probability for first row
#exp(y)/(1+exp(y)) for loyality

#Misclassification error - train data
p1
pred1=ifelse(p1>0.5,"Loyal","Disloyal")
tab1=table(Predicted=pred1,Actual=bin.log$Loyalty)
tab1

1-sum((diag(tab1))/sum(tab1))
#Misclassification is 10% 

check=data.frame(Pridicted=pred1,Actual=bin.log$Loyalty)
check

#Model Correction
bin.model=glm(Loyalty~Brand+Shopping,data = bin.log,family = binomial)
summary(bin.model)

#Prediction
p1=predict(bin.model,bin.log,type = 'response')
head(p1)
head(bin.log)
bin.log
p1

#y=-6.89399+(0.06983*Brand+0.04606*Shopping)
#y=-6.89399+(0.06983*55)+(0.04606*58)
#find probability for first row
#exp(y)/(1+exp(y)) for loyality

#Misclassification error - train data
p1
pred1=ifelse(p1>0.5,"Loyal","Disloyal")
tab1=table(Predicted=pred1,Actual=bin.log$Loyalty)
tab1

1-sum((diag(tab1))/sum(tab1))
#Misclassification is 25% 

check=data.frame(Pridicted=pred1,Actual=bin.log$Loyalty)
check

