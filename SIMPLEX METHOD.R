library(boot)

#Ex1. JK Sharma (Example 4.1)
obj1=c(3,5,4)
eq1=c(2,3,0)
eq2=c(0,2,5)
eq3=c(3,2,4)
con=rbind(eq1,eq2,eq3)
con
rhs=c(8,10,15)
simplex(a=obj1,A1=con,b1=rhs,A2=NULL,b2=NULL,A3=NULL,b3=NULL,maxi=TRUE)

#Ex2. JK Sharma (Example 4.2)
obj2=c(4,3)
eq4=c(2,1)
eq5=c(1,1)
eq6=c(1,0)
eq7=c(0,1)
con1=rbind(eq4,eq5,eq6,eq7)
rhs1=c(1000,800,400,700)
simplex(a=obj2,A1=con1,b1=rhs1,A2=NULL,b2=NULL,A3=NULL,b3=NULL,maxi=TRUE)

#Ex3. JK Sharma (Example 4.6)
obj3=c(5,3)
eq8=c(2,4)
rhs2=c(12)
eq9=c(5,2)
rhs3=c(10)
eq10=c(2,2)
rhs4=c(10)
simplex(a=obj3,A1=eq8,b1=rhs2,A2=eq9,b2=rhs3,A3=eq10,b3=rhs4,maxi=FALSE)

#OR

simplex(a=c(5,3),A1=c(2,4),b1=12,A2=c(5,2),b2=10,A3=c(2,2),b3=10,maxi=FALSE)

#Ex4. JK Sharma (Example 4.11)
obj4=c(3,9)
eq11=c(1,4)
eq12=c(1,2)
con3=rbind(eq11,eq12)
rhs5=c(8,4)
simplex(a=obj4,A1=con3,b1=rhs5,A2=NULL,b2=NULL,A3=NULL,b3=NULL,maxi=TRUE)

#Ex5. JK Sharma (Example 4.12) (Multiple / Alternate Solution)
obj5=c(6,4)
eq13=c(2,3)
eq14=c(3,2)
con4=rbind(eq13,eq14)
rhs6=c(30,24)

simplex(a=obj5,A1=con4,b1=rhs6,A2=c(1,1),b2=3,A3=NULL,b3=NULL,maxi=TRUE)
#####
#Ex6. JK Sharma (Example 4.13) (unbounded Solution)
obj6=c(3,5)
eq15=c(1,-2)
eq16=c(1,0)
con5=rbind(eq15,eq16)
rhs7=c(6,10)

simplex(a=obj6,A1=con5,b1=rhs7,A2=c(0,1),b2=1,A3=NULL,b3=NULL,maxi=TRUE)

#Ex7. JK Sharma (Example 4.14) (infeasible Solution)

simplex(a=c(6,4),A1=c(1,1),b1=5,A2=c(0,1),b2=8,A3=NULL,b3=NULL,maxi=TRUE)

#Extra method:Example 4.1
library(linprog)
obj1=c(3,5,4)
eq1=c(2,3,0)
eq2=c(0,2,5)
eq3=c(3,2,4)
con=rbind(eq1,eq2,eq3)
rhs=c(8,10,15)
#solveLP(obj1,con,rhs,maximum = TRUE)
solveLP(obj1,rhs,con,maximum = TRUE)
print(solveLP(obj1,rhs,con,maximum = TRUE))
