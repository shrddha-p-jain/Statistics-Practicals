#FORWARD SELECTION
mtcars
mtcars.lm=lm(mpg~1,data=mtcars)
mtcars.lm
summary(mtcars.lm)

add1(mtcars.lm,scope = mtcars,test = 'F')
mtcars.lm2=lm(mpg~wt,data = mtcars)
mtcars.lm2

add1(mtcars.lm2,scope = mtcars,test = 'F')
mtcars.lm3=lm(mpg~wt+cyl,data = mtcars)
mtcars.lm3

add1(mtcars.lm3,scope = mtcars,test = 'F')
summary(mtcars.lm3)
add1(mtcars.lm3,scope = mtcars,test = 'F')

mtcars.lm=lm(mpg~.,data=mtcars)#ALL
mtcars.lm3=lm(mpg~wt+cyl,data = mtcars)#MODEL
anova(mtcars.lm,mtcars.lm3)

#BACKWARD SELECTION
mtc.lm=lm(mpg~.,data=mtcars)#ALL
mtc.lm
formula(mtc.lm)
summary(mtc.lm)
drop1(mtc.lm,test='F')
mtc.lm2=lm(mpg~disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars)#ALL
mtc.lm2
drop1(mtc.lm2,test = 'F')

mtc.lm2=lm(mpg~disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars)#ALL
drop1(mtc.lm2,test = 'F')

mtc.lm2=lm(mpg~disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars)#ALL
drop1(mtc.lm2,test = 'F')
mtc.lm2=lm(mpg~disp+hp+drat+wt+qsec+v+am+gear+carb,data=mtcars)#ALL
drop1(mtc.lm2,test = 'F')
mtc.lm2=lm(mpg~disp+hp+drat+wt+qsec+v+am+gear+carb,data=mtcars)#ALL
drop1(mtc.lm2,test = 'F')
mtc.lm2=lm(mpg~disp+hp+drat+wt+qsec+v+am+gear+carb,data=mtcars)#ALL
drop1(mtc.lm2,test = 'F')
mtc.lm2=lm(mpg~disp+hp+drat+wt+qsec+v+am+gear+carb,data=mtcars)#ALL
drop1(mtc.lm2,test = 'F')





