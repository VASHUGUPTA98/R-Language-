#Q.1 (A) The following are the values of the cephalic index found in two samples of skulls,one consisting of 14 and the other of 12 individuals.

sample1 = c(74.1,77.7,74.0,74.4,73.8,79.3,75.8,82.8,72.2,75.2,74.2,77.1,78.4,76.3)
sample2 = c(70.8,73.9,74.2,70.4,69.2,71.2,75.8,72.4,76.4,78.1,72.8,73.3)
# Is it possible that average cephalic index in sample II is 75?
#H0: mu = 75
#h1: mu is not equal to 75
#Normality Test
shapiro.test(sample1)
shapiro.test(sample2)
hist(sample1)
hist(sample2)
t.test(sample1,mu=75)
#Is it reasonable to say that the average of index in the two samples is the same
#H0: mean of two samples are equal
#H1: mean of two samples are not equal
var.test(sample1,sample2)
t.test(sample1,sample2)



#Q.1(B) The manager of a car plant wishes to investigate how the plants electricity usage depends upon the plant's production.
prod =c(4.5,3.5,4.3,5.1,5.6,4.9,5.3,5.8,4.7,5.6,4.9,4.2)
elect_usage =c(2.5,2.3,2.5,2.7,2.9,3.1,3.2,3.5,3.1,3.3,2.6,2.5)
plant_prod=data.frame(prod,elect_usage)
plant_prod
cor(prod,elect_usage) # simple correlation
plant_prod = lm(elect_usage~prod,data=plant_prod)
summary(plant_prod) 
plot(prod,elect_usage)
abline(plant_prod)




#Q.1(C) Find the optimum solution of the following LPP by simplex method:
#Minimize Z = 5x1 - 6x2 + 4x3
#Subject to constraints: 3x1 + 4x2 + 6x3 ??? 9,
#x1 + 3x2 + 2x3 ??? 5,
#7x1 - 2x2 - x3 ??? 10,
#x1 - 2x2 + 4x3 ??? 4,
#2x1 + 5x2 - 3x3 = 3,
#x1, x2, x3 ??? 0.
library(boot)
Objec_Func=c(5,-6,4)
Eq1=c(3,4,6)
Eq2=c(1,3,2)
Eq3=c(1,-2,4)
con1=rbind(Eq1, Eq2, Eq3)
rhs=c(9,5,4)
simplex(a=Objec_Func, A1=c(7,-2,1), b1=10, A2=con1, b2=rhs, A3=c(2,5,-3), b3=3, maxi=FALSE)




#Q.2 Answer the following questions.
#(A) We are interested in how the heat evolved in the curing of cement is affected by the amounts of various chemical included in the cement mixture. The independent and dependent
#variables are listed below:
#X1 = amount of tricalcium aluminate,
#X2 = amount of tricalcium silicate,
#X3 = amount of tetracalcium alumino ferrite,
#X4 = amount of dicalcium silicate,
#Y = heat evolved in calories per gram of cement.
#Fit multiple regression model. Apply forward selection method and check forward selection method model and original model are same or not?
  
library(MASS)
X1=c(7,1,11,11,7,11,3,1,2,21,1,11,10)
X2=c(25,28,55,30,51,54,70,30,53,46,39,65,67)
X3=c(6,15,8,8,6,9,17,22,18,4,23,9,8)
X4=c(60,52,20,47,33,22,6,44,22,26,34,12,12)
Y=c(79,74,104,88,96,109,103,73,93,116,84,113,109)

cement = data.frame(Y, X1, X2, X3, X4)
cement

cement_pbd_12 = lm(Y~., data=cement)
summary(cement_pbd_12)
step=stepAIC(cement_pbd_12,direction = "both")
step$anova

#forward selection
cement_lin = lm(Y~1, data=cement)
summary(cement_lin)

add1(cement_lin,scope=cement,test = 'F')
cement_lin = lm(Y~X4, data=cement)
summary(cement_lin)

add1(cement_lin,scope=cement,test = 'F')
cement_lin = lm(Y~X4+X1, data=cement)
summary(cement_lin)

add1(cement_lin,scope=cement,test = 'F')



#Q,2(B)The adjoining table gives the results of an experiment on the effects of five manual treatments I, II, III, IV, V on the yield of sugarcane.
#Write down the ANOVA table. Test whether the treatments are equally effective and test for the differences between variety effects
t1 =c(552,	525,	472,	460,	413,	451,	405,	492,	467,	429,	431,	481,	381,	432,	325,	425,	441,	410,	430,	445,	572,	463,	471,	469,	493)
t2 =factor(c("I", "I", "I", "I", "I", "II", "II", "II", "II", "II", "III", "III", "III", "III", "III", "IV", "IV", "IV", "IV", "IV", "V", "V", "V", "V", "V"))
a =data.frame(t1, t2)
a

summary(a)

#checking balance design
with(a,table(t2))


crd.aov=aov(t1~t2,data=a)
summary(crd.aov)

model.tables(crd.aov,type = "means")

op =par(mar=c(5,8,4,2))
plot(TukeyHSD(crd.aov, ordered=T),cex.axis=0.7,las=1)
par(op)





