#data
#totalmilestraveled
x1<-c(89,66,78,111,44,77,80,66,109,76)
#numberofdeliveries
x2<-c(4,1,3,6,1,3,3,2,5,3)
#dailygasprice
x3<-c(3.84,3.19,3.78,3.89,3.57,3.57,3.03,3.51,3.54,3.25)
#totaltraveltime(dependentvariable)
y<-c(7,5.4,6.6,7.4,4.8,6.4,7,5.6,7.3,6.4)
install.packages("car")


#Model1
#milestraveledvstotaltraveltime
#visualizingdata
plot(x1,y)
#fitthemodel
model1<-lm(y~x1)
summary(model1)
#checkingforcorrelationbetweenmilestraveledandtraveltime
cor(x1,y,method="pearson")
#gettingtheANOVAtable
anova(model1)
#Extracting residuals
Residuals<-residuals(model1)
#checking first assumption Normality/non constant variance
shapiro.test(Residuals)
#checking 2nd assumption  constant variance
library(lmtest)
bptest(model1)
#checking third assumption  auto correlation
library(car)
dwtest(model1)

#Model2
#numberofdeliveriesvstotaltraveltime
#visudata
plot(x2,y)
#fitthemodel
model2<-lm(y~x2)
summary(model2)
#checkingforcorrelationbetweenmilestraveledandtraveltime
cor(x2,y,method="pearson")
#gettingtheANOVAtable
anova(model2)
#Extracting residuals
Residuals<-residuals(model2)
#checking first assumption Normality/non constant variance
shapiro.test(Residuals)
#checking 2nd assumption  constant variance
library(lmtest)
bptest(model2)
#checking third assumption  auto correlation
library(car)
dwtest(model2)

#Model3
#gaspricevstotaltraveltime
#visualizingdata
plot(x3,y)
#fitthemodel
model3<-lm(y~x3)
summary(model3)
#checkingforcorrelationbetweenmilestraveledandtraveltime
cor(x3,y,method="pearson")
#gettingtheANOVAtable
anova(model3)
#Extracting residuals
Residuals<-residuals(model3)
#checking first assumption Normality/non constant variance
shapiro.test(Residuals)
#checking 2nd assumption  constant variance
library(lmtest)
bptest(model3)
#checking third assumption  auto correlation
library(car)
dwtest(model3)


#Model4
#visualizingdatabetweentheindependentvariables(x1,x2).Checkingmulticollinearity.
#milestraveled,numberofdeliveriesvstotaltimetraveled
#visualizingdata
plot(x1+x2,y)
#fitthemodel
model4<-lm(y~x1+x2)
summary(model4)
cor(x1+x2,y)
#gettingtheANOVAtable
anova(model4)
library(car)
vif(model4)

#Extracting residuals
Residuals<-residuals(model4)
#checking first assumption Normality/non constant variance
shapiro.test(Residuals)
#checking 2nd assumption  constant variance
library(lmtest)
bptest(model4)
#checking third assumption  auto correlation
library(car)
dwtest(model4)


#Model5
#visualizingdatabetweentheindependentvariables(x1,x3).Checkingmulticollinearity.
#milestraveled,gaspricevstotaltimetraveled
#visualizingdata
plot(x1+x3,y)
#fitthemodel
model5<-lm(y~x1+x3)
summary(model5)
cor(x1+x3,y)
#gettingtheANOVAtable
anova(model5)
library(car)
vif(model5)
#Extracting residuals
Residuals<-residuals(model5)
#checking first assumption Normality/non constant variance
shapiro.test(Residuals)
#checking 2nd assumption  constant variance
library(lmtest)
bptest(model5)
#checking third assumption  auto correlation
library(car)
dwtest(model5)


#Model6
#visualizingdatabetweentheindependentvariables(x3,x2).Checkingmulticollinearity.
#gasprice,numberofdeliveriesvstotaltimetraveled
#visualizingdata
plot(x3+x2,y)


#fitthemodel
model6<-lm(y~x3+x2)
summary(model6)
cor(x3+x2,y)
#gettingtheANOVAtable
anova(model6)
library(car)
vif(model6)
#Extracting residuals
Residuals<-residuals(model6)
#checking first assumption Normality/non constant variance
shapiro.test(Residuals)
#checking 2nd assumption  constant variance
library(lmtest)
bptest(model6)
#checking third assumption  auto correlation
library(car)
dwtest(model6)

#Model7
#visualizingdatabetweentheindependentvariables(x1,x2,x3).Checkingmulticollinearity.
#milestraveled,numberofdeliveriesandgaspricevstotaltimetraveled
#visualizingdata
plot(x1+x2+x3,y)
#fitthemodel
model7<-lm(y~x1+x2+x3)
summary(model7)
cor(x1+x2+x3,y)
#gettingtheANOVAtable
anova(model7)
install.packages("car")
library(car)
vif(model7)#Extracting residuals
Residuals<-residuals(model7)
#checking first assumption Normality/non constant variance
shapiro.test(Residuals)
#checking 2nd assumption  constant variance
library(lmtest)
bptest(model7)
#checking third assumption  auto correlation
library(car)
dwtest(model7)

