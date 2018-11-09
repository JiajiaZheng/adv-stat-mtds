property <- read.table("http://www.pstat.ucsb.edu/faculty/yuedong/classes/data/property.txt", header = T)
attach(property)

### 1. EDA
View(property)
summary(property)

quartz()
par(mfrow=c(2,2), oma=c(5,4,2,2)+0.1, mar=c(2,2,2,2)+0.1)
boxplot(property$size, main="size")
boxplot(property$age, main="age")
boxplot(property$dc, main="dc")
boxplot(property$dt, main="dt")

quartz()
par(mfrow=c(1,2))
boxplot(property$price, main="price")
qqnorm(property[,"price"],main="price"); qqline(property[,"price"])

quartz()
par(mfrow=c(2,2))
qqnorm(property[,"size"], main="size"); qqline(property[,"size"])
qqnorm(property[,"age"], main="age"); qqline(property[,"age"])
qqnorm(property[,"dc"], main="dc"); qqline(property[,"dc"])
qqnorm(property[,"dt"], main="dt"); qqline(property[,"dt"])

quartz()
pairs(property)

### 2. Model fitting
model1 <- lm(price ~ 1, data=property)
model2 <- lm(price ~ size, data=property)
model3 <- lm(price ~ size + age, data=property)
model4 <- lm(price ~ size + age + dc, data=property)
model5 <- lm(price ~ size + age + dc + dt, data=property)
model6 <- lm(price ~ size + age + dc + dt + size*dc, data=property)
model7 <- lm(price ~ size + age + dc + dt + size*dc + dc*dt, data=property)
anova(model1, model2, model3, model4, model5, model6, model7)

# fit the full model
fit1 <- lm(price ~ size + age + dc + dt, data = property)
summary(fit1) #R^2 = 0.553, Ajusted R^2 = 0.5301, insignificant for variables dc and age

# check collinearity
library(car)
vif(fit1)

# fit model with interaction terms
fit2 <- lm(price ~ size + age + dc + dt + size*dc + dc*dt, data = property)
summary(fit2) #insignificant interaction terms, so abondon this model

# step function
step(fit1, direction = "both") 

# a need to remove variable dc
fit3 <- lm(price ~ size + age + dt)
fit3.1 <- glm(price ~ size + age + dt)
summary(fit3) #R^2 = 0.5525, Ajusted R^2 = 0.5355, all sinificant variables, AIC = 1010.6

# residuals plot to check the assumptions of fit3
library(boot)
quartz()
glm.diag.plots(fit3.1)

quartz()
par(mfrow=c(1,3))
plot(property$size, residuals(fit3), xlab="size", ylab="residuals", main="residuals v.s. size")
abline(h=0)
lines(lowess(property$size, residuals(fit3)), col="red", lty=2)
plot(property$age, residuals(fit3), xlab="age", ylab="residuals", main="residuals v.s. age")
abline(h=0)
lines(lowess(property$age, residuals(fit3)), col="red", lty=2)
plot(property$dt, residuals(fit3), xlab="dt", ylab="residuals", main="residuals v.s. dt")
abline(h=0)
lines(lowess(property$dt, residuals(fit3)), col="red", lty=2)


## Transformation
# Log transformation
fit4 <- lm(log(price) ~ size + age + dt)
fit4.1 <- glm(log(price) ~ size + age + dt)

summary(fit4) #R^2 = 0.546, Ajusted R^2 = 0.5288, all sinificant variables, AIC = -47.899

# Box-cox transformation
library(MASS)
quartz()
par(mfrow=c(1,1))
bc <- boxcox(fit3)
title("Log-likelihood plot for Box-Cox transformation")
bc$x[which.max(bc$y)] #lambda = 0.38
fit5 <- lm(price^0.38 ~ size + age + dt)
fit5.1 <- glm(price^0.38 ~ size + age + dt)
summary(fit5) #R^2 = 0.5522, Ajusted R^2 = 0.5352, all sinificant variables, AIC = 191.41

# Choose Log-transformation since it has similar R^2 but lower AIC

### Added variable and partial residual plot for fit3
# residual plots of covariates - size, age, dt
quartz()
par(mfrow=c(1,3))
plot(property$size, residuals(fit3), xlab="size", ylab="residuals", main="residuals v.s. size")
abline(h=0)
lines(lowess(property$size, residuals(fit3)), col="red", lty=2)
plot(property$age, residuals(fit3), xlab="age", ylab="residuals", main="residuals v.s. age")
abline(h=0)
lines(lowess(property$age, residuals(fit3)), col="red", lty=2)
plot(property$dt, residuals(fit3), xlab="dt", ylab="residuals", main="residuals v.s. dt")
abline(h=0)
lines(lowess(property$dt, residuals(fit3)), col="red", lty=2)
# potential high order terms of size, age, and dt


# added variable and partial residual plots for fit3
quartz()
par(mfrow=c(3,2))
# size
d1 <- residuals(lm(price ~ age+dt))
m1 <- residuals(lm(size ~ age+dt))
plot(m1,d1, xlab="size residual", ylab="price residual", main="Added variable for size" )
abline(0, coef(fit3)[2])
lines(lowess(m1,d1), col="red", lty=2)
pr1 <- residuals(fit3)+coef(fit3)[2]*size
plot(size,pr1, xlab="size", ylab="partial residual", main="Partial residual for size")
abline(0, coef(fit3)[2])
lines(lowess(size, pr1), col="red", lty=2)
# age
d2 <- residuals(lm(price ~ size+dt))
m2 <- residuals(lm(age ~ size+dt))
plot(m2,d2, xlab="age residual", ylab="price residual", main="Added variable for age" )
abline(0, coef(fit3)[3])
lines(lowess(m2,d2), col="red", lty=2)
pr2 <- residuals(fit3)+coef(fit3)[3]*age
plot(age,pr2, xlab="age", ylab="partial residual", main="Partial residual for age")
abline(0, coef(fit3)[3])
lines(lowess(age, pr2), col="red", lty=2)
# there may exist a quadrautic term for covariate - age
# dt
d3 <- residuals(lm(price ~ size+age))
m3 <- residuals(lm(dt ~ size+age))
plot(m3,d3, xlab="dt residual", ylab="price residual", main="Added variable for dt" )
abline(0, coef(fit3)[4])
lines(lowess(m3,d3), col="red", lty=2)
pr3 <- residuals(fit3)+coef(fit3)[4]*dt
plot(dt,pr3, xlab="dt", ylab="partial residual", main="Partial residual for dt")
abline(0, coef(fit3)[4])
lines(lowess(dt, pr3), col="red", lty=2)

# use log transformation, without covariate dc, high order terms of age
fit6 <- lm(log(price) ~ size + age + dt + I(age^2) + I(age^3))
fit6.1 <- glm(log(price) ~ size + age + dt + I(age^2) + I(age^3))
fit7 <- lm(log(price) ~ size + age + dt + I(age^2))
fit7.1 <- glm(log(price) ~ size + age + dt + I(age^2))
fit8 <- lm(price ~ size + age + dt + I(age^2))
fit8.1 <- glm(price ~ size + age + dt + I(age^2))
summary(fit6) # r-squared=0.5795,Adjusted R-squared=0.5521, p-value:2.81e-13, significant coefficients=intercept,size,dt
summary(fit7) # r-squared=0.5754,Adjusted R-squared=0.5536, p-value:7.252e-14, significant coefficients=intercept,size,age,dt,I(age^2)
summary(fit6.1) # AIC=-50.246
summary(fit7.1) # AIC=-51.453
summary(fit8.1) # AIC=1007.2


### Diagnostics
library(boot)
quartz()
glm.diag.plots(fit7.1)

# check residuals vs. fitted values - check for constant variance, outliers and inadequacy
quartz()
par(mfrow=c(1,2))
plot(fitted(fit7), residuals(fit7), xlab="Fitted", ylab="Residuals", main="Residuals v.s. Fitted")
abline(h=0)
# identify(fitted(fit6),residuals(fit6))
qqnorm(residuals(fit7), ylab="Residuals", main="QQ-plot of residuals")
qqline(residuals(fit7))

# plot of leverages and cook's statistic
quartz()
par(mfrow=c(1,2))
h <- hatvalues(fit7)
cd <- cooks.distance(fit7)
plot(h/(1-h), cd, ylab="Cook's distance")
identify(h/(1-h), cd)
library(car)
influencePlot(fit7)

# residual plots of covariates - size, age, dt, age^2
quartz()
par(mfrow=c(2,2))

plot(property$size, residuals(fit7), xlab="size", ylab="residuals", main="residuals v.s. size")
abline(h=0)
lines(lowess(property$size, residuals(fit7)), col="red", lty=2)

plot(property$age, residuals(fit7), xlab="age", ylab="residuals", main="residuals v.s. age")
abline(h=0)
lines(lowess(property$age, residuals(fit7)), col="red", lty=2)

plot(property$dt, residuals(fit7), xlab="dt", ylab="residuals", main="residuals v.s. dt")
abline(h=0)
lines(lowess(property$dt, residuals(fit7)), col="red", lty=2)

plot((property$age)^2, residuals(fit7), xlab="age", ylab="residuals", main="residuals v.s. age^2")
abline(h=0)
lines(lowess((property$age)^2, residuals(fit7)), col="red", lty=2)

# added variable and partial residual plots for fit7
quartz()
par(mfrow=c(4,2))
# size
d1 <- residuals(lm(log(price) ~ age+dt+age^2))
m1 <- residuals(lm(size ~ age+dt+age^2))
plot(m1,d1, xlab="size residual", ylab="price residual", main="Added variable for size" )
abline(0, coef(fit7)[2])
lines(lowess(m1,d1), col="red", lty=2)
pr1 <- residuals(fit7)+coef(fit7)[2]*size
plot(size,pr1, xlab="size", ylab="partial residual", main="Partial residual for size")
abline(0, coef(fit7)[2])
lines(lowess(size, pr1), col="red", lty=2)
# age
d2 <- residuals(lm(log(price) ~ size+dt+age^2))
m2 <- residuals(lm(age ~ size+dt+age^2))
plot(m2,d2, xlab="age residual", ylab="price residual", main="Added variable for age" )
abline(0, coef(fit7)[3])
lines(lowess(m2,d2), col="red", lty=2)
pr2 <- residuals(fit7)+coef(fit7)[3]*age
plot(age,pr2, xlab="age", ylab="partial residual", main="Partial residual for age")
abline(0, coef(fit7)[3])
lines(lowess(age, pr2), col="red", lty=2)
# there may exist a quadrautic term for covariate - age
# dt
d3 <- residuals(lm(log(price) ~ size+age+age^2))
m3 <- residuals(lm(dt ~ size+age+age^2))
plot(m3,d3, xlab="dt residual", ylab="price residual", main="Added variable for dt" )
abline(0, coef(fit7)[4])
lines(lowess(m3,d3), col="red", lty=2)
pr3 <- residuals(fit7)+coef(fit7)[4]*dt
plot(dt,pr3, xlab="dt", ylab="partial residual", main="Partial residual for dt")
abline(0, coef(fit7)[4])
lines(lowess(dt, pr3), col="red", lty=2)
# age^2
d4 <- residuals(lm(log(price) ~ size+age+dt))
m4 <- residuals(lm(age^2 ~ size+age+dt))
plot(m4,d4, xlab="age^2 residual", ylab="price residual", main="Added variable for age^2" )
abline(0, coef(fit7)[5])
lines(lowess(m4,d4), col="red", lty=2)
pr4 <- residuals(fit7)+coef(fit7)[5]*(age^2)
plot(age^2,pr4, xlab="age^2", ylab="partial residual", main="Partial residual for age^2")
abline(0, coef(fit7)[5])
lines(lowess(age^2, pr4), col="red", lty=2)

summary(fit7) 
# Multiple R-squared:  0.5754, Adjusted R-squared:  0.5536, p-value:7.252e-14, significant coefficients=intercept,size,age,dt,I(age^2)
confint(fit7)

summary(fit8)
# Multiple R-squared:  0.5804, Adjusted R-squared:  0.5589, F-statistic: 26.98 on 4 and 78 DF,  p-value: 4.601e-14

summary(fit3)
# Multiple R-squared:  0.5525, Adjusted R-squared:  0.5355, F-statistic: 32.51 on 3 and 79 DF,  p-value: 8.665e-14


property2 <- property_outlier_removed
fit10 <- lm(log(price) ~ size + age + dt + I(age^2), data = property2)
fit10.1 <- glm(log(price) ~ size + age + dt + I(age^2), data = property2)

summary(fit10)
# Multiple R-squared:  0.5868,	Adjusted R-squared:  0.5653, F-statistic: 27.34 on 4 and 77 DF,  p-value: 3.932e-14
summary(fit10.1)

