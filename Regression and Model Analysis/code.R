setwd("C:/Users/sr_te/OneDrive/Desktop/ST3131")
library(MASS)

data<- read.table("FEV.csv",sep= ",", header=TRUE)
data
data$Sex = as.factor(data$Sex)
data$Smoke = as.factor(data$Smoke)
attach(data)

#relationship of the response and the regressors 
pairs(FEV ~ Age + Hgt + Hgt_m)
plot(Age, FEV, main = "Scatter Plot of FEV vs Age", sub = "Figure 1.1", pch = 20)
plot(Hgt, FEV, main = "Scatter Plot of FEV vs Hgt", sub = "Figure 1.2", pch = 20)
boxplot(FEV ~ Sex, main = "Box Plot of FEV vs Sex", sub = "Figure 1.3")
boxplot(FEV ~ Smoke, main = "Box Plot of FEV vs Smoke", sub = "Figure 1.4")
length(which(data$Smoke == 1))
length(which(data$Smoke == 0))
plot(Hgt_m, FEV, main = "Scatter Plot of FEV vs Hgt_m", sub = "Figure 1.5", pch = 20)

# Histogram of FEV data
hist(FEV, main = "Histogram of FEV", sub = "Figure 1.6")

#relationship between the regressors 
plot(Age, Hgt, main = "Scatter Plot of Hgt vs Age", sub = "Figure 1.7", pch = 20)
plot(Age, Hgt_m, main = "Scatter Plot of Hgt_m vs Age", sub = "Figure 1.8", pch = 20)
plot(Hgt_m, Hgt, main = "Scatter Plot of Hgt vs Hgt_m", sub = "Figure 1.9", pch = 20)
cor(Age, Hgt)
cor(Age, Hgt_m)
cor(Hgt_m, Hgt)

########################Fitting model1##################################
model1 = lm(FEV ~ Age + Hgt + Sex + Smoke + Hgt_m)
summary(model1)

anova(model1)

################################Find VIF#################################
x <- cbind(Age, Hgt, Hgt_m)
#then finding the correlation:
x<- cor(x) #this is X'X (correlation form)
C<-solve(x)  #this is (X'X)^(-1) where X'X is in correlation form

VIF <- diag(C) # VIF for Hgt and Hgt_m >10, multicollinearity indicated, caused by Hgt and Hgt_m
VIF

# Exclude variable Hgt
new_x <- cbind(Age, Hgt_m)
new_x <- cor(new_x) #this is X'X (correlation form)
new_C <- solve(new_x)  #this is (X'X)^(-1) where X'X is in correlation form
new_VIF <- diag(new_C) # multicollinearity not indicated.
new_VIF

###################Fitting model2######################
model2 <- lm(FEV ~ Age + Sex + Smoke + Hgt_m)
summary(model2)

anova(model2)

##################Model Adequacy Checking for model2######################
#standardized residuals vs Fitted Values:
plot(model2$fitted.values,rstandard(model2), xlab="Fitted Values", ylab= "Standardized Residuals", main = "Standardized Residuals vs Fitted Values", sub = "Figure 2.2.1")
abline(h=0)
#standardized residuals vs Age:
plot(Age,rstandard(model2), xlab="Age", ylab= "Standardized Residuals", main = "Standardized Residuals vs Age", sub = "Figure 2.2.2")
abline(h=0)
#standardized residuals vs Hgt_m:
plot(Hgt_m,rstandard(model2), xlab="Hgt_m", ylab= "Standardized Residuals", main = "Standardized Residuals vs Hgt_m", sub = "Figure 2.2.3")
abline(h=0)
#Normal probability plot/QQ plot
qqnorm(rstandard(model2),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "Normal Probability Plot of Standardized Residuals of model2", sub = "Figure 2.2.4")
qqline(rstandard(model2),datax = TRUE)

########################## Transformation(Box-cox method) ##################
boxcox(model2, lambda=seq(-2, 2, by=0.5), optimize = TRUE, plotit = TRUE)
# lambda should be within the interval from about 0 to 0.3, for easy interpretation, lambda chosen as 0
# the plot proposes that lambda = 0 should be used, that means a transformation on
# y: log(y) should be used.

############################Fitting new model3#############################
model3 <- lm(log(FEV) ~ Age + Sex + Smoke + Hgt_m)

summary(model3)

#################Model Adequacy Checking for model3######################
#standardized residuals vs Fitted values:
plot(model3$fitted.values,rstandard(model3), xlab ="Fitted Values", ylab = "Standardized Residuals",main = "Standardized Residuals vs Fitted Values", sub = "Figure 2.4.1")
abline(h=0)
#standardized residuals vs Age:
plot(Age,rstandard(model3), xlab ="Age", ylab = "Standardized Residuals", main = "Standardized Residuals vs Age", sub = "Figure 2.4.2")
abline(h=0)
#standardized residuals vs Hgt_m:
plot(Hgt_m,rstandard(model3), xlab = "Hgt_m", ylab = "Standardized Residuals", main = "Standardized Residuals vs Hgt_m", sub = "Figure 2.4.3")
abline(h=0)

#Normal probability plot =  QQ plot
qqnorm(rstandard(model3),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "Normal Probability Plot of Standardized Residuals of model3", sub = "Figure 2.4.4")
qqline(rstandard(model3),datax = TRUE)


################# Detect outliers(influential points) ##################
SR <- rstandard(model3)
which(SR < -3)#outliers
which(SR > 3)#outliers
cook_dist <- cooks.distance(model3)
which(cook_dist > 1) # No influential points

######################## Stepwise Selection ####################
sw<-step(model3, direction = c("both"))

summary(sw)

anova(sw)
