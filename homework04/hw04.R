setwd("~/Documents/MSDS16/STAT_ Jeff/Day2/homework04")

#Read train data
train <- read.csv("hw04p01train.csv", header = T)
str(train) # two variables - disp and wt
# wt - Target variable
# disp - Explanatory Variable

#Understand the data
summary(train)
plot(train, pch = 20, cex=.2, main = "First order Linear Model Fit")
# The data shows a positive correlation between wt and disp
# If not liner, quadratic model should fit well

#Try Linear
lm1 <- lm(wt~disp, data = train)
summary(lm1)
mse1 <- mean(lm1$residuals^2) 
mse1 # 81383.52

#Check the fit visually
lines(train$disp, lm1$fitted.values, type='b', cex=.1, col ='red')

#Check 2nd order polynomial
lm2 <- lm(wt~poly(disp,2), data = train)
summary(lm2)
mse2 <- mean(lm2$residuals^2) 
mse2 # 73116
# There is an improvement in MSE - reduced by 10 to 11%
# From the plotting, quadratic seemed better due to a curvey pattern
# Check the second order model fit visually
plot(train, pch = 20, cex=.2, main = "Second order Linear Model Fit")
xval = seq(from = 0, to = 500, length.out = 40)
lines(xval, predict(lm2,data.frame(disp=xval)),type = "l",cex=0.1,col="red") #Looks better fit

#Try cubic - check if there is any improvement in mse
lm3 <- lm(wt~poly(disp,3), train)
summary(lm3)
mse3 <- mean(lm3$residuals^2) # 69438 - 5.5% reduction from quadratic

#Crossvalidate quadratic and cubic models
#divide train data into Tr - 60%, Ts - 40%
set.seed(6430)
tr <- train[sample(1:nrow(train),0.6*nrow(train),replace=F),]
ts <- train[-sample(1:nrow(train),0.6*nrow(train),replace=F),]

#Crossvalidate quadratic
lm2_tr <- lm(wt~poly(disp,2), tr)
#Model built on tr data, crossvalidated on ts data
mse2_CV <- mean((predict(lm2_tr,ts) - ts$wt)^2) #79602

#Crossvalidate cubic
lm3_tr <- lm(wt~poly(disp,3), tr)
#Model built on tr data, crossvalidated on ts data
mse3_CV <- mean((predict(lm3_tr,ts) - ts$wt)^2) #77901

#Though cubic is slightly better than qudratic, I prefer to choose quadratic to avoid overfitting

#Trying log transformation on both the variable 
lm_log <- lm(log(wt)~log(disp), train)
mse1_log <- mean((train$wt - exp(lm_log$fitted.values))^2) #72556
summary(lm_log)
#The log transformation on disp, wt has resulted in a better model than quadratic

plot(log(train$disp), log(train$wt), cex = .2, pch = 20) #Transformed data has a better pattern 

#Crossvalidate log transformation model
lm1_trlog <- lm(log(wt)~poly(log(disp),1), tr)
#Model built on tr data, crossvalidated on ts data
mse1_CVlog <- mean((exp(predict(lm1_trlog,ts)) - ts$wt)^2) #76979

#Model Selection
#quadratic
c(mse2, mse2_CV)
#73116.71   79602.19
#cubic
c(mse3, mse3_CV)
#69438.02   77901.35
#Log on wt and disp
c(mse1_log, mse1_CVlog)
#72556.86 76979.58

#I am choosing Log model, since its CV score is higher and not much variance in the mse values

#read the data to be predicted
tobe_predict <- read.csv("hw04p01predict.csv")
str(tobe_predict)
summary(tobe_predict)
predvect <- predict(lm_log, tobe_predict) #predicted values
predvect[1:4]
# Transform back to normal scale from log scale
predvect <- exp(predvect)
summary(predvect)

write.table(predvect, "hw04p01mypredictions.csv", row.names = F, col.names=F, sep = ",")

