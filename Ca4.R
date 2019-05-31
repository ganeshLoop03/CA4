# dataset containing population height and weight In ireland during 2018 will be analysed.
# To compensate for the influence of height
# on weight, Body Mass Index (BMI) was introduced that can be
# calculated as:BMI = weight/height^2

# This analysis will try to investigate the weights of different gender
# and age groups and the influence of height on weight and
# calculated BMI


Height_Weight_Data <- read.csv("H_W.csv")
Height_Weight_Data
str(Height_Weight_Data)

# function to calclate BMI using Height and weight
BMI = function(Height, Weight){
  return(0.45455*Weight/(.0254*Height)^2)
}


# testimg bmi function
BMI(71,180)

# adding bmi column t our dataframe
Height_Weight_Data$bmi = BMI(Height_Weight_Data$Height,Height_Weight_Data$Weight)
BMI_Data <- Height_Weight_Data
BMI_Data


# Creating a derived variable for the respondents age in 2019 based on the reported birth year.
# creating age column from date of birth 

BMI_Data$age <- (2019 - BMI_Data$birth_yr)
BMI_Data

# Gender and age table 

attach(BMI_Data)
table(Gender)

table(Gender, age)

# plot to check the linearity 

scatter.smooth(x=BMI_Data$Height, y=BMI_Data$bmi, main="Height ~ Weight")  # scatterplot
scatter.smooth(x=BMI_Data$Height, y=BMI_Data$age, main="Height ~ bmi")  # scatterplot


# Flat contingency table

highWeight <- Weight > mean(Weight)
ftable(Gender, age, highWeight)

# Numerical variables and summary statistics
  
mean(Weight)
mean(Height)
c(sd(Weight), sd(Height))
    
(V <- var(cbind(Weight, Height)))
cor(Weight, Height)
    
my.cor <- V[1, 2]/(sd(Weight) * sd(Height))
cat("Correlation r =", my.cor, "\n")


# Are there differences in weight and height in gender age classes?

aggregate(cbind(Weight, Height), list(age, Gender), mean)


x <- Height
oldpar <- par(mfrow = c(2, 2))
plot(x)
hist(x, col = "lightblue")
boxplot(x, col = "lightblue")
qqnorm(x)
par(oldpar)

# is weigth associated with the height?

oldpar = par(mfrow = c(2, 2))
plot(Height, Weight)
title(1)
plot(Height, Weight, pch = as.character(Gender))
title(2)
col <- c("red", "blue")[as.numeric(factor(Gender))]
plot(Height, Weight, col = col, cex = 1.5, lwd = 2)
title(3)
plot(Height, Weight, col = col)
title(4)


##################
# build linear regression model on heigth and weight variable

for (g in c("Female", "Male")) {select <- (Gender == g)
x <- Weight[select]

y <- Height[select]
rfit <- lm(y ~ x)
col <- c("darkred", "darkblue")[(g =="Male") + 1]
abline(rfit, col = col, lwd = 4)
cat("Gender:", g, "\n")
print(rfit)}

summary(rfit)

col <- c("red", "blue")[Gender]
plot(Height, Weight, col = col)
byFit <- by(BMI_Data, Gender, function(x) lm(Height ~ Weight, data = x))
lapply(byFit, abline, lwd = 4)

# Extracting coefficients from all models to check the 
# how good the fit is 

sapply(byFit, function(x) x$coefficients)


# Gender and age effects on height and weight regresion model 
summary(aov(Height ~ Gender + age))
summary(aov(Weight ~ Gender * age))

lmfit <- lm(Weight ~ Gender * Height)
print(lmfit)

# plot of predicted values shows interaction 

col <- c("red", "blue")[Gender]
plot(Height, Weight, col = col)
points(Height, lmfit$fitted.values, col = col, pch = 16)

# Welch Two Sample t-test

t.test(Height ~ Gender)

# Extracting part of a result and reusing to derive p value

(tweight <- t.test(Weight, age))

names(tweight)

tweight$estimate

pValue <- round(tweight$p.value, 3)
cat("p-value ( p =", pValue,")\n")

#############################

# Residuals - Provide a quick view of the distribution of the residuals, which by definition have a mean zero. 
# Coefficients - shows the regression beta coefficients and their statistical significance. 
# Predictor variables that are significantly associated to the outcome variable, are marked by stars.
# Residual standard error (RSE), R-squared (R2) and the F-statistic are metrics that 
# are used to check how well the model fits to our data.
# the estimates of the beta coefficients
# the standard errors (SE), which defines the accuracy of beta coefficients. 
# For a given beta coefficient, the SE reflects how the coefficient varies under 
# repeated sampling. It can be used to compute the confidence intervals and the t-statistic.
# the t-statistic and the associated p-value, which defines the statistical significance of the beta coefficients.
# summary shows that the prediction equation for height

# capturing model summary as an object Gender and age effects on height and weight 
modelSummary <- summary(lmfit)  

# model coefficients
modelCoeffs <- modelSummary$coefficients 
modelCoeffs

# get beta estimate for Height
beta.estimate <- modelCoeffs["Height", "Estimate"]

# get std.error for Height  
std.error <- modelCoeffs["Height", "Std. Error"]  
std.error

# calc t statistic
t_value <- beta.estimate/std.error  
t_value

# calc p Value
p_value <- 2*pt(-abs(t_value), df=nrow(BMI_Data)-ncol(BMI_Data))  
p_value

# fstatistic
f_statistic <- lmfit$fstatistic[1]  
f_statistic

# parameters for model p-value calc
f <- summary(lmfit)$fstatistic

model_p <- pf(f[1], f[2], f[3], lower=FALSE)
model_p

# AIC and BIC calculation 
AIC(lmfit)  

BIC(lmfit)  
  ########################
# capture model summary for heigth and weight model

# capture model summary as an object
modelSummary <- summary(rfit)
modelSummary

# model coefficients
modelCoeffs <- modelSummary$coefficients 
modelCoeffs

# get beta estimate for Height
beta.estimate <- modelCoeffs["Height", "Estimate"]

# get std.error for Height  
std.error <- modelCoeffs["Height", "Std. Error"]  
std.error

# calc t statistic
t_value <- beta.estimate/std.error  
t_value

# calc p Value
p_value <- 2*pt(-abs(t_value), df=nrow(BMI_Data)-ncol(BMI_Data))  
p_value

# fstatistic
f_statistic <- lmfit$fstatistic[1]  
f_statistic

# parameters for model p-value calc
f <- summary(lmfit)$fstatistic

model_p <- pf(f[1], f[2], f[3], lower=FALSE)
model_p

AIC(lmfit)  

BIC(lmfit) 

# build linear regression model on heigth and bmi variable

Height_bmi_relation <- lm(Height ~ bmi)
Height_bmi_relation

# statists t and p calculation for bmi and height model 

# capture model summary as an object
modelSummary <- summary(Height_bmi_relation)
modelSummary

# model coefficients
modelCoeffs <- modelSummary$coefficients 
modelCoeffs

# get beta estimate for Height
beta.estimate <- modelCoeffs["Height", "Estimate"]

# get std.error for Height  
std.error <- modelCoeffs["Height", "Std. Error"]  
std.error

# calc t statistic
t_value <- beta.estimate/std.error  
t_value

# calc p Value
p_value <- 2*pt(-abs(t_value), df=nrow(BMI_Data)-ncol(BMI_Data))  
p_value

# fstatistic
f_statistic <- lmfit$fstatistic[1]  
f_statistic

# parameters for model p-value calc
f <- summary(lmfit)$fstatistic

model_p <- pf(f[1], f[2], f[3], lower=FALSE)
model_p

AIC(lmfit)  

BIC(lmfit) 


# corelation between BMI,height and weight
# The correlation coefficient measures the level of the association between two variables 
# Its value ranges between -1 (perfect negative correlation: x increases, y decreases) 
# and +1 (perfect positive correlation: when x increases, y increases).
# A value closer to 0 suggests a weak relationship between the variables. A low 
# correlation (-0.2 < x < 0.2) probably suggests that much of variation of the outcome 
# variable (y) is not explained by the predictor (x). 
# In such case, we should probably look for better predictor variables.
X <- subset(BMI_Data, selec = c(Weight, Height, bmi))
X
col <- c("red", "blue")[Gender]
cor(X)

# plotting scattergrams
pairs(X, col = col, pch = 16)


# distribution of variables

oldpar <- par(mfrow = c(1, 3))
cols <- c("pink", "lightblue")
boxplot(split(Height, Gender), col = cols, main = "height")
boxplot(Weight ~ Gender, col = cols, main = "weight")
boxplot(bmi ~ Gender, col = cols, main = "BMI")
par(oldpar)

# Calculated sizes

pairs(X, cex = bmi/15, col = Gender)

# BMI classes

bmic <- cut(bmi, c(0, 13, 18, 25, 30, Inf))
levels(bmic)

levels(bmic) <- c("S", "s", "N", "h", "H")
bmic <- factor(bmic, levels = c("S", "s", "N", "h", "H"), ordered = T)
is.ordered(bmic)

# Color coded BMI classes

cols <- (6 - as.numeric(bmic))
pairs(X, cex = bmi/15, col = cols, pch = 16)

# Flat contingency table for categories of BMI


gabTable <- ftable(Gender, age, bmic)
gabTable


barplot(table(Gender, bmic), beside = TRUE, legend = TRUE)

###########
# Predicting Linear Models

# Create the training and test data

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(BMI_Data), 0.8*nrow(BMI_Data))  # row indices for training data
trainingData <- BMI_Data[trainingRowIndex, ]  # model training data
testData  <- BMI_Data[-trainingRowIndex, ]   # test data



# Build the model on training data
lmMod <- lm(Height ~ Weight, data=trainingData)  # build the model
HeightPred <- predict(lmMod, testData)  # predict height

summary (lmMod)


# Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals=testData$Height, predicteds=HeightPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy

head(actuals_preds)


# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 38.00%, min_max accuracy

# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 

# K-fold cross validation to improve accuracy 
install.packages("DMwR")
install.packages("DAAG")
library(DMwR)
library(DAAG)


DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)


cvResults <- suppressWarnings(CVlm(BMI_Data, form.lm=Weight ~ Height, m=5, dots=FALSE, seed=29,
                                   legend.pos="topleft",  printit=FALSE,
                                   main="Small symbols are predicted values while bigger ones are actuals."))
attr(cvResults, 'ms')  

###############

# predicting height from new weight

newWeight <-data.frame(Weight=200)

new_predicted_height_result <- predict(lmMod, newWeight)

new_predicted_height_result



#########################

lmMod1 <- lm(Height ~ bmi, data=trainingData)  # build the model for bmi and height relation 
HeightPred <- predict(lmMod1, testData)  # predict 

summary (lmMod1)


# Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals=testData$Height, predicteds=HeightPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)


# Min-Max Accuracy Calculation
# MAPE Calculation


DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

library(DAAG)
cvResults <- suppressWarnings(CVlm(BMI_Data, form.lm=bmi ~ Height, m=5, dots=FALSE, seed=29,
                                   legend.pos="topleft",  printit=FALSE,
                                   main="Small symbols are predicted values while bigger ones are actuals."))
attr(cvResults, 'ms')  


# predicting height from new bmi

newbmi <-data.frame(bmi=25)

new_predicted_height_result <- predict(lmMod1, newbmi)

new_predicted_height_result
