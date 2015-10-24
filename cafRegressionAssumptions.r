setwd("~/Dropbox/PhD/Placebo/Experiments/CaffeinePredictorsOfWIthdrawalSeverity/data")

source("~/Dropbox/PhD/Placebo/Experiments/CaffeinePredictorsOfWIthdrawalSeverity/data/masterCaffRegression.R")

library(car)
library(plyr)


caffReg <- mRFrame

############### descriptives for each of the numeric columns

caffRegMeans <- colMeans(data.frame(caffReg$totCaffeine, caffReg$numQuitAttempts, caffReg$yearsCaffUse, caffReg$numCupsCoffee, caffReg$B1Total, caffReg$B2Total))

# this is for the sd of the columns. The '2' tells the apply function to calculate column means. 1 would tell it to do rows. c(1,2) does both. You could apply any function to the FUN argument btw, including mean, which means you could calculate means with this method instead of the method you used above.



caffRegSDs <- apply(data.frame(caffReg$totCaffeine, caffReg$numQuitAttempts, caffReg$yearsCaffUse, caffReg$numCupsCoffee, caffReg$B1Total, caffReg$B2Total), 2, FUN = sd)

caffRegSEs <- caffRegSDs/sqrt(nrow(caffReg))


# number of each level of each factor. The apply function applies the 'count' function inside it to the columns specified in the first argument. We have restricted this to only the columns that are factors. In the first argument the sapply function is isolating only those columns of caffReg that are factors. If we just ran the count function called by 'apply' on caffReg it would give us counts for numeric vectors, dates etc. We want only the factors. The '2' specifies that we are conducting this operation on columns (1 would be rows). 

# you may have to detach dplyr to run the following, as it, along with plyr, also has a count function
detach("package:dplyr", unload = T)

apply(caffReg[, sapply(caffReg, is.factor)], 2, count)



# # could do the above another way if you had a list of column names that were factors

# caffFactNames <- colnames(caffReg[, sapply(caffReg, is.factor)])
# 
# apply(caffReg[, caffFactNames], 2, count)


# group means 

gMeansCaffReg <- with(caffReg, tapply(caffReg$B2Total, dSetID, mean))


#aggregate(data.frame(caffReg[, sapply(caffReg, is.numeric)]) ~ dSetID, data = caffReg, mean)
########## Simple Regression #############################

reg <- lm(B2Total ~ totCaffeine, data = caffReg)

b <- summary(reg)

b$coefficients[2,4] # delivers p-value for the regression by isolating the element in the element of the summary of the regression

## Hierachical regression

# model 1 

model1 <- lm(B2Total ~ totCaffeine, data = caffReg)

# model 2

model2 <- lm(B2Total ~ totCaffeine + numQuitAttempts, data = caffReg)

# model 3

model3 <- lm(B2Total ~ totCaffeine + numQuitAttempts + yearsCaffUse, data = caffReg)

# model 4

model4 <- lm(B2Total ~ totCaffeine + numQuitAttempts + yearsCaffUse + numCupsCoffee, data = caffReg)

# model 5

model5 <- lm(B2Total ~ totCaffeine + numQuitAttempts + yearsCaffUse + numCupsCoffee + B1Total, data = caffReg)


# look at the changing regression coefficients, p-values and t-statistics for how much each variable contributes to the overall model 

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

# to compare successive models use the anova function. The output here tells buy how much each new model predicts the variance in the DV over and above the previous model and whether or not this difference is significant.

anova(model1, model2, model3, model4, model5)

# for semipartial/part (delta-R squared) and partial correlation squared (partial eta-squared) we can use this function

require(lmSupport)

lm.sumSquares(model5)

# or we can also use the following function from the lmSupport package to give us change in r-squared and partial eta-squared

lm.deltaR2(model4, model5) # note you can only compare two models at a time.

# this does the same as above except it gives a p-value for the second model (first model must be subset of second)

modelCompare(model4, model5)


### using stepwise regression via the rms package


require(rms)

## creates empty regression object without predictors

regStep <- lm(B2Total ~ 1, data = caffReg)

## uses the previous object as the basis for a stepwise regression

# note: forward stepwise regression is a procedure whereby the variables which have the largest semi-partial r-squared and hence make the largest contribution to R-squared (variables which have the largets t-values) are included in the model and the least significant excluded. predictors are not admitted when they cease to make a contribution which is statistically significant at a level specified by the user.   

regStepFor <- step(regStep, direction = "forward", scope = (~ totCaffeine + numQuitAttempts + yearsCaffUse + numCupsCoffee + B1Total), data = caffReg, trace = 0)

regStepBack <- step(regStep, direction = "backward", scope = (~ totCaffeine + numQuitAttempts + yearsCaffUse + numCupsCoffee + B1Total), data = caffReg, trace = 0)

summary(regStepFor)
summary(regStepBack)

############################################################################################################################################################################################################################################################################
################## Testing assumptions of regression.###############################################################################################################################################  We will work on the lm object 'model5' ####################################################################################################################################################################################################################################################################

require(car)
require(MASS)
library(stats)
library(lmtest)

######################################################################################
################# Assumption 1: testing for independence of errors ####################
##########################################################################################

# first assumption of regression is indepedence of errors. This means that the error of observation for each observation is unrelated to that of adjacent observations. The Durbin Watson statistic is a test for a certain type of lack of independence, namely 1st-order autocorrelation (also called serial correlation) which means that the errors of adjacent observations are not independent (i.e. are correlated). 

# test for autocorrelated errors

# The Durbin Watson statistic can run from 0 to 4 but a value near 2 would indicate little or no autocorrelation. 

durbinWatsonTest(model5)

# You would write this up like so:
  
#  ‘There was independence of observations, as assessed by a Durbin-Watson statistic of 2.222’

##########################################################################################
#################### Assumption 2: Assumption of linearity ########################################################################################################################

# one of the assumptions of multiple linear regression is that the independent variables are linearly related to the dependent variable, and that each independent variable is linearly related to the DV

# You can check for this by plotting the studentised residuals against the unstandardised predicted values. If your residuals form a horizontal band then the relationship between your DV and IVs is likely linear. In general values close to the horizontal line are well predicted. Point over the line are underpredicted and points over the line are overpredicted.

# calculate standardised residuals
unStResid <- resid(model5)
stdResid <- (unStResid - mean(unStResid)) / sd(unStResid)

# studentised residuals
studResid <- rstudent(model5)

# unstandardised predicted values 
unStPredicted <- predict(model5)

plot(unStPredicted, studResid, main = "studentised residuals by unstandardised Predicted Values", ylab = "Studentised Residuals", xlab = "Unstandardised Predicted")

plot(unStPredicted, stdResid, main = "studentised residuals by unstandardised Predicted Values", ylab = "Standardised Residuals", xlab = "Unstandardised Predicted")

abline(0,0)

# you can also test this with partial regression plots. These are simply plotting the different predictors against the DV. Partial Regression plots should show a linear relationship. 

avPlots(model5)


######################################################################################################### Assumption 3: Assumption of homoscedasticity #######################################################################################################################

# The assumption of homoscedasticity is that the residuals are equal for all values of the predicted dependent variable. 

# you can test for this by running either the Breusch-Pagan test of heteroscedasticity

bptest(model5) # We're hoping for a non-significant value here. Significant means heteroscedasticity, non-sig means homoscedasticity, which is what we want

# or White's test for heteroscedasticity. The following gives a heteroscedasticity corrected covariance matrix.

coeftest(model5, vcov=hccm(model5))

# you can also look at the studentised residuals plot above. If there is homoscedasticity the spread of the studentised residuals will not increase or decrease as you move across the predicted values.

## Non-constant error variance. Constant error variance implies homoscedasticity, which we want. This is a test where the null hypothesis is constant error variance (i.e. equal spread of error variance around the predictors) therefore we WANT this test to be non-significant.

# non-constant error variance test. 

ncvTest(model5)


# plot sudentised residuals vs fitted values. An increasing trend in this plot would be saying that the absolute residuals are getting larger as the fitted values do -  indicating a spread that's related to the mean, a violation of the assumptions (the plot is used to assess potential heteroskedasticity). If no slope then even distribution of error across the IVs

spreadLevelPlot(model5)




###################################################################################################### Assumption 4: Assumption of non-colinearity ###########################################################################################################################

# multicolinearity occurs when you have 2 or more IVs that correlate highly with eachother. This will cause problems in demarcating which variables contribute how much of the variance in the DV

# there are two ways to test this, correlations and tolerance

# the method below yields correlations as Matrix first, and then same matrix as p-values

############## correlations. 

# If any of these are over r=.7 then you have mutlicollinearity

library(Hmisc)

IVs <- data.frame(caffReg$totCaffeine, caffReg$numQuitAttempts, caffReg$yearsCaffUse, caffReg$numCupsCoffee, caffReg$B1Total)

corsMatrix <- rcorr(as.matrix(IVs)) # needs to be a matrix not a dframe for this to work

###### tolerance and VIF

# Tolerance and VIF are two sides of the same things (VIF is 1/tolerance). If tolerance is <0.1 and VIF > 10 then you might havs a collinearity problems

vif(model5) # VIF = variance inflation factors



################################################################################################################ Assumption 5: No highly influential observation #####################################################################################################################



########## Testing for outliers

# qqplot for studentised residuals. You want the data to stick as close to the diagonal as possible
qqPlot(model5) 

# studentised deleted residuals. You are looking for any values greater than + or - 3

rstudent(model5)
         

# check leverage plots. Points that are further out along the extremes of x will push harder on the lever (i.e. the regression line) and thus will have more leverage. 

leveragePlots(model5)



########## Testing for influential observations

## added variable plots

avPlots(model5)

## Cook's distance measures the effect of deleting an observation. Data points with large residuals and high leverage may distort the outcome and accuracy of a regression. It is calculated as (for each observation) D = ((predicted Y from full model) - (predicted Y from refitted model with the observation ommitted))/ (number of parameters for model)*MSE(model).

# Some texts say that a observations with a cook's distance of >1 are considered to be inlfuential. Others identify D values > 4/(n-k-1) where n is number of obervations in the model and k is number of predictors. D values represent the distance one's estimates move within the confidence ellipsoid that represents a region of plausible values for the parameter (i.e. change in regression paramters when one exlcudes vs includes the observation)

cutoff <- 4/(nrow(caffReg) - length(model5$coefficients) - 1) # uses the element 'coefficients inside the model 5 object

# now plot cook's distance. Cook's distance values above 1 should be checked out

plot(model5, which = 4, cook.levels = cutoff)


## influence plot (Note: press esc to exit graph)

influencePlot(model5, id.method = "identify", main = "Influence Plot",
              sub = "Circle size is proportional to Cook's distance")


####################################################################################################  Assumption 6: Normality of residuals (i.e. error of prediciton) ####################################################################################

# distribution of studentised residuals in histogram. The histogram should be approximately normally distributed (which it sort of is) and the mean should be close to 0 and the SD close to 1.

sresid <- studres(model5)

hist(sresid, freq = FALSE, main = "DIstribution of Studentised Residuals")

xModel5 <- seq(min(sresid), max(sresid, length = 40))
yModel5 <- dnorm(xModel5)
lines(xModel5, yModel5)



# qq plot for studentised residuals. To confirm this you should look at a P-Plot. If the residuals are normally distributed the points should cluster tightly around the diagonal line. Regression is quite robust to violations of normality so the points don’t have to be perfectly aligned for the regression to go ahead. The further away from the diagonal the less normal the distribution of studentised residuals

qqPlot(model5, main = "QQ-Plot")








####################################################################################################### Assumption 7: Linearity and additivty #######################################################################################################################

# A key assumption behind using linear regression models for purposes of inference or prediction is that there is linearity and additivity of the relationship between dependent and independent variables:

#(a) The expected value of dependent variable is a straight-line function of each independent variable, holding the others fixed.

#(b) The slope of that line does not depend on the values of the other variables.

#(c)  The effects of different independent variables on the expected value of the dependent variable are additive



######## component + residual plot. Component residual plots, an extension of partial residual plots, are a good way to see if the predictors have a linear relationship to the dependent variable. A partial residual plot essentially attempts to model the residuals of one predictor against the dependent variable. A component residual plot adds a line indicating where the line of best fit lies. A significant difference between the residual line and the component line indicates that the predictor does not have a linear relationship with the dependent variable (which is a violation of the linearity assumption)

crPlots(model5)

# Ceres plots also test for linearity. Looks much the same as above.

ceresPlots(model5)


### the gvlma package performs a global validation of linear model assumptions as well as separate evaluationsof skewness kurtosis and heteroscedasticity

library(gvlma)

gvModel5 <- gvlma(model5)

summary(gvModel5)


