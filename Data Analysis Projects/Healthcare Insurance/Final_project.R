setwd("/Users/amard/Library/CloudStorage/OneDrive-William&Mary/W&M Materials & Courses/MATH_352_statistical_data_analysis/Final_project")

# Packages
install.packages("car")
library("car")
library(glue)
install.packages("caret")
library(caret)
install.packages("tidyverse")
library(tidyverse)

# Healthcare Insurance Analysis:
#NOTE: just focus on producing a best fit linear model to predict data by doing a cross-validation
################################################################################################
################################################################################################
#y - charges
insurance <- read.csv("insurance.csv") #reading in dataframe
insurance
dim(insurance) #looking at the dimensions 
colnames(insurance) #all columns

################################################################################################
################################################################################################
# Some helpful functions to make the process run faster:

num_betas <- function(model){ #returns the number of beta coefficients in the model
  paste(length(model$coefficients))
}

anova_hconclusion <- function(reduced, complete, alpha){ #runs a nested F-test on the models of choice
  
  #checking if the reduced model class is a list of models
  #the argument input: list(list('modelname' = model), etc)
  if (class(reduced) == 'list'){
    
    #look at each model in the list individually
    for (m in seq_along(reduced)){
      
      #calculate p-value from anova
      pval <- anova(reduced[[m]][[1]], complete)$"Pr(>F)"
      pval <- pval[2]
      
      #test p-value and draw conclusion
      if (pval > alpha){
        
        print(glue('Complete: {deparse(substitute(complete))}
                    Reduced: {names(reduced[m][[1]])}
                    anova({names(reduced[m][[1]])}, {deparse(substitute(complete))}) 
                    Global F p-value: {signif(pval, 4)}
                    Conclusion: fail to reject the null, all extra beta terms in complete model are not significant for predicting y
                    Model to use: {names(reduced[m][[1]])}'))
        cat('\n') #spacing between outputs
      }
      else{
        print(glue('Complete: {deparse(substitute(complete))}
                    Reduced: {names(reduced[m][[1]])}
                    anova({names(reduced[m][[1]])}, {deparse(substitute(complete))}) 
                    Global F p-value: {signif(pval, 4)}
                    Conclusion: reject the null, all extra beta terms in complete model are significant for predicting y
                    Model to use: {deparse(substitute(complete))}'))
        cat('\n')
      }
    }
  }
  
  #checking if the reduced model class is linear, only one model
  if (class(reduced) == 'lm'){
    pval <- anova(reduced, complete)$"Pr(>F)"
    pval <- pval[2]
    
    if (pval > alpha){
      print(glue('Complete: {deparse(substitute(complete))}
                    Reduced: {deparse(substitute(reduced))}
                    anova({deparse(substitute(reduced))}, {deparse(substitute(complete))}) 
                    Global F p-value: {signif(pval, 4)}
                    Conclusion: fail to reject the null, all extra beta terms in complete model are not significant for predicting y
                    Model to use: {deparse(substitute(reduced))}'))
    }
    else{
      print(glue('Complete: {deparse(substitute(complete))}
                    Reduced: {deparse(substitute(reduced))}
                    anova({deparse(substitute(reduced))}, {deparse(substitute(complete))}) 
                    Global F p-value: {signif(pval, 4)}
                    Conclusion: reject the null, all extra beta terms in complete model are significant for predicting y
                    Model to use: {deparse(substitute(complete))}'))
    }
  }
}

cv <- function(model){ #calculates the coefficient of variation
  #(residual error/mean of y) *100
  print((summary(model)$sigma/mean(insurance$charges)) * 100)
}


################################################################################################
################################################################################################
# 1 - We want to see which variables are important for the model by 
# Applying stepwise regression:
min.model = lm(charges ~1, data = insurance)
step(min.model,
     scope = (~age + sex + bmi + children + smoker + region),
     data = insurance,
     direction = 'forward') #adds a variable to the model if it's AIC is the smallest

# the model shows 'smoker', 'age', bmi', 'children', and 'region' as important variables
# since our goal is to only produce the best predictive model, we think that including 'sex' is an important
#factor for affecting the insurance charge

################################################################################################
################################################################################################
# 2 - We want to see the relationship of the quantitative variables to y to determine what order should be used:
par(mfrow = c(2, 2), mar = c(6, 6, 0.5, 0.5))
plot(insurance$age, insurance$charges, xlab = 'Age', ylab = 'Insurance cost') #looks curvilinear
plot(insurance$bmi, insurance$charges, xlab = 'BMI', ylab = 'Insurance cost') #looks linear
plot(insurance$children, insurance$charges, xlab = 'Children', ylab = 'Insurance cost') #looks curvilinear
dev.off()

################################################################################################
################################################################################################
# 3 - We want to build a hypothesized model:
# Start with a base model:
model1 <- lm(charges ~ age + children + bmi + age*children + age*bmi + children*bmi + I(age^2) + I(children^2)
                       + sex + smoker + region + sex*smoker + sex*region + smoker*region
                       + age*sex + age*smoker + age*region + age*sex*smoker + age*sex*region + age*smoker*region
                       + children*sex + children*smoker + children*region + children*sex*smoker + children*sex*region + children*smoker*region
                       + bmi*sex + bmi*smoker + bmi*region + bmi*sex*smoker + bmi*sex*region + bmi*smoker*region
                       + age*children*sex + age*children*smoker + age*children*region + age*children*sex*smoker + age*children*sex*region + age*children*smoker*region
                       + age*bmi*sex + age*bmi*smoker + age*bmi*region + age*bmi*sex*smoker + age*bmi*sex*region + age*bmi*smoker*region
                       + children*bmi*sex + children*bmi*smoker + children*bmi*region + children*bmi*sex*smoker + children*bmi*sex*region + children*bmi*smoker*region
                       + I(age^2)*sex + I(age^2)*smoker + I(age^2)*region + I(age^2)*sex*smoker + I(age^2)*sex*region + I(age^2)*smoker*region
                       + I(children^2)*sex + I(children^2)*smoker + I(children^2)*region + I(children^2)*sex*smoker + I(children^2)*sex*region + I(children^2)*smoker*region,
             data = insurance) #117 betas
#note: this model has 117 betas including the intercept, need to reduce the model to be computational efficient

# Other models:
#removing all quadratic terms
model2 <- lm(charges ~ age + children + bmi + age*children + age*bmi + children*bmi
             + sex + smoker + region + sex*smoker + sex*region + smoker*region
             + age*sex + age*smoker + age*region + age*sex*smoker + age*sex*region + age*smoker*region
             + children*sex + children*smoker + children*region + children*sex*smoker + children*sex*region + children*smoker*region
             + bmi*sex + bmi*smoker + bmi*region + bmi*sex*smoker + bmi*sex*region + bmi*smoker*region
             + age*children*sex + age*children*smoker + age*children*region + age*children*sex*smoker + age*children*sex*region + age*children*smoker*region
             + age*bmi*sex + age*bmi*smoker + age*bmi*region + age*bmi*sex*smoker + age*bmi*sex*region + age*bmi*smoker*region
             + children*bmi*sex + children*bmi*smoker + children*bmi*region + children*bmi*sex*smoker + children*bmi*sex*region + children*bmi*smoker*region,
             data = insurance) #91 betas

#removing all quantitative*qualitative interactions
model3 <- lm(charges ~ age + children + bmi + age*children + age*bmi + children*bmi + I(age^2) + I(children^2)
             + sex + smoker + region + sex*smoker + sex*region + smoker*region,
             data = insurance) #21 betas

#removing all quadratic*qualitative interactions
model4 <- lm(charges ~ age + children + bmi + age*children + age*bmi + children*bmi + I(age^2) + I(children^2)
             + sex + smoker + region + sex*smoker + sex*region + smoker*region
             + age*sex + age*smoker + age*region + age*sex*smoker + age*sex*region + age*smoker*region
             + children*sex + children*smoker + children*region + children*sex*smoker + children*sex*region + children*smoker*region
             + bmi*sex + bmi*smoker + bmi*region + bmi*sex*smoker + bmi*sex*region + bmi*smoker*region
             + age*children*sex + age*children*smoker + age*children*region + age*children*sex*smoker + age*children*sex*region + age*children*smoker*region
             + age*bmi*sex + age*bmi*smoker + age*bmi*region + age*bmi*sex*smoker + age*bmi*sex*region + age*bmi*smoker*region
             + children*bmi*sex + children*bmi*smoker + children*bmi*region + children*bmi*sex*smoker + children*bmi*sex*region + children*bmi*smoker*region,
             data = insurance) #93 betas

################################################################################################
################################################################################################
# 4 - We want to perform nested tests to see which model is the best:
#model1 - complete
#model(i) - reduced
#Ho: all added beta terms in model1 = 0
#Ha: at least one Bi in the added terms in model1 != 0

model_list <- list(list('model2' = model2),
                   list('model3' = model3),
                   list('model4' = model4))

anova_hconclusion(model_list, model1, alpha = 0.05)

################################################################################################
################################################################################################
# 4 continued:
# We see that model4 was the best, now from model4, try to see if it can be reduced further:
model4 <- lm(charges ~ age + children + bmi + age*children + age*bmi + children*bmi + I(age^2) + I(children^2)
             + sex + smoker + region + sex*smoker + sex*region + smoker*region
             + age*sex + age*smoker + age*region + age*sex*smoker + age*sex*region + age*smoker*region
             + children*sex + children*smoker + children*region + children*sex*smoker + children*sex*region + children*smoker*region
             + bmi*sex + bmi*smoker + bmi*region + bmi*sex*smoker + bmi*sex*region + bmi*smoker*region
             + age*children*sex + age*children*smoker + age*children*region + age*children*sex*smoker + age*children*sex*region + age*children*smoker*region
             + age*bmi*sex + age*bmi*smoker + age*bmi*region + age*bmi*sex*smoker + age*bmi*sex*region + age*bmi*smoker*region
             + children*bmi*sex + children*bmi*smoker + children*bmi*region + children*bmi*sex*smoker + children*bmi*sex*region + children*bmi*smoker*region,
             data = insurance) #93 betas

#removing qualitative*qualitative interactions
model5 <- lm(charges ~ age + children + bmi + age*children + age*bmi + children*bmi + I(age^2) + I(children^2)
             + sex + smoker + region
             + age*sex + age*smoker + age*region
             + children*sex + children*smoker + children*region
             + bmi*sex + bmi*smoker + bmi*region
             + age*children*sex + age*children*smoker + age*children*region
             + age*bmi*sex + age*bmi*smoker + age*bmi*region
             + children*bmi*sex + children*bmi*smoker + children*bmi*region,
             data = insurance) #44 betas

# Nested F-test:
#model4 - complete
#model(i) - reduced
#Ho: all added beta terms in model4 = 0
#Ha: at least one Bi in the added terms in model4 != 0

anova_hconclusion(model5, model4, alpha = 0.05)

################################################################################################
################################################################################################
# 4 continued:
# Model5 was the best, same process:
model5 <- lm(charges ~ age + children + bmi + age*children + age*bmi + children*bmi + I(age^2) + I(children^2)
             + sex + smoker + region
             + age*sex + age*smoker + age*region
             + children*sex + children*smoker + children*region
             + bmi*sex + bmi*smoker + bmi*region
             + age*children*sex + age*children*smoker + age*children*region
             + age*bmi*sex + age*bmi*smoker + age*bmi*region
             + children*bmi*sex + children*bmi*smoker + children*bmi*region,
             data = insurance) #44 betas

#removing quantitative*quantitative interactions
model6 <- lm(charges ~ age + children + bmi + I(age^2) + I(children^2)
             + sex + smoker + region
             + age*sex + age*smoker + age*region
             + children*sex + children*smoker + children*region
             + bmi*sex + bmi*smoker + bmi*region,
             data = insurance) #26 betas

#removing all quadratic terms
model7 <- lm(charges ~ age + children + bmi
             + sex + smoker + region
             + age*sex + age*smoker + age*region
             + children*sex + children*smoker + children*region
             + bmi*sex + bmi*smoker + bmi*region,
             data = insurance) #24 betas

# Nested F-test:
#model5 - complete
#model(i) - reduced
#Ho: all added beta terms in model5 = 0
#Ha: at least one Bi in the added terms in model5 != 0

model_list <- list(list('model6' = model6),
                   list('model7' = model7))

anova_hconclusion(model_list, model5, alpha = 0.05)

################################################################################################
################################################################################################
# 4 continued:
# Model 6 is the best model, one last time to try and reduce it:
model6 <- lm(charges ~ age + children + bmi + I(age^2) + I(children^2)
             + sex + smoker + region
             + age*sex + age*smoker + age*region
             + children*sex + children*smoker + children*region
             + bmi*sex + bmi*smoker + bmi*region,
             data = insurance) #26 betas

#terminology: standard error = residual standard error (not residual error)
summary(model6) #the standard error looks to be large
cv(model6) #another indication that the standard error is not relatively small (should less than 10%)

################################################################################################
################################################################################################
# 5 - From the large standard error, run residual analyses to see if any assumptions are violated:
yhat_6 <- predict(model6)
plot(yhat_6, model6$residuals, xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'lightseagreen')
# the residuals do not look heteroscedastic violating the constant variance assumption and almost displays a cone-like shape suggesting a 
#log transformation to stabilize the variance

#normality plot
qqnorm(model6$residuals)
qqline(model6$residuals)
#normality assumption violated (place this in slideshow)

################################################################################################
################################################################################################
# 6 - now we look at the log transformation of the y variable to get constant variance:

#since the model is now multiplicative, the y variable represents the 
#percent change in insurance charges for every 1 unit change in x
model8 <- lm(log(charges) ~ age + children + bmi + I(age^2) + I(children^2)
             + sex + smoker + region
             + age*sex + age*smoker + age*region
             + children*sex + children*smoker + children*region
             + bmi*sex + bmi*smoker + bmi*region,
             data = insurance) #26 betas

#from the summary output, the residual errors (s) is relatively smaller--from the coefficient of variation--in model8 meaning that
#the actual insurance charges are closer to their predicted values
(summary(model8)$sigma/mean(insurance$log_charges)) * 100 #(run this after code on line 308-309)

#residual analyses (put this on final slide of presentation)
#although the residuals suggest that it should be transformed, this is due to outliers in the data
yhat_8 <- predict(model8)
plot(yhat_8, model8$residuals, xlab = 'Fitted Values', ylab = 'Residuals') 
abline(h = 0)

qqnorm(model8$residuals)
qqline(model8$residuals)
#same issue, violated

################################################################################################
################################################################################################
# 7 - looking at the model's prediction

#prediction table: 
insurance <- transform(insurance, log_charges = log(charges)) #new column of transformed values
prediction_table_log <- cbind(insurance$log_charges, predict(model8, interval = "prediction"))
data.frame(prediction_table_log) #converting matrix to dataframe

prediction_table <- transform(prediction_table_log, V1 = exp(V1),
                                                    fit = exp(fit),
                                                    lwr = exp(lwr),
                                                    upr = exp(upr))
prediction_table

#prediction plot: 

#percentage
plot(predict(model8), insurance$log_charges, xlab = "Predicted Values", ylab = "Observed Values")
abline(a = 0, b = 1, lwd = 2, col = 'lightseagreen')

#actual counts
plot(prediction_table$V1, prediction_table$fit, xlab = "Predicted Values", ylab = "Observed Values")
abline(a = 0, b = 1, lwd = 2, col = 'lightseagreen')


# the model looks to do well with predicting insurance charges with the exception of having many 
#outliers