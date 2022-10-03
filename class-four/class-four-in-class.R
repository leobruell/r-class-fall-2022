# Class Four Notes 

# Regressions Intro ####
library(ggplot2)
data(tips, package='reshape2')
head(tips)     
dim(tips)
ggplot(tips, aes(x=total_bill, y=tip)) + geom_point() + geom_smooth(method='lm')

tip1 <- lm(tip ~ total_bill, data=tips)
# Terminology: y and x, response and predictor, target and feature.
# Professor doesn't like independent and dependent. 
# R is built on top of C and 4train
tip1
# Stats: coefficients, intercepts, slopes
# CS: weights, bias
# These are the same things, just different terms

summary(tip1)
# Residuals = measure of how much each value differs from the fitted values (error, more or less). 
# p-value: there is an arbitrary cutoff of .05, but this doesn't necessarily make sense. 

tip2 <- lm(tip ~ total_bill + size, data=tips)
tip2
summary(tip2)
# Size might not be a good thing to put in because size and bill are most likely highly correlated, which is bad. 
tips |> head()
tips |> dplyr::count(day)
tip3 <- lm(tip ~ size + total_bill + day, data=tips)
#dummy variables are also called indicator variables
tip3
summary(tip3)
# R-squared is not a good measure of fit, as it leads to over fitting. 
# Adjusted r-squared is a little better, but it is not adjusted enough. 
# P values of individual coefficients are not super important, but p-value of the entire model can be useful.
# Focusing too much on p-value is not good though as it can lead to people forcing their models to meet an arbitrary threshold. 

library(coefplot)
coefplot(tip3) #plots the coefficients 

coefplot(tip3, sort='magnitude')

library(broom)
tidy(tip3)
coefplot(tip3, sort='magnitude',interactive=TRUE)

#AIC (Akaike information criterion), lower is better. This is especially valuable for time series analysis
AIC(tip1, tip2, tip3)

#BIC
BIC(tip1, tip2, tip3)

new_diner <- data.frame(
    total_bill=75, size=3
)
new_diner
predict(tip2, newdata = new_diner)
#Generalized Additive Models = something to research later. 

# Interaction Terms ####
tip4 <- lm(tip ~ total_bill * size + day, data=tips)
tip4.1 <- lm(tip ~ total_bill + size + total_bill:size + day, data=tips)
tip4.1
BIC(tip1, tip2, tip3, tip4)




# Logistic Regression ####
tips |> head()
tip5 <- glm(tip ~ total_bill * size + day, data=tips) #generalized linear model, this is the same as lm with no specifications.
tip5

sex1 <- glm(sex ~ tip + day + size, data=tips, family=binomial)
sex1
summary(sex1)
#Results are reported on the odd-log scale. 

new_diner_3 <- data.frame(tip=c(5.7, 17.3), day=c('Thur', 'Fri'), size=c(1,3))
new_diner_3                          
predict(sex1, newdata = new_diner_3)
#The output is the log-odds of getting a male (the second outcome alphabetically). 
# These are hard to interpret, but we can transform it with inverse logit (sigmoid) to represent probability. 
invlogit(predict(sex1, newdata = new_diner_3))

predict(sex1, newdata = new_diner_3, type='response') #this gives the probability directly because response type is binary.
BIC(sex1) #This is not helpful because BIC is relative. 
#Logloss = a measure of how wrong a model is by comparing predcited probabilities with the actual results. 


# Count data ####
# Integer data like number of children, accidents, etc. 
# We should treat this differently when it is our predictor variable
# We can use Poisson regression

size1 <- glm(size ~ day + tip, data=tips, family=poisson)
size1
predict(size1, newdata = new_diner_3)
predict(size1, newdata = new_diner_3, type='response')
#With Poisson, you have to worry about over dispersion as in poisson mean = sd. We can use quasi-poisson instead

size1.2 <- glm(size ~ day + tip, data=tips, family=quasipoisson)
size1.2
multiplot(size1, size1.2) 
summary(size1.2) #if the dispersion parameter is greater than 1, it is a good idea to use the quasipoisson. 
# Conversely, if it is less than 1, you can just use the regular poisson model. 

tips |> tibble::as.tibble() |> 
    dplyr::mutate(new_col = size >=4)

tips |> tibble::as.tibble() |> 
    dplyr::mutate(weeknd = day %in% c('Sat','Sun'))




# Model Workflow ####
data(credit_data, package='modeldata')
credit <- tibble::as.tibble(credit_data)
credit
# Make train and test 
credit_split <- initial_split(credit, prop = .85, strata = 'Status') #this does split randomly
credit_split
train <- training(credit_split)
test <- testing(credit_split)

# Cross validation
library(rsample)
cv_splits <- vfold_cv(train, v=5, strata = 'Status') #10 is standard for v, strata is to try to get equal amounts of status in each fold.
cv_splits
#xgboost is a really good random tree model. 

