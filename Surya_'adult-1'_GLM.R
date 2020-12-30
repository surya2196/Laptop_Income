
# Getting the packages using a function

pak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("caret", "caTools", "gridExtra", "ggplot2", "ModelMetrics", "carData", "MASS", "BaylorEdPsych", "car", 
              "corrgram", "ROCR", "pROC")
pak(packages)


# Importing the data

setwd ("C:\\Users\\surya\\Downloads")
data1 = read.csv('adult-1.csv', stringsAsFactors = TRUE)
age = data1[,1]
wclass=data1[,2]
fwt=data1[,3]
edu=data1[,4]
nedu=data1[,5]
mar=data1[,6]
occ=data1[,7]
rel=data1[,8]
race=data1[,9]
sex=data1[,10]
gcap=data1[,11]
lcap=data1[,12]
hpw=data1[,13]
nat=data1[,14]
inc=data1[,15]
str(data1)


# Fitting the model

fit1 = glm(inc ~ age + fwt + nedu + 
             gcap + lcap + hpw + 
             sex + occ, family =
             binomial)

# Questions 1 and 2 - Null and Residual deviance values

summary(fit1)

# Question 3 - Value of Pseudo R square

PseudoR2(fit1)

# Question 4 - Odds Ratio

exp(coef(fit1))


# Finding the accurate prediction

pred1=predict(fit1)

pred=predict(fit1,type = "response")

yhat <- ifelse(fit1 $fitted.values >= 0.5, ">50K", "<=50K")

tab = table(inc, yhat)

accuracy = sum(diag(tab))/sum(tab) * 100

# Value of question 5

accuracy               

# Fitting 8 differnt models

fit_age = glm(inc ~ age, family =
                binomial)

fit_fwt = glm(inc ~ fwt, family =
                          binomial)

fit_nedu = glm(inc ~ nedu, family =
                           binomial)

fit_gcap = glm(inc ~ gcap, family =
                binomial)

fit_lcap = glm(inc ~ lcap, family =
                binomial)

fit_hpw = glm(inc ~ hpw, family =
                binomial)

fit_sex = glm(inc ~ sex, family = binomial)

fit_occ = glm(inc ~ occ, family =
                binomial)

fit_age$aic
fit_fwt$aic
fit_nedu$aic
fit_gcap$aic
fit_lcap$aic
fit_hpw$aic
fit_sex$aic
fit_occ$aic

# fit_age$aic = 51217.73; fit_fwt$aic = 53752.71; fit_nedu$aic = 47778.69; fit_gcap$aic = 48703.58; 
# fit_lcap$aic = 52853.07; fit_hpw$aic = 51164.23; fit_sex$aic = 51270.45; fit_occ$aic = 47560.92

# Question 6 - Occupation seems to be the predictor that best explains the income level

# Question 7 - aic vaues of Occupation < Education.num < Capital.gain < Hours.per.week

# Checking for multi-collinearity

sapply(data1, is.factor)
cor(data1 [sapply(data1, function(x) !is.factor(x))])

# The correlation between Capital.loss and hours.per.week isn't very strong.

table(edu,nedu)

# The correlation between Education and Education.num is very very strong (=1).

table(nat,sex)

# The correlation is not as strong as the one for Education.

# Question 8 - Hence, If we had fitted the model with all 14 variables, educational.num and education are the most likely to be collinear. 