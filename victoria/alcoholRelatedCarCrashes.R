
setwd("~/DataScienceAssignments2017/AssignmentVictoria/")

#library(gdata)
library(xlsx)
library(MASS)
#install.packages("rJava", dependencies = TRUE)
#install.packages("xlsx", dependencies = TRUE)
###################################################################
# 1. Identify demographic characteristics of the drivers that are #
# risk (or protective) factors of car accidents.                  #
###################################################################

# Load the data into a variable.
data_car_accidents <- read.xlsx("Data_Car_accidents.xlsx", 1)
data <- data_car_accidents
data$Accident <- as.numeric(data_car_accidents$Accident)
data$Accident[data$Accident == 1] <- 0
data$Accident[data$Accident == 2] <- 1
View(data)
# To identify the demographic characteristics of the drivers that are
# risk factors or protective for car accidents we can use glm for this.
# We do this for all combinations:
#                                 - Age and Accident
#                                 - Gender and Accident
#                                 - SocioeconmicStatus and Accident

# Age and Accident
accident_age <- glm(formula = Accident ~ Age, family = "binomial", data = data)
summary(accident_age)

# The logarithmic coefficient of age is 0.04298 with the p-value < 0.05
age_coefficient <- coefficients(accident_age)[2]

# The logarithmic confidence interval is [0.025, 0.062]
age_ci <- confint(accident_age, parm = c("Age"), level=0.95)
age_ci

# The natural coefficient (1.044) and confidence interval [1.026, 1.064]
exp(age_coefficient)
exp(age_ci)

# Gender and Accident
accident_gender <- glm(formula = Accident ~ Gender, family = "binomial", data = data)
summary(accident_gender)

# The logarithmic coefficient of gender is 1.5604 with the p-value < 0.05
gender_coefficient <- coefficients(accident_gender)[2]
gender_ci <- confint(accident_gender, parm = c("GenderMale"), level = 0.95)

# The natural coefficient (4.761) and confidence interval [2.348, 10.377]
exp(gender_coefficient)
exp(gender_ci)

# Socioeconomic status and accident
accident_socio <- glm(formula = Accident ~ Socioeconomic_status, family = "binomial", data = data)
summary(accident_socio)

# The logarithmic coefficient of social status is 0.0711 ad 0.1951 with the p-value > 0.05
socio_coefficient <- coefficients(accident_socio)
socio_ci <- confint(accident_socio, parm = c("Socioeconomic_statusMiddle", "Socioeconomic_statusUpper"), level = 0.95)
# The natural coefficient is 1.074 and 1.215 and confidence interval [0.470, 2.416] and [0.596, 2.502]
exp(socio_coefficient)
exp(socio_ci)

##########################################################################
# 2. Obtain the model relating BAC and car accidents (both, not adjusted #
# and adjusted for confounders). Interpret the not‐adjusted and adjusted #
# odds ratios. Is there a significant association between BAC and car    # 
# accidents?                                                             #
##########################################################################

# We find confounders for the adjusted model by checking if they fulfill the three confounder conditions
# - Associated with BAC
# - Risk factor for Accident (independent of BAC)
# - Not intermediate factor (BAC is not depended on potential confounder)
# The potential confounder: Gender, Age, Socioeconomin status

# Condition 1:
# Find association between each potential confounder and BAC by doing a general logistic model

# Age and BAC
bac_age <- glm(formula = BAC ~ Age, data = data)
summary(bac_age)

# Gender and BAC
bac_gender <- glm(formula = BAC ~ Gender, data = data)
summary(bac_gender)

# There is a significant association between age and BAC, and between gender and BAC. 
# This fulfills the first condition

# Condition 2:
# Both age and gender were shown in the previous exercise to be significantly associated with Accidents 
# independently from BAC. This means they both are risk factors for Accident and fulfills the second condition.

# Condition 3:
# Age and Gender are not intermediate factors, because intuitively blood-alcohol content cannot affect
# the age or gender

# All the confounder condition are by age and gender and can therefore be considered confounders

# Unadjusted Model
accident_bac <- glm(formula = Accident ~ BAC, family = "binomial", data = data)
summary(accident_bac)
# The logarithmic coefficient of BAC is 4.00 with the p-value < 0.05
bac_coefficient <- coefficients(accident_bac)[2]
bac_coefficient
# The logarithmic confidence interval is [3.03, 5.14]
bac_ci <- confint(accident_bac, parm = c("BAC"), level=0.95)
bac_ci
# The natural coefficient (54.70) and confidence interval [20.77, 171.21]
exp(bac_coefficient)
exp(bac_ci)

# Adjusted Model
accident_bac_adjusted <- glm(formula = Accident ~ BAC + Gender + Age, family = "binomial", data = data)
summary(accident_bac_adjusted)

# The logarithmic coefficient of BAC is 3.78, Gender is 0.92 and Age is 0.022 with the p-value < 0.05
adjusted_coefficient <- coefficients(accident_bac_adjusted)
adjusted_coefficient
# The logarithmic confidence interval of BAC is [2.77, 4.97], Gender is [-0.054, 1.93] and Age is [-0.0018, 0.047]
adjusted_ci <- confint(accident_bac_adjusted, parm = c("BAC", "GenderMale", "Age"), level=0.95)
adjusted_ci
# The natural coefficient of BAC is 43.76, Gender is 2.51 and Age is 1.02 
# and confidence interval for BAC is [15.90, 144.08], Gender is [0.95, 6.91] and Age is [1.00, 1.05]
exp(adjusted_coefficient)
exp(adjusted_ci)

# Conclusion: 
# Unadjusted model shows the BAC odds ratio is 54.7
# When BAC is increased by 1 the subject has 54.7 times higher odds of getting into an accident.
# Adjusted model shows the BAC odds ratio is 43.7
# When BAC is increased by 1 the subject has 43.7 times higher odds of getting into an accident.

########################################################################
# 3. Is there any other potential confounder (not included in the file #
# ”Data_car_accidents”) that should have been considered in the study? #
# How would you include it in the analysis?                            #
########################################################################

# Weight (scale)
# Poor Vision (Yes/No)
# Medication not appropriate for driving (Yes/No)
# Physical condition (Good/Bad)

######################################################################
# 4. Plot the unadjusted (crude) and adjusted models. Comment on the #
# similarities or differences between the two.                       #
######################################################################

# Unadjusted model
plot(data$BAC, fitted.values(accident_bac))
# Adjusted model
plot(data$BAC, fitted.values(accident_bac_adjusted))

# The unadjusted plot shows that the model is perfectly fittet to only BAC
# The adjusted plot shows that the model is not perfectly fittet to BAC, because the fit also depend on other variables

##################################################################
# 5. What is the probability that a 40 yr male whose BAC is >1‰, #
# causes a car accident? What will be the probability, 10, 20,   #
# 30 and 40 years later? Is this change linear?                  #
##################################################################
summary(accident_bac_adjusted)

accident_predict = function(x) {
  years_0 <- data.frame(BAC = 0.0, Age = x, Gender = "Male")
  years_1 <- data.frame(BAC = 1.0, Age = x, Gender = "Male")
  result <- 1-(predict(accident_bac_adjusted, years_0, type="response")[1] + predict(accident_bac_adjusted, years_1, type="response")[1])  
  return(result)
}

age <- c(40,50,60,70)
p1 <- accident_predict(40)
p1
p2 <- accident_predict(50)
p2
p3 <- accident_predict(60)
p3
p4 <- accident_predict(70)
p4
p5 <- accident_predict(80)
p5
accident <- c(p1-p2,p2-p3,p3-p4,p4-p5)
plot(age,accident)

# The 

###################################################################
# 6. We obtain information on a new set of drivers (17 subjects). #
# Evaluate the predictive performance of the model by calculating #
# the accuracy, sensitivity, specificity and precision of the     #
# model using this new dataset. Consider the threshold value for  #
# the probability as equal to 0.5. The data for the 17 subjects   #
# is in the Excel file ”Data_car_accidents” (sheet: ”Data_17_     #
# subjects”).                                                     #
###################################################################

newData <- read.xlsx("Data_Car_accidents.xlsx", sheetIndex=2)
newData$Accident <- as.numeric(newData$Accident)
newData$Accident[newData$Accident == 1] <- 0
newData$Accident[newData$Accident == 2] <- 1
View(newData)
pred <- predict(accident_bac_adjusted, newData, type="response")
pred
str(data)
str(newData)

pred.glm <- rep(0, length(pred))
pred.glm[pred > 0.5] <- 1
table(newData$Accident, pred.glm)

TN <- 8
FN <- 4
FP <- 1
TP <- 4
#accuracy (0.71)
(TN+TP)/(TN+FP+FN+TP)
#precision (0.8)
TP/(FP+TP)
#sensitivity (0.5)
TP/(TP+FN)
#specificity (0.89)
TN/(TN+FP)