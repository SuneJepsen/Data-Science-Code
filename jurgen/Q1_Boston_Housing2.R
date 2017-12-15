library(MASS)
library(corrplot)

#######################
# Logistic regression
#######################

#?Boston
#max(Boston$crim)
#head(Boston)
#str(Boston)
summary(Boston)

# Load Boston data set
data("Boston")

# Create list with same size as crime coloum with all zeros
iscrime <- rep(0, length(Boston$crim))

# Insert 1's where crime is over median 
iscrime[Boston$crim > median(Boston$crim)] <- 1

# Add iscrime coloum to Boston dataset
Boston <- data.frame(Boston, iscrime)

summary(Boston)

# Set random number generator in R
set.seed(1234)

# Create training data - 70% training  data and 30% test data
train <- sample(1:dim(Boston)[1], dim(Boston)[1]*.7, rep=FALSE)

# Create test data (-train means all not all train data)
test <- -train

# Add training data to variable
Boston.train <- Boston[train, ]

# Add test data to variable (all data which is not train)
Boston.test <- Boston[test, ]

# From the the crime data, add the only test data to iscrime.test variable
iscrime.test <- iscrime[test]

# Generate logistic regression model
# GLM Parameter explaination
# iscrime: The variable we want to predict
# ~ .: all variables in Boston dataset is set as predictors 
# - iscrime -crim: remove iscrime and crim colums as predictors
# data = Boston: use Boston data set
# binomial: logit
fit.glm1 <- glm(iscrime ~ . -crim, family = binomial(logit), data = Boston)
#fit.glm1 <- glm(iscrime ~ -crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv + rvYes, family = binomial(logit), data = Boston)
#fit.glm1 <- glm(iscrime ~ -crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, family = binomial(logit), data = Boston)
probs <- predict(fit.glm1, Boston.test, type = "response")

# Create list with same size as probs with all zeros 
pred.glm <- rep(0, length(probs))

# Insert 1's where probability is larger than 0.5    
pred.glm[probs > 0.5] <- 1

# Confusion matrix in order to determine how many observations were correctly or incorrectly classified
# Correct prediction: 65 suburbs with no crimes over median and 68 suburbs with crimes over median
# wrong prediction: 15 which the model predicted to no crime had crime. 4 which the model predicted had crime which hadnt crime
table(pred.glm, iscrime.test)

# Computes the rate of misclassification (0.112)
mean(pred.glm != iscrime.test)
# Summary of the models coefficients and p-values
summary(fit.glm1)

# Correlation matrix
# Find candidates which indicates the positive correlation on crime
corrplot::corrplot.mixed(cor(Boston[, -1]), upper="circle")

# Generate logistic regression model
# GLM Parameter explaination
# iscrime: The variable we want to predict
# nox (0.72) + indus (0.6) + age (0.61) + rad (0.62): Predictors 
# 
#fit.glm <- glm(iscrime ~ -crim + dis + zn + nox + medv + rad + tax + ptratio + black, data = Boston.train, family = binomial)

# 0.01
#fit.glm <- glm(iscrime ~ dis  + nox  + rad  + ptratio, data = Boston.train, family = binomial)

# 0.001
fit.glm <- glm(iscrime ~  nox  + rad, data = Boston.train, family = binomial)

summary(fit.glm)


# Use our model and see how it predicts on Test data set 
# The predict function, predicts the probability for crime, given the predictors 
probs <- predict(fit.glm, Boston.test, type = "response")

# Create list with same size as probs with all zeros 
pred.glm <- rep(0, length(probs))

# Insert 1's where probability is larger than 0.5    
pred.glm[probs > 0.5] <- 1

# Confusion matrix in order to determine how many observations were correctly or incorrectly classified
# Correct prediction: 65 suburbs with no crimes over median and 68 suburbs with crimes over median
# wrong prediction: 15 which the model predicted to no crime had crime. 4 which the model predicted had crime which hadnt crime
table(pred.glm, iscrime.test)

# Computes the rate of misclassification (0.112)
mean(pred.glm != iscrime.test)

###############################
# Linear discriminant analysis
###############################
#crime

#crime <- factor(crime)

str(iscrime)
# Create model
#fit.lda <- lda(iscrime ~., data = Boston)

#Use the p values to determine which variable to sufficient for the model training

#LDA Model with the training data
# 0.05 significants level
fit.lda.train <- lda(iscrime ~  dis + zn + nox + medv + rad + tax + ptratio + black, data = Boston.train)

#0.01
fit.lda.train <- lda(iscrime ~ -crim + dis  + nox  + rad  + ptratio, data = Boston.train)
fit.lda.train
#0.001
fit.lda.train <- lda(iscrime ~ -crim + nox  + rad, data = Boston.train )
fit.lda.train
plot(fit.lda.train)


# Use the model for prediction on test data
pred.lda <- predict(fit.lda.train, Boston.test)


# Confusion matrix
table(pred.lda$class, iscrime.test)

# Misclassification rate
mean(pred.lda$class != iscrime.test)


#######
# KNN
#######
library(class)



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#data = scale(Boston[,-c(1,15)])

# Nomalize data
data <- as.data.frame(lapply(Boston[,2:14], normalize))

# Set random generator
set.seed(1234)

# Create training data - 70% training  data and 30% test data
train <- sample(1:dim(Boston)[1], dim(Boston)[1]*.70, rep=FALSE)

# Create test data (-train means all not all train data)
test <- -train

# 5% significance
#training_data = data[train, c("nox" , "indus" , "age" , "rad", "dis", "zn", "tax", "ptratio")]

# 1% significance
#training_data = data[train, c("nox" ,  "rad", "dis",  "ptratio")]

# 0.1% significance
training_data = data[train, c("nox" ,  "rad")]

# 5% significance
#testing_data = data[test, c("nox" , "indus" , "age" , "rad", "dis", "zn", "tax", "ptratio")]

# 1% significance
#testing_data = data[test, c("nox" ,  "rad", "dis",  "ptratio")]

# 0.1% significance
testing_data = data[test, c("nox" ,  "rad")]


# Add iscrime to train.iscrime
train.iscrime = Boston$iscrime[train]

# Add iscrime to test.iscrime
test.iscrime= Boston$iscrime[test]

# Create list for accuracy for each K
acc_ls <- list()

# accuracy function
accuracy  <- function(x,y){
  acc <- 0
  
  for(i in 1:length(y)){
    if(y[i] == x[i]){
      acc <- acc + 1
    }
  }
  return(100*acc/length(y))
  
}

# Run prediction for each k value and get the accuracy
# Unlike LDA and logistic regression we do not train and validate in two steps
# In KNN we do both in one step
k <- 1:35
error_rate = NULL
for(i in k){
  pred_k <- knn(Boston.train[,2:14], Boston.test[,2:14], train.iscrime, i)
  acc_ls[i] <- accuracy(pred_k,test.iscrime)
  
}

# Average of all accuracies
mean(unlist(acc_ls))

# Plot the accuracy of all k's
plot(k,acc_ls)

# KNN training in respect to subset and k.
# From the prediction of finding the best k with best accuracy, we get k=3 and k=5
# The outcome is a prediction model on the testting_data
knn_pred_y = knn(training_data, testing_data, train.iscrime, k = 5)

table(knn_pred_y, test.iscrime)

# For this KNN (k=3), we have a test error rate of ...
mean(knn_pred_y != test.iscrime)

