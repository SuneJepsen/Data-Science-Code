library(MASS)
library(corrplot)

##############################################################################
# Logistic regression
##############################################################################

# ressource: https://rpubs.com/datascientiest/248183

#head(Boston)
#str(Boston)
#summary(Boston)

# Load Boston data set
data("Boston")

# Create list with same size as crime coloum with all zeros
crim01 <- rep(0, length(Boston$crim))

# Insert 1's where crime is over median 
crim01[Boston$crim > median(Boston$crim)] <- 1

# Add crim01 coloum to Boston dataset
Boston <- data.frame(Boston, crim01)

summary(Boston)

# Set random number generator in R
set.seed(1234)

# Create training data
train <- sample(1:dim(Boston)[1], dim(Boston)[1]*.7, rep=FALSE)

# Create test data (-train means all train data with negative sign)
 test <- -train
 
# Add training data to variable
Boston.train <- Boston[train, ]

# Add test data to variable (all data which is not train)
Boston.test <- Boston[test, ]

# From the the crime data, add the only test data to crim01.test variable
crim01.test <- crim01[test]

# Generate logistic regression model
# GLM Parameter explaination
  # crim01: The variable we want to predict
  # ~ .: all variables in Boston dataset is set as predictors 
  # - crime01 -crim: remove crime01 and crim colums as predictors
  # data = Boston: use Boston data set
  # binomial: logit
fit.glm1 <- glm(crim01 ~ . - crim01 - crim, data = Boston, family = binomial)

# Summary of the models coefficients and p-values
summary(fit.glm1)

# Correlation matrix
  # Find candidates which indicates the positive correlation on crim01
corrplot::corrplot.mixed(cor(Boston[, -1]), upper="circle")

# Generate logistic regression model
  # GLM Parameter explaination
  # crim01: The variable we want to predict
  # nox (0.72) + indus (0.6) + age (0.61) + rad (0.62): Predictors 
fit.glm <- glm(crim01 ~ nox + indus + age + rad, data = Boston, family = binomial)
Summary(fit.glm)


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
table(pred.glm, crim01.test)

# Computes the rate of misclassification (0.125)
mean(pred.glm != crim01.test)

##############################################################################
# Linear discriminant analysis
##############################################################################


# Create model
fit.lda <- lda(crim01 ~ nox + indus + age + rad , data = Boston)

# Use the model for prediction on test data
pred.lda <- predict(fit.lda, Boston.test)

# Confusion matrix
table(pred.lda$class, crim01.test)

# Misclassification rate
mean(pred.lda$class != crim01.test)


##############################################################################
# KNN
##############################################################################

data = scale(Boston[,-c(1,15)])
set.seed(1234)
train <- sample(1:dim(Boston)[1], dim(Boston)[1]*.7, rep=FALSE)
test <- -train
training_data = data[train, c("nox" , "indus" , "age" , "rad")]
testing_data = data[test, c("nox" , "indus" , "age" , "rad")]
## KNN take the training response variable seperately
train.crime01 = Boston$crim01[train]
## we also need the have the testing_y seperately for assesing the model later on
test.crime01= Boston$crim01[test]

library(class)
set.seed(1234)
knn_pred_y = knn(training_data, testing_data, train.crime01, k = 1)
table(knn_pred_y, test.crime01)

# For this KNN (k=1), we have a test error rate of 9.21%
mean(knn_pred_y != test.crime01)

knn_pred_y = NULL
error_rate = NULL
for(i in 1:dim(testing_data)[1]){
  set.seed(1234)
  knn_pred_y = knn(training_data,testing_data,train.crime01,k=i)
  error_rate[i] = mean(test.crime01 != knn_pred_y)
}

### find the minimum error rate
min_error_rate = min(error_rate)
print(min_error_rate)

### get the index of that error rate, which is the k
K = which(error_rate == min_error_rate)
print(K)

# Minimum error rate is 6.57% at K=3

library(ggplot2)
qplot(1:dim(testing_data)[1], error_rate, xlab = "K",
      ylab = "Error Rate",
      geom=c("point", "line"))
