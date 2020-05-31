# Helper packages
library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting
library(rsample)   # for data splitting

# Modeling packages
library(caret)     # for logistic regression modeling

# Model interpretability packages
library(vip)       # variable importance

# access data
attrition <- rsample::attrition

# initial dimension
dim(attrition)
## [1] 1470   31

# response variable
head(attrition$Attrition)
## [1] Yes No  Yes No  No  No 
## Levels: No Yes

# Helper packages
library(dplyr)     # for data manipulation
library(ggplot2)   # for awesome graphics

# Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(h2o)       # for resampling and model training

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o

# Job attrition data
churn <- rsample::attrition %>% 
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)
churn.h2o <- as.h2o(churn)

# orginal response distribution
table(churn$Attrition) %>% prop.table()
## 
##        No       Yes 
## 0.8387755 0.1612245

# stratified sampling with the rsample package
set.seed(123)
split_strat  <- initial_split(churn, prop = 0.7, 
                              strata = "Attrition")
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)

# consistent response ratio between train & test
table(train_strat$Attrition) %>% prop.table()
## 
##       No      Yes 
## 0.838835 0.161165
table(test_strat$Attrition) %>% prop.table()
## 
##        No       Yes 
## 0.8386364 0.1613636

# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations

# Feature engineering packages
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks

df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Create training (70%) and test (30%) sets for the 
# rsample::attrition data.
set.seed(123)  # for reproducibility
churn_split <- initial_split(df, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = churn_train)
model2 <- glm(Attrition ~ OverTime, family = "binomial", data = churn_train)

tidy(model1)
## # A tibble: 2 x 5
##   term           estimate std.error statistic       p.value
##   <chr>             <dbl>     <dbl>     <dbl>         <dbl>
## 1 (Intercept)   -0.924    0.155         -5.96 0.00000000259
## 2 MonthlyIncome -0.000130 0.0000264     -4.93 0.000000836
tidy(model2)
## # A tibble: 2 x 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)    -2.18     0.122    -17.9  6.76e-72
## 2 OverTimeYes     1.41     0.176      8.00 1.20e-15

exp(coef(model1))
##   (Intercept) MonthlyIncome 
##     0.3970771     0.9998697
exp(coef(model2))
## (Intercept) OverTimeYes 
##   0.1126126   4.0812121

confint(model1)  # for odds, you can use `exp(confint(model1))`
##                       2.5 %        97.5 %
## (Intercept)   -1.2267754960 -6.180062e-01
## MonthlyIncome -0.0001849796 -8.107634e-05
confint(model2)
##                 2.5 %    97.5 %
## (Intercept) -2.430458 -1.952330
## OverTimeYes  1.063246  1.752879

model3 <- glm(
  Attrition ~ MonthlyIncome + OverTime,
  family = "binomial", 
  data = churn_train
)

tidy(model3)
## # A tibble: 3 x 5
##   term           estimate std.error statistic  p.value
##   <chr>             <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)   -1.43     0.176         -8.11 5.25e-16
## 2 MonthlyIncome -0.000139 0.0000270     -5.15 2.62e- 7
## 3 OverTimeYes    1.47     0.180          8.16 3.43e-16

set.seed(123)
cv_model1 <- train(
  Attrition ~ MonthlyIncome, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model3 <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# extract out of sample performance measures
summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3
    )
  )
)$statistics$Accuracy
##             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## model1 0.8349515 0.8349515 0.8365385 0.8388478 0.8431373 0.8446602    0
## model2 0.8349515 0.8349515 0.8365385 0.8388478 0.8431373 0.8446602    0
## model3 0.8365385 0.8495146 0.8792476 0.8757893 0.8907767 0.9313725    0

# predict class
pred_class <- predict(cv_model3, churn_train)

# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = "Yes"), 
  reference = relevel(churn_train$Attrition, ref = "Yes")
)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction Yes  No
##        Yes  93  25
##        No   73 839
##                                           
##                Accuracy : 0.9049          
##                  95% CI : (0.8853, 0.9221)
##     No Information Rate : 0.8388          
##     P-Value [Acc > NIR] : 5.360e-10       
##                                           
##                   Kappa : 0.6016          
##                                           
##  Mcnemar's Test P-Value : 2.057e-06       
##                                           
##             Sensitivity : 0.56024         
##             Specificity : 0.97106         
##          Pos Pred Value : 0.78814         
##          Neg Pred Value : 0.91996         
##              Prevalence : 0.16117         
##          Detection Rate : 0.09029         
##    Detection Prevalence : 0.11456         
##       Balanced Accuracy : 0.76565         
##                                           
##        'Positive' Class : Yes             
## 

library(ROCR)

# Compute predicted probabilities
m1_prob <- predict(cv_model1, churn_train, type = "prob")$Yes
m3_prob <- predict(cv_model3, churn_train, type = "prob")$Yes

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf2 <- prediction(m3_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = "black", lty = 2)
plot(perf2, add = TRUE, col = "blue")
legend(0.8, 0.2, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)

# Perform 10-fold CV on a PLS model tuning the number of PCs to 
# use as predictors
set.seed(123)
cv_model_pls <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 16
)

# Model with lowest RMSE
cv_model_pls$bestTune
##    ncomp
## 14    14

# results for model with lowest loss
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
##   ncomp  Accuracy     Kappa AccuracySD   KappaSD
## 1    14 0.8757518 0.3766944 0.01919777 0.1142592

# Plot cross-validated RMSE
ggplot(cv_model_pls)

vip(cv_model3, num_features = 20)

#s

# Helper packages
library(recipes)  # for feature engineering

# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process

# Model interpretability packages
library(vip)      # for variable importance


df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Create training (70%) and test (30%) sets for the
# rsample::attrition data. Use set.seed for reproducibility
set.seed(123)
churn_split <- initial_split(df, prop = .7, strata = "Attrition")
train <- training(churn_split)
test  <- testing(churn_split)

# train logistic regression model
set.seed(123)
glm_mod <- train(
  Attrition ~ ., 
  data = train, 
  method = "glm",
  family = "binomial",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10)
)

# train regularized logistic regression model
set.seed(123)
penalized_mod <- train(
  Attrition ~ ., 
  data = train, 
  method = "glmnet",
  family = "binomial",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# extract out of sample performance measures
summary(resamples(list(
  logistic_model = glm_mod, 
  penalized_model = penalized_mod
)))$statistics$Accuracy
##                      Min.   1st Qu.    Median      Mean   3rd Qu.
## logistic_model  0.8365385 0.8495146 0.8792476 0.8757893 0.8907767
## penalized_model 0.8446602 0.8759280 0.8834951 0.8835759 0.8915469
##                      Max. NA's
## logistic_model  0.9313725    0
## penalized_model 0.9411765    0


