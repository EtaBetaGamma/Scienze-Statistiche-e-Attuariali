
# Packages ----------------------------------------------------------------

library(MASS)
library(randomForest)
library(gbm)
library(caret)


# Data --------------------------------------------------------------------

data("swiss")
View(swiss)


# Cross Validation --------------------------------------------------------

#leave one out

train.control <- trainControl(method = "LOOCV")
train.control

mod1 <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)

summary(mod1)

mod2 <- train(Fertility ~ ., data = swiss, method = "rpart", trControl = train.control)

summary(mod2) 


names(getModelInfo())


