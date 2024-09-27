
# Package -----------------------------------------------------------------

library(rpart)
library(rpart.plot)
library(MASS)
library(pROC)
library(boot)
library(caTools)
library(caret)

# Dataset -----------------------------------------------------------------

data("melanoma")
View(melanoma)

# Alberi di classificazione -----------------------------------------------

index <- sample.split(Y = melanoma$ulcer, SplitRatio = 0.8)

train <- melanoma[index, ]
View(train)
table(train$ulcer)/sum(table(train$ulcer))
table(melanoma$ulcer)/sum(table(melanoma$ulcer))

indice_test <-  which(index == FALSE)
test <- melanoma[indice_test, ] 
View(test)

albero_class <- rpart(ulcer ~ ., data = train, method = "class")

rpart.plot(albero_class)

prev_train <- predict(albero_class, new.data = train, type = "class")
prev_test <- predict(albero_class, new.data = test, type = "class")

conf_train <- confusionMatrix(as.factor(prev_train), as.factor(train$ulcer))
conf_train$table
sum(diag(conf_train$table))/sum(conf_train$table)

conf_test <- confusionMatrix(as.factor(prev_test), as.factor(test$ulcer))
length(prev_test)
