
# Packages ----------------------------------------------------------------

library(MASS)
library(randomForest)
library(gbm)
library(caret)

# Data -------------------------------------------------------------------

data("Boston")
View(Boston)
set.seed(234)
n <- nrow(Boston)
index <- sample(1:n, 0.75*n, replace = F)
train <- Boston[index, ]
test <- Boston[-index, ]

# Analisi bagging -----------------------------------------------------------------

bag.boston <- randomForest(medv ~ ., data = train, mtry = 13, ntree = 3, importance = T)

bag.boston

str(bag.boston)

bag.boston$oob.times

prev <- predict(bag.boston, newdata = test)

bag2 <- randomForest(medv ~ ., data = train, mtry = 13, ntree = 100, importance = T)
prev2 <- predict(bag2, newdata = test)

bag3 <- randomForest(medv ~ ., data = train, mtry = 13, ntree = 200, importance = T)
prev3 <- predict(bag3, newdata = test)

mse1 <- mean((test$medv - prev)^2)

mse2 <- mean((test$medv - prev2)^2)

mse3 <- mean((test$medv - prev3)^2)

bag.boston$importance

#second acolonna in importance è riduzione di impurità cpon quella variabile
#prima colonna incremento mse tolta quella variabile

varImpPlot(bag.boston)


# Random Forest -----------------------------------------------------------

#varia l'mtry
#per regressione si usa p/3
#per classificazione radice(p)

rf.baston <- randomForest(medv ~ ., data = train, mtry = 4, ntree = 100, importance = T)

prev_rf <- predict(rf.baston, newdata = test)
head(prev_rf)
mse_rf100 <- mean((test$medv - prev_rf)^2)

rf.baston2 <- randomForest(medv ~ ., data = train, mtry = 4, ntree = 200, importance = T)
prev_rf_2 <- predict(rf.baston2, newdata = test)
mse_rf200 <- mean((test$medv - prev_rf_2)^2)


prev5 <- predict(rf.baston2, newdata = train)
prev6 <- predict(bag3, newdata = train)

mean((train$medv - prev5)^2)
mean((train$medv - prev6)^2)

#ogni volta che facciamo l'analisi, avviene un nuovo campionamento, quindi va settato lo stesso seed ogni volta per fare dei confronti

set.seed(123)
rf.baston3 <- randomForest(medv ~ ., data = train, mtry = 4, ntree = 200, importance = T, seed = 123)

set.seed(123)
rf.baston2 <- randomForest(medv ~ ., data = train, mtry = 13, ntree = 200, importance = T, seed = 123)

rf.baston2$oob.times
rf.baston3$oob.times
  

# Boosting ----------------------------------------------------------------


boost.boston <- gbm(medv ~ ., data = train, n.trees = 200000, shrinkage = 0.01, distribution = "gaussian", verbose = T)

summary(boost.boston)

aaa <- predict(boost.boston, newdata = test)

mean((test$medv - aaa)^2)




