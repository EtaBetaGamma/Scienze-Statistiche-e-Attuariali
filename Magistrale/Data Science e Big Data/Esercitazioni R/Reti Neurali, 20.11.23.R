
# Package -----------------------------------------------------------------
library(neuralnet)
library(MASS)
library(boot)
library(plyr)

# Data --------------------------------------------------------------------
data("Boston")
View(Boston)
?Boston
colSums(is.na(Boston))


# Analisi rete neurale--------------------------------------------------------

set.seed(1234)
index <- sample(1:nrow(Boston), round(0.8 * nrow(Boston)))

#reti neurali risentono di unità di misura differenti

maxs <- apply(Boston, 2, max)
mins <- apply(Boston, 2, min)

#si va a standardizzare il dataset

dati_std <- as.data.frame(scale(Boston, center = mins, scale = maxs - mins))

train <- dati_std[index, ]
test <- dati_std[-index, ]

nomi <- names(train)

f <- formula(paste("medv ~ ", paste(nomi [!nomi %in% "medv"], collapse = "+")))

reteneurale <- neuralnet(f, data = train, linear.output = TRUE, hidden = c(5, 3))

summary(reteneurale)

reteneurale$response

plot(reteneurale)

prev.test.nn <- compute(reteneurale, test[, 1:13])

prev.test.nn

prev.test.or <- prev.test.nn$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)

test.or <- test$medv * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)

train.or <- train$medv * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
MSE_rete <- sum((test.or - prev.test.or)^2 / nrow(test))
MSE_rete

test1 <- Boston[index, ]
train1 <- Boston[-index, ]

##confronto con regressione lineare
mod1 <- lm(medv ~. , data = train1)

summary(mod1)

ypred <- predict(mod1, test1)

View(ypred)
head(ypred)
mse_lm <- mean(test1$medv - ypred)^2

mse_lm

par(mfrow = c(1, 2))

plot(test1$medv, prev.test.nn$net.result, col = "red")
abline(0, 1, lwd = 2)


# Cross-Validation --------------------------------------------------------

set.seed(456)

reg.cv <- glm(medv ~., data = Boston)

CVREG <- cv.glm(Boston, reg.cv, K = 10)

CVREG

?cv.glm

CVREG$delta

cv.error <- NULL
k <- 10

pbar <- create_progress_bar("text")
pbar$init(k)

for (i in 1:k){
  index <- sample(1:nrow(Boston), round(0.9 * nrow(Boston)))
  train.cv <- dati_std[index, ]
  test.cv <- dati_std[-index, ]
  rete.cv <- neuralnet(f, data = train.cv, hidden = c(5, 3), linear.output = TRUE)
  prev.ret <- compute(rete.cv, test.cv[, 1:13])
  prev.ret.cv <- prev.ret$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  test.r.cv <- test.cv$medv * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  cv.error[i] <- (sum(test.r.cv - prev.ret.cv)^2) / nrow(test.cv)
  pbar$step()
}
cv.error
mean(cv.error)

boxplot(cv.error, col = "red", border = "blue", horizontal = T)

