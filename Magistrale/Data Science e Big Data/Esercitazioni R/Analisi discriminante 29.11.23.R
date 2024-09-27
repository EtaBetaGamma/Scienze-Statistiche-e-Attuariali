
# Packages ----------------------------------------------------------------

library(MASS)


# Dati --------------------------------------------------------------------

data("iris")
View(iris)

# Analisi -----------------------------------------------------------------

set.seed(123)
n <- nrow(iris)
indice <- sample(1:n, 0.8*n, replace = F)
train <- iris[indice, ]
test <- iris[-indice, ]

fit.lda <- lda(Species ~ ., data = train)
prev_train <- predict(fit.lda, newdata = train)
prev_test <- predict(fit.lda, newdata = test)

mat.conf <- table(prev_test$class, test$Species)

