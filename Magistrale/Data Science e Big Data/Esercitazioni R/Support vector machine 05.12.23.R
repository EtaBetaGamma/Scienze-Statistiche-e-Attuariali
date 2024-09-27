
# Packages ----------------------------------------------------------------

library(e1071)



# Creazione dati ----------------------------------------------------------

set.seed(1)

x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))

x[y == 1, ] <- x[y == 1, ] + 1
plot(x[ ,2], x[ ,1], col = (3 - y))

dati <- data.frame(x = x, y = as.factor(y))


# Analisi -----------------------------------------------------------------

fit.svm <- svm(y ~ ., data = dati, kernel = "linear", cost = 10)
?svm

fit.svm
summary(fit.svm)
plot(fit.svm, dati)
fit.svm$index

fit.svm2 <- svm(y ~ ., data = dati, kernel = "linear", cost = 5)
plot(fit.svm2, dati)

tune.output <- tune(svm, y ~ ., data = dati, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

tune.output

summary(tune.output)

bestmod <- tune.output$best.model

plot(bestmod, dati)


fit.svm3 <- svm(y ~ ., data = dati, kernel = "radial", cost = 5)
plot(fit.svm3, dati)


# SVM 2 classi ------------------------------------------------------------


set.seed((1))
x<-rbind(x,matrix(rnorm(50*2),ncol=2))
y<-c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dati<-data.frame(x=x,y=as.factor(y))
plot(x[,2],x[,1],col=(y+1))
fit.SVM_rad3<-svm(y~., data=dati, kernel="radial", cost=10)

plot(fit.SVM_rad3,dati)

table(dati[, "y"], predict(fit.SVM_rad3, newx = dati))


