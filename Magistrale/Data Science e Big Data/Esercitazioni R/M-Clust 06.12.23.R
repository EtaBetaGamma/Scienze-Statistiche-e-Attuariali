
# Pakcages ----------------------------------------------------------------

library(mclust)


# Dati --------------------------------------------------------------------

data("diabetes")
View(diabetes)
table(diabetes$class)


# Cluster -----------------------------------------------------------------
class <- diabetes$class
X <- diabetes[, -1]

clPairs(X, class)

bic <- mclustBIC(X)
plot(bic)
x11()

#in legenda: 14 tipi diversi di analisi

mod1 <- Mclust(X, x = bic)
plot(mod1, what = "classification")
plot(mod1, what = "uncertainty")



# Dati 2 ------------------------------------------------------------------



data("banknote")
View(banknote)
table(banknote$Status)
Y <- banknote[, -1]
class2 <- banknote$Status

mod2 <- MclustDA(Y, class2)

summary(mod2)

plot(mod2, "classification")
plot(mod2, "scatterplot")
