data("Pima.tr")
?Pima.tr
View(Pima.tr)
length(Pima.tr)
attach(Pima.tr)

#variabile risposta è dicotomica
#modello di regressione logistica
Pima.tr$type <- ifelse(Pima.tr$type == "Yes", 1, 0)

modellologit <- glm(type ~ npreg + glu + bp + skin + bmi + age, family = binomial)
summary(modellologit)

#coefficienti significativamente uguali a zero, faccio il confronto lrt

modellolog1 <- glm(type ~ glu + bmi + age, family = binomial)
summary(modellolog1)

#pacchetto lmtest
lrtest(modellologit, modellolog1)

#pvalue alto, accetto H0, non c'è differenza significativa tra i due modelli, i parametri sono significativamente pari a 0

#commenatre i parametri in termini di rapporto tra quote
glucosio <- exp(0.030850)
glucosio
#all'aumentare di un livello il glucosio, il rapporto tra la quota di avere e non avere il diabete aumenta di 1.03
#si va per tutte e tre le variabili significative

#calcolo intervallo di confidenza wald e verosimiglianza
#wald
coefficientiw <- modellolog1$coefficients
stderror <- summary(modellolog1)$coefficients[,2]
low <- coefficientiw[3] - qnorm(0.975)*stderror[3]
up <- coefficientiw[3] + qnorm(0.975)*stderror[3]
int <- c(low, up)
int

#verosimiglianza
confint(modellolog1, parm = "bmi", level = 0.95)
low
up

#intervallo di confidenza per prob di diabete quando glu=190, bmi=35, age=45
#vado a fare la previsione 

previsione <- predict(modellolog1, data.frame(glu = 190, bmi = 35, age = 45), se.fit = T, type = "response")
previsione

lowp <- previsione$fit - qnorm(0.975) * previsione$se.fit
upp <- previsione$fit + qnorm(0.975) * previsione$se.fit
intervallop <- c(lowp, upp)
intervallop

#valutare sensitività e specifità con soglia 0.5
previsioni <- predict.glm(modellolog1, data = Pima.tr, type = "response")
previsioni

prev <- ifelse(previsioni>0.5, 1, 0)
#costruisco matrice di confusione
matconf <- table(Pima.tr$type, prev)
matconf
benclass <- (113+39)/(200)
benclass
sensitività <- matconf[2,2]/(matconf[2, 2] + matconf[2, 1]) ###1 corretti su 1 totali
specifità <- matconf[1, 1]/(matconf[1, 1]+ matconf[1, 2]) ###0 corretti su 0 totali
sensitività
specifità

#curva ROC
library(pROC)
curvaRoc <- roc(Pima.tr$type~previsioni, plot = T)
#calcolo area 
auc(curvaRoc)

