library(FactoMineR)
library(pls)
library(ISLR)

########### ACP ###########

data("decathlon")
head(decathlon)

ACP<-PCA(decathlon[,1:10])

#tutte e 10 le variabili sono spiegate dalle prime due componenti, con la differenza che:

#le variabili che incidono di piu sulla prima componente sono shot.put, discus, high.jump, 400m,
#110m.hurdle,100m, dato che la loro proiezione sull'asse delle ascisse che rappresenta la componente 1
#è più lunga

ACP$var$contrib #contributo di ogni variabile ad ogni componente

######## PCR #########

data(Hitters)
Hitters<-na.omit(Hitters)

set.seed(234)

PCR.ris<-pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(PCR.ris)

#output training ci fornisce la percentuale cumulata via via della variabilità spiegata dalle componenti

# K=10 folds cross validation
# adjCV è un indice calcolato per verificare la bomntà della crosso validation

validationplot(PCR.ris, val.type="MSE")
#plot dell'andamento dell'MSE con la cross validation al variare del numero di componenti

str(PCR.ris)

PCR.ris$validation$adj

MSEP(PCR.ris)

#CROSS VALIDATION su TRAIN SET per effettuare il confronto con gli altri metodi
n<-nrow(Hitters)
indice<-sample(1:n,0.8*n,replace=FALSE)
train<-indice

PCR.ris_train<-pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(PCR.ris_train)

validationplot(PCR.ris_train, val.type="MSE")

MSEP(PCR.ris_train)
#il valore minimo è quello con 6 componenti 

x<-Hitters[,-19]

pcr.pred<-predict(PCR.ris_train, x[-indice,],ncomp=6)
pcr.pred

y.test<-Hitters[-indice,19]
mean((pcr.pred-y.test)^2)

pcr.pred2<-predict(PCR.ris_train, x[-indice,],ncomp=2)
pcr.pred2
mean((pcr.pred2-y.test)^2)

PCR.ris_train$coefficients


######## PLS #######
set.seed(234)

pls.ris<-plsr(Salary~., data=Hitters, subset=train,scale=TRUE, validation="CV")
summary(pls.ris)

validationplot(pls.ris,val.type="MSE")

pls.pred<-predict(pls.ris, x[-indice,],ncomp=4)
pls.pred

y.test<-Hitters[-indice,19]
mean((pls.pred-y.test)^2)

pls.pred2<-predict(pls.ris, x[-indice,],ncomp=2)
pls.pred2
mean((pls.pred2-y.test)^2)

pls.ris$coefficients

