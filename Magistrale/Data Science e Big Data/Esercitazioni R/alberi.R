# 17 novembre 2023

library(rpart)
library(rpart.plot)

data(Hitters) #dataset del Salary in funzione delle altre variabili
head(Hitters)

Hitters <- na.omit(Hitters) #eliminiamo i dati mancanti

#procedura di divisione del campione in train e test
#campione base e test nella proporzione di 75% e 25% 

n <- nrow(Hitters)
indice <- sample(1:n,0.75*n,replace=FALSE)
train <- Hitters[indice,]
test <- Hitters[-indice,]

dim(Hitters)
dim(train)/dim(Hitters)
dim(test)/dim(Hitters)

#costruiamo l'albero sul campione train specificando come metodo l'anova (perchè la variabile risposta è di tipo quantitativo)
tree.mod <- rpart(Salary~.,data=train,method="anova")
tree.mod

#in tree.mod troviamo il numero di nodi, lo split, la numerosità, il valore della devianza e il valore dell'y stimato

#nella radice dell'albero (root) si ha proprio la media delle y di tutto il campione base
mean(train$Salary) 

#tree.mod deve essere letta congiuntamente al grafico

#PLOT con il comando rpart.plot

rpart.plot(tree.mod,digits=3,type=3,fallen.leaves = TRUE)

#NODI TERMINALI
#il numero è il valore atteso del salario
# la % è la % di osservazioni presenti nel nodo


#con type=5 il tipo di gfrafico è diverso: variabili nei nodi e condizioni di disuguaglianza all'esterno
rpart.plot(tree.mod,digits=3,type=5,fallen.leaves = TRUE) 


#type=2 e extra=1 ci consente di vedere cosa succede in ogni nodo con numerosità n e valore atteso
rpart.plot(tree.mod,digits=3,type=2,extra=1,fallen.leaves = TRUE) 


#effettuiamo la previsione sul campione test con il comando predict e metodo sempre anova
previsione <- predict(tree.mod,test,method="anova")
previsione

#calcoliamo l'MSE
MSE <- mean((test$Salary-previsione)^2)
MSE

#nel comando rpart possiamo specificare diversi parametri (vedi l'help rpart.control)

#minsplit: di default il numero minimo di split è 20
#impostiamo minsplit=10: in ogni nodo ci devono essere almeno 10 osservazioni affinchè l'albero possa continuare ad essere ulteriormente suddiviso

tree.mod <- rpart(Salary~.,data=train,method="anova",minsplit=10)
tree.mod
rpart.plot(tree.mod,digits=3,type=2,extra=1,fallen.leaves = TRUE)

#minbucket: il numero minimo di osservazioni in qualsiasi nodo terminale <leaf>. 

tree.mod <- rpart(Salary~.,data=train,method="anova",minbucket=10)
tree.mod
rpart.plot(tree.mod,digits=3,type=2,extra=1,fallen.leaves = TRUE)

#cp= parametro di complessità
#cp: indica l'incremento o decremento che ci deve essere nella devianza per far si che ci sia uno stop
#di default è 0.01

tree.mod2 <- rpart(Salary~.,data=train,method="anova",cp=0.001)
tree.mod2
rpart.plot(tree.mod2,digits=3,type=2,extra=1,fallen.leaves = TRUE)

#maxdepth: massimo numero numero di suddivisioni (considera come nodo ogni suddivisione)

tree.mod <- rpart(Salary~.,data=train,method="anova",maxdepth=15) # 15 sono tutti i rami, le suddivisiono (non i nodi)
tree.mod
rpart.plot(tree.mod,digits=3,type=2,extra=1,fallen.leaves = TRUE)

#quale variabile è la più rilevante nella costruzione del grafico?
#basta richiamare il summary

summary(tree.mod)

#nella sezione variable importance per ogni variabile c'è il numero di nodi in cui quella variabile risulta tra le più importanti per la suddivisione

####### potatura dell'albero

#costruiamo l'albero intero ed effettuiamo la potatura dell'albero

tree.mod2 <- rpart(Salary~.,data=train,method="anova",cp=0.001)
tree.mod2
rpart.plot(tree.mod2,digits=3,type=5,extra=1,fallen.leaves = TRUE)

help(prune)

#per tagliare l'albero impostiamo cp=0.01
#cp deve essere maggiore di quello precedente altrimenti l'albero non viene tagliato

tree.prune <- prune(tree.mod2,cp=0.01)
#plottiamo il nuovo albero potato
rpart.plot(tree.prune,digits=3,type=5,extra=1,fallen.leaves = TRUE)

#avremmo potuto costruire direttamente l'albero con livello cp=0.01
#dipende quali sono gli obiettivi dell'analisi
#magari si vuole valutare prima tutta la struttura dell'albero e poi si effettua la potatura

#per decidere il parametro cp si dovrebbe usare la cross-validation
