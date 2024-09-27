library(ISLR)
data("Hitters")
head(Hitters)

#obiettivo: spiegare la dipendenza della variabile salario dei giocatori dalle altre variabili 

dim(Hitters)
sum(is.na(Hitters$Salary)) #ricerca e somma dei valori mancanti

#sostituire la media delle osservazioni presenti ai valori mancanti (soluzione 1)
#stima delle componenti (ACP, AC) per la sostituzione dei valori mancanti tramite pacchetto MICE (soluzione 2)

Hitters<-na.omit(Hitters)
dim(Hitters)

library(leaps) #pacchetto utile per la selezione del miglior subset (sottoinsieme) di variabili
risultati_1<-regsubsets(Salary~.,data=Hitters)

summary(risultati_1)

#il modello con una sola esplicativa migliore è quello con la variabile CRBI

#il modello con due esplicative migliori è CRBI e HITS

risultati_2<-regsubsets(Salary~.,data=Hitters,nvmax=19 ) #nvmax 19 : 19 variabili provate
summary(risultati_2)

#scegliere il migliore dei modelli con differenti numeri di esplicative

risultati_2.summary<-summary(risultati_2)
names(risultati_2.summary)

risultati_2.summary$rsq  #r quadro
r_quadro_corretto<-risultati_2.summary$adjr2 #r quadro corretto
max(r_quadro_corretto)

#par(mfrow=c(2,2))
plot(risultati_2.summary$rss, xlab="Numero variabili", ylab="RSS", type="l")

plot(risultati_2.summary$adjr2, xlab="Numero variabili", ylab="adjr2", type="l")

which.max(r_quadro_corretto)

#sulla base del R-quadro corretto il miglior modello è l'undicesimo, con undici variabili

points(11,r_quadro_corretto[11], col="green", cex=2, pch=20)

plot(risultati_2.summary$bic, xlab="Numero variabili", ylab="BIC", type="l")
which.min(risultati_2.summary$bic)

points(6,risultati_2.summary$bic[6], col="red", cex=2, pch=20)

plot(risultati_2.summary$cp, xlab="Numero variabili", ylab="cp", type="l")
which.min(risultati_2.summary$cp)


points(10,risultati_2.summary$cp[10], col="blue", cex=2, pch=20)

plot(risultati_2,scale="adjr2")
#graifco udile a capire all'aumentare del R2 quali sonio le variabili coinvolte nel modello

coefficienti<-coef(risultati_2,11)
coefficienti

### STEPWHISE FORWARD ###

ris_forw<-regsubsets(Salary~.,data=Hitters,nvmax=19, method="forward")
summary(ris_forw)

coef(ris_forw,8)

### STEPWHISE BACKWARD ###
ris_backw<-regsubsets(Salary~.,data=Hitters,nvmax=19, method="backward")
summary(ris_backw)

coef(ris_forw,8)

### Trainig and test set ###

set.seed(345)
#settare il seme, vvero il punto di partenza per il campionamento in modo da avere lo stesso campione

#campione base per la costruzione dei modelli
#train<-sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
#train
#table(train) #non buona come procedura

#campione base 2

n<-nrow(Hitters)
indice<-sample(1:nrow(Hitters), 0.8*nrow(Hitters))
indice

train<-Hitters[indice,]
dim(train)
dim(Hitters)

test<-Hitters[-indice,]
dim(test)

#consideriamo il MSE sulla previsione, quindi su i test sets diversi e scegliamo il più basso #

reg.fit<-regsubsets(Salary~., data= train, nvmax=19)
summary(reg.fit)


test.mat<-model.matrix(Salary~., data=test)
test.mat


val.errors<-rep(NA,19)

for(i in 1:19){
 coefi<-coef(reg.fit, id=i) 
 pred<-test.mat[,names(coefi)]%*%coefi
 val.errors[i]<-mean((test$Salary-pred)^2)
  
}

val.errors

min<-which.min(val.errors)
min

#migliore modello ai fini previsivi è quello con 13 variabili (MSE più basso tra i modelli sul campione test)
#in questo caso non possiamo parlare di overfitting visto che stiamo lavorando sul campione test

coef(reg.fit,min)

### CROSS VALIDATION ###
k=10 

set.seed(123)
folds<-sample(1:k, n , replace=TRUE)
folds ### ci indica in quale cartella è finita ogni osservazione

#al primo step si tolgono tutte le osservazioni del gruppo 1, stimo il modello senza queste osservazioni e uso il campione
# gruppo 1 come test set e cosi per ogni grupppo

cv.errors<-matrix(NA,k,19, dimnames=list((NULL),paste(1:19))) #10 
cv.errors

#consideriamo al variare di j quale gruppo eliminare per considerarlo come test set e usare gli altri gruppi
#come train set

predict.regsub<-function(object,newdata,id)
{
form<-as.formula(object$call[[2]])
mat<-model.matrix(form,newdata)
coefff<-coef(object,id=id)
xvars<-names(coefff)
mat[,xvars]%*%coefff
  
}

for(j in 1:k){
  best.fit<-regsubsets(Salary~., data=Hitters[folds!=j,],nvmax=19)
  
for(i in 1:19){
  previsione<-predict.regsub(best.fit,newdata=Hitters[folds==j,],id=i)
  cv.errors[j,i]<-mean((Hitters$Salary[folds==j]-previsione)^2)
  }
}
cv.errors


media.cv.errors<-apply(cv.errors,2,mean)
media.cv.errors
which.min(media.cv.errors)

plot(media.cv.errors, type="b") #b=bot sia punti che linee

coef(best.fit,10)

############################################################################################

#ridge e lasso non funzionano con data frame ma con MATRICI


###RIDGE###

library(glmnet)
x<-model.matrix(Salary~., Hitters)[,-1] 
#escludiamo intercetta per formula di costruzione della ridge e della lasso che non pongono attenzione sulla
#intercetta e automaticamente viene esclusa anche la riposta Y

x

y<-Hitters$Salary

grid<-10^seq(10, -2, length=100)  #seq restituisce una sequenza di valori equidistanti 
grid

fit.ridge<-glmnet(x,y,alpha=0, lambda=grid) #alpha=0 RIDGE, alpha=1 LASSO
summary(fit.ridge)

#in questo modo non conosciamo il valore migliore di lambda, possiamo solo scegliere un lambda e vedere
#che modello restituisce

fit.ridge$lambda[40]
coef(fit.ridge)[,40]
coef(fit.ridge)[,60]


previsioni<-predict(fit.ridge, s=30.56, type="coefficients")[1:20]
#scopo diverso dell'uso del predict, lo usiamo per stimare nuovi beta per valori di lambda diversi da quelli prima considerati.


#procedura corretta è CROSS VALIDATION per selezionare lambda ma possiamp inizialmente provare con gli MSE

#minimizzazione dell'MSE per diversi LAMBDA
y.test<-y[-indice]

fit.ridge2<-glmnet(x[indice,], y[indice], lambda=grid) #costruito sul train set


############ 11 ottobre ###########

#previsioni sul test set
previsioni1<-predict(fit.ridge, s=20, newx=x[-indice,])
previsioni1

previsioni2<-predict(fit.ridge, s=10, newx=x[-indice,])
previsioni2

#mse sul test set
MSE.ridge1<-(mean(previsioni1-y.test)^2)
MSE.ridge1

MSE.ridge2<-(mean(previsioni2-y.test)^2)
MSE.ridge2

length(previsioni)

### CROSS VALIDATION ###
set.seed(345)

cv.out<-cv.glmnet(x[indice,],y[indice], alpha=0)
cv.out

plot(cv.out)
# 19 indica il numero di variabili portate a 0 che non ci sono nella ridge

best.lambda<-cv.out$lambda.min
best.lambda

#abbiamo fatto una cross validation sul campione train per selezionare il miglior lambda

#ora dobbiamo verificare se questo lambda funziona sul campione test

prev.cv<-predict(fit.ridge,s=best.lambda,newx=x[-indice,])
prev.cv
 

MSE.prev.cv<-(mean(prev.cv-y.test)^2)
MSE.prev.cv

previsioni_best<-predict(fit.ridge, s=best.lambda, type="coefficients")[1:20]
previsioni_best

#### RIEPILOGO ###

#nella ridge cerchiamo il lambda che minimizza l'MSE

#prima abbiamo calcolato i lambda sul test

#poi abbiamo selezionato i lambda sul train 

#può succedere che il lambda selezionato sul train funzioni bene sul train ma non sul test

#### LASSO ####

fit.lasso<-glmnet(x[indice,], y[indice], alpha=1, lambda=grid)
summary(fit.lasso)

plot(fit.lasso)
#al variare di lambda alcuni coefficienti rimangono significativi altri vanno a zero

set.seed(678)
cv.lasso<-cv.glmnet(x[indice,],y[indice], alpha=1)
plot(cv.lasso)

best.lambda.lasso<-cv.lasso$lambda.min
best.lambda.lasso

prev.lasso<-predict(fit.lasso, s=best.lambda.lasso, newx=x[-indice,])
prev.lasso

MSE.lasso<-(mean(prev.lasso-y.test)^2)
MSE.lasso

out.fin<-glmnet(x,y,alpha=1,lambda=grid)
lasso.coef<-predict(out.fin, type="coefficients", s=best.lambda.lasso)[1:20]
lasso.coef

lasso.coef[lasso.coef!=0]

#proviamo col campione train
out.fin.train<-glmnet(x[indice,],y[indice],alpha=1,lambda=grid)
lasso.coef.train<-predict(out.fin.train, type="coefficients", s=best.lambda.lasso)[1:20]
lasso.coef.train
