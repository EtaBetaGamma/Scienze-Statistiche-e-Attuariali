library(VGAM)
library(MASS)
library(brant)


table(datilogit$apply) #controllare che non ci sia troppa differenza tra le frequenze delle categorie

datilogit$pared<-as.factor(datilogit$pared)
datilogit$public<-as.factor(datilogit$public)

#modello multinomiale
fit.mult<-vglm(apply~public+pared+gpa, family=multinomial, data=datilogit)
summary(fit.mult)

###interpretazione summary

# la modalità della var. di risposta presa come riferimento è la terza in ordine alfab. (very likely)
# il metodo di stima utilizzato è quello di Fisher-Scoring
# non bisogna basarsi sui p-value associati alle singole stime ma la valutazione va fatta con AIC o LRT

### interpretazione stima pared 2 ###
#quando la variabile "pared" assume modalità 1 ("genitori laureati") la probabilità che lo studente scelga la modalità 2 (unlikely)
# ( " sceglie di non andare all'uni") diminusice di exp((-1.3742)-1)*100 (74%)  

#la differenza tra le 3 modalità della var. di risposta non dipende dalla var. public#

#previsioni
pred<-predict(fit.mult,newdata=data.frame(datilogit),type="response")
pred

#restituisce le probabilità associate alle tre modalità della var. di risposta

prev<-apply(pred,1,which.max) 
prev #restituisce per ogni riga qual è la colonna con la prob. più alta
table(prev)

prev<-factor(prev,level=1:3)

#matrice di confusione
mat.conf<-table(datilogit$apply,prev)
colnames(mat.conf)<-levels(as.factor(datilogit$apply))
mat.conf
sum(diag(mat.conf))/(sum(mat.conf))
