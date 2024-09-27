library(VGAM)
library(MASS)
library(brant)

datilogit<-logit

modello1<-polr(factor(apply)~.,data=datilogit, Hess=TRUE) #comando per impostare un modello ordinale 

brant(modello1) #test per la verifica dell'ipotesi di PROPORZIONALITA' fra le categorie della VAR di risposta

###il summary ci restituisce i p value che ci portano a respingere l'ipotesi di proporzionalità per tutte le variabili
#tranne che per la variabile PUBLIC, dunque non possiamo considerare l'ipotesi di PROPORZIONALITA' per il modello 
#con tutte le variabili

#ordiniamo le modalità non considerando più l'ordine alfabetico
datilogit$apply<-factor(datilogit$apply,levels=c("unlikely","somewhat likely","very likely",ordered=TRUE))

###cumulative logit model (proportional odds ma senza proporzionalità) 
fit.prop<-vglm(apply~., data=datilogit, family=cumulative(parallel=FALSE),maxit=50)
summary(fit.prop)

#commento coefficiente gpa1: all'aumentare di 1 unità di GPA la prob che l'individuo scelga la modalità 1 (unlikely)
#rispetto alla modalità 2 (somewhat likely) o alla 3 (very likely)  diminusice (-44%)

(exp(-0.5919)-1)*100

#commento coefficiente gpa2:all'aumentare di 1 unità di GPA la prob che l'individuo scelga la modalità 1 e 2 (unlikely e somewhat likely)
#rispetto alla modalità  3 (very likely)  diminusice (-51%)

( 0.4872298 - 1 )*100

fit.prop1<-vglm(apply~., data=datilogit, family=cumulative(parallel=FALSE,reverse=TRUE),maxit=50)
summary(fit.prop1)

###adjacent category model  
#modello più adatto in caso di NON PROPORZIONALITA'
fit.adj<-fit.prop<-vglm(apply~., data=datilogit, family=acat(parallel=FALSE),maxit=50)
summary(fit.adj)

##continuation ratio model
fit.cont<-fit.prop<-vglm(apply~., data=datilogit, family=sratio(parallel=FALSE,reverse=TRUE),maxit=50)
summary(fit.cont)


