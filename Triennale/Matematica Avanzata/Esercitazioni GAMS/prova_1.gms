SET setRighe #set sul numero di vincoli /riga1, riga2, riga3/
         setcolonne #set su numero di variabili /v1*v3/;
ALIAS (setRighe, i, i1)
         (setcolonne, j, j1);

scalar P;
P = 3;

parameter c(setcolonne) / v1 30, v2 20, v3 10/
                 b(setRighe) / riga1 10, riga2 2, riga3 8/;
table matrix(i, setcolonne)
       v1        v2      v3
riga1  2         1       3
riga2  1         1       1
riga3  2         1       5;

positive variable x(j);

variable FO;

equations
vincoli(i)
obiettivo;

vincoli(i).. sum(j, matrix(i,j)*x(j)) =L= b(i);

obiettivo.. FO =E= sum(j, c(j)*x(j));


model modelloprova /ALL/;

Solve modelloprova Maximizing FO using LP;
display x.l;
