set depositi /1, 2, 3/
         clienti /1, 2, 3, 4, 5/;
alias (depositi, i)
         (clienti, j);

parameter costruzione (i) /1 5000, 2 7500, 3 6000/
                 capacita (i) /1 180, 2 230, 3 500/
                         domanda (j) /1 91, 2 170, 3 135, 4 150, 5 110/;

table matrix (i, j)
         1       2       3       4       5
1        7       6       13      5       4
2        6       10      7       10      2
3        3       5       1       5       10;

positive variables
x(i, j);

binary variables
y(i)

variable
FO

equations
vinc_domanda(j)
vinc_capacita(i)
vinc_numero
obiettivo;

vinc_domanda(j).. sum (i, x(i, j)) =E= domanda(j);
vinc_capacita(i).. sum (j, x(i, j)) =L= capacita(i)*y(i);
vinc_numero .. sum(i, y(i)) =L= 2;
obiettivo.. FO =E= sum(i,sum(j, x(i,j) * matrix(i, j))) + sum(i, y(i)*costruzione(i));

model esercizio8esempio /ALL/;


Solve esercizio8esempio Minimizing FO using MIP;

display x.l, y.L, FO.L;
