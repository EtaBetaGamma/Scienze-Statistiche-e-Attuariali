set stabilimento / 1, 2/
         mese / 1, 2, 3, 4, 5/;
alias (stabilimento, i)
         (mese, j);

parameters domanda(j) /1 250, 2 300, 3 150, 4 450, 5 400/
                 costi(i) /1 1.5, 2 1.7/
                         fissi(i) /1 5000, 2 7000/
scalar scortei /0/
         scorte5 /100/
                 lb1 /0.15/;

table matrix(i, j)

         1       2       3       4       5
1        200     350     500     300     150
2        300     350     450     250     350;

binary variables

y(i);

positive variables

x(i, j)
z(j);

variables
FO;

equations
vinc_1
vinc_capacita (i, j)
vinc_domanda (j)
vinc_magazzino (j)
vinc_bilanciamento (j)
obiettivo ;

vinc_1.. sum(i, y(i)) =E= 1;
vinc_capacita (i, j).. x(i, j) =L= matrix(i, j) * y(i);
vinc_domanda (j).. sum(i, x(i, j)*y(i))+z(j) =G= domanda(j)+z(j);
vinc_bilanciamento (j).. sum(i, x(i, j)+z(j)) =G= domanda(j);
vinc_magazzino (j).. z(j) =L= 350;
obiettivo.. FO =E= sum(i, x(i, j)*y(i)*costi(i)) + sum(i, y(i)*fissi(i) + z(j)*0.5);

model es9_2 /ALL/;


Solve es9_2 Minimizing FO using MIP;

display x.l, y.L, z.L FO.L;


