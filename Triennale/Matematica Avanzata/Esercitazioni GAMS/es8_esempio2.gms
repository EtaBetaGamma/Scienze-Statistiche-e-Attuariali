set siti /1, 2, 3, 4, 5, 6/
         quartieri /1, 2, 3, 4, 5, 6, 7, 8/
alias (siti, i)
         (quartieri, j)

table matrix(i,j)
         1       2       3       4       5       6       7       8
1        2.1     1.7     2.8     0.3     0.8     2.2     1.8     0.7
2        1.5     2.2     3.1     2.2     0.2     1.9     2.3     1.3
3        0.9     1.6     2.3     0.3     1.7     1.6     0.9     2.7
4        1.8     3.1     2.7     2.6     3.1     0.6     0.2     0.7
5        0.1     2.5     1.8     3.1     0.4     1.2     0.7     1.1
6        0.5     1.4     3.1     0.5     0.2     1.5     2.2     0.8;

binary variables

y(i)
x(i, j)

Variables
FO

equations
servizio_quartieri (j)
sportelli
apertura_siti (i, j)
obiettivo;

servizio_quartieri (j).. sum(i, x(i, j)) =E= 1;
sportelli.. sum(i, y(i)) =E= 2;
apertura_siti(i, j).. x(i, j) =L= y(i);
obiettivo.. FO =E= sum(i,sum(j, x(i,j) * matrix(i, j)));

model esercizio8esempio2 /ALL/;


Solve esercizio8esempio2 Minimizing FO using MIP;

display x.l, y.L, FO.L;





