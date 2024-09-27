set tipo /a, b, c, d, e, f, g/
         perczucc /canna, mais, barb/;
alias (tipo, j)
          (perczucc, i);

parameter c(j) /a 10, b 11, c 12, d 13, e 14, f 12, g 15/
                 b(i) /canna 52, mais 56, barb 59/
                         d(j) /a 1, b 5, c 7, d 10, e 5, f 8, g 10/;

table matrix(i, j)
         a       b       c       d       e       f       g
canna   0.1     0.10    0.20    0.30    0.40    0.20    0.60
mais    0.30    0.40    0.40    0.20    0.60    0.60    0.10
barb    0.60    0.50    0.40    0.50     0      0.20    0.30;

positive variables x(j)

variables FO;

equations
vincoli(i)
vincolo1(i)
vincolo2(j)
obiettivo;

vincoli(i).. sum(j, matrix(i, j)*x(j)) =E= b(i);
vincolo1(i).. sum(j, x(j)) =E= 167;
vincolo2(j).. x(j) =G= d(j);

obiettivo.. FO =E= sum(j, x(j)*c(j));

model zucchero /ALL/;


Solve zucchero Minimizing FO using LP;

display x.l;


