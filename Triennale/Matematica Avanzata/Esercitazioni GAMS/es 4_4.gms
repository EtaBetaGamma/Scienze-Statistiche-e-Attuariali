set reparti /1, 2, 3/
         frigo /a, b, c, d/;
alias (reparti, i)
         (frigo, j);
parameter operai(i) /1 40, 2 30, 3 30/
                 prezzo(j) /a 150, b 100, c 70, d 50/;

table matrix(i, j)
         a       b       c       d
1        1       1       2       1
2        2       2       1       3
3        1       0.5     1       2;

positive variable x(i,j);

variable FO;

equations

vincoloproduzione1
vincoloproduzione2
vincolooperai1(i)
obiettivo(i);

vincoloproduzione1.. sum(i, x(i, "B")) =L= sum(i,sum(j, x(i,j)))*0.5;
vincoloproduzione2.. sum(i, x(i, "B")) =G= sum(i,sum(j, x(i,j)))*0.25;
vincolooperai1(i).. sum(j, x(i,j)*matrix(i,j)) =L= operai(i)*8*5;
obiettivo(i).. FO =E= sum(j, x(i,j)*prezzo(j));

model esercizio44 /ALL/;


Solve esercizio44 Maximizing FO using LP;

display x.l;




