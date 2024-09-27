
set siti / 1, 2, 3, 4/
         zone /1, 2, 3, 4, 5/;
alias (siti, i)
         (zone, j);

scalar ognizona /1/
         minsegnale /15/;


table matrix(i, j)

         1       2       3       4       5
1        25      17      28      4       12
2        10      20      21      9       8
3        8       2       4       23      25
4        13      13      4       12      15;

binary variables
y(i)
x(i, j);

variables
FO;

equations

vinc_zona(j)
vinc_segnalemin
vinc_segnalemax1
vinc_segnalemax2
vinc_34
vinc_maxzone(i)
obiettivo;

vinc_zona(j).. sum(i, x(i,j)) =G= ognizona;
vinc_segnalemin.. sum(i, sum(j, x(i,j)*matrix(i,j))) =G= minsegnale;
vinc_segnalemax1.. x("1", "2") + x("2", "2") =L= 1;
vinc_segnalemax2.. x("1", "3") + x("2", "3") =L= 1;
vinc_34.. y("4") =L= y("3");
vinc_maxzone(i).. sum(j, x(i,j)) =L= 5*y(i);
obiettivo.. FO =E= sum(i, sum(j, x(i, j)*matrix(i, j)));

model es9_3 /ALL/;


Solve es9_3 Maximizing FO using MIP;

display x.l, y.L, FO.L;





