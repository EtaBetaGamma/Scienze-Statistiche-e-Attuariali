set alimenti /pane, latte, uova, carne, dolce/
         nutrienti /calorie, proteine, calcio/;
alias (alimenti, j)
         (nutrienti, i);
parameter costo(j) /pane 2, latte 3, uova 4, carne 19, dolce 20/
               porz(j) /pane 4, latte 8, uova 3, carne 2, dolce 2/
                 fabb(i) /calorie 2000, proteine 50, calcio 70/;

table matrix(i, j)

            pane latte   uova    carne   dolce
calorie     110  160     180     260     420
proteine    4    8       13      14      4
calcio      2    285     54      80      22;

positive variables x(j);

variables FO;

equations
vincolifabbisogno(i)
vincoliporz(j)
vincololatte(j)
vincolodolce(j)
vincolouovacarne(j)
obiettivo;

vincolifabbisogno(i).. sum(j, matrix(i,j)*x(j)) =G= fabb(i);
vincoliporz(j).. x(j) =L= porz(j);
vincololatte(j).. x("latte") =G= 2 ;
vincolodolce(j).. x("dolce") =G= 1   ;
vincolouovacarne(j).. x("uova")+x("carne") =L= 3;
obiettivo.. FO =E= sum(j, x(j)*costo(j))      ;

model cibo /ALL/;


Solve cibo Minimizing FO using LP;

display x.l;