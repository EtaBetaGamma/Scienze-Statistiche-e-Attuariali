set clienti /1, 2, 3, 4/
         stabilimento/ n, r, c/;

alias (clienti, i)
         (stabilimento, j);

parameter   rich(i) /1 4000, 2 3400, 3 2700, 4 3500/
                 costo(j) /n 45, r 65, c 37/
                         capacita(j) /n 7500, r 4800, c 6500/;


positive variables x(j);

variables FO;

equations

vdomanda
vprodNapoli1
vprodNapoli2
vcapacita(j)
obiettivo;

vdomanda.. sum(j, x(j)) =E= sum(i, rich(i));
vprodNapoli1.. x("N") =G= 1/2 * x("r");
vprodNapoli2.. x("N") =G= 1/2 * x("c");
vcapacita(j)..  x(j) =L= capacita(j);
obiettivo.. FO =E= sum(j, x(j)*costo(j));

model esercizio34 /ALL/;


Solve esercizio34 Minimizing FO using LP;

display x.l;

