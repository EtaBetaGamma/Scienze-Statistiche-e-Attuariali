set alimenti /carne, legumi, pasta, olio/
         macronutriente /grassi, carbo, proteine/;
alias (alimenti, i)
         (macronutriente, j);

parameter c(alimenti) /carne 110, legumi 337, pasta 371, olio 884 /
              b(macronutriente) /grassi 90, carbo 90, proteine 60/;
table matrix (j, i)
             carne       legumi    pasta         olio
grassi       2.6         1.5       1.5           100
carbo        0.0         60.7      74.7          0
proteine     20.2        22.3      13.0          0
;

positive variables x(i);

variables FO;

equations
vincoli (j)
obiettivo;

vincoli(j).. sum(i, matrix(j, i)*x(i)) =G= b(j);

obiettivo.. FO =E= sum(i, c(i)*x(i));

model modelloatleta /ALL/;

Solve modelloatleta Minimizing FO using LP;

display x.l;
