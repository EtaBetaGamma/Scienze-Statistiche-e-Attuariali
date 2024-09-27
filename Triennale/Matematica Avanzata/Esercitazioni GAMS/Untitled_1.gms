set alimenti /carne, legumi, pasta, olio/
         macronutriente /grassi, carbo, proteine/;
alias (alimenti, i)
         (macronutriente, j);
table matrix (i, j)
             carne       legumi    pasta         olio
grassi       2.6         1.5       1.5           100
carbo        0.0         60.7      74.7          0
proteine     20.2        22.3      13.0          0
;
parameter c(alimenti) /carne 110, legumi 337, pasta 371, olio 884 /
              b(macronutriente) /grassi 90, carbo 90, proteine 60/;
positive variables x(j);

variables FO;

equations
vincoli (i)
obiettivo;

vincoli(i).. sum(j, matrix(i,j)*x(j)) =L= b(j)

obiettivo.. FO =E= sum(iìj, c(i)*x(j))

model modelloatleta /ALL/;

Solve modelloatleta Maximizing FO using LP;

