set stoccaggio /mar, apr, mag/;

alias (stoccaggio, i);
parameters costo(i) /mar 200, apr 300, mag 400/
                 capacita(i) /mar 160, apr 150, mag 140/
                       domanda(i) /mar 100, apr 130, mag 150/
                         magmax(i) /mar 100, apr 100, mag 50/
                             mag(i) /mar 42, apr i("mar"), mag i("apr")/;

positive variables
x(i);

variables
FO;

equations
magmaggio
vmagmax(i)
vcap(i)
vmag(i)
obiettivo;

magmaggio.. i("mag")=50;
vcap(i).. x(i) =L= capacita(i);
vmagmax.. i =L= magmax(i);


