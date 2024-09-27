set DMU /NAmerica, Europa, Asia, Africa, SAmerica/
         inputs /PD, Marketing/
                 output /Smart_Phone, Tablet, Laptop/;
table matrix1(DMU, inputs)

                 PD      Marketing
NAmerica         5       14
Europa           10      18
Asia             9       16
Africa           7       12
SAmerica         9       15;

table matrix2(DMU, output)

                 Smart_Phone     Tablet  Laptop
NAmerica         9               4       16
Europa           8               2       9
Asia             9               4       10
Africa           6               1       8
SAmerica         10              4       14;

positive variables
t(output)
w(inputs);

variables
FO;

equations
vinc_input(output)
vinc_pesi1(inputs, output)
obiettivo;

vinc_input(DMU).. sum(inputs, w(inputs)*matrix1(DMU, inputs))=E= 1;
vinc_pesi1(inputs, output).. sum(output, t(output) * matrix2(DMU, inputs)) - sum(inputs, w(inputs)*matrix1(DMU * inputs))=L= 0;
obiettivo.. FO =E= sum("NAmerica", t(output)*matrix1(DMU, inputs);

model dea1 /ALL/;


Solve dea1 Maximizing FO using LP;

display t.1, w.L;




