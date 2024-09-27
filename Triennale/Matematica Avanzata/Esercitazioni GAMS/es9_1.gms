set amici/ antonio, gianluca, anna, domenico, giusy, carlo, claudia/
         pasti /antipasto, primo, secondo, contorno, dolce/;
alias (amici, i)
         (pasti, j);

table matrix(i, j)
                 antipasto       primo   secondo contorno        dolce
antonio          7               6       5       7               8
gianluca         6               8       7       6               5
anna             6               5       6       7               8
domenico         7               8       6       6               6
giusy            5               6       7       8               6
carlo            7               8       5       6               8
claudia          8               5       5       6               7;

binary variables
x(i, j)
;
variables
FO;

equations
vinc_unaportata (j)
vinc_AG1
vinc_Car1
vinc_Car2
vinc_Car3
vinc_Ant
obiettivo;

vinc_Ant.. x("antonio", "dolce") =E= 0;
vinc_Car1.. x("carlo", "antipasto") =E= 0;
vinc_Car2.. x("carlo", "secondo") =E= 0   ;
vinc_Car3.. x("carlo", "contorno") =E= 0   ;
vinc_AG1.. x("anna", "secondo") + x("giusy","contorno") =L= 1;
vinc_unaportata(j).. sum(i, x(i, j)) =E= 1  ;
obiettivo.. FO =E= sum(i,sum(j, x(i,j) * matrix(i, j)));

model es91 /ALL/;


Solve es91 Maximizing FO using MIP;

display x.l, FO.L;



