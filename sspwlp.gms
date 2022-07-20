set
   i 'plant' /1*10/
   j 'warehouse' /1*10/
   k 'market' /1*10/
;

parameter d(k) demand at market 'k'
$call GDXXRW gams.xlsx par=d rng=Sheet1!a2:b51  cdim=0 rdim=1
$GDXIN gams.gdx
$Load d
$GDXIN

parameter capp(i) capacity of plant 'i'
$call GDXXRW gams.xlsx par=capp rng=Sheet1!a106:b155  cdim=0 rdim=1
$GDXIN gams.gdx
$Load capp
$GDXIN

parameter capw(j) capacity of warehouse 'j'
$call GDXXRW gams.xlsx par=capw rng=Sheet1!a66:b95  cdim=0 rdim=1
$GDXIN gams.gdx
$Load capw
$GDXIN


parameter cpw(i,j) cost of transporting sum of all market demand from plant 'i' to warehouse 'j'
$call GDXXRW gams.xlsx par=cpw rng=Sheet1!a158:ay208 rdim=1 cdim=1
$GDXIN gams.gdx
$Load cpw
$GDXIN

parameter cwm(j,k) cost of transporting sum of all market demand from warehouse 'j' to market 'm'
$call GDXXRW gams.xlsx par=cwm rng=Sheet1!a212:ay261 rdim=1 cdim=1
$GDXIN gams.gdx
$Load cwm
$GDXIN

parameter fp(i) location cost of plant 'i'
$call GDXXRW gams.xlsx par=fp rng=Sheet1!a264:b313  cdim=0 rdim=1
$GDXIN gams.gdx
$Load fp
$GDXIN

parameter fw(j) location cost of warehouse 'j'
$call GDXXRW gams.xlsx par=fw rng=Sheet1!a316:b365  cdim=0 rdim=1
$GDXIN gams.gdx
$Load fw
$GDXIN

variable
xpw(i,j)  quantity moved from plants i to warehouse j
xwm(j,k)  quantity moved from warehouse j to market k
yp(i)     location variable for plant 'i'
yw(j)     location variable for warehouse 'j'
z                  objective function for Optimal_Cost;

Binary variable yp,yw;
positive  variable xpw,xwm;

equations
objfn         objective function
pl_rel        plant relation
pl_rel_loc    plant relation with its location
ware_rel      warehouse relation
ware_rel_loc  warehouse relation with its location
pl_ware_rel1  plant and warehouse relation one
pl_ware_rel2  plant and warehouse relation two
mar_ware_rel  market and warehouse relation
pl_ware_loc   plant location relation
ware_pl_loc   warehouse location relation
flow_con1     flow constraint one
flow_con2     flow constraint two
flow_con3     flow constraint three
flow_con4     flow constraint four
flow_con5     flow constraint five
locp          presence of plant
cap_pl        capacity of plant
cap_wa        capacity of warehouse
demand        demand in market
locw          presence of warehouse

;




objfn..     z =e= sum((i,j),(cpw(i,j)*xpw(i,j))) + sum((j,k),(cwm(j,k)*xwm(j,k)))+ sum((i),(yp(i)*fp(i)))+sum((j),(yw(j)*fw(j)));
locp(i)..             yp(i) = 1  $ fp(i) =g= 1;
locw(j)..             yw(j)= 1  $ fw(j) =g= 1;
demand..              sum((k),d(k)) =e= 1;
cap_pl..              sum((i),capp(i)) =g= 1;
cap_wa..              sum((j),capw(j)) =g= 1;
pl_rel(i)..           sum((j),xpw(i,j)) =l= capp(i);
pl_rel_loc(i)..       sum((j),xpw(i,j)) =l= (capp(i)*yp(i));
ware_rel(j)..         sum((i),xpw(i,j)) =l= capw(j);
ware_rel_loc(j)..     sum((i),xpw(i,j)) =l= (capw(j)*yw(j));
pl_ware_rel1(i,j)..   xpw(i,j) =l= (yp(i)*capw(j));
pl_ware_rel2(i,j)..   xpw(i,j) =l= (yw(j)*capp(i));
mar_ware_rel(j,k)..   xwm(j,k) =l= (d(k)*yw(j));
pl_ware_loc(i)..      sum((j),xpw(i,j)) =l= yp(i);
ware_pl_loc(j)..      sum((i),xpw(i,j)) =l= yw(j);
flow_con1(j)..        sum((i),xpw(i,j)) =e= sum((k),xwm(j,k));
flow_con2..           sum((i),capp(i)*yp(i)) =g= 1;
flow_con3..           sum((j),capw(j)*yw(j)) =g= 1;
flow_con4(i,j)..      xpw(i,j) =g= 0;
flow_con5(j,k)..      xwm(j,k) =g= 0;




model project1 /objfn, demand, cap_pl, cap_wa, pl_rel, pl_rel_loc, ware_rel, ware_rel_loc,
                pl_ware_rel1, pl_ware_rel2,mar_ware_rel,pl_ware_loc,ware_pl_loc,
                flow_con1, flow_con2, flow_con3, flow_con4, flow_con5/;

solve project1 using mip minimizing z;

display z.l;







