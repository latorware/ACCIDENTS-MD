
setwd("/home2/users/alumnes/1251863/Downloads")
dd <- read.csv("Accidents5000Reduit.csv", header=T);

table(dd$zona)
table(dd$dat)
table(dd$nomCom)
table(dd$nomDem)
table(dd$F_MORTS)
table(dd$F_FERITS_GREUS)
table(dd$F_FERITS_LLEUS)
table(dd$F_UNITATS_IMPLICADES)
table(dd$F_VIANANTS_IMPLICADES)
table(dd$F_BICICLETES_IMPLICADES)
table(dd$VEHICLES_MOTOR)
table(dd$C_VELOCITAT_VIA)
  dd$C_VELOCITAT_VIA[dd$C_VELOCITAT_VIA==999 |dd$C_VELOCITAT_VIA==0 ]  <- "NA" 
table(dd$D_ACC_AMB_FUGA)
  dd$D_ACC_AMB_FUGA[dd$D_ACC_AMB_FUGA=="Sense Especificar" ]  <- "NA" 
table(dd$D_CLIMATOLOGIA)
  dd$D_CLIMATOLOGIA[dd$D_CLIMATOLOGIA=="Sense especificar" ]  <- "NA" 
table(dd$)
table(dd$)
table(dd$)
table(dd$)
table(dd$)
table(dd$)
table(dd$)
table(dd$)
table(dd$)
table(dd$)
table(dd$)






options(max.print=1000000)


dd$C_VELOCITAT_VIA[dd$C_VELOCITAT_VIA==999 |dd$C_VELOCITAT_VIA==0 ]  <- "NA" 

write.csv(accidents,"C:\\Users\\simon\\Downloads\\Accidents5000.csv", row.names = TRUE)