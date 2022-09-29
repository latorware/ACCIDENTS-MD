accidents <- read.csv("C:\\Users\\simon\\Downloads\\Accidents_de_tr_nsit_amb_morts_o_ferits_greus_a_Catalunya.csv")

while(nrow(accidents) > 5000) {
  randIndex = as.integer(runif(1, min = 0, max = nrow(accidents)-1))
  accidents2 <- accidents[-c(randIndex), ]
  print(nrow(accidents))
}

write.csv(accidents,"C:\\Users\\simon\\Downloads\\Accidents5000.csv", row.names = TRUE)



viaSE <- accidents[which(accidents$via == "SE"),names(accidents) %in% c("via")]
pk99 <- accidents[which(accidents$pk == "999999"),names(accidents) %in% c("pk")]
boira <- accidents[which(accidents$D_BOIRA == "Si"),names(accidents) %in% c("D_BOIRA")]


orig <- read.csv("C:\\Users\\simon\\Downloads\\Accidents_de_tr_nsit_amb_morts_o_ferits_greus_a_Catalunya.csv")

table(accidents$Any)/nrow(accidents)
table(orig$Any)/nrow(orig)
