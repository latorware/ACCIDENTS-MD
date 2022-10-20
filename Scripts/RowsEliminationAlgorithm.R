setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
accidents <- read.csv("AccidentsOriginal(greus-mortals).csv")

while(nrow(accidents) > 5000) {
  randIndex = as.integer(runif(1, min = 0, max = nrow(accidents)-1))
  accidents2 <- accidents[-c(randIndex), ]
  print(nrow(accidents))
}

write.csv(accidents,"Accidents5000Reduit.csv", row.names = TRUE)

